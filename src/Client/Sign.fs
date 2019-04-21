module Sign

open Elmish
open ViewUtil
open Fable.Import.Browser
open Shared
open Shared.UserModel
open Shared.ServerMessages

type IsValidInput =
    | Valid
    | InValid

type UserInput = {
    input : string
    validation : IsValidInput
} with
    static member Init() =
        {input = "";validation = InValid}
    member this.UpdateInput inp v =
        {input = inp;validation = v} 


type SignState =
    | Idle
    | Waiting
    | SignedOk
    | SignedFailed of SignErrors


type InputState = {
    userBuffer : UserInput
    mailBuffer : UserInput
    passwordBuffer : UserInput
} with
    static member Init() =
        {userBuffer = UserInput.Init();mailBuffer = UserInput.Init();passwordBuffer =  UserInput.Init()}

    member this.UpdateP inp v = {this with passwordBuffer = this.passwordBuffer.UpdateInput inp v}
    member this.UpdateU inp v = {this with userBuffer = this.userBuffer.UpdateInput inp v}
    member this.UpdateE inp v = {this with mailBuffer = this.mailBuffer.UpdateInput inp v}

type Model = {
    userModel : User option
    inputState : InputState
    signedState : SignState
}

type InputMessages = 
    | UserNameChanged of string
    | EmailChanged of string
    | PasswordChanged of string

type ServerMessages = 
    | ServerResponse of ServerMessages.SignInResponse
    | Err of exn

type Msg = 
    | Ignore
    | InputMessages of InputMessages 
    | ServerMessages of ServerMessages
    | OnSigned
    | CloseModal

let init() =
    {signedState = Idle;userModel = None;inputState = InputState.Init()},Cmd.none

let (|ValidUsr|_|) = function | (UserNameChanged newVal) -> createUserName newVal |> (function | Some x -> x.GetName() |> Some | _ -> None) | _ -> None
let (|ValidEmail|_|) = 
    function 
    | (EmailChanged newVal) -> createEmail newVal |> (function | Some x -> x.GetStringEmail() |> Some | _ -> None)
    | _ -> None

let (|ValidPswd|_|) = function | (PasswordChanged p) -> createPassword p |> (function | Some x -> x.GetPswd() |> Some | _ -> None) | _ -> None

let (|IsValidUser|_|) {inputState = ii} =
    ii |>
    function
    | {passwordBuffer = {validation = Valid} as p;userBuffer = {validation = Valid} as u;mailBuffer = {validation = Valid} as e} as md -> 
        let usr,em,pswd = createUserName u.input ,createEmail e.input ,createPassword p.input
        Some (User.CreateUser usr.Value em.Value pswd.Value)
    | _ -> None

module Sender =
    
    let sendSign =
        function
        | {username = usr;email = em;password = pswd} ->
            let message = {ClientMessages.SignInMessage.username = usr.GetName();ClientMessages.SignInMessage.email = em.GetStringEmail();ClientMessages.SignInMessage.password = pswd.GetPswd()}

            PromiseSender.sendGenericJson<ServerMessages.SignInResponse,Msg> message 3 Decoders.userErrosDecoder.signinDecoder "/api/register" (ServerResponse>>ServerMessages) (Err >> ServerMessages)
            |> Some


let update (msg : Msg) (model:Model) =

    let inputUpdate (msg : InputMessages) (model:Model) =
        match msg,model with
        | ValidPswd p, _ ->
        {model with inputState = model.inputState.UpdateP p Valid},Cmd.none
        | ValidUsr usr,_ ->
            {model with inputState = model.inputState.UpdateU usr Valid},Cmd.none
        | ValidEmail em,_ ->
            {model with inputState = model.inputState.UpdateE em Valid},Cmd.none
        | UserNameChanged newVal,_ ->
            {model with inputState = model.inputState.UpdateU newVal InValid},Cmd.none
        | EmailChanged newVal,_ ->
            {model with inputState = model.inputState.UpdateE newVal InValid},Cmd.none
        | PasswordChanged newVal,_ ->
            {model with inputState = model.inputState.UpdateP newVal InValid},Cmd.none

    let serverUpdate (msg : ServerMessages) (model:Model) =
        match msg,model with
        | ServerResponse {state = true},_ ->
            {model with signedState = SignedOk},[]
        | ServerResponse {state = false;errorMessage = Some er},_ ->
            console.log(er)
            {model with signedState = SignedFailed er},[]
        | Err ex,_ ->
            console.log(ex)
            model,[]
        | ServerResponse {state = false;errorMessage = None},_ ->
            model,[]

    console.log(msg)
    match msg,model with
    | InputMessages m,_ ->
        inputUpdate m model
    | ServerMessages m,_ ->
        serverUpdate m model
    | Ignore,_ -> model,Cmd.none
    | OnSigned,IsValidUser user ->
        let r = Sender.sendSign user 
        console.log(r)
        {model with signedState = Waiting},r.Value
    | OnSigned,_ ->
        model,[]
    | CloseModal,_ ->
        {model with signedState = Idle},[]




let view (model : Model) (dispatch : Msg -> unit) =
    let getValidator onvalid oninvalid =
        function
        | {validation = InValid} ->
            {isValid = false;onValid = onvalid;onInValid = oninvalid}
        | {validation = Valid} ->
            {isValid = true;onValid = onvalid;onInValid = oninvalid}

    let frm isL = 
        renderForm [
            TextInput {
                onchange = UserNameChanged >> InputMessages >> dispatch
                labeltext = (LabelText "Username")
                isPassword = false
                placeholder = (PlaceHolder "Insert username here...")
                icon = None
            } , Some (getValidator "Name is valid" "Name has to be at least 6 chars long" model.inputState.userBuffer)
            TextInput {
                onchange = EmailChanged >> InputMessages >> dispatch
                labeltext = (LabelText "Email")
                isPassword = false
                placeholder = (PlaceHolder "Insert your e-mail here...")
                icon = None
            }, Some (getValidator  "Valid Email" "Wrong Email" model.inputState.mailBuffer)
            TextInput {
                onchange = PasswordChanged >> InputMessages >> dispatch
                labeltext = (LabelText "Type password")
                isPassword = true
                placeholder = (PlaceHolder "password")
                icon = None
            } , Some (getValidator "Password is valid" "Password has to contain at leas one Upper letter and one digit" model.inputState.passwordBuffer)
        ] (fun () -> dispatch OnSigned) isL

    let renderBdy = 
        function
        | {signedState = Idle} -> frm false
        | {signedState = Waiting} -> frm true
        | {signedState = SignedOk} -> signedOkPage (fun () -> dispatch CloseModal)
        | {signedState = SignedFailed reason} -> signedFailedPage reason (fun () -> dispatch CloseModal)
        
    renderBdy model


    