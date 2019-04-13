module Sign

open Elmish
open Elmish.React
open UserModel
open ViewUtil
open Fable.Import.Browser
open Fable.PowerPack.Fetch
open Fable.PowerPack
open Shared
open Shared.UserModel

type IsValidInput =
    | Valid
    | InValid

type UserInput = {
    input : string
    validation : IsValidInput
}

type SignState =
    | Idle
    | Waiting
    | SignedOk
    | SignedFailed

type Model = {
    userModel : User option
    userBuffer : UserInput
    mailBuffer : UserInput
    passwordBuffer : UserInput
    signedState : SignState
}

type Msg = 
    | Ignore
    | UserNameChanged of string
    | EmailChanged of string
    | PasswordChanged of string
    | ServerResponse of ServerMessages.SignInResponse
    | Err of exn
    | OnSigned

let init() =
    {passwordBuffer = {input = "";validation = InValid};signedState = Idle;userModel = None;userBuffer = {input = "";validation = InValid};mailBuffer = {input = "";validation = InValid}},Cmd.none

let (|ValidUsr|_|) = function | (UserNameChanged newVal) -> createUserName newVal | _ -> None
let (|ValidEmail|_|) = function | (EmailChanged newVal) -> createEmail newVal | _ -> None
let (|ValidPswd|_|) = function | (PasswordChanged p) -> createPassword p | _ -> None

let (|IsValidUser|_|) = 
    function
    | {passwordBuffer = {validation = Valid} as p;userBuffer = {validation = Valid} as u;mailBuffer = {validation = Valid} as e} as md -> 
        let usr,(Some (Email.Valid em)),pswd = createUserName u.input , createEmail e.input ,createPassword p.input
        Some (User.CreateUser usr.Value em pswd.Value)
    | _ -> None

module Sender =
    let sendSign =
        function
        | {username = usr;email = (Email.Valid em);password = pswd} ->

            let message = {ClientMessages.SignInMessage.username = usr.GetName();ClientMessages.SignInMessage.email = em.GetStringEmail();ClientMessages.SignInMessage.password = pswd.GetPswd()}

            let defProps = [
                    RequestProperties.Method HttpMethod.POST
                    Fetch.requestHeaders [HttpRequestHeaders.ContentType "application/json"]
                    RequestProperties.Body (unbox(Thoth.Json.Encode.Auto.toString(3, message)))
            ]

            let cmd = promise{
                let! res = Fetch.fetchAs<ServerMessages.SignInResponse> "/api/register" (Thoth.Json.Decode.Auto.generateDecoder()) defProps
                return res
            } 
            
            Cmd.ofPromise (fun _ -> cmd) () ServerResponse Err |> Some

        | _ -> None


let update (msg : Msg) (model:Model) = 
    match msg,model with
    | Ignore,_ -> model,Cmd.none
    | OnSigned,IsValidUser user ->
        let r = Sender.sendSign user 
        {model with signedState = Waiting},r.Value
    | OnSigned,_ ->
        model,[]
    | ValidPswd p, _ ->
        {model with passwordBuffer = {input = p.GetPswd();validation = Valid}},Cmd.none
    | ValidUsr usr,_ ->
        {model with userBuffer = {input = usr.GetName();validation = Valid}},Cmd.none
    | ValidEmail em,_ ->
        {model with mailBuffer = {input = (em.GetMail()).Value;validation = Valid}},Cmd.none
    | UserNameChanged newVal,_ ->
        {model with userBuffer = {input = newVal;validation = InValid}},Cmd.none
    | EmailChanged newVal,_ ->
        {model with mailBuffer = {input = newVal;validation = InValid}},Cmd.none
    | PasswordChanged p,_ ->
        {model with userBuffer = {input = p;validation = InValid}},Cmd.none
    | ServerResponse {state = true},_ ->
        {model with signedState = SignedOk},[]
    | ServerResponse {state = false},_ ->
        {model with signedState = SignedFailed},[]



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
                onchange = UserNameChanged >> dispatch
                labeltext = (LabelText "Username")
                isPassword = false
                placeholder = (PlaceHolder "Insert username here...")
                icon = None
            } , Some (getValidator "Name is valid" "Name is invalid" model.userBuffer)
            TextInput {
                onchange = EmailChanged >> dispatch
                labeltext = (LabelText "Email")
                isPassword = false
                placeholder = (PlaceHolder "Insert your e-mail here...")
                icon = None
            }, Some (getValidator  "Email avaialable" "Wrong Email" model.mailBuffer)
            TextInput {
                onchange = PasswordChanged >> dispatch
                labeltext = (LabelText "Type password")
                isPassword = true
                placeholder = (PlaceHolder "password")
                icon = None
            } , Some (getValidator "Password is valid" "Password is invalid" model.passwordBuffer)
        ] (fun () -> dispatch OnSigned) isL

    let renderBdy = 
        function
        | {signedState = Idle} -> frm false
        | {signedState = Waiting} -> frm true
        | {signedState = SignedOk} -> signedOkPage()
        | {signedState = SignedFailed} -> signedFailedPage()
        
    renderBdy model


    