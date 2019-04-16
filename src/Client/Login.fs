module Login

open Elmish
open ViewUtil
open Fable.Import.Browser
open Shared.ServerMessages
open Shared.ClientMessages

type Inp = 
        | Nick
        | Pswd

type LoginState = 
    | Idle
    | Logging
    | LoginFailed of LoginErrors

type InputChanged =
    | Login of string
    | Password of string

type Msg =
    | InputChanged of InputChanged
    | Signed
    | Logged of LogInResponse
    | Err of exn

type Model = {
    nickBuffer : string
    passwordBuffer : string
    loginState : LoginState
}

let init() =
    {loginState = Idle;nickBuffer = "";passwordBuffer = ""},Cmd.none

let update (msg : Msg) (model:Model) =
    match msg,model with
    | InputChanged (Login s),_ ->
        {model with nickBuffer = s},Cmd.none
    | InputChanged (Password s),_ ->
        {model with passwordBuffer = s},Cmd.none
    | Signed, _ ->  
        {model with loginState = Logging},PromiseSender.sendGenericJson<LogInResponse,Msg> {username = model.nickBuffer;password = model.passwordBuffer} 2 Decoders.userErrosDecoder.loginDecoder "/api/login" Logged Err
    | Logged {state = true;errorMessage = None},_ ->
        model,[]
    | Logged {state = _;errorMessage = Some er},_ ->
        {model with loginState = LoginFailed er},[]
    | Logged {state = _;errorMessage = None},_ ->
        {model with loginState = LoginFailed InternalServError},[]
    | Err er,_ ->
        console.log(er)
        {model with loginState = LoginFailed InternalServError},[]
    
let view (model : Model) (dispatch : Msg -> unit) =
   
    let getValidator onvalid oninvalid =
        function
        | Nick,{loginState = LoginFailed WrongNick} ->
            {isValid = false;onValid = onvalid;onInValid = oninvalid}
        | Pswd,{loginState = LoginFailed WrongPassword} ->
            {isValid = false;onValid = onvalid;onInValid = oninvalid}
        | _ , _ ->
            {isValid = true;onValid = onvalid;onInValid = oninvalid}

    let formBody = 
        [
            TextInput {
                onchange = Login >> InputChanged >> dispatch
                labeltext = (LabelText "Username")
                isPassword = false
                placeholder = (PlaceHolder "Insert username here...")
                icon = None
            }, Some (getValidator "" "Wrong username" (Nick,model))
            TextInput {
                onchange = Password >> InputChanged >> dispatch
                labeltext = (LabelText "Password")
                isPassword = true
                placeholder = (PlaceHolder "Insert Password here...")
                icon = None
            }, Some (getValidator "" "Wrong password" (Pswd,model))

        ], (fun () -> dispatch Signed)

    let renderBody isL = 
        formBody ||> (fun x y -> renderForm x y isL) 

    match model.loginState with
    | Idle -> renderBody false
    | Logging -> renderBody true
    | LoginFailed AccountNotConfirmed -> formBody ||> (fun x y -> renderFormWithWarning x y false "You need to confirm your account first!") 
    | LoginFailed InternalServError -> formBody ||> (fun x y -> renderFormWithWarning x y false "Something bad happened sorry!")
    | _ -> renderBody false
