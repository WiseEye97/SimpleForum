module Login

open Elmish
open ViewUtil
open Shared.ServerMessages
open Shared.ClientMessages

type LoginState = 
    | Idle
    | Logging

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
    | Logged {state = false;errorMessage = Some er},_ ->
        model,[]
    | Logged {state = true;errorMessage = Some InternalServError},_ ->
        model,[]
    
let view (model : Model) (dispatch : Msg -> unit) =

    let renderBody isL = 
        renderForm [
            TextInput {
                onchange = Login >> InputChanged >> dispatch
                labeltext = (LabelText "Username")
                isPassword = false
                placeholder = (PlaceHolder "Insert username here...")
                icon = None
            }, None
            TextInput {
                onchange = Password >> InputChanged >> dispatch
                labeltext = (LabelText "Password")
                isPassword = true
                placeholder = (PlaceHolder "Insert Password here...")
                icon = None
            }, None
        ] (fun () -> dispatch Signed) isL

    match model.loginState with
    | Idle -> renderBody false
    | Logging -> renderBody true
