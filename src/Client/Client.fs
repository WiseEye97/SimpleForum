module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Thoth.Json

open Shared
open ViewUtil.NavHelper
open ViewUtil

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server

type SubModel =
    | Login of Login.Model
    | Sign of Sign.Model
    | BlogWriter of WriteBlog.Model
    | Blog of Blog.Model
    | Home

type Model = {
    isLogged : bool
    subModel : SubModel
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| Ignore
| SignMsg of Sign.Msg
| LoginMsg of Login.Msg
| BlogWriterMsg of WriteBlog.Msg
| BlogMsg of Blog.Msg
| InitSign
| InitLogin
| InitBlog
| InitForum
| InitBlogWriter


// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
   {isLogged = false;subModel = Home},[]



// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel, msg with
    | _ , InitBlog ->
        let blogModel,blogCmd = Blog.init()
        {currentModel with subModel = Blog blogModel},Cmd.map BlogMsg blogCmd
    | _ , InitLogin ->
        let loginModel,loginCmd = Login.init()
        {currentModel with subModel = Login loginModel},Cmd.map LoginMsg loginCmd
    | _ , InitSign -> 
        let signModel,signCmd = Sign.init()
        {currentModel with subModel = Sign signModel},Cmd.map SignMsg signCmd
    | _ , InitBlogWriter ->
        let writerModel,writerCmd = WriteBlog.init()
        {currentModel with subModel = BlogWriter writerModel},Cmd.map BlogWriterMsg writerCmd
    | {subModel = Blog bg},BlogMsg m ->
        let blogModel,blogCmd = Blog.update m bg
        {currentModel with subModel = Blog blogModel},Cmd.map BlogMsg blogCmd
    | {subModel = BlogWriter writerModel},BlogWriterMsg m ->
        let writerModel,writerCmd = WriteBlog.update m writerModel
        {currentModel with subModel = BlogWriter writerModel},Cmd.map BlogWriterMsg writerCmd
    | {subModel = Sign signModel},SignMsg msg ->
        let signModel,signCmd = Sign.update msg signModel
        {currentModel with subModel = Sign signModel},Cmd.map SignMsg signCmd
    |  {subModel = Login loginModel},LoginMsg (Login.Msg.Logged {state = true;errorMessage = None}) -> 
        {currentModel with isLogged = true},[]
    | {subModel = Login loginModel},LoginMsg msg ->
        let loginModel,loginCmd = Login.update msg loginModel
        {currentModel with subModel = Login loginModel},Cmd.map LoginMsg loginCmd
    | _ , _ -> currentModel,[]

module MainLayout =
    let navbar {isLogged = isLogged} dispatch =
        renderNavbar
            [
                match isLogged with
                | true -> 
                    yield! [
                        Link {isActive = false;text = LinkText "LogOut";action = (fun _ -> dispatch InitSign);location = End}
                        Link {isActive = false;text = LinkText "Forum";action = (fun _ -> dispatch InitForum);location = Start}
                        Link {isActive = false;text = LinkText "Write Blog";action = (fun _ -> dispatch InitBlogWriter);location = Start}
                    ]
                | false ->
                    yield! [
                        Link {isActive = false;text = LinkText "Sign Up";action = (fun _ -> dispatch InitSign);location = End}
                        Link {isActive = false;text = LinkText "Log In";action = (fun _ -> dispatch InitLogin);location = End}
                    ]
                yield Link {isActive = false;text = LinkText "Blog";action = (fun _ -> dispatch InitBlog);location = Start}
                yield Link {isActive = true;text = LinkText "About me";action = (fun _ -> ());location = Start}
            ]
            "https://upload.wikimedia.org/wikipedia/en/thumb/d/d5/Fsharp%2C_Logomark%2C_October_2014.svg/1200px-Fsharp%2C_Logomark%2C_October_2014.svg.png"

        
    let renderBody dispatch = 
        function
        | {subModel = Sign signModel} ->
            Sign.view signModel (SignMsg >> dispatch)
        | {subModel = Login loginModel} ->
            Login.view loginModel (LoginMsg >> dispatch)
        | {subModel = BlogWriter bg} ->
            WriteBlog.view bg (BlogWriterMsg >> dispatch)
        | {subModel = Blog bg} ->
            Blog.view bg (BlogMsg >> dispatch)
        | _ -> div [] []


let view (model : Model) (dispatch : Msg -> unit) =
    div [] [
        MainLayout.navbar model dispatch
        MainLayout.renderBody dispatch model
    ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
