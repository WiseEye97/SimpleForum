module WriteBlog

open Elmish
open ViewUtil


type TextType =
    | Normal
    | Code

type TextSection = {
    content : string
    tp : TextType
}

type Model = {
    sections : TextSection list
    currentBuffer : TextSection option
}

type Msg = 
    | TextChanged of string
    | TypeChanged of TextType
    | AddSection
    | NewSection
    | AddArticle

let init()  =
    {sections = [];currentBuffer = None},Cmd.none

let update (msg : Msg) (model:Model) =
    match msg,model with
    | TextChanged s,{currentBuffer = Some x} ->
        {model with currentBuffer = Some {x with content = s}},Cmd.none
    | TypeChanged t,{currentBuffer = Some x} ->
        {model with currentBuffer = Some {x with tp = t}},Cmd.none
    | AddSection,{sections = ss; currentBuffer = Some x} ->
        {model with sections = x :: ss;currentBuffer = None},Cmd.none
    | NewSection,{currentBuffer = None} ->
        {model with currentBuffer = Some {content = "";tp = Normal}}, Cmd.none  
    | _ ->
        model,[]

let view  (model : Model) (dispatch : Msg -> unit) =
    let p1 = 
        match model with
        | {currentBuffer = Some _} ->
            [
                TextArea {
                    onChange = (fun s ->
                        s |> TextChanged |> dispatch
                    )
                }
                RadioInput {
                    name = RadioName "CodeType"
                    values = [
                        {value = "Code";onSelect = (fun _ -> Code |> TypeChanged |> dispatch)}
                        {value = "Normal";onSelect = (fun _ -> Normal |> TypeChanged |> dispatch)}
                    ]
                }
                ButtonWithIcon {
                    iconName = IconType "fas fa-check"
                    onClick = (fun _ -> dispatch AddSection)
                }
            ]
        | {currentBuffer = None} ->
            [
                ButtonWithIcon {
                    iconName = IconType "fas fa-plus"
                    onClick = (fun _ -> dispatch NewSection)
                }
            ]
        |> Seq.map (fun x -> x,None)

    renderForm p1 (fun _ -> dispatch AddArticle) false