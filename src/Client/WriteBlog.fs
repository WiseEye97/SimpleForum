module WriteBlog

open Elmish
open ViewUtil
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser

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
    | TextChanged s,{currentBuffer = Some ({content = _} as y)} ->
        {model with currentBuffer = Some {y with content = s}},Cmd.none
    | TypeChanged TextType.Normal,{currentBuffer = Some {content = x}} ->
        {model with currentBuffer = Some {content = x;tp = TextType.Normal}},Cmd.none
    | TypeChanged TextType.Code,{currentBuffer = Some {content = x}} ->
        {model with currentBuffer = Some {content = x;tp = TextType.Code}},Cmd.none
    | AddSection,{sections = ss; currentBuffer = Some x} ->
        {model with sections = x :: ss;currentBuffer = None},Cmd.none
    | NewSection,{currentBuffer = None} ->
        {model with currentBuffer = Some {content = "";tp = TextType.Normal}}, Cmd.none  
    | _ ->
        model,[]

let view  (model : Model) (dispatch : Msg -> unit) =
    let sections = 
        model.sections
        |> List.rev
        |> List.map (function
            | {content = c;tp = Normal} -> BlogParser.renderNormal c
            | {content = c;tp = Code} -> BlogParser.renderCode c
        )
        
    let high : unit -> unit = import "parseCode" "./highlightCode.js" 
    high()

    let p1 = 
        match model with
        | {currentBuffer = Some x} ->
            [
                TextArea {
                    onChange = (fun s ->
                        s |> TextChanged |> dispatch
                    )
                }

                RadioInput {
                    name = RadioName "CodeType"
                    values = [
                        {isSelected = x.tp = Code;value = "Code";onSelect = (fun _ -> Code |> TypeChanged |> dispatch)}
                        {isSelected = x.tp = TextType.Normal;value = "Normal";onSelect = (fun _ -> TextType.Normal |> TypeChanged |> dispatch)}
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
    |> BlogParser.renderBlogWriter sections