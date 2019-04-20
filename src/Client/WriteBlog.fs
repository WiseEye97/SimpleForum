module WriteBlog

open Elmish
open ViewUtil
open Fable.Core.JsInterop
open Fable.Import.Browser
open PromiseSender
open Shared.ClientMessages
open Shared.ServerMessages
open Thoth.Json
open Fable.Import.RemoteDev
open Decoders.userErrosDecoder

type DropDownState = {
    isActive : bool
    allCategories : string list
    category : string option
}


type Model = {
    title : string
    sections : TextSection list
    currentBuffer : TextSection option
    dropD : DropDownState
    newCategory : string
}

type Msg = 
    | TextChanged of string
    | TypeChanged of TextType
    | CategoriesFethced of string []
    | CantFetchCategories of exn
    | TitleChanged of string
    | ChangeCategory of string
    | NewCategoryChanged of string
    | CategoryResp of AddCategoryResponse*string
    | CategoryNotAdded of exn
    | AddCategory
    | ToogleDpd
    | AddSection
    | NewSection
    | AddArticle
    | ArticleAdded

let init()  =
    {newCategory = "";dropD = {isActive = false;category = None;allCategories = []};title = "";sections = [];currentBuffer = None},sendGet<string [],Msg> (Decode.array Decode.string) "/api/getCategories" CategoriesFethced CantFetchCategories

let update (msg : Msg) (model:Model) =
    match msg,model with
    | CategoryResp ({state = true},nc),_ ->
        {model with dropD = {model.dropD with allCategories = nc :: model.dropD.allCategories }},Cmd.none
    | NewCategoryChanged s,_ ->
        {model with newCategory = s},Cmd.none
    | AddCategory,{newCategory = nc} ->
        {model with newCategory = ""},sendGet<AddCategoryResponse,Msg> newCategoryDecoder (sprintf "/api/addCategory?cat=%s" nc) (fun r -> CategoryResp (r,nc)) CategoryNotAdded
    | ChangeCategory s,_ ->
        {model with dropD = {model.dropD with category = Some s}},Cmd.none
    | ToogleDpd,_ ->
        {model with dropD = {model.dropD with isActive = not model.dropD.isActive}},Cmd.none
    | TitleChanged s,_ ->
        {model with title = s},Cmd.none
    | CategoriesFethced cat,_ ->
        {model with dropD = {model.dropD with allCategories = List.ofArray cat}},Cmd.none
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
    | AddArticle,{dropD = {category = Some c}} ->
        {model with currentBuffer = None;sections = []},sendGenericJson<string,Msg> ({NewBlog.category = c;NewBlog.topic = model.title;NewBlog.sections = model.sections}) 1 (Decode.string) "/api/sendBlog" (fun _ -> ArticleAdded) (fun _ -> ArticleAdded)  
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

    let info = 
        [   
            DropDown {
                items = [
                    for dpd in model.dropD.allCategories ->
                        let current = model.dropD.category
                        {
                            onClick = (fun _ -> dpd |> ChangeCategory |> dispatch)
                            name = dpd
                            isActive = current.IsSome && current.Value = dpd
                        }
                ]
                desc = "Select blog category"
                isActive = model.dropD.isActive
                onExpand = (fun _ -> dispatch ToogleDpd)
            }
            InpWithButton {
                placeholder = PlaceHolder "Add new Category"
                name = "Add"
                onChange = (fun s -> s |> NewCategoryChanged |> dispatch)
                onClick = (fun _ -> dispatch AddCategory)
            }
            TextInput {
                onchange = (fun s -> s |> TitleChanged |> dispatch)
                labeltext = LabelText "Title:"
                icon = None
                isPassword = false
                placeholder = PlaceHolder "Insert title here..."
            }
        ]
        |> Seq.map (fun x -> x,None)

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

    let p1 = Seq.concat [info;p1]

    renderForm p1 (fun _ -> dispatch AddArticle) false
    |> BlogParser.renderBlogWriter sections