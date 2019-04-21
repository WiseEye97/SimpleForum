module WriteBlog

open Elmish
open ViewUtil
open Fable.Core.JsInterop
open PromiseSender
open Shared.ClientMessages
open Shared.ServerMessages
open Thoth.Json
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

type ServerMessages = 
    | CategoriesFethced of string []
    | CantFetchCategories of exn
    | CategoryResp of AddCategoryResponse*string
    | CategoryNotAdded of exn
    | ArticleAdded

type InfoMessages = 
    | TitleChanged of string
    | ChangeCategory of string
    | NewCategoryChanged of string
    | AddCategory
    | ToogleDpd

type FormBodyMessages =
    | TextChanged of string
    | TypeChanged of TextType  
    | AddSection
    | NewSection
    | AddArticle
     

type Msg =
    | InfoMessages of InfoMessages
    | ServerMessages of ServerMessages
    | FormBodyMessages of FormBodyMessages
    

let init()  =
    {newCategory = "";dropD = {isActive = false;category = None;allCategories = []};title = "";sections = [];currentBuffer = None},sendGet<string [],Msg> (Decode.array Decode.string) "/api/getCategories" (CategoriesFethced >> ServerMessages) (CantFetchCategories >> ServerMessages) 

let update (msg : Msg) (model:Model) =

    let serverUpdate (msg : ServerMessages) (model:Model) =
        match msg,model with
        | CategoryResp ({state = true},nc),_ ->
            {model with dropD = {model.dropD with allCategories = nc :: model.dropD.allCategories }},Cmd.none
        | CategoriesFethced cat,_ ->
            {model with dropD = {model.dropD with allCategories = List.ofArray cat}},Cmd.none
        | _ -> 
            model,Cmd.none

    let infoUpdate (msg : InfoMessages) (model:Model) =
        match msg,model with
        | NewCategoryChanged s,_ ->
            {model with newCategory = s},Cmd.none
        | AddCategory,{newCategory = nc} ->
            {model with newCategory = ""},sendGet<AddCategoryResponse,Msg> newCategoryDecoder (sprintf "/api/addCategory?cat=%s" nc) (fun r -> CategoryResp (r,nc) |> ServerMessages) (CategoryNotAdded >> ServerMessages) 
        | ChangeCategory s,_ ->
            {model with dropD = {model.dropD with category = Some s}},Cmd.none
        | ToogleDpd,_ ->
            {model with dropD = {model.dropD with isActive = not model.dropD.isActive}},Cmd.none
        | TitleChanged s,_ ->
            {model with title = s},Cmd.none

    let formUpdate (msg : FormBodyMessages) (model:Model) =
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
        | AddArticle,{dropD = {category = Some c}} ->
            {model with currentBuffer = None;sections = []},sendGenericJson<string,Msg> ({NewBlog.category = c;NewBlog.topic = model.title;NewBlog.sections = model.sections}) 1 (Decode.string) "/api/sendBlog" (fun _ -> ArticleAdded |> ServerMessages) (fun _ -> ArticleAdded |> ServerMessages)

    match msg,model with
    | ServerMessages m,md ->
        serverUpdate m md
    | InfoMessages m,md ->
        infoUpdate m md
    | FormBodyMessages m,md ->
        formUpdate m md


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
                            onClick = (fun _ -> dpd |> ChangeCategory |> InfoMessages |> dispatch)
                            name = dpd
                            isActive = current.IsSome && current.Value = dpd
                        }
                ]
                desc = "Select blog category"
                isActive = model.dropD.isActive
                onExpand = (fun _ -> ToogleDpd |> InfoMessages |> dispatch)
            }
            InpWithButton {
                placeholder = PlaceHolder "Add new Category"
                name = "Add"
                onChange = (NewCategoryChanged >> InfoMessages >> dispatch)
                onClick = (fun _ -> AddCategory |> InfoMessages |> dispatch)
            }
            TextInput {
                onchange = (TitleChanged >> InfoMessages >> dispatch)
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
                    onChange = TextChanged >> FormBodyMessages >> dispatch
                }

                RadioInput {
                    name = RadioName "CodeType"
                    values = [
                        {isSelected = x.tp = Code;value = "Code";onSelect = (fun _ -> Code |> TypeChanged |> FormBodyMessages |> dispatch)}
                        {isSelected = x.tp = TextType.Normal;value = "Normal";onSelect = (fun _ -> Normal |> TypeChanged |> FormBodyMessages |> dispatch)}
                    ]
                }

                ButtonWithIcon {
                    iconName = IconType "fas fa-check"
                    onClick = (fun _ -> AddSection |> FormBodyMessages |> dispatch)
                }
            ]
        | {currentBuffer = None} ->
            [
                ButtonWithIcon {
                    iconName = IconType "fas fa-plus"
                    onClick = (fun _ -> NewSection |> FormBodyMessages |> dispatch)
                }
            ]
        |> Seq.map (fun x -> x,None)

    let p1 = Seq.concat [info;p1]

    renderForm p1 (fun _ -> AddArticle |> FormBodyMessages |> dispatch) false
    |> BlogParser.renderBlogWriter sections