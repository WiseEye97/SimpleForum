module Blog

open Elmish
open Thoth.Json
open ViewUtil.Tabs
open ViewUtil.Lists
open PromiseSender
open Sign
open Shared

type BlogCategory = BlogCategory of string
with 
    override this.ToString() =
        match this with
        | BlogCategory s -> s

type BlogTitle = BlogTitle of string
with
    override this.ToString() =
        match this with
        | BlogTitle s -> s

(*type CategoriesModel = {
    
}*)

type Model = {
    categories : Map<BlogCategory,BlogTitle list> option
    currentCategory : BlogCategory option
}

type ServerMessages = 
    | CategoriesFethced of string []
    | CantFetchCategories of exn
    | CantFetchBlogs of exn
    | BlogsFetched of BlogCategory * (BlogTitle list)

type Msg =
    | ServerMessages of ServerMessages
    | FetchBlogsOfCat of BlogCategory
    | ShowBlog of BlogTitle
    | Ignore

let init() =
    {currentCategory = None;categories = None},sendGet<string [],Msg> (Decode.array Decode.string) "/api/getCategories" (CategoriesFethced >> ServerMessages) (CantFetchCategories >> ServerMessages)
    

let update (msg : Msg) (model:Model) =
    let serverUpdate (msg:ServerMessages) (model:Model) =
        let (|CatFetchedAsList|_|) =
            function 
            | CategoriesFethced arr -> arr |> List.ofArray |> List.map (fun x -> BlogCategory x , []) |> Map.ofList  |> Some
            | _ -> None

        match msg,model with
        | CatFetchedAsList li,_ -> {model with categories = Some li},Cmd.none
        | BlogsFetched (cat,li),{categories = Some mp} ->
            {model with currentCategory = Some cat;categories = Map.add cat li mp |> Some},Cmd.none
        | _ ->
            model,Cmd.none

    let isFetched (cat : BlogCategory) =
        match model.categories.Value.[cat] with
        | [] -> false
        | _ -> true 

    match msg,model with
    | ServerMessages m,md ->
        serverUpdate m md
    | FetchBlogsOfCat cat,_ when isFetched cat ->
        {model with currentCategory = Some cat},Cmd.none
    | FetchBlogsOfCat cat,_ ->
        model,sendGet<string [],Msg> (Decode.array Decode.string) (cat.ToString() |> sprintf "/api/getBlogs?cat=%s") (fun x -> x |> Seq.map BlogTitle |> Seq.toList |> (fun y -> cat,y) |> BlogsFetched |> ServerMessages) (CantFetchBlogs >> ServerMessages)
    | _ , _ ->
        model,Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    let tab = 
        match model with
        | {categories = Some li} ->
            [
                for cat in li ->
                    let categoryName = cat.Key.ToString()
                    {
                        name = categoryName
                        isActive = model.currentCategory.IsSome && model.currentCategory.Value = cat.Key
                        onSelect = fun _ -> categoryName |> BlogCategory |> FetchBlogsOfCat |> dispatch
                    }
            ]
            |> renderTab
        | _ ->
            renderTab []
    let blogs = 
        match model with
        | {categories = Some mp;currentCategory = Some ct} ->
                mp.[ct]
                |> List.map (fun x ->
                    {
                        name = x.ToString()
                        onClick = (fun _ -> x |> ShowBlog |> dispatch)
                    }
                )
                |> renderList
        | _ -> renderList []
            
    ViewUtil.renderBlogSelector tab blogs 
    
