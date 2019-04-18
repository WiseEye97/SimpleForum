module ViewUtil

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.Browser
open Shared.ServerMessages


type OnSubmit = (unit -> unit)
type OnChange = (string -> unit)

type LabelText = LabelText of string
type PlaceHolder = PlaceHolder of string
type IconType = IconType of string

module NavHelper =

    type BrandUrl =  string
    type LinkText = LinkText of string
    type OnLinkClick = (unit -> unit)

    type NavLocation =
        | Start
        | Middle
        | End
    
    type NavLink = {
        location : NavLocation
        action : OnLinkClick
        text : LinkText
    }

    type NavBarElementType = 
        | Link of NavLink
        | Menu


    let renderNavbar (elems : seq<NavBarElementType>) (brand : BrandUrl)  =
        let grouper =
            function
            | Link {location = l} -> l
            | _ -> Start

        let items = 
            elems
            |> Seq.groupBy grouper    
            |> Seq.map (fun (k,sq) ->
                sq
                |> Seq.map (function
                    | Link ({text = LinkText t} as l) ->
                        a [ClassName "navbar-item";OnClick (fun _ -> l.action())] [
                            str t
                        ]
                    | Menu -> div [] []
                )
                |> fun x -> (k,x)
            )

        let getLocation loc =
            items
            |> Seq.tryFind (fun (l,_) -> l = loc)
            |> function
               | Some x -> snd x   
               | None -> Seq.empty


        nav [ClassName "navbar";Role "navigation"] [
            div [ClassName "navbar-brand"] [
                div [ClassName "navbar-item"] [
                    img [Src brand;ClassName "Logo"]        
                ]
            ]
            div [ClassName "navbar-menu"] [
               div [ClassName "navbar-start"] [
                   yield! getLocation Start
               ]
               div [ClassName "navbar-end"] [
                   yield! getLocation End
               ]
            ]
        ]

type ValidatorStyler = {
    isValid : bool
    onValid : string
    onInValid : string
}

type TextInput = {
    onchange : OnChange
    labeltext : LabelText
    icon : IconType option
    isPassword : bool
    placeholder : PlaceHolder
}

type RadioName = RadioName of string
type RadioValue = {
    value : string
    onSelect : (unit -> unit)
}

type RadioInput = {
    name : RadioName
    values : seq<RadioValue>
}

type ButtonWithIcon = {
    iconName : IconType
    onClick : (unit -> unit)
}

type TextArea = {
    onChange : OnChange
}

type InputType =
    | TextInput of TextInput
    | RadioInput of RadioInput
    | ButtonWithIcon of ButtonWithIcon
    | TextArea of TextArea


let renderForm inputs (onSubmit:OnSubmit) isLoading =
    let insertToField x =
        div [ClassName "field"] [
            yield! x        
        ]

    let getLabelText (LabelText txt) = txt
    let getPlaceHolderText (PlaceHolder txt) = txt

    let onChangeHandler newVal = 
        function
        | {onchange = onch} ->
            newVal |> onch

    let insertIcon =
        function
        | {icon = Some (IconType ic)} -> 
            span [ClassName "icon is-small is-left"] [
                i [ClassName ic] []
            ]
            |> Some
        | _ -> None

    let renderSuccesClass =
        function
        | Some {isValid = true} -> " is-success"
        | Some _ -> " is-danger"
        | _ -> ""

    let renderHelperP = 
        function
        | Some {isValid = true;onValid = onv} ->
            p [ClassName "help is-success"] [str onv]  
        | Some {onInValid = oinv} -> 
            p [ClassName "help is-danger"] [str oinv]
        | _ -> str ""     


    let renderedInputs = 
        inputs
        |> Seq.map ((function
            | TextInput t,validator ->
                [
                    label [ClassName "label"] [t.labeltext |> getLabelText |> str]
                    div [ClassName "control"] [
                        yield input [OnChange (fun v -> onChangeHandler v.Value t);ClassName ("input" + renderSuccesClass validator);(if t.isPassword then Type "password" else Type "text");t.placeholder |>  getPlaceHolderText |> Placeholder]
                        match insertIcon t with
                        | Some x -> yield x
                        | _ -> ()
                    ]
                    renderHelperP validator
                ]
            | RadioInput ({name = RadioName n} as r),_ ->
                [
                    div [ClassName "control"] [
                        yield!
                            r.values
                            |> Seq.map (fun v ->
                                label [ClassName "radio"] [
                                    input [Type "radio";Name n;Value v.value;OnSelect (fun _ -> v.onSelect())]
                                    p [] [str v.value]        
                                ]
                            )
                    ]
                ] 
            | ButtonWithIcon {iconName = IconType n;onClick = onc},_ ->
                [
                    div [ClassName "control"] [
                        a [ClassName "button";OnClick (fun _ -> onc())] [
                            span [ClassName "icon"] [
                                i [ClassName n] []
                            ]
                        ]
                    ]
                ]
            | TextArea t, _ ->
                [
                    textarea [ClassName "textarea";OnChange (fun v -> v.Value |> t.onChange)] []
                ]
                ) >> insertToField)
        |> Seq.toList
    div [ClassName "columns is-mobile is-centered"] [
        div [ClassName "column is-half form"] [
            yield! renderedInputs
            yield [div [ClassName "control"] [
                button [OnClick (fun _ -> onSubmit());(if isLoading then "is-loading" else "") |> sprintf "button is-link %s" |> ClassName] [
                    str "Submit"        
                ]
            ]]
            |> insertToField
        ]
    ]

let private createModal onClose content=
    div [ClassName "modal is-active"] [
        div [ClassName "modal-background"] []
        div [ClassName "modal-content"] [
            p [] [str content]
            button [ClassName "button is-danger";OnClick (fun _ -> onClose())] [
                i [ClassName "far fa-window-close"] []
            ]
        ]
    ]

let renderFormWithWarning inputs (onSubmit:OnSubmit) isLoading warningMessage =
    div [] [
        renderForm inputs onSubmit isLoading
        p [] [str warningMessage]
    ]
    

let signedOkPage onClose =
    createModal onClose "Signed successfully , Now you need to confirm you email"   
    
let signedFailedPage reason onClose =
    match reason with
    | NickExists -> "User with this name already exists"
    | EmailExists -> "Account with this email already exists"
    |  SignErrors.InternalError -> "Ooops internal server error please try later"
    |> createModal onClose
    
