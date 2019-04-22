module ViewUtil

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.Browser
open Shared.ServerMessages
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser


type OnSubmit = (unit -> unit)
type OnChange = (string -> unit)

type LabelText = LabelText of string
type PlaceHolder = PlaceHolder of string
type IconType = IconType of string

module Tabs = 
    type TabItem = {
        isActive : bool
        name : string
        onSelect : (unit -> unit)
    }

    let renderTab (items:seq<TabItem>) =
        div [ClassName "tabs"] [
            ul [] [
               for item in items ->
                  li [if item.isActive then yield (ClassName "is-active")] [
                      a [OnClick (fun _ -> item.onSelect())] [str item.name]
                  ]  
            ]
        ]

module Lists =

    type ListItem = {
        name : string
        onClick : (unit -> unit)
    }

    let renderList (items:seq<ListItem>) =
        ul []
            [
               for item in items ->
                    li [OnClick (fun _ -> item.onClick())] [
                        a [] [str item.name]        
                    ]
            ] 
        

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
        isActive : bool
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
                        a [(if l.isActive then "is-active" else "") |> sprintf "navbar-item %s" |> ClassName;OnClick (fun _ -> l.action())] [
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


        nav [ClassName "navbar is-dark";Role "navigation"] [
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
    isSelected : bool
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

type DropDownItem = {
    onClick : (unit -> unit)
    name : string
    isActive : bool
}

type DropDown = {
    onExpand : unit -> unit
    desc : string
    isActive : bool
    items : DropDownItem list
}

type InpWithButton = {
    placeholder : PlaceHolder
    name : string
    onChange : string -> unit
    onClick : unit -> unit
}

type InputType =
    | TextInput of TextInput
    | RadioInput of RadioInput
    | ButtonWithIcon of ButtonWithIcon
    | TextArea of TextArea
    | DropDown of DropDown
    | InpWithButton of InpWithButton



let renderForm inputs (onSubmit:OnSubmit) isLoading =
    let insertToField (x,hasAddons) =
        div [(if hasAddons then "has-addons" else "") |> sprintf "field %s" |> ClassName] [
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
                ],false
            | RadioInput ({name = RadioName n} as r),_ ->
                [
                    div [ClassName "control"] [
                        yield!
                            r.values
                            |> Seq.map (fun v ->
                                label [ClassName "radio"] [
                                    input [Checked v.isSelected;Type "radio";Name n;Value v.value;OnClick (fun _ -> console.log("xxxx");v.onSelect())]
                                    p [] [str v.value]        
                                ]
                            )
                    ]
                ],false 
            | ButtonWithIcon {iconName = IconType n;onClick = onc},_ ->
                [
                    div [ClassName "control"] [
                        a [ClassName "button";OnClick (fun _ -> onc())] [
                            span [ClassName "icon"] [
                                i [ClassName n] []
                            ]
                        ]
                    ]
                ],false
            | TextArea t, _ ->
                [
                    textarea [ClassName "textarea";OnChange (fun v -> v.Value |> t.onChange)] []
                ],false
            | DropDown d,_ ->
                [
                    div [(if d.isActive then "is-active" else "") |> sprintf "dropdown %s" |> ClassName] [
                        div [ClassName "dropdown-trigger"] [
                            button [ClassName "button";AriaHasPopup true;OnClick (fun _ -> d.onExpand())] [
                                span [] [str d.desc]
                                span [ClassName "icon is-small"] [
                                    i [ClassName "fas fa-angle-down"] []
                                ]
                            ]
                        ]

                        div [ClassName "dropdown-menu";Role "menu"] [
                            div [ClassName "dropdown-content"] [
                                for item in d.items ->
                                    a [OnClick (fun _ -> item.onClick());(if item.isActive then "is-active" else "") |> sprintf "dropdown-item %s" |> ClassName] [
                                        str item.name
                                    ]        
                            ]
                        ]
                    ]
                ],false
            | InpWithButton ({placeholder = PlaceHolder p} as x),_ ->
                [
                    div [ClassName "control"] [
                        input [OnChange (fun v -> v.Value |> x.onChange);ClassName "input";Type "text";Placeholder p]
                    ]
                    div [ClassName "control"] [
                        a [OnClick (fun _ -> x.onClick());ClassName "button is-info"] [
                            str x.name
                        ]
                    ]
                ],true
                ) >> insertToField)
        |> Seq.toList
    div [ClassName "columns is-mobile is-centered"] [
        div [ClassName "column is-half form"] [
            yield! renderedInputs
            yield ([div [ClassName "control"] [
                button [OnClick (fun _ -> onSubmit());(if isLoading then "is-loading" else "") |> sprintf "button is-link %s" |> ClassName] [
                    str "Submit"        
                ]
            ]],false)
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

module BlogParser =

    let high : unit -> unit = import "parseCode" "./highlightCode.js" 
    
    let renderCode (s: string) =
        pre [] [
            code [ClassName "language-fsharp"] [
                    str s
            ]
        ]
        
    let renderNormal (s:string) =
        div [ClassName "content"] [
            str s
        ]
    
    let renderBlogWriter sections form  =

        div [ClassName "container"] [
            div [] [
                for sec in sections do
                    yield sec
                    yield div [ClassName "is-divider"] []
            ]
            form
        ]
    
let renderBlogSelector tab blogs =
    div [ClassName "container"] [
        tab
        blogs
    ]

