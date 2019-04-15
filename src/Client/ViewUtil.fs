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

type InputType =
    | TextInput of TextInput
    
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

let signedOkPage onClose =
    createModal onClose "Signed successfully , Now you need to confirm you email"   
    
let signedFailedPage reason onClose =
    match reason with
    | NickExists -> "User with this name already exists"
    | EmailExists -> "Account with this email already exists"
    | InternalError -> "Ooops internal server error please try later"
    |> createModal onClose
    
