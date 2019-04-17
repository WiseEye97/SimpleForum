module Decoders

open Thoth.Json
open Shared.ServerMessages


module userErrosDecoder =

    let decodeSignOpt = 
        function
        | "NickExists" -> NickExists
        | "EmailExists" -> EmailExists
        | "InternalError" -> SignErrors.InternalError
        | _ -> SignErrors.InternalError

    let decodeLoginOpt =
        function
        | "WrongNick" -> WrongNick
        | "WrongPassword" -> WrongPassword
        | "InternalServError" -> InternalServError
        | "AccountNotConfirmed" -> AccountNotConfirmed
        | _ -> InternalServError

    let decodeOptionError  : string -> obj -> Result<SignErrors array,Decode.DecoderError> =
        Decode.field "Fields" (Decode.array (Decode.map decodeSignOpt (Decode.field "Case" Decode.string)))

    let decodeOptionErrorWith<'a> (optionDecoder : (string -> 'a))  =
        Decode.field "Fields" (Decode.array (Decode.map optionDecoder (Decode.field "Case" Decode.string)))

    let decodeErrorMsg : string -> obj -> Result<SignErrors option,Decode.DecoderError> =
        Decode.map2 (fun x y ->
            match x,List.ofArray y with
            | "Some",x::_ -> Some x
            | "None",_ -> None
            | _ -> None  
        ) (Decode.field "Case" Decode.string)
          decodeOptionError  
    
    let decodeErrorMsgWith<'a> (optionDecoder : (string -> 'a))  =
        Decode.map2 (fun x y ->
            match x,List.ofArray y with
            | "Some",x::_ -> Some x
            | "None",_ -> None
            | _ -> None  
        ) (Decode.field "Case" Decode.string)
          (decodeOptionErrorWith<'a> optionDecoder)  
        
    let ServerResponseDecoder : string -> obj -> Result<SignInResponse,Decode.DecoderError> =
        Decode.map2 (fun x y ->
                     {state = x;errorMessage = y}
        ) (Decode.field "state" Decode.bool)
          (Decode.field "errorMessage" (Decode.oneOf [Decode.nil None;decodeErrorMsg])) 

    let SimpleServerResponseDecoder<'a> (mapper : bool -> 'a option -> SimpleServerResponse<'a>) (optionDecoder : (string -> 'a)) =
        Decode.map2 
          mapper  
          (Decode.field "state" Decode.bool)
          (Decode.field "errorMessage" (Decode.oneOf [decodeErrorMsgWith<'a> optionDecoder;Decode.nil None])) 

    let loginDecoder : string -> obj -> Result<SimpleServerResponse<LoginErrors>,Decode.DecoderError> = SimpleServerResponseDecoder<LoginErrors> (fun x y -> {state = x;errorMessage = y}) decodeLoginOpt
    let signinDecoder : string -> obj -> Result<SimpleServerResponse<SignErrors>,Decode.DecoderError> = SimpleServerResponseDecoder<SignErrors> (fun x y -> {state = x;errorMessage = y}) decodeSignOpt 