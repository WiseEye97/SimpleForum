module Decoders

open Thoth.Json
open Shared.ServerMessages


module userErrosDecoder =

    let decodeOpt = 
        function
        | "NickExists" ->
            NickExists
        | "EmailExists" ->
            EmailExists
        | "InternalError" ->
            InternalError
        | _ ->
            InternalError

    let decodeOptionError : string -> obj -> Result<UserErros array,Decode.DecoderError> =
        Decode.field "Fields" (Decode.array (Decode.map decodeOpt (Decode.field "Case" Decode.string)))

    let decodeErrorMsg : string -> obj -> Result<UserErros option,Decode.DecoderError> =
        Decode.map2 (fun x y ->
            match x,List.ofArray y with
            | "Some",x::_ -> Some x
            | "None",_ -> None
            | _ -> None  
        ) (Decode.field "Case" Decode.string)
          decodeOptionError  
        
    let ServerResponseDecoder : string -> obj -> Result<SignInResponse,Decode.DecoderError> =
        Decode.map2 (fun x y ->
                     {state = x;errorMessage = y}
        ) (Decode.field "state" Decode.bool)
          (Decode.field "errorMessage" (Decode.oneOf [Decode.nil None;decodeErrorMsg])) 