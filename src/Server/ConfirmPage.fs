module ConfirmPage

open Giraffe.GiraffeViewEngine
open Shared.ServerMessages

let index (res:Result<unit,Shared.ServerMessages.ConfirmErrors>) =
    match res with
    | Ok _ ->
        p [] [str "Your account has been confirmed you can now log in here -> "]
    | Error er ->
        let errorMessage =
            er
            |> function
                | UserNotFound -> "Sorry user with this nick doesnt exists"
                | InvalidUrl -> "Cannot procces your request"
                | InternalError -> "Sorry we have problems in our system come back later"
        p [] [str errorMessage]


