module Controllers

open System.IO
open Giraffe
open FSharp.Control.Tasks.V2

    type PostData = Stream
    type QueryData = string option

    module UserController =
        
        let insertUser (requestStream : PostData) =
            task {
                let! resp = Services.insertUserService requestStream
                return text resp
            }
        let loginUser (requestStream : PostData) = 
            task {
                let! resp = Services.loginUserService requestStream
                return text resp
            }
        
        let activateAccount (data : QueryData)  = 
            task {
                let! resp = Services.confirmUserService data
                return htmlView (ConfirmPage.index resp)
            }

            
