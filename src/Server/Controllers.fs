module Controllers

open System.IO
open Giraffe
open FSharp.Control.Tasks.V2

    module UserController =
        
        let insertUser (requestStream : Stream) =
            task {
                let! resp = Services.insertUserService requestStream
                return text resp
            }
        let loginUser (requestStream : Stream) = 
            task {
                let! resp = Services.loginUserService requestStream
                return text resp
            }
            
