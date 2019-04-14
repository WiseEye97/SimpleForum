module Controllers
open System.IO
open Giraffe
open Saturn
open FSharp.Control.Tasks.V2

    module UserController =
        
        let insertUser (requestStream : Stream) =
            task {
                let! resp = Services.insertUserService requestStream
                return text resp
            }
            
