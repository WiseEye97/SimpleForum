module JsonHelper

open Newtonsoft.Json
open System.IO


let serialize obj =
    JsonConvert.SerializeObject obj

let deserialize<'a> str =
    try
      JsonConvert.DeserializeObject<'a> str
      |> Result.Ok
    with
      // catch all exceptions and convert to Result
      | ex -> Result.Error ex
      
let des<'a> str =
      deserialize<'a> str

let deserializeRequest<'a> (req: Stream) =
      let buffer = Array.create 100 (byte 0)

      req.Read(buffer,0,100)
      |> printfn "read -> %d"

      buffer 
      |> Array.takeWhile (fun e -> e <> byte(0)) 
      |> System.Text.UTF8Encoding.UTF8.GetString
      |> fun s -> printfn "%s" s; s
      |> des<'a>

open DbHandler
open Shared.ServerMessages
open Shared.UserModel

let serializeDbResult (dbRes : Result<unit, UserDBController.UserErros>) =
    match dbRes with
    | Ok () -> {state = true}
    | _ -> {state = false}
    |> serialize

let deserializeUserSignReq (req : Stream) =
    match deserializeRequest<Shared.ClientMessages.SignInMessage> req with
    | Ok r -> User.CreateFromStrings (r.username , r.password , r.email)
    | _ -> None


