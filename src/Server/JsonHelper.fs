module JsonHelper

open Newtonsoft.Json
open System.IO


let serialize obj =
    let s = JsonConvert.SerializeObject obj
    printfn "%s" s
    s


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
      let buffer = Array.create 200 (byte 0)

      req.Read(buffer,0,200)
      |> printfn "read -> %d"

      buffer 
      |> Array.takeWhile (fun e -> e <> byte(0)) 
      |> System.Text.UTF8Encoding.UTF8.GetString
      |> fun s -> printfn "%s" s; s
      |> des<'a>


open Shared.ServerMessages
open Shared.UserModel

let serializeDbResult<'e> (dbRes : Result<unit, 'e>) =
    printfn "dREs -> %A" dbRes
    match dbRes with
    | Ok () -> {Shared.ServerMessages.SimpleServerResponse.state = true;errorMessage = None}
    | Error er -> {state = false;errorMessage = Some er}
    |> serialize

let deserializeUserSignReq (req : Stream) =
    match deserializeRequest<Shared.ClientMessages.SignInMessage> req with
    | Ok r -> User.CreateFromStrings (r.username , r.password , r.email)
    | _ -> None

let deserializeLoginReq (req : Stream) =
    match deserializeRequest<Shared.ClientMessages.LogInMessage> req with
    | Ok r -> Some r
    | _ -> None


