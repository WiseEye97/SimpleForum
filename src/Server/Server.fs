open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared


let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port = "SERVER_PORT" |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us


let tryFetchQueryArg (collection : seq<System.Collections.Generic.KeyValuePair<string,'a>>) (arg:string) = 
    collection
    |> Seq.tryFind (fun kv -> kv.Key = arg)
    |> Option.map (fun x -> x.Value)

let webApp = router {
    post "/api/register" (fun next ctx ->
        task {
            let! action = Controllers.UserController.insertUser ctx.Request.Body
            return! action next ctx
        })
    post "/api/login" (fun next ctx ->
        task {
            let! action = Controllers.UserController.loginUser ctx.Request.Body
            return! action next ctx
        })
    get "/api/confirm" (fun next ctx ->
        task {
            let arg = tryFetchQueryArg ctx.Request.Query "x" |> Option.map (fun x -> x.ToString())

            let! action = Controllers.UserController.activateAccount arg
            return! action next ctx
        }
    )
}

let configureSerialization (services:IServiceCollection) =
    services.AddSingleton<Giraffe.Serialization.Json.IJsonSerializer>(Thoth.Json.Giraffe.ThothSerializer())

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    service_config configureSerialization
    use_gzip
}

run app
