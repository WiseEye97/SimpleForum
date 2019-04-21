open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared
open Saturn.ControllerHelpers
open Shared.ClientMessages
open Shared.UserModel


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
            let! model = Controller.getJson<LogInMessage> ctx
            
            let! action = Controllers.UserController.loginUser model

            return! action next ctx
        })
    get "/api/confirm" (fun next ctx ->
        task {
            let arg = tryFetchQueryArg ctx.Request.Query "x" |> Option.map (fun x -> x.ToString())

            let! action = Controllers.UserController.activateAccount arg
            return! action next ctx
        })

    get "/api/addCategory" (fun next ctx ->
        task {
            let arg = tryFetchQueryArg ctx.Request.Query "cat" |> Option.map (fun x -> x.ToString())
            let! action = Controllers.AzureController.createCategory arg
            return! action next ctx
        })

    get "/api/getCategories" (fun next ctx -> 
        task {
           let! categories = Controllers.AzureController.getAllCategories()
           match categories with
           | Some c -> return! c next ctx 
           | _ -> return! RequestErrors.badRequest (text "klops") next ctx      
        }
    )
    get "/api/testAzure" (fun next ctx ->
        task {
            let! _ =  AzureHandler.xmlToBlog "Wprowadzenie F" "Hello F#2"
            return! text "" next ctx
        })
        
    post "/api/sendBlog" (fun next ctx ->
        task {
            let! model = Controller.getJson<Shared.ClientMessages.NewBlog> ctx
            printfn "model -> %A" model
            do! AzureHandler.insertBlog model    
            return! text "" next ctx
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
