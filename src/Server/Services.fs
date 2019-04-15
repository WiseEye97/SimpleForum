module Services

    open System.IO
    open Shared.ClientMessages
    open Shared.UserModel
    open Shared.ServerMessages
    open System.Threading.Tasks
    open FSharp.Control.Tasks.V2

    type Service = (Stream -> Task<string>)

    type JsonDeserializer<'a> = (Stream -> 'a option)
    type DbUserService<'a> = User -> Task<'a>
    type DbLoginService<'a> = LogInMessage -> Task<'a>

    type DbResp = Result<unit, SignErrors>
    type DbLoginRespr = Result<unit, LoginErrors>
    type DbJsonSerializer<'a> = 'a -> string

    let private insertUser (db : DbUserService<DbResp>) (deser : JsonDeserializer<User>) (serializer : DbJsonSerializer<DbResp>) (request:Stream) =
        request
        |> deser
        |> function
            | Some usr ->
                task{
                    let! r = usr |> db
                    return (r |> serializer)
                }
            | None ->
                task {
                    return {state = false;errorMessage = None} |> JsonHelper.serialize
                }

    let private loginUser (db : DbLoginService<DbLoginRespr>) (deser : JsonDeserializer<LogInMessage>) (serializer : DbJsonSerializer<DbLoginRespr>) (request:Stream) =
        request
        |> deser
        |> function
            | Some loginMsg -> 
                task {
                    let! r = loginMsg |> db
                    return (r |> serializer)
                }
            | None -> 
                task {
                    return {state = false;errorMessage = None} |> JsonHelper.serialize
                }

    let loginUserService : Service =
        loginUser DbHandler.UserDBController.loginUser JsonHelper.deserializeLoginReq JsonHelper.serializeDbResult

    let insertUserService : Service = 
        insertUser DbHandler.UserDBController.insertUser JsonHelper.deserializeUserSignReq JsonHelper.serializeDbResult