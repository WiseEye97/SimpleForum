module Services

    open DbHandler
    open Giraffe
    open Saturn
    open System.IO
    open Shared.UserModel
    open Shared.ServerMessages
    open System.Threading.Tasks
    open FSharp.Control.Tasks.V2

    type JsonDeserializer<'a> = (Stream -> 'a option)
    type DbUserService<'a> = (User -> 'a)

    type DbResp = Result<unit, UserErros>
    type DbJsonSerializer = DbResp -> string

    let private insertUser (db : DbUserService<Task<DbResp>>) (deser : JsonDeserializer<User>) (serializer : DbJsonSerializer) (request:Stream) =
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
                
    let insertUserService : (Stream -> Task<string>) = 
        insertUser DbHandler.UserDBController.insertUser JsonHelper.deserializeUserSignReq JsonHelper.serializeDbResult