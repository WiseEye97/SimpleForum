module Services

    open System.IO
    open Shared.ClientMessages
    open Shared.UserModel
    open Shared.ServerMessages
    open System.Threading.Tasks
    open FSharp.Control.Tasks.V2
    open System.Threading.Tasks

    type Service = (Stream -> Task<string>)
    type QueryService = (string option -> Task<string>)

    type JsonDeserializer<'a> = (Stream -> 'a option)
    type DbUserService<'a> = User -> Task<'a>
    type DbLoginService<'a> = LogInMessage -> Task<'a>
    type DbConfirmService = UserName -> Task<Result<unit,ConfirmErrors>>


    type DbResp = Result<unit, SignErrors>
    type DbLoginRespr = Result<unit, LoginErrors>
    type DbConfirmResp = Result<unit,ConfirmErrors>
    type DbJsonSerializer<'a> = 'a -> string

    type EmailService = User -> Task

    let private insertUser (emailService:EmailService) (db : DbUserService<DbResp>) (deser : JsonDeserializer<User>) (serializer : DbJsonSerializer<DbResp>) (request:Stream) =
        request
        |> deser
        |> function
            | Some usr ->
                task{
                    let! r = usr |> db
                    match r with
                    | Ok _ -> 
                        do! usr |> emailService
                    | _ -> ()
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

    let private confirmUser (db : DbConfirmService) (user: string option) =
        task {
            match user with
            | Some (UserNamePatt username) ->
                let! r = username |> db
                return r
            | _ -> return Error InvalidUrl
        }

    let loginUserService : Service =
        loginUser DbHandler.UserDBController.loginUser JsonHelper.deserializeLoginReq JsonHelper.serializeDbResult

    let insertUserService : Service = 
        insertUser EmailSender.sendActivationEmail DbHandler.UserDBController.insertUser JsonHelper.deserializeUserSignReq JsonHelper.serializeDbResult
    
    let confirmUserService  =
        confirmUser DbHandler.UserDBController.confirmUser

    let loggerService message =
        printfn "logging -> %A " message 