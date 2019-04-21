module Services

    open System.IO
    open Shared.ClientMessages
    open Shared.UserModel
    open Shared.ServerMessages
    open System.Threading.Tasks
    open FSharp.Control.Tasks.V2
    open System.Threading.Tasks
    open JsonHelper

    type Service = (Stream -> Task<string>)
    type QueryService = (string option -> Task<string>)

    type DbService<'a,'b> = 'a -> Task<'b>

    type JsonDeserializer<'a> = (Stream -> 'a option)
    type DbUserService<'a> = User -> Task<'a>
    type DbLoginService<'a> = LogInMessage -> Task<'a>
    type DbConfirmService = UserName -> Task<Result<unit,ConfirmErrors>>

    type DbBlogPostService<'a> = NewBlog -> Task<'a>

    type DbResp = Result<unit, SignErrors>
    type DbLoginRespr = Result<unit, LoginErrors>
    type DbConfirmResp = Result<unit,ConfirmErrors>
    type DbJsonSerializer<'a> = 'a -> string

    type EmailService = User -> Task


    let private genericService<'a,'b> (dbService : DbService<'a,'b>) (deser : JsonDeserializer<'a>) (dbSerializer : DbJsonSerializer<'b>) (onDeserFail : (unit -> string)) (request : Stream) =
        request
        |> deser
        |> function
            | Some x -> 
                task {
                    let! r = x |> dbService
                    return (r |> dbSerializer)
                }
            | None -> 
                task {
                    return onDeserFail()
                }


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

    let private loginUser (db : DbLoginService<DbLoginRespr>) (serializer : DbJsonSerializer<DbLoginRespr>) (model:LogInMessage) =
        task {
                let! r = model |> db
                return (r |> serializer)
        }
           

    let private confirmUser (db : DbConfirmService) (user: string option) =
        task {
            match user with
            | Some (UserNamePatt username) ->
                let! r = username |> db
                return r
            | _ -> return Error InvalidUrl
        }

    
    let loginUserService =
        loginUser DbHandler.UserDBController.loginUser JsonHelper.serializeDbResult

    let insertUserService : Service = 
        insertUser EmailSender.sendActivationEmail DbHandler.UserDBController.insertUser JsonHelper.deserializeUserSignReq JsonHelper.serializeDbResult
    
    let confirmUserService  =
        confirmUser DbHandler.UserDBController.confirmUser

    let private createCategory (azureService : (string -> Task<Result<unit,BlogPostErrors>>)) (categoryName:string option) =
        task{
            match categoryName with
            | Some x ->
                let! r =  x |> azureService
                return r |> serializeDbResult<BlogPostErrors>
            | None -> 
                return WrongUrl |> Error |> serializeDbResult<BlogPostErrors>
        }
    
    let private getAllCategories (azureService : unit -> Task<Result<string array,BlogPostErrors>>) = 
        task{
            let! r = azureService()
            return
                r 
                |> serializeFetch<string array,BlogPostErrors>
               
        }

    let getAllCategoriesService() = getAllCategories AzureHandler.getAllCategories 

    let createCategoryService = 
        createCategory AzureHandler.createCategory
