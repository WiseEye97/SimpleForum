module DbHandler

open System.Data
open System.Data.SqlClient
open System.Threading.Tasks
open FSharp.Control.Tasks.V2

let private openConnection() =
    task {
        let con = new SqlConnection(Config.connectionString) 
        if con.State = ConnectionState.Closed then
            do! con.OpenAsync()
        return con
    }

let private createParam name tp size value =
    let p = SqlParameter(name,tp, size)
    p.Value <- value
    p

let private executeReader commandText sqlparams (callBack : SqlDataReader -> Task<Result<unit,'a>>) onError = 
    task {
        use! con = openConnection()

        use com = con.CreateCommand()

        com.CommandType <- CommandType.Text
        com.CommandText <- commandText

        com.Parameters.AddRange sqlparams
        let! a =
            try
                task {
                   use! reader = com.ExecuteReaderAsync()
                   let! r = reader |> callBack
                   return (r |> Ok)     
                }
                
            with
            | ex ->
                printfn "exception occured %A" ex
                task {return Error onError}
        return        
            match a with
            | Error x -> Error x
            | Ok (Error x) -> Error x
            | Ok (Ok x) -> Ok x 
    }

let private executeNonReader commandText sqlparams onDbError =
    task {
        use! con = openConnection()

        use com = con.CreateCommand()

        com.CommandType <- CommandType.Text
        com.CommandText <- commandText

        com.Parameters.AddRange sqlparams

        try
            let! _ = com.ExecuteNonQueryAsync()
            return Ok ()
        with
        | :? System.Data.Common.DbException as ex ->
            printfn "an error occured %s" ex.Message
            return Error onDbError
        
    }
    

module UserDBController =

    open Shared.UserModel
    open Shared.ServerMessages
    open Shared.ClientMessages

    
    let isUserExists (user : User) =
        task {   
            let usrN,usrM = user.username.GetName(),user.email.GetStringEmail()
            
            return!
                executeReader 
                    "SELECT TOP(1) nick,mail FROM usersNew WHERE (usersNew.nick = @nick OR usersNew.mail = @mail) AND usersNew.isConfirmed = 1;" 
                    [|
                        createParam "@nick" SqlDbType.VarChar 100 usrN
                        createParam "@mail" SqlDbType.VarChar 100 usrM
                    |]
                    (fun reader ->
                        task {
                            if reader.Read() then
                                let x,y = reader.GetString(0),reader.GetString(1)
                                 
                                match (x,y) with
                                | a,_ when a = usrN -> return Error NickExists
                                | _,b when b = usrM -> return Error EmailExists
                                | _,_ -> return Error SignErrors.InternalError
                            else
                                return Ok ()
                        }
                        
                    )
                    SignErrors.InternalError    
        }
        
        
    let insertUser (user : User) =
        task {
            let! exists = isUserExists user
            match exists with
            | Error x -> return Error x
            | Ok _ ->
 
                return!
                    executeNonReader 
                        "INSERT INTO usersNew (nick,mail,pswd,isConfirmed) VALUES(@nick,@mail,@pswd,0);"
                        [|
                            SqlParameter("@nick",user.username.GetName())
                            SqlParameter("@mail",user.email.GetStringEmail())
                            SqlParameter("@pswd",user.password.GetPswd())
                        |]
                        SignErrors.InternalError               
        }
    
    let loginUser (data : LogInMessage) =
        task{

            let (|ValidLogin|_|) (nick,pswd,isC) = if nick = data.username && pswd = data.password && isC = 1 then Some () else None
            let (|WrongPassword|_|) (nick,pswd,_) = if nick = data.username && pswd <> data.password then Some () else None
            let (|NotConfirmed|_|) (nick,pswd,_) = if nick = data.username && pswd = data.password then Some () else None

            return!
                executeReader
                    "SELECT usersNew.nick,usersNew.pswd,usersNew.isConfirmed FROM usersNew WHERE usersNew.nick = @nick;"
                    [|
                        createParam "@nick" SqlDbType.VarChar 100 data.username
                        createParam "@pswd" SqlDbType.VarChar 100 data.password
                        |]
                    (fun reader -> task {
                            let r = 
                                if reader.Read() then
                                    let dat = reader.GetString(0),reader.GetString(1),reader.GetInt32(2)
                                    match dat with
                                    | ValidLogin -> Ok () 
                                    | WrongPassword -> Error WrongPassword
                                    | NotConfirmed -> Error AccountNotConfirmed
                                    | _ -> Error InternalServError
                                else
                                    Error WrongNick
                            return r 
                        }
                    )
                    InternalServError
        }

    let confirmUser (username : UserName) =
        task{
            return!
                executeNonReader 
                    "update usersNew set isConfirmed = 1 where nick = @nick;"
                    [|
                        createParam "@nick" SqlDbType.VarChar 100 (username.GetName())
                    |]
                    ConfirmErrors.InternalError
        }


    
    