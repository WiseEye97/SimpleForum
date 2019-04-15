module DbHandler

open System.Data
open System.Data.SqlClient
open FSharp.Control.Tasks.V2

let openConnection() =
    task {
        let con = new SqlConnection(Config.connectionString) 
        if con.State = ConnectionState.Closed then
            do! con.OpenAsync()
        return con
    }


module UserDBController =

    open Shared.UserModel
    open Shared.ServerMessages
    open Shared.ClientMessages

    
    let isUserExists (user : User) =
        task {
            use! con = openConnection()

            if con.State = ConnectionState.Closed then
                con.Open()
        
            use com = con.CreateCommand()
            com.CommandType <- CommandType.Text
            com.CommandText <- "SELECT TOP(1) nick,mail FROM usersNew WHERE (usersNew.nick = @nick OR usersNew.mail = @mail) AND usersNew.isConfirmed = 1;"

            let usrN,usrM = user.username.GetName(),user.email.GetStringEmail()

            let p = SqlParameter("@nick",SqlDbType.VarChar, 100)
            p.Value <- usrN

            let p2 = SqlParameter("@mail",SqlDbType.VarChar, 100)
            p2.Value <- usrM 

            com.Parameters.AddRange [|
                        p
                        p2
            |]
            
            let! reader = com.ExecuteReaderAsync()
            
            if reader.Read() then
                let x,y = reader.GetString(0),reader.GetString(1)
                 
                match (x,y) with
                | a,_ when a = usrN -> return Error NickExists
                | _,b when b = usrM -> return Error EmailExists
                | _,_ -> return Error SignErrors.InternalError
                
            else
                return Ok ()
        }
        
        
    let insertUser (user : User) =
        task {
            use! con = openConnection()
            let! exists = isUserExists user
            match exists with
            | Ok _ ->
                use com = con.CreateCommand()
                com.CommandType <- CommandType.Text
                com.CommandText <- "INSERT INTO usersNew (nick,mail,pswd,isConfirmed) VALUES(@nick,@mail,@pswd,0);"
                
                com.Parameters.AddRange [|
                        SqlParameter("@nick",user.username.GetName())
                        SqlParameter("@mail",user.email.GetStringEmail())
                        SqlParameter("@pswd",user.password.GetPswd())
                    |]

                let! _ = com.ExecuteNonQueryAsync()

                return Ok ()

            | x -> return x

        }
    
    let loginUser (data : LogInMessage) =
        task{

            let (|ValidLogin|_|) (nick,pswd,isC) = if nick = data.username && pswd = data.password && isC = 1 then Some () else None
            let (|WrongPassword|_|) (nick,pswd,_) = if nick = data.username && pswd <> data.password then Some () else None
            let (|NotConfirmed|_|) (nick,pswd,_) = if nick = data.username && pswd = data.password then Some () else None

            use! con = openConnection()
            use com = con.CreateCommand()
            com.CommandType <- CommandType.Text
            com.CommandText <- "SELECT usersNew.pswd,usersNew.isConfirmed FROM usersNew WHERE usersNew.nick = @nick;"

            let p1 = SqlParameter("@nick",SqlDbType.VarChar, 100)
            p1.Value <- data.username

            let p2 = SqlParameter("@pswd",SqlDbType.VarChar, 100)
            p2.Value <- data.password

            let! reader = com.ExecuteReaderAsync()
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


    
    