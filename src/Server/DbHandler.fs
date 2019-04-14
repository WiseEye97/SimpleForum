module DbHandler

open Config
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

    
    let isUserExists (user : User) =
        task {
            use! con = openConnection()

            if con.State = ConnectionState.Closed then
                con.Open()
        
            use com = con.CreateCommand()
            com.CommandType <- CommandType.Text
            com.CommandText <- "SELECT TOP(1) nick,mail FROM usersNew WHERE usersNew.nick = @nick OR usersNew.mail = @mail;"

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
                | _,_ -> return Error InternalError
                
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


    
    