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

    type UserErros = 
        | NickExists

    let isUserExists (user : User) =
        task {
            use! con = openConnection()

            if con.State = ConnectionState.Closed then
                con.Open()
        
            use com = con.CreateCommand()
            com.CommandType <- CommandType.Text
            com.CommandText <- "SELECT TOP(1) * FROM usersNew WHERE USERS.nick = '@nick';"

            let param = SqlParameter("@nick",user.username)
            com.Parameters.Add param |> ignore

            
            let! reader = com.ExecuteReaderAsync()
            return reader.HasRows
        }
        
        
    let insertUser (user : User) =
        task {
            use! con = openConnection()
            let! exists = isUserExists user
            if exists then
                return Error NickExists
            else
                use com = con.CreateCommand()
                com.CommandType <- CommandType.Text
                com.CommandText <- "INSERT INTO userNew (nick,mail,pswd,isConfirmed) VALUES('@nick','@mail','@pswd',0);"

                com.Parameters.AddRange [|
                        SqlParameter("@nick",user.username.GetName())
                        SqlParameter("@mail",user.email.GetMail().Value)
                        SqlParameter("@pswd",user.password.GetPswd())
                    |]
                let! _ = com.ExecuteNonQueryAsync()

                return Ok ()
        }


    
    