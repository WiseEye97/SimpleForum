namespace Shared


module ClientMessages =

    type SignInMessage = {
        username : string
        email : string
        password : string
    }

    type LogInMessage = {
        username : string
        password : string
    }

module ServerMessages =

    type SignErrors = 
        | NickExists
        | EmailExists
        | InternalError

    type LoginErrors =
        | WrongNick
        | WrongPassword
        | AccountNotConfirmed
        | InternalServError

    type SimpleServerResponse<'e> = {
        state : bool
        errorMessage : 'e option 
    }

    type SignInResponse = {
        state : bool
        errorMessage : SignErrors option 
    }

    type LogInResponse = SimpleServerResponse<LoginErrors>



module UserModel = 
    open System.Text.RegularExpressions
    open System

    type UserName = private UserName of string
    with
        member this.GetName() = 
            match this with
            | UserName usr -> usr

    type ValidEmail = private ValidEmail of string
    with
        member this.GetStringEmail() =
            match this with
            | ValidEmail em -> em

    type Password = private Password of string
    with 
        member this.GetPswd() = 
            match this with
            | Password p -> p

    
        
    let createUserName (s:string) = 
        if s.Length > 6 && (not (s.Contains "@")) then
            Some (UserName s)
        else
            None
    
    let createEmail (s:string) =
        if Regex.IsMatch(s,@"^([\w\.\-]+)@([\w\-]+)((\.(\w){2,3})+)$") then
            s |> ValidEmail |> Some
        else
            None

    let createPassword (s:string) =
        let rec loop : (int * char list -> bool) =
            function
            | 0,_ -> true
            | _,[] -> false
            | x,h::t when Char.IsDigit h -> loop (x&&&3, t)
            | x,h::t when Char.IsLower h -> loop (x&&&5, t)
            | x,h::t when Char.IsLetter h -> loop (x&&&6, t)
            | x,_::t -> loop (x, t)

        let li = s.ToCharArray() |> List.ofArray

        if loop (7,li) then Some (Password s) else None
        

    let (|UserNamePatt|_|) (s:string) = createUserName s
    let (|PasswordPatt|_|) (s:string) = createPassword s
    let (|ValidEmaikPatt|_|) (s:string) = createEmail s
       

    type User = {
        username : UserName
        email : ValidEmail
        password : Password
    }

    with 
        member this.UpdateUserByName (usrName:UserName) = {this with username = usrName}
        member this.UpdateUserByEmail (email:ValidEmail) = {this with email = email}
        static member CreateUser (usrName:UserName) (email:ValidEmail) (password : Password) = {
            username = usrName
            email = email
            password = password
        }
        static member CreateFromStrings : (string*string*string) -> User option  =
            function
            | UserNamePatt usr , PasswordPatt pswd, ValidEmaikPatt em -> Some {username = usr;email = em;password = pswd}
            | _ -> None


            