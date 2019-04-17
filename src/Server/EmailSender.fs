module EmailSender

open Shared.UserModel


let private sendEmail (destination : string) title content =
    let mail = new System.Net.Mail.MailMessage()
    mail.To.Add destination
    mail.From <- System.Net.Mail.MailAddress("arekgpn@gmail.com", title, System.Text.Encoding.UTF8)
    mail.Subject <- "This mail is send from asp.net application"
    mail.SubjectEncoding <- System.Text.Encoding.UTF8
    mail.Body <- content
    mail.BodyEncoding <- System.Text.Encoding.UTF8
    mail.IsBodyHtml <- true 
    mail.Priority <- System.Net.Mail.MailPriority.High
    let client = new System.Net.Mail.SmtpClient()
    client.Credentials <- System.Net.NetworkCredential("arekgpn@gmail.com", "9K%XXaIs137929")
    client.Port <- 587
    client.Host <- "smtp.gmail.com"
    client.EnableSsl <- true
    client.SendMailAsync mail
        
    
let sendActivationEmail (user:User) =
    sendEmail (user.email.GetStringEmail()) "Auth" (sprintf "To activate an account go here -> http://localhost:8085/api/confirm?x=%s" (user.username.GetName())) 

let sendNotification (destination : string) (from:string) = 
    from
    |> sprintf "%s is sending you a message"
    |> sendEmail destination "New Message" 