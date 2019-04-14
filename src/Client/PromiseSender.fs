module PromiseSender

open Elmish
open Fable.PowerPack.Fetch
open Fable.PowerPack

let sendGenericJson<'a,'b> content fields decoder url (onS : 'a -> 'b) onE =
        let defProps = [
                    RequestProperties.Method HttpMethod.POST
                    Fetch.requestHeaders [HttpRequestHeaders.ContentType "application/json"]
                    RequestProperties.Body (unbox(Thoth.Json.Encode.Auto.toString(fields, content)))
        ]
        let cmd = promise{
                let! res = Fetch.fetchAs<'a> url decoder defProps
                return res
        } 

        Cmd.ofPromise (fun _ -> cmd) () onS onE