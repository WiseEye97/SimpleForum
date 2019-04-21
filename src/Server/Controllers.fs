module Controllers

open System.IO
open Giraffe
open FSharp.Control.Tasks

    type PostData = Stream
    type QueryData = string option

    module UserController =
        open Shared.ClientMessages

        let insertUser (requestStream : PostData) =
            task {
                let! resp = Services.insertUserService requestStream
                return text resp
            }
        let loginUser (loginData : LogInMessage) = 
            task {
                let! resp = Services.loginUserService loginData
                return text resp
            }
        
        let activateAccount (data : QueryData)  = 
            task {
                let! resp = Services.confirmUserService data
                return htmlView (ConfirmPage.index resp)
            }

    module AzureController =
         
        let createCategory (dirname:string option) = 
            task {
                let! resp = Services.createCategoryService dirname
                return text resp
            }
        
        let getAllCategories() = 
            task {
                let! r = Services.getAllCategoriesService()
                if r.IsSome then
                    return Some(text r.Value)
                else
                    return None
            }

            
        

        

            
