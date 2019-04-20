module Controllers

open System.IO
open Giraffe
open FSharp.Control.Tasks.V2
open FSharp.Control.Tasks
open System.Threading.Tasks
open System.Text
open System.Threading
open Giraffe.HttpStatusCodeHandlers


    type PostData = Stream
    type QueryData = string option

    module UserController =
        
        let insertUser (requestStream : PostData) =
            task {
                let! resp = Services.insertUserService requestStream
                return text resp
            }
        let loginUser (requestStream : PostData) = 
            task {
                let! resp = Services.loginUserService requestStream
                return text resp
            }
        
        let activateAccount (data : QueryData)  = 
            task {
                let! resp = Services.confirmUserService data
                return htmlView (ConfirmPage.index resp)
            }

    module BlogController =
        open Shared.ClientMessages
        
        let insertBlog (blog : NewBlog) =
            
            ()

    module AzureController =
        open Microsoft.Azure // Namespace for Azure Configuration Manager
        open Shared.ClientMessages 
        open Microsoft.WindowsAzure.Storage // Namespace for Storage Client Library
        open Microsoft.WindowsAzure.Storage.Blob // Namespace for Azure Blobs
        open Microsoft.WindowsAzure.Storage.File // Namespace for Azure Files

        let storageAccount = CloudStorageAccount.Parse Config.azureConnection

        let fileClient = storageAccount.CreateCloudFileClient()

        let share = fileClient.GetShareReference "forum"

        let insertToFile (content:string) (filename:string) =
                task {
                    let! exists = share.ExistsAsync()

                    if exists then
                        let rootDir = share.GetRootDirectoryReference()
                        let file = rootDir.GetFileReference filename
                        do! file.UploadTextAsync content
                    else
                        printfn "do not exists"
                    return ()
                }
                
        let readFile() = 
            task{
                let rootDir = share.GetRootDirectoryReference()
                let ff = rootDir.GetDirectoryReference "Wprowadzenie F"

                let file = ff.GetFileReference "Hello F#2"
                let! content = file.DownloadTextAsync()
                printfn "content -> %s" content
            }
        
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

        let insertBlog (blog : NewBlog) =
            ()
            
        

        

            
