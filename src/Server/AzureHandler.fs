module AzureHandler
    open Microsoft.Azure // Namespace for Azure Configuration Manager
    open Shared.ClientMessages 
    open Microsoft.WindowsAzure.Storage // Namespace for Storage Client Library
    open Microsoft.WindowsAzure.Storage.Blob // Namespace for Azure Blobs
    open Microsoft.WindowsAzure.Storage.File // Namespace for Azure Files
    open FSharp.Control.Tasks.V2
    open Shared.ServerMessages
    open Microsoft.WindowsAzure.Storage.File
    open Microsoft.WindowsAzure.Storage.File
    open System.Linq
    open Shared.ClientMessages
    open Saturn.Utils
    open Giraffe.Serialization
    open System.Xml
    open System.IO

    
    let storageAccount = CloudStorageAccount.Parse Config.azureConnection

    let fileClient = storageAccount.CreateCloudFileClient()

    let share = fileClient.GetShareReference "forum"

    let createCategory (dirname:string)  = 
        task {
            let rootDir = share.GetRootDirectoryReference()
            let newDir = rootDir.GetDirectoryReference dirname
            let! isCreated = newDir.CreateIfNotExistsAsync()
            return
                if isCreated then
                    Ok ()
                else
                    Error CantAccesServer
        }
    
    let getAllCategories() =
        task{
            let rootDir = share.GetRootDirectoryReference()
            let! contents = rootDir.ListFilesAndDirectoriesSegmentedAsync (FileContinuationToken())
            let dirs = contents.Results
                            .OfType<CloudFileDirectory>()
                            .Select(fun x -> x.Name)
                            .ToArray()
            return Ok dirs
        }

    let getBlogsFromCategory (category:string) =
        task {
            let rootDir = share.GetRootDirectoryReference()
            let catDir = rootDir.GetDirectoryReference category
            let! contents = catDir.ListFilesAndDirectoriesSegmentedAsync (FileContinuationToken())
            let files = contents.Results
                            .OfType<CloudFile>()
                            .Select(fun x -> x.Name)
                            .ToArray()
            return files
        }

    let xmlToBlog (category:string) (title:string) =
        task{
            let rootDir = share.GetRootDirectoryReference()
            let catDir = rootDir.GetDirectoryReference category
            let blogFile = catDir.GetFileReference title
            
            let! content = blogFile.DownloadTextAsync()
            let settings = XmlReaderSettings()
            settings.Async <- true
            use strReader = new StringReader(content)
            use reader = XmlReader.Create(strReader,settings)

            let rec loop res lastNode =
                task{
                    let! r = reader.ReadAsync()
                    if r then
                      
                        match reader.NodeType with
                        | XmlNodeType.Element ->
                            return! loop res reader.Name
                        | XmlNodeType.Text ->
                            let nl = 
                                match lastNode with
                                    | "code" ->
                                        {content = reader.Value;tp = Code} :: res 
                                    | "normal" ->
                                        {content = reader.Value;tp = Normal} :: res
                                    | _ -> res 
                            return! loop nl lastNode
                        | _ -> return! loop res lastNode
                               
                    else
                        return res
                }
                
            let! sections =  loop [] ""
            let tt = {category = category;topic = title;sections = sections}
            printfn "tt -> %A" tt
            return tt
        }

    let insertBlog (blog: NewBlog) =
        task{
            let rootDir = share.GetRootDirectoryReference()
            let categoryDir = rootDir.GetDirectoryReference blog.category
            let! isExists = categoryDir.ExistsAsync()
            if isExists then
                let file = categoryDir.GetFileReference blog.topic
                let xmlHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                let bd = 
                    [ 
                        for section in blog.sections ->
                            match section with
                            | {content = c;tp = Code} -> sprintf "<code>%s</code>" c
                            | {content = c;tp = Normal} -> sprintf "<normal>%s</normal>" c
                    ]
                    |> String.concat ""
                printfn "%s" bd    
                let content = sprintf "%s <blog>%s</blog>" xmlHeader bd

                do! file.UploadTextAsync content
        }