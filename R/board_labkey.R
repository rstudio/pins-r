labkey_headers <- function(board, verb, path, file, ...) {
  
  if(verb == "PUT")
    verb <- "POST"
  
  headers <- Rlabkey::labkey.getRequestOptions(method = verb)
  return(headers)
}


board_initialize.labkey <- function(board,
                                    api_key = Sys.getenv("LABKEY_API_KEY"),
                                    base_url, 
                                    folder,
                                    cache = NULL,
                                    ...) {
  
  pin_subdir <- "pins"
  
  if (nchar(api_key) == 0) stop("The 'labkey' board requires a 'api_key' parameter.")
  if (nchar(base_url) == 0)  stop("The 'labkey' board requires a 'base_url' parameter for the labkey server.")
  if (nchar(folder) == 0)  stop("The 'labkey' board requires a 'folder' parameter for top folder to house the subdirectory for all pins")
  
  
  # Globally sets the api_key and base_url
  Rlabkey::labkey.setDefaults(apiKey = api_key, baseUrl = base_url)
  
  dirExists <- Rlabkey::labkey.webdav.pathExists(folderPath = folder, remoteFilePath = pin_subdir)
  
  # Make labkey directory if does not exist
  if (!dirExists)
    Rlabkey::labkey.webdav.mkDir(folderPath = folder, remoteFilePath = pin_subdir)
  
  
  # Build URL
  labkey_url <- Rlabkey:::labkey.webdav.validateAndBuildRemoteUrl(baseUrl = base_url,
                                                                  folderPath = folder,
                                                                  fileSet = "@files",
                                                                  remoteFilePath = pin_subdir)
  
  
  board_register_datatxt(name = board$name,
                         url = labkey_url,
                         headers = labkey_headers,
                         cache = cache,
                         needs_index = FALSE,
                         connect = FALSE,
                         ...)
  
  board_get(board$name)
}