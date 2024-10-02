#' @title generate OneDrive token
#'
#' @description OneDrive
#'
#' @importFrom Microsoft365R personal_onedrive
#' @param dir set directory of token file.
#' @return A token rds file.
#' @export
onedrive_token <- function (dir = NULL,new_user=F)
{
  if(new_user){
    AzureAuth::clean_token_directory()
  }
  od <- get_personal_onedrive()
    if(is.null(dir)){
      if(!dir.exists(paste0(getwd(),"/data"))){
        dir.create(paste0(getwd(),"/data"))
      }
      saveRDS(od$token,"data/token.rds")
    }else{
      if(!dir.exists(paste0(dir,"/data"))){
        dir.create(paste0(dir,"/data"))
      }
      saveRDS(od$token,paste0(dir,"/data/token.rds"))
    }


}
