#' @title generate dropbox token
#'
#' @description source form https://stackoverflow.com/questions/71393752/get-a-refresh-token-for-dropbox-api-using-rdrop2-and-drop-auth
#'
#' @param new_user if TRUE is means generate a new token
#' @param dir set directory of token file.
#' @return A token rds file.
#' @export
dropbox_token <- function (new_user = FALSE,dir = NULL,
                           key = "mmhfsybffdom42w",
                           secret = "l8zeqqqgm1ne5z0",
                           cache = TRUE, rdstoken = NA)
{


  if (new_user == FALSE & !is.na(rdstoken)) {
    if (file.exists(rdstoken)) {
      return(readRDS(rdstoken))
    }
    else {
      stop("token file not found")
    }
  }
  else {
    if (new_user && file.exists(".httr-oauth")) {
      message("Removing old credentials...")
      file.remove(".httr-oauth")
    }
    dropbox <- httr::oauth_endpoint(authorize = "https://www.dropbox.com/oauth2/authorize?token_access_type=offline",
                                    access = "https://api.dropbox.com/oauth2/token")
    # added "?token_access_type=offline" to the "authorize" parameter so that it can return an access token as well as a refresh token
    dropbox_app <- httr::oauth_app("dropbox", key, secret)
    dropbox_token <- httr::oauth2.0_token(dropbox, dropbox_app,
                                          cache = cache)
    if (!inherits(dropbox_token, "Token2.0")) {
      stop("something went wrong, try again")
    }
    if(is.null(dir)){
      saveRDS(dropbox_token,"token.rds")
    }else{
      saveRDS(dropbox_token,paste0(dir,"/","token.rds"))
    }

  }
}
