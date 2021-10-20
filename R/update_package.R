#' Function to update the package from github. Mainly for simpler updating on the shiny server
#'
#' @param repo user/repo name
#' @param token_file file with token
#' @param token auth_token for devtools::install_github
#'
#' @examples
#' update_package()
#' update_package(repo = "User/Repo_Name", token = "your_token")
#'
#' @importFrom devtools install_github
#' @export


update_package <- function(repo = "elMrt/auto-statistics", token_file, token){
  if(!missing(token_file)){
    token <- readLines(token_file, warn = FALSE)
  }
  tryCatch({
    devtools::install_github(repo = repo, auth_token = token)
  },
  error=function(cond) {
    message(cond)
  }
  )
}
