#' Function to update the package from github. Mainly for simpler updating on the shiny server
#'
#' @param repo user/repo name
#' @param token_file file with token
#' @param token auth_token for devtools::install_github
#' @param branch name of branch to update from
#' @param ... used for devtools::install_github
#'
#' @examples
#' update_package()
#' update_package(repo = "User/Repo_Name", token = "your_token", branch = "shiny")
#'
#' @importFrom devtools install_github
#' @export


update_package <- function(repo = "elMrt/auto-statistics", token_file, token, branch = "main", ...){
  if(!missing(token_file)){
    token <- readLines(token_file, warn = FALSE)
    # read branch from second Line -> token has length of 2
    if(length(token) != 1){
      branch = token[2]
      token = token[1]
    }
  }
  tryCatch({
    devtools::install_github(repo = repo, auth_token = token, ref = branch, ...)
  },
  error=function(cond) {
    message(cond)
  })
}
