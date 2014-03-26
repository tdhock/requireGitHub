##' Install a specific package version from GitHub if it is not already.
##' @param ... character vectors matching username/repo@@ref
##' @return character matrix with 3 columns: GithubUsername,
##' GithubRepo, GithubSHA1
##' @author Toby Dylan Hocking
##' @export
##' @examples
##' requireGitHub::requireGitHub(
##'   "tdhock/animint@@057f34055ee876404caae7abc1c077a7bf126580",
##'   "rstudio/ggvis@@8aa5ae207b3da0ff218fb5a3829bbdd59e54043c")
requireGitHub <- function(...){
  repo.code <- c(...)
  stopifnot(is.character(repo.code))
  pattern <-
    paste0("(?<GithubUsername>[^/]+)",
           "/",
           "(?<GithubRepo>[^#]+)",
           "@",
           "(?<GithubSHA1>[a-f0-9]{40})")
  match <- str_match_perl(repo.code, pattern)
  pkg.counts <- table(match[,"GithubRepo"])
  bad <- pkg.counts > 1
  if(any(bad)){
    print(pkg.counts[bad])
    stop("can not require a package more than once")
  }
  bad <- is.na(match[,1])
  if(any(bad)){
    print(repo.code[bad])
    stop("did not match pattern ", pattern)
  }
  check.names <- c("GithubUsername", "GithubSHA1")
  for(pkg.i in 1:nrow(match)){
    pkg.info <- match[pkg.i,]
    pkg.name <- pkg.info[["GithubRepo"]]
    pkg.desc <- packageDescription(pkg.name)
    same <- sapply(check.names, function(x){
      pkg.info[[x]] == pkg.desc[[x]]
    })
    if(!all(same)){
      require(devtools)
      install_github(repo.code[[pkg.i]])
    }
    require(pkg.name, character.only=TRUE)
  }
  invisible(match[,-1])
}
