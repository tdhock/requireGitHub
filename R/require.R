### Install a package from GitHub if it is not already.
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
  if(any
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
  }
  invisible(match[,-1])
}

