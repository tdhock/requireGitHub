requireGitHub_code <- function(...){
  pkgs <- match.call()[-1]
  repo.code <- c()
  for(pkg.i in seq_along(pkgs)){
    pkg.name <- as.character(pkgs[[pkg.i]])
    pkg.info <- packageDescription(pkg.name)
    repo.code[[pkg.i]] <- with(pkg.info, {
      sprintf("%s/%s@%s", GithubUsername, GithubRepo, GithubSHA1)
    })
  }
  txt <- deparse(repo.code)
  txt.return <- sub("c[(]", "requireGitHub(", gsub("[ ]+", "\n ", txt))
  cat(txt.return, "\n")
  invisible(repo.code)
}