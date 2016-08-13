##' Print a requireGitHub declaration.
##' @param ... unquoted package names.
##' @return An invisible character vector of repository/package
##' version codes.
##' @author Toby Dylan Hocking
##' @export
##' @examples
##' if(FALSE){
##' requireGitHub_code(requireGitHub)
##' }
requireGitHub_code <- function(...){
  pkgs <- match.call()[-1]
  repo.code <- c()
  for(pkg.i in seq_along(pkgs)){
    pkg.name <- as.character(pkgs[[pkg.i]])
    pkg.info <- packageDescription(pkg.name)
    tryCatch({
      repo.code[[pkg.i]] <- with(pkg.info, {
        sprintf("%s/%s@%s", GithubUsername, GithubRepo, GithubSHA1)
      })
    }, error=function(e){
      stop("GitHub meta-data not in ", pkg.name, " DESCRIPTION")
    })
  }
  txt <- deparse(repo.code)
  txt.return <-
    sub("c[(]", "requireGitHub::requireGitHub(\n  ", gsub("[ ]+", "\n ", txt))
  cat(txt.return, "\n")
  invisible(repo.code)
}
