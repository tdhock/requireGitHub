##' Install a specific package version from GitHub if it is not already.
##' @param ... character vectors matching username/repo@@ref
##' @return data.frame with 4 columns: GithubUsername,
##' GithubRepo, GithubSHA1, install (result of requireGitHub_package)
##' @author Toby Dylan Hocking
##' @export
##' @examples
##' if(FALSE){
##' requireGitHub::requireGitHub(
##'   "tdhock/animint@@057f34055ee876404caae7abc1c077a7bf126580",
##'   "rstudio/ggvis@@8aa5ae207b3da0ff218fb5a3829bbdd59e54043c")
##' }
requireGitHub <- function(...){
  repo.code <- c(...)
  stopifnot(is.character(repo.code))
  match <- nc::capture_first_vec(
    repo.code,
    "^",
    GithubUsername="[^/]+",
    "/",
    GithubRepo="[^/@]+",
    "@",
    GithubSHA1="[a-f0-9]{40}",
    "$")
  repo.vec <- match[["GithubRepo"]]
  pkg.counts <- table(repo.vec)
  bad <- pkg.counts > 1
  if(any(bad)){
    print(pkg.counts[bad])
    stop("can not require a package more than once")
  }
  bad <- is.na(match[,1])
  if(any(bad)){
    print(repo.code[bad])
    stop("did not match user/repo@sha1")
  }
  match.df <- data.frame(match, install=NA, row.names=repo.vec)
  for(pkg.i in 1:nrow(match)){
    pkg.info <- match[pkg.i,]
    match.df$install[[pkg.i]] <- requireGitHub_package(
      pkg.info[["GithubUsername"]],
      pkg.info[["GithubRepo"]],
      pkg.info[["GithubSHA1"]],
      pkg.info[["GithubRepo"]])

  }
  invisible(match.df)
}

##' require a specific version of a package from GitHub, installing
##' it if necessary.
##'
##' This function can be used for packages which are not in the root
##' directory of a GitHub repo, or for packages which do not have the
##' same name as the GitHub repo.
##' @title require a package from GitHub
##' @param username github username, e.g. "aldro61"
##' @param path repo name and path to R pkg, e.g. "mmit/Rpackage"
##' @param sha1 hash
##' @param pkg.name the package name that you give to library ("mmit"
##' in the example above).
##' @return was the package installed? TRUE or FALSE
##' @author Toby Dylan Hocking
##' @export
##' @examples
##' if(FALSE){
##' requireGitHub_package(
##'   "aldro61",
##'   "mmit/Rpackage",
##'   "edf81ba77fdd4b005ad89b81d9e12d289c8146e9",
##'   "mmit")
##' }
##' @importFrom utils packageDescription
##' @importFrom remotes install_github
requireGitHub_package <- function(username, path, sha1, pkg.name){
  stopifnot(
    is.character(username), length(username)==1,
    is.character(path), length(path)==1,
    is.character(sha1), length(sha1)==1,
    is.character(pkg.name), length(pkg.name)==1
    )
  pkg.desc <- suppressWarnings(utils::packageDescription(pkg.name))
  do.install <- FALSE
  if(is.na(pkg.desc)[1]){
    do.install <- TRUE
  }else{
    check.vec <- c(
      GithubUsername=username,
      GithubSHA1=sha1)
    same <- sapply(names(check.vec), function(x){
      if(x %in% names(pkg.desc)){ #installed using devtools.
        check.vec[[x]] == pkg.desc[[x]]
      }else{#installed manually, so we don't know what version it is.
        FALSE
      }
    })
    if(!all(same)){
      do.install <- TRUE
    }
  }
  if(do.install){
    remotes::install_github(
      sprintf("%s/%s@%s", username, path, sha1),
      upgrade_dependencies=FALSE,
      dependencies=FALSE,
      build_vignettes=FALSE)
  }
  require(pkg.name, character.only=TRUE)
  do.install
}
