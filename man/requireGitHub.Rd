\name{requireGitHub}
\alias{requireGitHub}
\title{Install a specific package version from GitHub if it is not already.}
\usage{
requireGitHub(...)
}
\arguments{
  \item{...}{character vectors matching username/repo@ref}
}
\value{
character matrix with 3 columns: GithubUsername,
GithubRepo, GithubSHA1
}
\description{
Install a specific package version from GitHub if it is not
already.
}
\examples{
requireGitHub::requireGitHub(
  "tdhock/animint@057f34055ee876404caae7abc1c077a7bf126580",
  "rstudio/ggvis@8aa5ae207b3da0ff218fb5a3829bbdd59e54043c")
}
\author{
Toby Dylan Hocking
}

