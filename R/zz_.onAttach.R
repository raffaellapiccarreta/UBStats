#' @importFrom utils globalVariables
.onLoad <- function(lib, pkg){
  # quiets concerns of R CMD check re: the .'s that appear in pipelines
  # (see https://github.com/tidyverse/magrittr/issues/29)
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(unique(c(".private_creator_markdown_piccarreta")))
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(sprintf("Package %s (%s) loaded.\nTo cite, type citation(\"%s\")",
    pkgname, utils::packageDescription(pkgname)$Version, pkgname))
  # packageStartupMessage("Caution: ", pkgname, " is under active developement! Breaking changes may occur in the future.")
  packageStartupMessage("Please report improvements and bugs to: https://github.com/raffaellapiccarreta/UBStats/issues")
}
