#' \code{jurisMiner} package
#'
#' Text mining of Brazilian judicial decisions
#'
#'
#' @docType package
#' @name jurisMiner
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")

  utils::globalVariables(c(".", "apply.monthly","value","n","total","desc" ,"tempo_processamento","stage"))
