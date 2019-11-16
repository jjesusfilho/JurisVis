#' \code{JurisVis} package
#'
#' Text mining of Brazilian judicial decisions
#'
#'
#' @docType package
#' @name JurisVis
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")

  utils::globalVariables(c(".", "apply.monthly","value","n","total" ,"stage",
                           "df","vara","processing_time","foro","comarca","target", "decisao","percentual"))
