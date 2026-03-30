#' Archive Data Dictionary
#'
#' It archives and compares REDCap data dictionaries.
#'
#' This package is experimental. Please submit bugs to
#' \url{https://github.com/couthcommander/archivedd}.
#'
#' @docType package
#'
#' @author Cole Beck \email{cole.beck@@vumc.org}
#'
#' Maintainer: Cole Beck \email{cole.beck@@vumc.org}
#'
#' @import DBI
#' @import duckdb
#' @import redcapAPI
#' @import R6
#' @importFrom compareDF compare_df create_output_table
#' @importFrom htmltools htmlEscape
#'
#' @examples
#' \donttest{
#' e <- new.env()
#' # this would work if you had these REDCap projects, API access, and shelter
#' unlockREDCap(c('rdb_users', 'rdb_data'), 'https://redcap.vumc.org/api/', 'redcapAPI', envir = e)
#' x <- ARCD$new()
#' x$set(e[['rdb_data']])
#' x$set(e[['rdb_users']])
#' t1 <- x$ls()$tname[1]
#' t2 <- x$ls()$tname[2]
#' x$diff(t1, t2)
#' }
"_PACKAGE"
