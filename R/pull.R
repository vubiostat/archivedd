#' Pull REDCap Data Dictionary
#'
#' Print method for \sQuote{packageInfo} class.
#'
#' Print its argument and return it invisibly.
#'
#' @param rcon A redcapConnection object.
#'
#' @examples
#' \donttest{
#' e <- new.env()
#' # this would work if you had these REDCap projects, API access, and shelter
#' unlockREDCap(c('rdb_users', 'rdb_data'), 'https://redcap.vumc.org/api/', 'redcapAPI', envir = e)
#' md <- pull(e[['rdb_data']])
#' md$info
#' dim(md$data)
#' }
#' @export

pull <- function(rcon) {
  stopifnot(inherits(rcon, 'redcapConnection'))
  con <- dbConnect(duckdb(), dbdir = ":memory:")
  on.exit(dbDisconnect(con, shutdown = TRUE))
  dd <- rcon$metadata()
  pname <- rcon$projectInformation()$project_title
  ts <- floor(unclass(Sys.time()))
  duckdb_register(con, pname, dd)
  res <- dbGetQuery(con, sprintf("SELECT sha256(list(%s)::text) AS checksum FROM %s", pname, pname))
  info <- data.frame(name = pname, timestamp = ts, tname = paste(pname, ts, sep = '_'), checksum = res$checksum)
  duckdb_unregister(con, pname)
  list(info = info, data = dd)
}
