#' ARCD, Data Dictionary Archiver
#'
#' @description
#' ARCD class and its methods
#'
#' @examples
#' \donttest{
#' e <- new.env()
#' # this would work if you had these REDCap projects, API access, and shelter
#' unlockREDCap(c('rdb_users', 'rdb_data'), 'https://redcap.vumc.org/api/', 'redcapAPI', envir = e)
#' x <- ARCD$new()
#' # `pull` and `set` REDCap metadata
#' md <- pull(e[['rdb_data']])
#' x$set(md)
#' # or do it in a single step
#' x$set(e[['rdb_users']])
#'
#' # show contents of archive
#' x$ls()
#' # retrieve an archived data dictionary
#' dd <- x$get(x$ls()$tname[1])
#' dim(dd)
#' # show location of "duckdb" database
#' x
#'
#' t1 <- x$ls()$tname[1]
#' t2 <- x$ls()$tname[2]
#' # "simple" difference
#' x$sdiff(t1, t2)
#' # show difference in web browser
#' x$diff(t1, t2)
#' }
#' @export

ARCD <- R6Class("ARCD",
  public = list(
    #' @field con [DBIConnection]\cr
    #' \sQuote{duckdb} database connection.
    con = NULL,
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] ARCD class.
    #' @param dbdir (`character(1)`)\cr
    #'   A file name for a duckdb database; defaults to "ARCD.duckdb".
    initialize = function(dbdir = 'ARCD.duckdb') {
      self$con <- DBI::dbConnect(duckdb(), dbdir = dbdir, read_only = FALSE)
      self$setup()
    },
    #' @description
    #' Print method.
    #' @param ... Additional arguments passed to \code{cat}.
    #' @return itself invisibly.
    print = function(...) {
      # print file location, however you do that
      loc <- self$con@driver@dbdir
      str_head <- 'ARCD object, duckdb file lives here:'
      str <- sprintf('%s\n%s', str_head, loc)
      pnd <- paste(rep('#', max(nchar(str_head), nchar(loc))), collapse = '')
      cat(sprintf('%s\n%s\n%s\n', pnd, str, pnd), ...)
      invisible(self)
    },
    #' @description
    #' Setup method.
    #' @keywords internal
    #' @return itself invisibly.
    setup = function() {
      seq_qry <- "CREATE SEQUENCE IF NOT EXISTS info_id START 1"
      dbExecute(self$con, seq_qry)
      tbl_qry <- "CREATE TABLE IF NOT EXISTS info (id INTEGER PRIMARY KEY DEFAULT nextval('info_id'), name VARCHAR, timestamp INTEGER, tname VARCHAR, checksum VARCHAR)"
      dbExecute(self$con, tbl_qry)
      invisible(self)
    },
    #' @description
    #' List all archived data dictionaries.
    #' @return \sQuote{dbGetQuery} result
    ls = function() {
      sel_qry <- "SELECT * FROM info ORDER BY name, timestamp"
      dbGetQuery(self$con, sel_qry)
    },
    #' @description
    #' Retrieve an archived data dictionary.
    #' @param tname (`character(1)`) table name.
    #' @return \sQuote{dbGetQuery} result.
    get = function(tname) {
      dbGetQuery(self$con, sprintf("SELECT * FROM %s", tname))
    },
    #' @description
    #' Show simple difference between two data dictionaries.
    #' @param md metadata or a \sQuote{redcapConnection}.
    #' @return TRUE for success or FALSE for failure.
    set = function(md) {
      # case where "md" is redcapConnection - call helper function `pull`
      if(inherits(md, 'redcapConnection')) {
        rcon <- md
        md <- pull(rcon)
      }
      id_qry <- sprintf("SELECT MAX(id) AS i FROM info WHERE checksum = '%s'", md$info$checksum)
      res <- dbGetQuery(self$con, id_qry)
      if(is.na(res$i)) {
        dat <- md$info[,c('name','timestamp','tname','checksum')]
        ins_qry <- do.call(sprintf, c("INSERT INTO info (name, timestamp, tname, checksum) VALUES ('%s', %s, '%s', '%s')", dat))
        dbExecute(self$con, ins_qry)
        dbWriteTable(self$con, dat$tname, md$data)
        res <- dbGetQuery(self$con, id_qry)
      }
      # NA would indicate failure (returns FALSE)
      !is.na(res$i)
    },
    #' @description
    #' Show simple difference between two data dictionaries.
    #' @param tnameA (`character(1)`) first table name.
    #' @param tnameB (`character(1)`) second table name.
    #' @return numeric vector.
    sdiff = function(tnameA, tnameB) {
      row_qry <- "SELECT sha256(struct_pack(*columns(*))::text) AS hsh FROM %s"
      q1 <- dbGetQuery(self$con, sprintf(row_qry, tnameA))
      q2 <- dbGetQuery(self$con, sprintf(row_qry, tnameB))
      d1 <- setdiff(q1$hsh, q2$hsh)
      d2 <- setdiff(q2$hsh, q1$hsh)
      same <- intersect(q1$hsh, q2$hsh)
      c(A = length(d1), AB = length(same), B = length(d2))
    },
    #' @description
    #' Show difference between two data dictionaries.
    #' @param tnameA (`character(1)`) first table name.
    #' @param tnameB (`character(1)`) second table name.
    #' @param file_name (`character(1)`) optional file name to save differences as HTML.
    #' @return HTML output.
    diff = function(tnameA, tnameB, file_name = NULL) {
      dA <- self$get(tnameA)
      dB <- self$get(tnameB)
      tA <- as.data.frame(lapply(dA, htmltools::htmlEscape))
      tB <- as.data.frame(lapply(dB, htmltools::htmlEscape))
      cdiff <- compareDF::compare_df(tA, tB, 'form_name')
      compareDF::create_output_table(cdiff, file_name = file_name)
    },
    #' @description
    #' Delete the duckdb database.
    #' @keywords internal
    destroy = function() {
      loc <- self$con@driver@dbdir
      DBI::dbDisconnect(self$con)
      unlink(loc)
    }
  ),
  private = list(
    finalize = function() {
      DBI::dbDisconnect(self$con)
    }
  )
)
