# ---
# repo: DfE-R-Community/r-standalone
# file: standalone-sql-connections.R
# last-updated: 2023-06-27
# license: https://unlicense.org
# dependencies: 
# imports: [cli, DBI, odbc, dplyr, config]
# ---

# nocov start

#' Connect to a database
#' 
#' This function assumes you have a `config.yml` file at the top-level of your
#' project containing the configuration you want to use for each database. A
#' typical config.yml will look something like this:
#'
#' ```
#' default:
#'   appr:
#'     driver: "ODBC Driver 17 for SQL Server"
#'     server: "T1PRANMSQL\SQLPROD,60125"
#'     database: "MA_AM_S_APPR"
#'     uid: ""
#'     pwd: ""
#'     trusted: "yes"
#' ```
#' To connect to the `appr` database using these settings you would use
#' `connect_to_db("appr")`.
#'
#' @param db The name for the database. This should correspond to a section
#'   in your config file where the configuration for the database is set out
#' @param config_file The config file to use
#' @md
#'
#' @return A DBI connection object
#' @noRd
connect_to_db <- function(db, config_file = "config.yml") {
  
  config <- config::get(db, file = config_file)
  required_fields <- c("driver", "server", "database", "uid", "pwd", "trusted")
  missing_fields <- setdiff(required_fields, names(config))
  
  if (length(missing_fields) > 0L) {
    cli::cli_abort(c(
      "Required fields missing in {.file {config_file}}",
      i = "Check {.field {not_found}}"
    ))
  }
  
  DBI::dbConnect(
    odbc::odbc(),
    Driver = config[["driver"]],
    Server = config[["server"]],
    Database = config[["database"]],
    UID = config[["uid"]],
    PWD = config[["pwd"]],
    Trusted_Connection = config[["trusted"]] 
  )
  
}

#' Connect to a database table
#' 
#' This function returns a 'lazy' connection to a database table, which will
#' give a preview of the first few rows, but won't read the whole dataset into
#' your R session. To read the full dataset, you need to call [dplyr::collect()]
#' on the output.
#' 
#' @inheritParams connect_to_db
#' @param schema,table The schema/table to connect to 
#' @param con A DBI connection. You may want to set this manually, e.g.
#'   if you want to perform joins before calling [dplyr::collect()] on the
#'   result, in which case both tables will need to have been created using
#'   the same connection object.
#'
#' @return A dplyr 'lazy' tbl
#' @noRd
connect_to_table <- function(db, schema, table, con = connect_to_db(db)) {
  dplyr::tbl(con, DBI::Id(Schema = schema, Table = table))
}

#' Read data using an SQL script
#'
#' @param sql_script The SQL script to use
#' @inheritParams connect_to_table
#'
#' @return A tibble
#' @noRd
read_from_sql <- function(sql_script, db, con = connect_to_db(db)) {
  sql <- readLines(sql_script)
  line_is_use_statement <- grepl("^\\s*use\\s+", sql)
  
  if (any(line_is_use_statement)) {
    n_bad <- sum(line_is_use_statement)
    lines <- which(line_is_use_statement)
    cli::cli_warn(c(
      "SQL scripts should not contain {.strong USE}-statments",
      i = "The database to use should be specified in {.file config.yml",
      i = "Check {.file {sql_script}} {cli::qty(n_bad)} line{?s} {.emph {lines}}"
    ))
    sql <- sql[!line_is_use_statement]
  }
  
  sql <- dbplyr::sql(paste(sql, collapse = "\n"))
  dplyr::tbl(con, sql)
}

# nocov end