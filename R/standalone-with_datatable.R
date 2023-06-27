# ---
# repo: DfE-R-Community/r-standalone
# file: standalone-with_datatable.R
# last-updated: 2023-06-27
# license: https://unlicense.org
# dependencies: 
# imports: [rlang, cli, dplyr, dtplyr]
# ---

#' Run a function using data.table
#'
#' Data manipulations using {dplyr} can often be made faster by using 
#' {data.table}. {dtplyr} is a package which automatically translated {dplyr}
#' code to {data.table}, and this function provides a wrapper which allows
#' this behaviour to be toggled within a project by setting 
#' `options(use_datatable = FALSE)` or `options(use_datatable = TRUE)`. This
#' function will also fall back to {dplyr} if the translated code throws any
#' errors or warnings.
#'
#' @param x A dataframe/tibble
#' @param f A function using dplyr code to be translated to data.table. This 
#'   will be called on `x`
#' @param ... Extra arguments passed to `f()`
#'
#' @return The result of calling `f()` on `x`
#' @noRd
#' 
#' @examples
#' iris |> 
#'   with_datatable(\(x) {
#'     x |> 
#'       dplyr::mutate(
#'         Sepal.Area = Sepal.Length * Sepal.Width,
#'         Petal.Area = Petal.Length * Petal.Width
#'       ) |> 
#'       dplyr::summarise(
#'         .by = Species,
#'         dplyr::across(c(Sepal.Area, Petal.Area), mean)
#'       )
#'   })
with_datatable <- function(x, f, ...) {
  
  stopifnot(is.data.frame(x))
  f <- rlang::as_function(f)
  
  cli::cli_inform(
    c(
      "Some {.pkg dplyr} code will be run using {.pkg data.table}",
      i = "Use {.code options(use_datatable = FALSE)} to turn off this behaviour"
    ),
    .frequency = "once",
    .frequency_id = "use_datatable"
  )
  
  if (!getOption("use_datatable", TRUE)) {
    return(f(x, ...))
  }
  
  fall_back_to_dplyr <- function(type) {
    function(cond) {
      cli::cli_warn(
        c(
          "Could not use {.pkg data.table} to run code",
          i = "{.pkg dplyr} was used instead",
          i = "Using {.pkg dtplyr} gave the following {type}: {.code {cond$message}}"
        )
      )
      f(x, ...)
    }
  }
  
  tryCatch(
    dplyr::collect(f(dtplyr::lazy_dt(x), ...)),
    warning = fall_back_to_dplyr("warning"),
    error = fall_back_to_dplyr("error")
  )
  
}
