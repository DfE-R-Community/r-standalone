test_that("with_datatable() works", {
  
  f <- function(x) {
    x |>
      dplyr::as_tibble() |> 
      dplyr::mutate(
        Sepal.Area = Sepal.Length * Sepal.Width,
        Petal.Area = Petal.Length * Petal.Width
      ) |>
      dplyr::summarise(
        .by = Species,
        dplyr::across(c(Sepal.Area, Petal.Area), mean)
      )
  }
  
  expect_identical(f(iris), with_datatable(iris, f))
  
})
