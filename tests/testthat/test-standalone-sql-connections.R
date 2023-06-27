test_that("SQL connections work", {
  
  skip_if_offline()
  
  tryCatch(
    con <- connect_to_db("dwh"),
    error = function(e) skip("Could not connect to DWH_PL")
  )
  
  expect_no_error(
    connect_to_table(NULL, "DART", "Dim_Dataset", con) |> 
      head(10) |> 
      dplyr::collect()
  )
  
  expect_warning(
    results <- read_from_sql("test-query.sql", con = con),
    "SQL scripts should not contain"
  )
  
  expect_s3_class(dplyr::collect(results), "tbl_df")
  
})
