test_that("(academic|financial)_year works", {
  expect_equal(length(academic_year()), 0)
  expect_equal(length(financial_year()), 0)
  
  expect_equal(academic_year(2020) + 1, academic_year(2021))
  expect_equal(financial_year(2020) + 1, financial_year(2021))
  
  expect_error(c(academic_year(2020, as.Date("2020-01-01")), academic_year(2020, as.Date("2020-02-01"))))
  expect_error(c(financial_year(2020, as.Date("2020-01-01")), financial_year(2020, as.Date("2020-02-01"))))
  
  expect_error(academic_year(2020) + 1.5)
  expect_error(financial_year(2020) + 1.5)
  
  expect_error(academic_year(10000))
  expect_error(financial_year(10000))
  expect_error(academic_year(-1))
  expect_error(financial_year(-1))
  
  expect_error(academic_year(2020) + Sys.Date())
  expect_error(financial_year(2020) + Sys.Date())
})

test_that("(academic|financial)_year works with ggplot2", {
  
  plot_data <- data_frame(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2029-01-01"), by = "1 year"),
    value = 1:10
  )
  
  expect_no_error(
    ay_plot <- ggplot(plot_data, aes(academic_year(date), value)) +
      geom_col()
  )
  
  expect_equal(layer_scales(ay_plot)$x$labels(2021), "2021/22")
  
  expect_no_error(
    fy_plot <- ggplot(plot_data, aes(financial_year(date), value)) +
      geom_col()
  )
  
  expect_equal(layer_scales(fy_plot)$x$labels(2021), "2021-22")
  
})