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
