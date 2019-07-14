context("Input data Argument Validation")

test_that("passing a non-vector object throws an error",{
  expect_error(object = scorecard_calculation(data= 1))
  expect_error(object = scorecard_calculation(data= 3.14))
  expect_error(object = scorecard_calculation(data= "test"))
  expect_error(object = scorecard_calculation(data = matrix(1:10,1:10, ncol=2, nrow=10)))
  expect(object = scorecard_calculation( data = data.frame(a= c("a", "b"), stringsAsFactors = TRUE)))
})

context("Input data Column Validation")

test_that("passing a non-numeric or integer object as first column data throws an error",{
  expect_error(object = scorecard_calculation(data = data.frame(a= c("a","b"), stringsAsFactors = FALSE),
                                              df_scorecard = data.frame(a= c("a","b"), b= 1:2, stringsAsFactors = TRUE)))
  expect_error(object = scorecard_calculation(data = data.frame(a= c("a","b"), stringsAsFactors = TRUE),
                                              df_scorecard = data.frame(a= c("a","b"), b= 1:2, stringsAsFactors = TRUE)))
  expect_error(object = scorecard_calculation(data = data.frame(a= c(TRUE,FALSE), stringsAsFactors = TRUE),
                                              df_scorecard = data.frame(a= c("a","b"), b= 1:2, stringsAsFactors = TRUE)))
  
  })

test_that("passing more than one column in data throws an error",{
  expect_error(object = scorecard_calculation(data = data.frame(a= c("a","b"), b=1:2, stringsAsFactors = FALSE),
                                              df_scorecard = data.frame(a= c("a","b"), b= 1:2, stringsAsFactors = TRUE)))
})

context("Input df_scorecard Argument Validation")

test_that("passing a non-dataframe object throws an error",{
  expect_error(object = scorecard_calculation(df_scorecard= 1))
  expect_error(object = scorecard_calculation(df_scorecard= 3.14))
  expect_error(object = scorecard_calculation(df_scorecard= "test"))
  expect_error(object = scorecard_calculation(df_scorecard = matrix(1:10,1:10, ncol=2, nrow=10)))
})

context("Input First Column df_scorecard Argument Is text")

test_that("passing non-character or non-factor first column throws an error",{
  expect_error(object = scorecard_calculation(df_scorecard = data.frame(a= 1:2, b= 1:2, stringsAsFactors = FALSE)))
  expect_error(object = scorecard_calculation(df_scorecard = data.frame(a= c("a","b"), b= 1:2, stringsAsFactors = TRUE)))
})

context("Input Second Column df_scorecard Argument Is text")

test_that(" passing non-numeric Argument second column throws an error",{
  expect_error(object = scorecard_calculation(data = data.frame(a= c("a","b"), stringsAsFactors = TRUE),
                                              df_scorecard = data.frame(a= c("a","b"), b= 1:2, stringsAsFactors = TRUE)))
  expect_error(object = scorecard_calculation(data = data.frame(a= c("a","b"), b= 1:2, stringsAsFactors = FALSE),
                                              df_scorecard = data.frame(a= c("a","b"), b= c("aa", "bb"), stringsAsFactors = TRUE)))
  
})

context("Input Third Column df_Scorecard Argument is valid logical operator")

test_that(" passing an unvalid logical Argument in third column throws an error",{
  expect_error(object = scorecard_calculation(data = data.frame(a= c("a","b"), stringsAsFactors = F),
                                              df_scorecard = data.frame(a= c("a","b"), b= 1:2, c = c("equal", "smaller", 1), stringsAsFactors = TRUE)))
  
  
})
