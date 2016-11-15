context("geo")

test_that("geo works", {
  
  expect_equal(25.31015, dms2dd(dd2dms(25.31015)))
  
  expect_equal(-80.37198, dms2dd(dd2dms(-80.37198)))

})