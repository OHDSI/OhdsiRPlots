test_that("Create Ohdsi Palette", {
  skip_if(skipCdmTests, "CDM settings not configured")
  output <- createOhdsiPalette()
  testthat::expect_length(object = output, n = 5)
})