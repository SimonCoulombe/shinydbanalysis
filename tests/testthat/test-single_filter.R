test_that("single_filter_server can be called without error", {
  
  column_info <- load_demo_column_info("diamonds")
  
  # Test with numeric column
  expect_no_error({
    shiny::testServer(
      single_filter_server,
      args = list(
        column_info = column_info,
        column_name = "carat",
        initial_value = NULL
      ),
      {
        # Check that the module returns the expected structure
        expect_true(is.character(session$returned$column))
        expect_true(is.character(session$returned$type))
        expect_true(shiny::is.reactive(session$returned$value))
        expect_true(shiny::is.reactive(session$returned$remove))
        expect_true(shiny::is.reactivevalues(session$returned$is_active))
      }
    )
  })
  
  # Test with categorical column
  expect_no_error({
    shiny::testServer(
      single_filter_server,
      args = list(
        column_info = column_info,
        column_name = "cut",
        initial_value = NULL
      ),
      {
        expect_true(is.character(session$returned$column))
        expect_true(is.character(session$returned$type))
        expect_equal(session$returned$column, "cut")
        expect_equal(session$returned$type, "categorical")
      }
    )
  })
  
  # Test with date column (using gapdata dataset)
  column_info_gapdata <- load_demo_column_info("gapdata")
  expect_no_error({
    shiny::testServer(
      single_filter_server,
      args = list(
        column_info = column_info_gapdata,
        column_name = "date",
        initial_value = NULL
      ),
      {
        expect_true(is.character(session$returned$column))
        expect_true(is.character(session$returned$type))
        expect_equal(session$returned$column, "date")
        expect_equal(session$returned$type, "date")
      }
    )
  })
})

test_that("single_filter_demo runs without error", {
  skip_on_cran()

  # This test just verifies the demo app can be created without error
  expect_no_error({
    app <- single_filter_demo()
    expect_s3_class(app, "shiny.appobj")
  })
})
