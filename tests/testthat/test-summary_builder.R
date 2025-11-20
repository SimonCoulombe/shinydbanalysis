test_that("summary_builder_server can be called without error", {
  
  column_info <- load_demo_column_info("diamonds")
  
  expect_no_error({
    shiny::testServer(
      summary_builder_server,
      args = list(
        selected_table_name = shiny::reactiveVal("diamonds"),
        column_info = shiny::reactiveVal(column_info)
      ),
      {
        # Check that the module returns the expected structure
        expect_true(shiny::is.reactive(session$returned$summary_specs))
        expect_true(shiny::is.reactive(session$returned$group_vars))
        expect_true(shiny::is.reactive(session$returned$needs_summary))
        expect_true(shiny::is.reactive(session$returned$banding_configs))
        expect_true(shiny::is.reactive(session$returned$regrouping_configs))
        
        # Initially, should have count enabled but no metrics
        expect_equal(length(session$returned$summary_specs()), 1)
        expect_equal(session$returned$summary_specs()[[1]]$func, "count")
        expect_true(session$returned$needs_summary())
        expect_null(session$returned$group_vars())
        expect_equal(length(session$returned$banding_configs()), 0)
        expect_equal(length(session$returned$regrouping_configs()), 0)
      }
    )
  })
  
  # Test with different table
  column_info_iris <- load_demo_column_info("iris")
  expect_no_error({
    shiny::testServer(
      summary_builder_server,
      args = list(
        selected_table_name = shiny::reactiveVal("iris"),
        column_info = shiny::reactiveVal(column_info_iris)
      ),
      {
        expect_true(shiny::is.reactive(session$returned$summary_specs))
        expect_true(shiny::is.reactive(session$returned$needs_summary))
        expect_true(session$returned$needs_summary())
      }
    )
  })
  
  # Test with gapdata (has date column)
  column_info_gapdata <- load_demo_column_info("gapdata")
  expect_no_error({
    shiny::testServer(
      summary_builder_server,
      args = list(
        selected_table_name = shiny::reactiveVal("gapdata"),
        column_info = shiny::reactiveVal(column_info_gapdata)
      ),
      {
        expect_true(shiny::is.reactive(session$returned$summary_specs))
        expect_equal(length(session$returned$summary_specs()), 1)
        expect_equal(session$returned$summary_specs()[[1]]$func, "count")
      }
    )
  })
})

test_that("summary_builder_demo runs without error", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  
  # This test just verifies the demo app can be created without error
  expect_no_error({
    app <- summary_builder_demo()
    expect_s3_class(app, "shiny.appobj")
  })
})
