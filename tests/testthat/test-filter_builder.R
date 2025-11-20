test_that("filter_builder_server can be called without error", {
  
  storage_info <- get_demo_storage_info()
  
  expect_no_error({
    shiny::testServer(
      filter_builder_server,
      args = list(
        storage_info = storage_info,
        selected_table_name = shiny::reactiveVal("diamonds"),
        restricted_columns = shiny::reactiveVal(character(0))
      ),
      {
        # Check that the module returns the expected structure
        expect_true(shiny::is.reactive(session$returned$where_clause))
        expect_true(shiny::is.reactive(session$returned$current_filters))
        
        # Initially, should have no filters
        expect_equal(session$returned$where_clause(), "")
        expect_equal(length(session$returned$current_filters()), 0)
      }
    )
  })
  
  # Test with restricted columns
  expect_no_error({
    shiny::testServer(
      filter_builder_server,
      args = list(
        storage_info = storage_info,
        selected_table_name = shiny::reactiveVal("diamonds"),
        restricted_columns = shiny::reactiveVal(c("price", "depth"))
      ),
      {
        expect_true(shiny::is.reactive(session$returned$where_clause))
        expect_true(shiny::is.reactive(session$returned$current_filters))
      }
    )
  })
  
  # Test with different table
  expect_no_error({
    shiny::testServer(
      filter_builder_server,
      args = list(
        storage_info = storage_info,
        selected_table_name = shiny::reactiveVal("iris"),
        restricted_columns = shiny::reactiveVal(character(0))
      ),
      {
        expect_equal(session$returned$where_clause(), "")
        expect_equal(length(session$returned$current_filters()), 0)
      }
    )
  })
})

test_that("filter_builder_demo runs without error", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  
  # This test just verifies the demo app can be created without error
  expect_no_error({
    app <- filter_builder_demo()
    expect_s3_class(app, "shiny.appobj")
  })
})
