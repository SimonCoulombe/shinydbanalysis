test_that("group_builder_server can be called without error", {
  
  column_info <- load_demo_column_info("diamonds")
  
  expect_no_error({
    shiny::testServer(
      group_builder_server,
      args = list(
        selected_table_name = shiny::reactiveVal("diamonds"),
        column_info = shiny::reactiveVal(column_info),
        acceptable_dimensions = NULL
      ),
      {
        # Check that the module returns the expected structure
        expect_true(shiny::is.reactive(session$returned$group_vars))
        expect_true(shiny::is.reactive(session$returned$banding_configs))
        expect_true(shiny::is.reactive(session$returned$regrouping_configs))
        
        # Initially, should have no grouping
        expect_null(session$returned$group_vars())
        expect_equal(length(session$returned$banding_configs()), 0)
        expect_equal(length(session$returned$regrouping_configs()), 0)
      }
    )
  })
  
  # Test with acceptable_dimensions
  expect_no_error({
    shiny::testServer(
      group_builder_server,
      args = list(
        selected_table_name = shiny::reactiveVal("diamonds"),
        column_info = shiny::reactiveVal(column_info),
        acceptable_dimensions = shiny::reactiveVal(c("cut", "color", "clarity"))
      ),
      {
        expect_true(shiny::is.reactive(session$returned$group_vars))
        expect_true(shiny::is.reactive(session$returned$banding_configs))
        expect_true(shiny::is.reactive(session$returned$regrouping_configs))
      }
    )
  })
  
  # Test with different dataset
  column_info_iris <- load_demo_column_info("iris")
  expect_no_error({
    shiny::testServer(
      group_builder_server,
      args = list(
        selected_table_name = shiny::reactiveVal("iris"),
        column_info = shiny::reactiveVal(column_info_iris),
        acceptable_dimensions = NULL
      ),
      {
        expect_null(session$returned$group_vars())
        expect_equal(length(session$returned$banding_configs()), 0)
        expect_equal(length(session$returned$regrouping_configs()), 0)
      }
    )
  })
})

test_that("group_builder_demo runs without error", {
  skip_on_cran()

  # This test just verifies the demo app can be created without error
  expect_no_error({
    app <- group_builder_demo()
    expect_s3_class(app, "shiny.appobj")
  })
})

test_that("helper functions work correctly", {
  
  # Test create_band_labels
  expect_equal(
    create_band_labels(c(1, 2, 3)),
    c("<1", "[1,2)", "[2,3)", ">=3")
  )
  
  expect_equal(
    create_band_labels(c(0.5)),
    c("<0.5", ">=0.5")
  )
  
  # Test parse_regrouping_mapping
  mapping_text <- "Group A: value1, value2\nGroup B: value3, value4"
  result <- parse_regrouping_mapping(mapping_text)
  expect_equal(result$value1, "Group A")
  expect_equal(result$value2, "Group A")
  expect_equal(result$value3, "Group B")
  expect_equal(result$value4, "Group B")
  
  # Test with empty lines and whitespace
  mapping_text2 <- "  Group A: value1, value2  \n\n  Group B: value3  "
  result2 <- parse_regrouping_mapping(mapping_text2)
  expect_equal(result2$value1, "Group A")
  expect_equal(result2$value3, "Group B")
  
  # Test with invalid format
  mapping_text3 <- "Invalid format"
  result3 <- parse_regrouping_mapping(mapping_text3)
  expect_equal(length(result3), 0)
})
