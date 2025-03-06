mod_navpanel_shinydbanalysis_ui <- function(id, title = "Input Data Explorer", width = 1/2) {
  ns <- shiny::NS(id)
  
  nav_panel(
    title = title,
    layout_sidebar(
      sidebar = sidebar(
        table_picker_ui(ns("table")),
        filter_builder_ui(ns("filters")),
        summary_builder_ui(ns("summaries")),
        data_fetcher_ui(ns("fetcher"), style = "hover"),
        width = 650
      ),
      navset_tab(
        nav_panel(
          title = "Data",
          uiOutput(ns("error_display")),
          DT::dataTableOutput(ns("results")),
          div(
            style = "margin: 15px 0;",
            h4("Executed Query:", class = "text-muted"),
            verbatimTextOutput(ns("executed_query"))
          )
        ),
        nav_panel(
          title = "Visualization",
          plot_builder_ui(ns("plot"))
        ),
        nav_panel(
          title = "Debug Information",
          div(class = "debug-panel",
              div(class = "debug-section",
                  h4("table_picker_server returns:"),
                  tags$pre(
                    "selected_table():",
                    textOutput(ns("table_selected_table"), inline = TRUE)
                  )
              ),
              
              div(class = "debug-section",
                  h4("filter_builder_server returns:"),
                  tags$pre(
                    "where_clause():",
                    textOutput(ns("filter_where_clause"), inline = TRUE)
                  )
              ),
              
              div(class = "debug-section",
                  h4("summary_builder_server returns:"),
                  tags$pre(
                    "summary_specs():",
                    verbatimTextOutput(ns("summary_specs")),
                    "group_vars():",
                    textOutput(ns("summary_group_vars"), inline = TRUE),
                    "needs_summary():",
                    textOutput(ns("summary_needs_summary"), inline = TRUE)
                  )
              ),
              
              div(class = "debug-section",
                  h4("data_fetcher_server returns:"),
                  tags$pre(
                    "error():",
                    textOutput(ns("fetcher_error"), inline = TRUE),
                    "\n\nexecuted_query():",
                    verbatimTextOutput(ns("fetcher_executed_query")),
                    "\n\ndata():",
                    verbatimTextOutput(ns("fetcher_data_str"))
                  )
              )
          )
        )
      )
    )
  )
  
}



mod_navpanel_shinydbanalysis_server <- function(id, pool, storage_info, restricted_columns ) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Initialize modules
    table_results <- table_picker_server("table", pool, storage_info, restricted_columns)
    
    filter_results <- filter_builder_server(
      "filters",
      storage_info = storage_info,
      selected_table = table_results$selected_table,
      restricted_columns = restricted_columns
    )
    
    # Get current column info reactively for summary builder
    current_column_info <- reactive({
      req(table_results$selected_table())
      read_column_info(
        tablename = table_results$selected_table(),
        storage_type = storage_info$storage_type,
        column_info_dir = storage_info$column_info_dir,
        adls_endpoint = storage_info$adls_endpoint,
        adls_container = storage_info$adls_container,
        sas_token = storage_info$sas_token,
        restricted_columns = restricted_columns
      )
    })
    
    summary_results <- summary_builder_server(
      "summaries",
      selected_table = table_results$selected_table,
      column_info = current_column_info
    )
    
    fetched_data <- data_fetcher_server(
      "fetcher",
      pool = pool,
      table_builder = table_results,
      filter_builder = filter_results,
      summary_builder = summary_results
    )
    
    plot_results <- plot_builder_server("plot", fetched_data)
    
    # Error display
    output$error_display <- renderUI({
      error <- fetched_data$error()
      if (!is.null(error)) {
        div(
          class = "alert alert-danger",
          error
        )
      }
    })
    
    # Show executed query above results
    output$executed_query <- renderPrint({
      req(fetched_data$executed_query())
      cat(fetched_data$executed_query())
    })
    
    # Debug outputs
    output$table_selected_table <- renderText({
      table_results$selected_table() %||% "NULL"
    })
    
    output$filter_where_clause <- renderText({
      filter_results$where_clause() %||% "NULL"
    })
    
    output$summary_specs <- renderPrint({
      str(summary_results$summary_specs())
    })
    
    output$summary_group_vars <- renderText({
      paste(summary_results$group_vars(), collapse = ", ") %||% "NULL"
    })
    
    output$summary_needs_summary <- renderText({
      summary_results$needs_summary()
    })
    
    output$fetcher_error <- renderText({
      fetched_data$error() %||% "NULL"
    })
    
    output$fetcher_executed_query <- renderPrint({
      cat(fetched_data$executed_query() %||% "NULL")
    })
    
    output$fetcher_data_str <- renderPrint({
      str(fetched_data$data())
    })
    
    # Results table
    output$results <- DT::renderDataTable({
      error <- fetched_data$error()
      if (!is.null(error)) {
        return(data.frame(Error = error))
      }
      
      data <- fetched_data$data()
      if (is.null(data)) {
        return(data.frame(Message = "Click 'Fetch Data' to load data"))
      }
      
      # Define custom formatters for different column types
      DT::datatable(
        data,  # Removed head() to show all rows
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatDate(
          columns = names(data)[sapply(data, inherits, "Date")],
          method = "toLocaleDateString"
        ) %>%
        DT::formatRound(
          columns = names(data)[sapply(data, is.numeric)],
          digits = 2
        )
    })
  })
}

#' demo_shinydbanalysis_app_all_in_one_module
#'
#' @param pool 
#' @param storage_info 
#' @param restricted_columns 
#'
#' @export
#'
#' @examples
#' library(bslib)
#' storage_info <- list(  storage_type = "local",column_info_dir = "column_info")
#' restricted_columns <- character(0)
#' demo_shinydbanalysis_app_all_in_one_module(pool, storage_info, restricted_columns)

demo_shinydbanalysis_app_all_in_one_module <-function(pool, storage_info, restricted_columns){

  ui <- page_navbar(
    title = "Dataset Analysis Tool",
    mod_navpanel_shinydbanalysis_ui("x")
  )

  server <- function(input, output, session){
    mod_navpanel_shinydbanalysis_server("x",pool, storage_info, restricted_columns)
  }
  shiny::shinyApp(ui, server)

}
#demo_shinydbanalysis_app_all_in_one_module()
