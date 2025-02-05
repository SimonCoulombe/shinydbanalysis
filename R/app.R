#' Run the Shiny Database Analysis Application
#'
#' @param pool A database connection pool object
#' @param storage_type Either "local" or "adls"
#' @param local_dir Path for local storage (default: "column_info")
#' @param adls_endpoint ADLS endpoint URL (required if storage_type = "adls")
#' @param adls_container ADLS container name (required if storage_type = "adls")
#' @param sas_token ADLS SAS token (required if storage_type = "adls")
#' @export
run_app <- function(pool,
                    storage_type = "local",
                    local_dir = "column_info",
                    adls_endpoint = NULL,
                    adls_container = NULL,
                    sas_token = NULL) {
  
  # Validate storage configuration
  storage_type <- match.arg(storage_type, c("local", "adls"))
  
  if (storage_type == "adls") {
    if (is.null(adls_endpoint) || is.null(adls_container) || is.null(sas_token)) {
      stop("ADLS endpoint, container, and SAS token are required when storage_type is 'adls'")
    }
  }
  
  # Create storage info list
  storage_info <- list(
    storage_type = storage_type,
    local_dir = local_dir,
    adls_endpoint = adls_endpoint,
    adls_container = adls_container,
    sas_token = sas_token
  )
  
  ui <- fluidPage(
    titlePanel("Dataset Analysis Tool"),
    
    sidebarLayout(
      sidebarPanel(
        table_picker_ui("table"),
        hr(),
        filter_builder_ui("filters"),
        hr(),
        group_builder_ui("groups"),
        hr(),
        summary_builder_ui("summaries"),
        hr(),
        data_fetcher_ui("fetcher", style = "hover")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Data",
                   div(
                     style = "margin: 15px 0;",
                     h4("Executed Query:", class = "text-muted"),
                     verbatimTextOutput("executed_query"),
                     uiOutput("error_display"),
                     hr()
                   ),
                   DT::dataTableOutput("results")
          ),
          tabPanel("Debug Information",
                   div(class = "debug-panel",
                       div(class = "debug-section",
                           h4("table_picker_server returns:"),
                           tags$pre(
                             "selected_table():",
                             textOutput("table_selected_table", inline = TRUE)
                           )
                       ),
                       
                       div(class = "debug-section",
                           h4("filter_builder_server returns:"),
                           tags$pre(
                             "where_clause():",
                             textOutput("filter_where_clause", inline = TRUE)
                           )
                       ),
                       
                       div(class = "debug-section",
                           h4("group_builder_server returns:"),
                           tags$pre(
                             "group_vars():",
                             textOutput("group_vars", inline = TRUE)
                           )
                       ),
                       
                       div(class = "debug-section",
                           h4("summary_builder_server returns:"),
                           tags$pre(
                             "summary_specs():",
                             verbatimTextOutput("summary_specs")
                           )
                       ),
                       
                       div(class = "debug-section",
                           h4("data_fetcher_server returns:"),
                           tags$pre(
                             "error():",
                             textOutput("fetcher_error", inline = TRUE),
                             "\n\nexecuted_query():",
                             verbatimTextOutput("fetcher_executed_query"),
                             "\n\ndata():",
                             verbatimTextOutput("fetcher_data_str")
                           )
                       )
                   )
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    # Initialize modules
    table_info <- table_picker_server("table", pool, storage_info)
    
    filter_results <- filter_builder_server(
      "filters",
      storage_info = storage_info,
      selected_table = table_info$selected_table
    )
    
    # Get current column info reactively for group builder
    current_column_info <- reactive({
      req(table_info$selected_table())
      read_column_info(
        tablename = table_info$selected_table(),
        storage_type = storage_info$storage_type,
        local_dir = storage_info$local_dir,
        adls_endpoint = storage_info$adls_endpoint,
        adls_container = storage_info$adls_container,
        sas_token = storage_info$sas_token
      )
    })
    
    group_results <- group_builder_server(
      "groups",
      selected_table = table_info$selected_table,
      column_info = current_column_info
    )
    
    summary_results <- summary_builder_server(
      "summaries",
      selected_table = table_info$selected_table,
      column_info = current_column_info
    )
    
    fetched_data <- data_fetcher_server(
      "fetcher",
      pool = pool,
      table_info = table_info,
      filter_builder = filter_results,
      group_builder = group_results,
      summary_builder = summary_results
    )
    
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
      table_info$selected_table() %||% "NULL"
    })
    
    output$filter_where_clause <- renderText({
      filter_results$where_clause() %||% "NULL"
    })
    
    output$group_vars <- renderText({
      paste(group_results$group_vars(), collapse = ", ") %||% "NULL"
    })
    
    output$summary_specs <- renderPrint({
      str(summary_results$summary_specs())
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
        head(data, 10),
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
  }
  
  shinyApp(ui = ui, server = server)
}