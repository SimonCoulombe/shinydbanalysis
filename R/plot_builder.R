#' Create plot builder UI components
#' @param id Character. The module ID
#' @return A list of Shiny UI elements
#' @export
plot_builder_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "well",
    style = "padding: 15px; margin-bottom: 15px;",
    h4("Data Visualization", style = "margin-top: 0;"),
    
    # Plot type selection
    selectInput(
      ns("plot_type"),
      "Plot Type:",
      choices = c(
        "Select plot type" = "",
        "Line" = "line",
        "Bar" = "bar",
        "Scatter" = "scatter",
        "Box" = "box"
      )
    ),
    
    # Dynamic UI for plot options
    uiOutput(ns("plot_options")),
    
    # Plot output
    plotly::plotlyOutput(ns("plot"))
  )
}

#' Create plot builder server logic
#' @param id Character. The module ID
#' @param fetched_data Reactive. Data from data_fetcher
#' @return List of reactive expressions
#' @export
plot_builder_server <- function(id, fetched_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Dynamic UI for plot options based on plot type
    output$plot_options <- renderUI({
      req(input$plot_type, fetched_data$data())
      
      data <- fetched_data$data()
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      all_cols <- names(data)
      
      switch(input$plot_type,
             "line" = tagList(
               selectInput(ns("x_var"), "X Variable:", choices = all_cols),
               selectInput(ns("y_var"), "Y Variable:", choices = numeric_cols),
               selectInput(ns("group_var"), "Group By (optional):", 
                           choices = c("None" = "", all_cols), 
                           selected = "")
             ),
             "bar" = tagList(
               selectInput(ns("x_var"), "X Variable:", choices = all_cols),
               selectInput(ns("y_var"), "Y Variable:", choices = numeric_cols),
               selectInput(ns("fill_var"), "Fill By (optional):", 
                           choices = c("None" = "", all_cols),
                           selected = "")
             ),
             "scatter" = tagList(
               selectInput(ns("x_var"), "X Variable:", choices = numeric_cols),
               selectInput(ns("y_var"), "Y Variable:", choices = numeric_cols),
               selectInput(ns("color_var"), "Color By (optional):", 
                           choices = c("None" = "", all_cols),
                           selected = "")
             ),
             "box" = tagList(
               selectInput(ns("x_var"), "X Variable:", choices = all_cols),
               selectInput(ns("y_var"), "Y Variable:", choices = numeric_cols)
             )
      )
    })
    
    # Create plot
    output$plot <- plotly::renderPlotly({
      req(input$plot_type, input$x_var, input$y_var, fetched_data$data())
      
      data <- fetched_data$data()
      
      p <- ggplot2::ggplot(data)
      
      p <- switch(input$plot_type,
                  "line" = {
                    if (input$group_var != "") {
                      p + ggplot2::geom_line(ggplot2::aes(
                        x = .data[[input$x_var]], 
                        y = .data[[input$y_var]], 
                        color = .data[[input$group_var]]
                      ))
                    } else {
                      p + ggplot2::geom_line(ggplot2::aes(
                        x = .data[[input$x_var]], 
                        y = .data[[input$y_var]]
                      ))
                    }
                  },
                  "bar" = {
                    if (input$fill_var != "") {
                      p + ggplot2::geom_bar(ggplot2::aes(
                        x = .data[[input$x_var]], 
                        y = .data[[input$y_var]], 
                        fill = .data[[input$fill_var]]
                      ), stat = "identity", position = "dodge")
                    } else {
                      p + ggplot2::geom_bar(ggplot2::aes(
                        x = .data[[input$x_var]], 
                        y = .data[[input$y_var]]
                      ), stat = "identity")
                    }
                  },
                  "scatter" = {
                    if (input$color_var != "") {
                      p + ggplot2::geom_point(ggplot2::aes(
                        x = .data[[input$x_var]], 
                        y = .data[[input$y_var]], 
                        color = .data[[input$color_var]]
                      ))
                    } else {
                      p + ggplot2::geom_point(ggplot2::aes(
                        x = .data[[input$x_var]], 
                        y = .data[[input$y_var]]
                      ))
                    }
                  },
                  "box" = {
                    p + ggplot2::geom_boxplot(ggplot2::aes(
                      x = .data[[input$x_var]], 
                      y = .data[[input$y_var]]
                    ))
                  }
      )
      
      p <- p + ggplot2::theme_minimal() +
        ggplot2::labs(
          x = input$x_var,
          y = input$y_var,
          title = paste("Plot of", input$y_var, "by", input$x_var)
        )
      
      plotly::ggplotly(p)
    })
  })
}