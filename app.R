library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Define UI using bslib layout
ui <- page_sidebar(
  theme = bs_theme(version = 5),
  title = "APR Prototype",

  sidebar = sidebar(
    selectInput("selectDepartment",
                "Department",
                choices = c("GEOG", "OCNG"),
                selected = "GEOG"),
    selectInput("degreesIncluded",
                "Degrees Included in Review",
                choices = c("BS-GEOG", "BS-GIST", "MGSC", "MS", "PHD"),
                selected = c("BS-GEOG", "BS-GIST", "MGSC", "MS", "PHD"),
                multiple = TRUE)
  ),

  # Main Panel arranged into 3 columns
  layout_columns(
    col_widths = c(2, 5, 5),

    # --- Column 1: Action Controls ---
    div(
      h5("Actions", class = "fw-bold mb-3"),
      actionButton("createTemplateBtn", "Create Template", class = "btn-primary w-100 mb-3"),
      uiOutput("downloadUI")
    ),

    # --- Column 2: Example Table Preview ---
    card(
      card_header(
        class = "bg-light",
        h5("Table Preview", class = "m-0 font-weight-bold")
      ),
      card_body(
        gt::gt_output("Table1")
      )
    ),

    # --- Column 3: Visualization Gallery Card ---
    card(
      card_header(
        class = "bg-light",
        h5("Visualization Gallery", class = "m-0 font-weight-bold")
      ),
      card_body(
        # Item 1
        layout_columns(
          col_widths = c(1, 6, 5),
          checkboxInput("select_viz1", NULL, value = FALSE),
          plotOutput("chart1Output", height = "120px"),
          div(
            h6("1st Year Retention", class = "fw-bold mb-1"),
            p("5-year trend lines across degree programs.",
              class = "text-muted small mb-0")
          )
        ),
        hr(class = "my-2"),

        # Item 2
        layout_columns(
          col_widths = c(1, 6, 5),
          checkboxInput("select_viz2", NULL, value = FALSE),
          plotOutput("chart2Output", height = "120px"),
          div(
            h6("Degrees Awarded", class = "fw-bold mb-1"),
            p("Bar chart comparing degrees conferred annually.",
              class = "text-muted small mb-0")
          )
        ),
        hr(class = "my-2"),

        # Item 3
        layout_columns(
          col_widths = c(1, 6, 5),
          checkboxInput("select_viz3", NULL, value = FALSE),
          img(src = "https://via.placeholder.com/120x80.png?text=Scholarly",
              class = "img-fluid img-thumbnail"),
          div(
            h6("Scholarly Output", class = "fw-bold mb-1"),
            p("Publications vs. conference presentations.",
              class = "text-muted small mb-0")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  file_path <- "apr_template_GEOG.docx"

  if (file.exists(file_path)) {
    success <- file.remove(file_path)
    if (success) {
      message("File successfully deleted.")
    } else {
      warning("Failed to delete file. Check file permissions or access locks.")
    }
  } else {
    message("File does not exist.")
  }
  table1_data <- reactive({
    req(input$selectDepartment)
    readMetricsLocal(input$selectDepartment)
  })

  # Reactive list capturing state of visualization checkboxes
  selected_visualizations <- reactive({
    list(
      retention_trend = isTRUE(input$select_viz1),
      degrees_awarded = isTRUE(input$select_viz2),
      scholarly_output = isTRUE(input$select_viz3)
    )
  })

  chart1 <- reactive({
    req(table1_data(), input$selectDepartment, input$degreesIncluded)
    create5yrTrendRetentionChart(table1_data(), input$selectDepartment, input$degreesIncluded)
  })

  chart2 <- reactive({
    req(table1_data(), input$selectDepartment, input$degreesIncluded)
    create5yrDegreesAwardedChart(table1_data(), input$selectDepartment, input$degreesIncluded)
  })

  output$chart1Output <- renderPlot({ chart1() })
  output$chart2Output <- renderPlot({ chart2() })

  docx_exists <- reactivePoll(
    1000, session,
    checkFunc = function() {
      target_file <- paste0("apr_template_", input$selectDepartment, ".docx")
      if (file.exists(target_file)) file.info(target_file)$mtime else FALSE
    },
    valueFunc = function() {
      target_file <- paste0("apr_template_", input$selectDepartment, ".docx")
      file.exists(target_file)
    }
  )

  observeEvent(input$createTemplateBtn, {
    req(input$selectDepartment)
    cat("createTemplateBtn pushed\n")

    theQuartoInputFile <- paste0(getwd(), "/apr_template.qmd")
    theOutputFile <- paste0("apr_template_", input$selectDepartment, ".docx")

    withProgress(message = paste("Processing request for", input$selectDepartment), value = 0, {
      incProgress(0.5, detail = "Rendering report...")
      quarto::quarto_render(
        input = theQuartoInputFile,
        output_file = theOutputFile,
        execute_params = createQMD_parameterList_for_APR(
          input$selectDepartment,
          input$degreesIncluded,
          table1_data(),
          selected_visualizations()
        )
      )
      incProgress(0.5, detail = "Finished!")
    })
  })

  output$Table1 <- gt::render_gt({
    req(table1_data())
    createTable1(table1_data(), input$selectDepartment, "BS-GEOG")
  })

  output$downloadUI <- renderUI({
    if (docx_exists()) {
      downloadButton("downloadWordDoc", "Download Word Doc", class = "btn-success w-100")
    }
  })

  output$downloadWordDoc <- downloadHandler(
    filename = function() {
      paste0("apr_template_", input$selectDepartment, ".docx")
    },
    content = function(file) {
      expected_file <- paste0("apr_template_", input$selectDepartment, ".docx")

      if (!file.exists(expected_file)) {
        quarto::quarto_render(
          input = paste0(getwd(), "/apr_template.qmd"),
          output_file = expected_file,
          execute_params = createQMD_parameterList_for_APR(
            input$selectDepartment,
            input$degreesIncluded,
            table1_data(),
            selected_visualizations()
          )
        )
      }

      file.copy(expected_file, file)
    }
  )
}

shinyApp(ui = ui, server = server)
