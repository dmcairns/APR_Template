#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("APR Prototype"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("selectDepartment",
                        "Department",
                        choices=c("GEOG", "OCNG"),
                        selected="GEOG"),
            selectInput("degreesIncluded",
                        "Degrees Included in Review",
                        choices=c("BS-GEOG", "BS-GIST", "MGSC", "MS", "PHD"),
                        selected=c("BS-GEOG", "BS-GIST", "MGSC", "MS", "PHD"),
                        multiple=TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           actionButton("createTemplateBtn",
                        "Create Template")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  table1_data <- reactive({
    req(input$selectDepartment)
    readMetricsLocal(input$selectDepartment)
  })
    observeEvent(input$createTemplateBtn,{
      req(input$selectDepartment)
      cat("createTemplateBtn pushed\n")

      theQuartoInputFile <- paste0(getwd(), "/apr_template.qmd")
      theOutputFile <- paste0("apr_template_", input$selectDepartment)

      withProgress(message = paste("Processing request for", input$selectDepartment), value = 0, {
        incProgress(0.5, detail = "Rendering report...")
        quarto::quarto_render(
          input = theQuartoInputFile,
          output_file = theOutputFile,
          execute_params = createQMD_parameterList_for_APR(input$selectDepartment,
                                                           input$degreesIncluded,
                                                           table1_data()),
        )
        # file_trigger_templates(file_trigger_templates() + 1)
        incProgress(0.5, detail = "Finished!")
      })
    })

    # Add a download button that appears if the selected file exists (use reactivePoll to determine)
}

# Run the application
shinyApp(ui = ui, server = server)
