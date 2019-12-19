library(shiny)
source("minCHAT_check.R")

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("ACLEW Annotation Scheme: minCHAT error spotter"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Annotation file ----
      fileInput("file1", "Choose your annotation file",
                accept = c("text/tab-separated-values",
                         ".txt")),

      # Submit button:
      actionButton("submit", "Submit")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      uiOutput("report"),
      uiOutput("downloadErrors"),
      uiOutput("capitalizedwords"),
      uiOutput("hyphenatedwords")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  report <- eventReactive(input$submit, {
    req(input$file1)
    check.annotations(input$file1$datapath, input$file1$name)
  })

  output$report <- renderUI({
    req(report())
    
    tagList(
      tags$br(),
      renderText(paste0("Number of potential errors detected: ",
        as.character(report()$n.a.alerts))),
      renderText("(downloadable list below)"),
      tags$br(),
      renderText(paste0("Number of capitalized word types detected: ",
        as.character(report()$n.capitals))),
      renderTable(report()$capitals),
      tags$br(),
      renderText(paste0("Number of hyphenated word types detected: ",
        as.character(report()$n.hyphens))),
      renderTable(report()$hyphens),
      tags$br()
    )
  })
  
  output$downloadErrors <- renderUI({
    # Output file name
    time.now <- gsub('-|:', '', as.character(Sys.time()))
    time.now <- gsub(' ', '_', time.now)
    
    errors <- report()$alert.table
  
    output$downloadErrorsHandler <- downloadHandler(
      filename = paste0("minCHATerrorcheck-",time.now,"-possible_errors.csv"),
      content = function(file) {
        write_csv(errors, file)
      },
      contentType = "text/csv"
    )
    
    downloadButton("downloadErrorsHandler", "Errors detected? Download a list of suspected issues here.")
  })
}

# Create Shiny app ----
shinyApp(ui, server)
