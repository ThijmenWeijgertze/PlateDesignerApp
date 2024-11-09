library(shiny)
library(readxl)
library(bslib)
library(ggplate)

# UI
ui <- page_sidebar(
  # UI title
  title = "Plate Designer",
  # start sidebar
  sidebar = sidebar(
    # sidebar title
    "Parameters",
    # sidebar position
    position = "left",
    # sidebar .xlsx file input
    fileInput(
      "file1", 
      "Choose .xlsx File", 
      accept = ".xlsx"
    ),
    selectInput(
      "wellNumber", 
      "Select the plate type:", 
      list(
        "6 wells" = "6", 
        "12 wells" = "12", 
        "24 wells" = "24",
        "48 wells" = "48",
        "96 wells" = "96",
        "384 wells" = "384",
        "1536 wells" = "1536"
      ) 
    ),
    input_switch(
      "labelSwitch", "Labels", value = TRUE
    ),
    input_switch(
      "labelLegend", "Legend", value = TRUE
    ),
    selectInput(
      "wellShape", 
      "Select the shape of the wells:", 
      list("round", "square") 
    ),
    textInput(
      "titleField", 
      "Title", 
      placeholder = "Enter Title Here..."
    ),
    numericInput( 
      "textSize", 
      "Text Size", 
      value = 23,
    ), 
    downloadButton("downloadPlot", "Download Plot")
  ),
  card(
    card_header("Plate Design"),
    uiOutput(outputId = "plotOutputUI")
  )
)

# server
server <- function(input, output) {
  
  # Reactive expression to read the file
  file_data <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath, sheet = paste0(input$wellNumber, "well"))
  })
  
  output$PlatePlot <- renderPlot({
    data <- file_data()
    plate_plot(
      data = data,
      position = Well,
      value = Compound,
      label = Value,
      show_legend = input$labelLegend,
      plate_size = input$wellNumber,
      plate_type = input$wellShape,
      title = input$titleField,
      title_size = input$textSize
    )
  })
  
  output$PlatePlot2 <- renderPlot({
    data <- file_data()
    plate_plot(
      data = data,
      position = Well,
      value = Compound,
      show_legend = input$labelLegend,
      plate_size = input$wellNumber,
      plate_type = input$wellShape,
      title = input$titleField,
      title_size = input$textSize
    )
  })
  
  output$plotOutputUI <- renderUI({
    if (input$labelSwitch) {
      plotOutput(outputId = "PlatePlot")
    } else {
      plotOutput(outputId = "PlatePlot2")
    }
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plate_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1920, height = 1080, res = 300)
      if (input$labelSwitch) {
        print(plate_plot(
          data = file_data(),
          position = Well,
          value = Compound,
          label = Value,
          show_legend = input$labelLegend,
          plate_size = input$wellNumber,
          plate_type = input$wellShape,
          title = input$titleField,
          title_size = input$textSize
        ))
      } else {
        print(plate_plot(
          data = file_data(),
          position = Well,
          value = Compound,
          show_legend = input$labelLegend,
          plate_size = input$wellNumber,
          plate_type = input$wellShape,
          title = input$titleField,
          title_size = input$textSize
        ))
      }
      dev.off()
    }
  )
}

# Run
shinyApp(ui = ui, server = server)