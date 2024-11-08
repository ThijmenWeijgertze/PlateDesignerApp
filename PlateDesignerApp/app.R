library(shiny)
library(readxl)
library(bslib)
library(ggplate)

# Define UI ----
ui <- page_sidebar(
  title = "Plate Designer",
  sidebar = sidebar(
    "Parameters", 
    position = "left",
    fileInput(
      "file1", 
      "Choose .xlsx File", 
      accept = ".xlsx"),
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
  ),
  card(
    card_header("Plate Design"),
    plotOutput(outputId = "PlatePlot")
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Reactive expression to read the file
  file_data <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath, sheet = paste0(input$wellNumber,"well"))
  })
  
  output$PlatePlot <- renderPlot({
    data <- file_data()
    plate_plot(
      data = data,
      position = Well,
      value = Compound,
      label = Value,
      plate_size = input$wellNumber,
      plate_type = input$wellShape,
      title = input$titleField,
      title_size = 23  
    )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)


