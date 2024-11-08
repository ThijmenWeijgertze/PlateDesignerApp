library(shiny)
library(readxl)
library(here)
library(bslib)
library(ggplate)

# Define UI ----
ui <- page_sidebar(
  title = "title panel",
  sidebar = sidebar(
    "Parameters", 
    position = "left",
    fileInput(
      "file1", 
      "Choose .xlsx File", 
      accept = ".xlsx"),
    selectInput( 
      "wellNumber", 
      "Select the correct plate:", 
      list("6 wells" = "6", "12 wells" = "12", "24 wells" = "24") 
    ),
    selectInput( 
      "plateType", 
      "Select a plate type:", 
      list("round", "square") 
    ),
    textInput( 
      "titleField", 
      "Title", 
      placeholder = "Enter Title Here..."
    ), 
  ),
  "Main contents",
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
      plate_type = input$plateType,
      title = input$titleField,
      title_size = 23  
    )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)


