# libraries
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
    # browse excel file
    fileInput(
      "excelFile", 
      "Choose .xlsx File", 
      accept = ".xlsx"
    ),
    # choose well plate type
    selectInput(
      "wellNumber", 
      "Plate type", 
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
    # shape of the wells
    selectInput(
      "wellShape", 
      "Shape", 
      list("round", "square") 
    ),
    # title of the plot
    textInput(
      "titleField", 
      "Title", 
      placeholder = "Enter Title Here..."
    ),
    # size of the title text
    numericInput( 
      "textSize", 
      "Title Size", 
      value = 15
    ),
    # label size
    numericInput( 
      "labelSize", 
      "Label Size", 
      value = 2,
      min = 0
    ),
    # legend rows
    numericInput( 
      "legendRows", 
      "Number of legend rows", 
      value = 3,
      min = 1
    ),
    # seed for the randomizer
    numericInput(
      "randomSeed", 
      "Randomizer Seed", 
      value = 2024,
      min = 1
    ),
    # switch to hide labels
    input_switch(
      "labelSwitch", 
      "Labels",
      value = TRUE
    ),
    # switch to hide legend
    input_switch(
      "labelLegend", 
      "Legend", 
      value = TRUE
    ),
    # switch to randomize
    input_switch(
      "randomSwitch", 
      "Randomize",
      value = FALSE
    ),
    input_dark_mode(id = "mode"),
    # button to download the plot
    downloadButton(
      "downloadPlot", 
      "Plot"
    ),
    # button to download the gradient plot
    downloadButton(
      "downloadPlotGradient", 
      "Gradient Plot"
    ),
    # button to download the gradient plot
    downloadButton(
      "downloadPlotCompoundAsLabel", 
      "Plot With Compound As Label"
    )
  ),
  # making the tabs
  fluidPage(
    tabsetPanel(
      # Normal tab
      tabPanel("Regular plate",
        card(
          card_header("Plate Design"),
            uiOutput(outputId = "PlatePlotOutputUI")
              )),
      tabPanel("Gradient Plate",
        card(
          card_header("Plate Design Gradient"),
            uiOutput(outputId = "PlatePlotGradientOutputUI")
              )),
      tabPanel("Plate With Compound As Label",
        card(
          card_header("Plate Design With Compound As Label"),
            uiOutput(outputId = "PlatePlotCompoundAsLabelOutputUI")
)))))

# server
server <- function(input, output) {

  # loading data
  file_data <- reactive({
    # loading in normal data
    req(input$excelFile)
    data <- read_excel(input$excelFile$datapath, sheet = paste0(input$wellNumber, "well"))
    # randomize the data if switch is set to TRUE
    if (input$randomSwitch) {
      # set seed (if filled in)
      set.seed(input$randomSeed)
      # randomize column 2 and 3
      data[, 2:3] <- data[sample(nrow(data)), 2:3]
    }
    # return the data
    return(data)
  })
  
  # rendering the normal plot with labels
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
      title_size = input$textSize,
      legend_n_row = input$legendRows,
      label_size = input$labelSize
    )
  })
  
  # rendering the normal plot without labels
  output$PlatePlotNoLabel <- renderPlot({
    data <- file_data()
    plate_plot(
      data = data,
      position = Well,
      value = Compound,
      show_legend = input$labelLegend,
      plate_size = input$wellNumber,
      plate_type = input$wellShape,
      title = input$titleField,
      title_size = input$textSize,
      legend_n_row = input$legendRows,
      label_size = input$labelSize
    )
  })
  
  # rendering the normal plot with labels
  output$PlatePlotGradient <- renderPlot({
    data <- file_data()
    plate_plot(
      data = data,
      position = Well,
      value = Value,
      label = Value,
      show_legend = input$labelLegend,
      plate_size = input$wellNumber,
      plate_type = input$wellShape,
      title = input$titleField,
      title_size = input$textSize,
      label_size = input$labelSize
    )
  })
  
  # rendering the normal plot without labels
  output$PlatePlotNoLabelGradient <- renderPlot({
    data <- file_data()
    plate_plot(
      data = data,
      position = Well,
      value = Value,
      show_legend = input$labelLegend,
      plate_size = input$wellNumber,
      plate_type = input$wellShape,
      title = input$titleField,
      title_size = input$textSize,
      label_size = input$labelSize
    )
  })
  
  # rendering the plot with the compound as label
  output$PlatePlotCompoundAsLabel <- renderPlot({
    data <- file_data()
    plate_plot(
      data = data,
      position = Well,
      value = Compound,
      label = Compound,
      show_legend = input$labelLegend,
      plate_size = input$wellNumber,
      plate_type = input$wellShape,
      title = input$titleField,
      title_size = input$textSize,
      legend_n_row = input$legendRows,
      label_size = input$labelSize
    )
  })
  
  # choosing which plot to show based on the label switch
  output$PlatePlotOutputUI <- renderUI({
    if (input$labelSwitch) {
      plotOutput(outputId = "PlatePlot")
    } else {
      plotOutput(outputId = "PlatePlotNoLabel")
    }
  })
  
  # choosing which gradient plot to show based on the label switch
  output$PlatePlotGradientOutputUI <- renderUI({
    if (input$labelSwitch) {
      plotOutput(outputId = "PlatePlotGradient")
    } else {
      plotOutput(outputId = "PlatePlotNoLabelGradient")
    }
  })
  
  output$PlatePlotCompoundAsLabelOutputUI <- renderUI({
    plotOutput(outputId = "PlatePlotCompoundAsLabel")
  })
  
  # downloading plot
  output$downloadPlot <- downloadHandler(
    # PNG file name
    filename = function() {
      # (gsub removes any spaces)
      paste(gsub(" ", "", input$titleField), Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # PNG properties (width, height, resolution)
      png(file, width = 10, height = 6, unit = "in", res = 300)
      # plot with labels
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
          title_size = input$textSize,
          legend_n_row = input$legendRows,
          label_size = input$labelSize
        ))
      # plot without labels
      } else {
        print(plate_plot(
          data = file_data(),
          position = Well,
          value = Compound,
          show_legend = input$labelLegend,
          plate_size = input$wellNumber,
          plate_type = input$wellShape,
          title = input$titleField,
          title_size = input$textSize,
          legend_n_row = input$legendRows,
          label_size = input$labelSize
        ))
      }
      # stop PNG render process
      dev.off()
    }
  )
  # downloading gradient plot
  output$downloadPlotGradient <- downloadHandler(
    # PNG file name
    filename = function() {
      # (gsub removes any spaces)
      paste(gsub(" ", "", input$titleField), Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # PNG properties (width, height, resolution)
      png(file, file, width = 10, height = 6, unit = "in", res = 300)
      # plot gradient with labels
      if (input$labelSwitch) {
        print(plate_plot(
          data = file_data(),
          position = Well,
          value = Value,
          label = Value,
          show_legend = input$labelLegend,
          plate_size = input$wellNumber,
          plate_type = input$wellShape,
          title = input$titleField,
          title_size = input$textSize,
          label_size = input$labelSize
        ))
        # plot gradient without labels
      } else {
        print(plate_plot(
          data = file_data(),
          position = Well,
          value = Value,
          show_legend = input$labelLegend,
          plate_size = input$wellNumber,
          plate_type = input$wellShape,
          title = input$titleField,
          title_size = input$textSize,
          label_size = input$labelSize
        ))
      }
      # stop PNG render process
      dev.off()
    }
  )
  # downloading gradient plot
  output$downloadPlotCompoundAsLabel <- downloadHandler(
    # PNG file name
    filename = function() {
      # (gsub removes any spaces)
      paste(gsub(" ", "", input$titleField), Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # PNG properties (width, height, resolution)
      png(file, file, width = 10, height = 6, unit = "in", res = 300)
      # plot with compound as label
      print(plate_plot(
        data = file_data(),
        position = Well,
        value = Compound,
        label = Compound,
        show_legend = input$labelLegend,
        plate_size = input$wellNumber,
        plate_type = input$wellShape,
        title = input$titleField,
        title_size = input$textSize,
        legend_n_row = input$legendRows,
        label_size = input$labelSize
      ))
      # stop PNG render process
      dev.off()
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)