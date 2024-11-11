# libraries
library(shiny)
library(readxl)
library(bslib)
library(ggplate)
library(shinyjs)

# UI
ui <- page_sidebar(
  tags$head(
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$script(src = "custom.js"),
    useShinyjs()
  ),
  # UI title
  title = "Plate Designer",
  # start sidebar
  sidebar = sidebar(
    id = "main-sidebar",
    # sidebar title
    tags$h2(
      class = "sidebar-title",
      "Parameters"
    ),
    # sidebar position
    position = "left",
    # download the excel file
    downloadButton("downloadData", "Download Excel Template"),
    # browse excel file
    fileInput(
      "excelFile", 
      "Choose .xlsx File", 
      accept = ".xlsx"
    ),
    selectInput("theme_selector", "Themes",
                choices = c(
                  "Mint" = "mint",
                  "Forest" = "forest",
                  "Ocean" = "ocean",
                  "Sunset" = "sunset",
                  "Lavender" = "lavender",
                  "Citrus" = "citrus",
                  "Coral" = "coral",
                  "Berry" = "berry",
                  "Spring" = "spring",
                  "Sky" = "sky"
                ),
                selected = "mint"
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
      ),
      selected = "96"
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
    # themes
    selectInput("theme_selector", "Themes",
                choices = c(
                  "Mint" = "mint",
                  "Forest" = "forest",
                  "Ocean" = "ocean",
                  "Sunset" = "sunset",
                  "Lavender" = "lavender",
                  "Citrus" = "citrus",
                  "Coral" = "coral",
                  "Berry" = "berry",
                  "Spring" = "spring",
                  "Sky" = "sky"
                ),
                selected = "mint"
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
    # Width
    numericInput(
      "downloadWidth", 
      "Download Width", 
      value = 17,
      min = 1
    ),
    # Height
    numericInput(
      "downloadHeight", 
      "Download Height", 
      value = 8,
      min = 1
    ),
    # Res
    numericInput(
      "downloadRes", 
      "Download Resolution", 
      value = 300,
      min = 1
    ),
    # button to download the plot
    downloadButton(
      "downloadPlot", 
      "Regular plate",
      class = "custom-download-btn"
    ),
    # button to download the gradient plot
    downloadButton(
      "downloadPlotGradient", 
      "Gradient Plate",
      class = "custom-download-btn"
    ),
    # button to download the gradient plot
    downloadButton(
      "downloadPlotCompoundAsLabel", 
      "Plate With Compound As Label",
      class = "custom-download-btn"
    ),
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
               ))),
    "* Thanks to the ggplate package for making this possible",
    tags$br(),
    "* Packages used: shiny, readxl, bslib, ggplate, shinyjs"
  ))

# server
server <- function(input, output) {
  # set theme
  observe({
    theme <- input$theme_selector
    js <- sprintf("document.documentElement.setAttribute('data-theme', '%s');", theme)
    shinyjs::runjs(js)
  })
  
  # for downloading the excel data
  output$downloadData <- downloadHandler(
    filename = function() {
      "PlateDesignTemplate.xlsx"
    },
    content = function(file) {
      file.copy("data/PlateDesignTemplate.xlsx", file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # loading data
  file_data <- reactive({
    # requires a choosen excell file
    req(input$excelFile)
    # choose the correct sheet and store it in data
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
      show_legend = !input$labelLegend,
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
      png(file, input$downloadWidth, input$downloadHeight, unit = "in", res = input$downloadRes)
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
    },
    contentType = "image/png"
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
      png(file, input$downloadWidth, input$downloadHeight, unit = "in", res = input$downloadRes)
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
    },
    contentType = "image/png"
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
      png(file, input$downloadWidth, input$downloadHeight, unit = "in", res = input$downloadRes)
      # plot with compound as label
      print(plate_plot(
        data = file_data(),
        position = Well,
        value = Compound,
        label = Compound,
        show_legend = !input$labelLegend,
        plate_size = input$wellNumber,
        plate_type = input$wellShape,
        title = input$titleField,
        title_size = input$textSize,
        legend_n_row = input$legendRows,
        label_size = input$labelSize
      ))
      # stop PNG render process
      dev.off()
    },
    contentType = "image/png"
  )
}

# Run the app
shinyApp(ui = ui, server = server)

# to publish on github:
# install.packages("shinylive") (if not done yet)
# shinylive::export(appdir = ".", destdir = "docs", resources = c("data", "www"))
# git -b checkout gh-pages (use this when you have not created a gh-pages branch)
# git checkout gh-pages (otherwise use this)
# mv docs/* .
# git add . (or git add --all)
# git commit -m "commit message"
# git push origin gh-pages