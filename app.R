library(shiny)
library(ggplot2)
library(readxl)
library(haven)
library(DT)
library(colourpicker)

ui <- fluidPage(
  titlePanel("Dynamic Plot Generator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Data File",
                accept = c(".csv", ".xlsx", ".xls", ".sas7bdat", ".xpt")),
      
      radioButtons("dataSource", "Data Input Method",
                   choices = c("Upload File" = "file", "Manual Input" = "manual")),
      
      conditionalPanel(
        condition = "input.dataSource == 'manual'",
        textAreaInput("xvec", "Enter X-axis vector (comma separated)", "1,2,3,4,5"),
        textAreaInput("yvec", "Enter Y-axis vector (comma separated)", "10,20,30,40,50")
      ),
      
      uiOutput("varSelectUI"),
      
      selectInput("plotType", "Select Plot Type",
                  choices = c("Histogram", "Box Plot", "Line Chart", 
                              "Scatter Plot", "Bar Graph", "Area Under the Curve")),
      textInput("xlabel", "X-axis Label", ""),
      textInput("ylabel", "Y-axis Label", ""),
      colourInput("plotColor", "Choose Plot Color", value = "#2C3E50"),
      
      hr(),
      selectInput("downloadFormat", "Download Format", choices = c("PNG", "JPEG")),
      downloadButton("downloadPlot", "Download Plot")
    ),
    
    mainPanel(
      plotOutput("plot"),
      DTOutput("dataPreview")
    )
  )
)

server <- function(input, output, session) {
  userData <- reactive({
    req(input$dataSource)
    
    if (input$dataSource == "file") {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      switch(ext,
             csv = read.csv(input$file$datapath),
             xlsx = read_excel(input$file$datapath),
             xls = read_excel(input$file$datapath),
             sas7bdat = read_sas(input$file$datapath),
             xpt = read_xpt(input$file$datapath),
             validate("Unsupported file format"))
    } else {
      x <- as.numeric(unlist(strsplit(input$xvec, ",")))
      y <- as.numeric(unlist(strsplit(input$yvec, ",")))
      data.frame(x = x, y = y)
    }
  })
  
  output$varSelectUI <- renderUI({
    df <- userData()
    req(df)
    if (input$dataSource == "file") {
      tagList(
        selectInput("xvar", "Select X variable", choices = names(df)),
        selectInput("yvar", "Select Y variable", choices = names(df))
      )
    } else {
      return(NULL)
    }
  })
  
  getPlot <- reactive({
    df <- userData()
    plotType <- input$plotType
    color <- input$plotColor
    
    if (input$dataSource == "manual") {
      x <- df$x
      y <- df$y
      xlabel <- ifelse(input$xlabel != "", input$xlabel, "x")
      ylabel <- ifelse(input$ylabel != "", input$ylabel, "y")
    } else {
      req(input$xvar, input$yvar)
      x <- df[[input$xvar]]
      y <- df[[input$yvar]]
      xlabel <- ifelse(input$xlabel != "", input$xlabel, input$xvar)
      ylabel <- ifelse(input$ylabel != "", input$ylabel, input$yvar)
    }
    
    plot_df <- data.frame(x = x, y = y)
    
    gg <- ggplot(plot_df, aes(x = x, y = y)) +
      xlab(xlabel) +
      ylab(ylabel)
    
    gg <- switch(plotType,
                 "Histogram" = ggplot(plot_df, aes(x = x)) + 
                   geom_histogram(fill = color, bins = 30) + xlab(xlabel),
                 "Box Plot" = gg + geom_boxplot(fill = color),
                 "Line Chart" = gg + geom_line(color = color),
                 "Scatter Plot" = gg + geom_point(color = color),
                 "Bar Graph" = gg + geom_bar(stat = "identity", fill = color),
                 "Area Under the Curve" = gg + geom_area(fill = color, alpha = 0.6),
                 NULL)
    
    return(gg)
  })
  
  
  output$plot <- renderPlot({
    gg <- getPlot()
    print(gg)
  })
  
  output$dataPreview <- renderDT({
    df <- userData()
    datatable(df, options = list(pageLength = 5))
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("plot_", Sys.Date(), ".", tolower(input$downloadFormat))
    },
    content = function(file) {
      gg <- getPlot()
      format <- tolower(input$downloadFormat)
      if (format == "png") {
        ggsave(file, plot = gg, device = "png", width = 7, height = 5)
      } else {
        ggsave(file, plot = gg, device = "jpeg", width = 7, height = 5)
      }
    }
  )
}

shinyApp(ui, server)
