

library(shiny)
iris_li <- read.csv("data/play_data.csv")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  pageWithSidebar(
    headerPanel('Test k-means clustering'),
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(iris_li)),
      selectInput('ycol', 'Y Variable', names(iris_li),
                  selected=names(iris_li)[[2]]),
      numericInput('clusters', 'Cluster count', 3,
                   min = 1, max = 9)
    ),
    mainPanel(
      plotOutput('plot1')
    )
  ))



# Define server logic required to draw a histogram ----
#iris_li <- read.csv("C:/Personal/80126902/Desktop/R_Shinny/App-2/data/play_data.csv")


server <- function(input, output) {
  
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris_li[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}

shinyApp(ui = ui, server = server)

