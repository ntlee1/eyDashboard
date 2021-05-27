library(shiny)
library(shinyalert)
library(here)

ui <- fluidPage(
  #Welcome Popup
  #test <- modalDialog(includeHTML(here::here("html","welcome.html"))),
  
  headerPanel(list(img(src = "eylogo.png",
                       height = 90,
                       width = 90,
                       style = "vertical-align: bottom;"))),
  
  sidebarLayout(
    navlistPanel("KICKSTARTER PREDICTOR",
                 tabPanel("Walkthrough",
                          htmlOutput("introVid")),
                 tabPanel("General Summary",
                          tabsetPanel(
                            tabPanel("New Project Timeline",
                                     timelineStateCountPlot),
                            tabPanel("Category ",
                                     selectInput("subcatPlot",
                                                 "Select Subcategory",
                                                 choices = c("Food",
                                                             "Art"),
                                                 selected = "Art"),
                                     plotOutput("myPlot")
                                     
                                  
                                     ))
                          ),
                 tabPanel("Optimal Name Length"),
                 tabPanel("Optimal Category"),
                 tabPanel("Optimal Campaign Length"),
                 tabPanel("Optimal Goal Amount")
                 
    ),
    mainPanel(
   
      
      
    )
  )
)

server <- function(input, output, session) {
  #showModal(test)
  
  output$introVid <- renderUI({
    tags$iframe(src = "https://www.youtube.com/embed/hfPnq3i4Udw",
                width = 600, height = 400)
  })
  

  #EDIT Plotly in viewer pane not
  output$myPlot <- renderPlot({
  if(input$subcatPlot =="Food") {
    subCatPlotOutput[1]
  }
  })
  
  
}

shinyApp(ui, server)






























