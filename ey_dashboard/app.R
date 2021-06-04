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
                                     Kik$timelineStCtPlot),
                            tabPanel("Category ",
                                     selectInput("subcatPlot",
                                                 "Select Subcategory",
                                                 choices = c(Kik$subCatPlotInput$mainCategory),
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
  #EDIT Fix order
  output$myPlot <- renderPlot({
    switch(input$subcatPlot, 
           "Food" = Kik$subCatPlotOutput[1],
           "Music" = Kik$subCatPlotOutput[2],
           "Comics" = Kik$subCatPlotOutput[3],
           "Design" = Kik$subCatPlotOutput[4],
           "Art" = Kik$subCatPlotOutput[5],
           "Fashion" = Kik$subCatPlotOutput[6],
           "Film & Video" = Kik$subCatPlotOutput[7],
           "Publishing" = Kik$subCatPlotOutput[8],
           "Technology" = Kik$subCatPlotOutput[9],
           "Games" = Kik$subCatPlotOutput[10],
           "Photography" = Kik$subCatPlotOutput[11],
           "Dance" = Kik$subCatPlotOutput[12],
           "Crafts" = Kik$subCatPlotOutput[13],
           "Journalism" = Kik$subCatPlotOutput[14],
           "Theater" = Kik$subCatPlotOutput[15])
  })
  
  
}

shinyApp(ui, server)






























