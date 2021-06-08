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
                                     plotOutput("myPlot")),
                            tabPanel("Campaign Length",
                                     Kik$projLenMedianPlot)
                                     
                                  
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
  output$introVid <- renderUI({
    tags$iframe(src = "https://www.youtube.com/embed/hfPnq3i4Udw",
                width = 600, height = 400)
  })
  
  output$myPlot <- renderPlot({
    switch(input$subcatPlot, 
           "Food" = Kik$subcatPlot("Food"),
           "Music" = Kik$subcatPlot("Music"),
           "Comics" = Kik$subcatPlot("Comics"),
           "Design" = Kik$subcatPlot("Design"),
           "Art" = Kik$subcatPlot("Art"),
           "Fashion" = Kik$subcatPlot("Fashion"),
           "Film & Video" = Kik$subcatPlot("Film & Video"),
           "Publishing" = Kik$subcatPlot("Publishing"),
           "Technology" = Kik$subcatPlot("Technology"),
           "Games" = Kik$subcatPlot("Games"),
           "Photography" = Kik$subcatPlot("Photography"),
           "Dance" = Kik$subcatPlot("Dance"),
           "Crafts" = Kik$subcatPlot("Crafts"),
           "Journalism" = Kik$subcatPlot("Journalism"),
           "Theater" = Kik$subcatPlot("Theater"))
  })
  
  
}

shinyApp(ui, server)






























