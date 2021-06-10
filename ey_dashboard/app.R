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
                                     plotOutput("myTimeline")),
                            tabPanel("Category",
                                     selectInput("mainCatPlot",
                                                 "Select Main Category",
                                                 choices = c(Kik$mainCatPlotInput$mainCategory),
                                                 selected = "Art"),
                                     plotOutput("myPlot")),
                            tabPanel("Avg Campaign Length",
                                     plotOutput("AvgCampLen")),
                            tabPanel("Funds Ratio",
                                     tableOutput("myFundsRatio")),
                            tabPanel("Partial Funds",
                                     selectInput("partialPlot",
                                                 "Select Size",
                                                 choices = c("Small",
                                                             "Mid",
                                                             "Large",
                                                             "Prem"),
                                                             selected = "Small"),
                                     plotOutput("partialFundPlot")),
                            tabPanel("Name Length",
                                     plotOutput("myCharPlot"))
                            )
                          ),
                 tabPanel("Campaign Name Analysis",
                          tabsetPanel(
                            tabPanel("Top Words Overall",
                                     tableOutput("kikNmAllOut")),
                            tabPanel("Top 100 Words Main Category",
                                     selectInput("tknRankMain",
                                                 "Select Main Category",
                                                 choices = unique(Kik$kiksrt$main_category),
                                                 selected = "Art"),
                                     tableOutput("tknRankMainTable")),
                            tabPanel("Top 100 Words Sub Category",
                                     selectInput("tknRankSub",
                                                 "Select Sub Category",
                                                 choices = unique(Kik$kiksrt$category),
                                                 selected = "Poetry"),
                                     tableOutput("tknRankSubTable")),
                            tabPanel("Top 60 Main WC",
                                     selectInput("nmTknMainPlot",
                                                 "Select Main Category",
                                                 choices = unique(Kik$kiksrt$main_category),
                                                 selected = "Art"),
                                     plotOutput("nmTknMainPlotOut")),
                            tabPanel("Top 60 Sub WC",
                                     selectInput("nmTknSubPlot",
                                                 "Select Sub Category",
                                                 choices = unique(Kik$kiksrt$category),
                                                 selected = "Poetry"),
                                     plotOutput("nmTknSubPlotOut")),
                            tabPanel("Main Category FX Rank",
                                     selectInput("FX",
                                                 "Select Base Currency",
                                                 choices = unique(Kik$kiksrt$currency),
                                                 selected = "USD"),
                                     selectInput("FX2",
                                                 "Select Compare Currency",
                                                 choices = unique(Kik$kiksrt$currency),
                                                 selected = "GBP"),
                                     selectInput("FXCat",
                                                 "Select Main Category",
                                                 choices = unique(Kik$kiksrt$main_category),
                                                 selected = "Art"),
                                     tableOutput("FXTable"))
                          ))
    ),
    mainPanel(
   
      
      
    )
  )
)

server <- function(input, output, session) {
  #showModal(test)
  Shy <- new.env()
  
#Campaign Name Analysis --------------------------------------------------------

#Top 100 Words Overall
  output$kikNmAllOut <- renderTable(Kik$kikNmAll)

#Top 100 Words Main Category
  output$tknRankMainTable <- renderTable({
    input1 <- reactive({input$tknRankMain})
    myPlot <- Kik$tknRankMain(input1())
    myPlot
  })
  
#Top 100 Words Sub Category
  output$tknRankSubTable <- renderTable({
    input1 <- reactive({input$tknRankSub})
    myPlot <- Kik$tknRankSub(input1())
    myPlot
  })
  
  
#Top 60 Main Category WC
  output$nmTknMainPlotOut <-renderPlot({
    myCat <- reactive({input$nmTknMainPlot})
    
    myPlot <- Kik$nmTknMainPlot(mainCat = myCat())
    myPlot
  })

#Top 60 Sub Cat WC
  output$nmTknSubPlotOut <-renderPlot({
    myCat <- reactive({input$nmTknSubPlot})
    
    myPlot <- Kik$nmTknSubPlot(subCat = myCat())
    myPlot
  })  

#Main Cat FX Rank
  output$FXTable <- renderTable({
    myFx1 <- reactive({input$FX})
    myFx2 <- reactive({input$FX2})
    myCat <- reactive({input$FXCat}) 
    
    myTable <- Kik$tknFxRank(mainCat = myCat(), baseCurr = myFx1(), curr2 = myFx2())
    myTable
  })
  
  
  
  
  
  
  
  
  
  
  output$AvgCampLen <- renderPlot(Kik$projLenMedianPlot)
  
  output$myCharPlot <- renderPlot(Kik$charPlot)
  
  output$partialFundPlot <- renderPlot({
    switch(input$partialPlot,
           "Small" = Kik$partialFailPlot("Small"),
           "Mid" = Kik$partialFailPlot("Mid"),
           "Large" = Kik$partialFailPlot("Large"),
           "Prem" = Kik$partialFailPlot("Prem"))
  })
  
  output$myFundsRatio <- renderTable(Kik$catRatioResults)
  
  output$myTimeline <- renderPlot(Kik$timelineStCtPlot)
  
  output$introVid <- renderUI({
    tags$iframe(src = "https://www.youtube.com/embed/hfPnq3i4Udw",
                width = 600, height = 400)
  })
  

  #EDIT Plotly in viewer pane not
  #EDIT Fix order
  output$myPlot <- renderPlot({
    switch(input$mainCatPlot, 
           "Food" = Kik$mainCatPlot("Food"),
           "Music" = Kik$mainCatPlot("Music"),
           "Comics" = Kik$mainCatPlot("Comics"),
           "Design" = Kik$mainCatPlot("Design"),
           "Art" = Kik$mainCatPlot("Art"),
           "Fashion" = Kik$mainCatPlot("Fashion"),
           "Film & Video" = Kik$mainCatPlot("Film & Video"),
           "Publishing" = Kik$mainCatPlot("Publishing"),
           "Technology" = Kik$mainCatPlot("Technology"),
           "Games" = Kik$mainCatPlot("Games"),
           "Photography" = Kik$mainCatPlot("Photography"),
           "Dance" = Kik$mainCatPlot("Dance"),
           "Crafts" = Kik$mainCatPlot("Crafts"),
           "Journalism" = Kik$mainCatPlot("Journalism"),
           "Theater" = Kik$mainCatPlot("Theater"))
  })
  
  
}

shinyApp(ui, server)































