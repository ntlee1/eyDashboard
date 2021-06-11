library(shiny)
library(shinyalert)
library(here)
library(shinyWidgets)
library(rsconnect)

eyYellow <- "color: #FFE700;"
tabTitle <- "color: white"
Shy <- new.env()


ui <- fluidPage(
  #Welcome Popup
 # modalDialog(includeHTML(here::here("html","welcome.html"))),

  shinyWidgets::setBackgroundColor("#1C2134"),
  tags$style(HTML(".tabbable > .nav  > li > a:link {background-color:#222A35}")),
  tags$style(HTML(".col-sm-4 {background-color: #222A35}")),
  tags$style(HTML(".navbar-brand {color: #FFFFFF")),
  tags$style(HTML(".col-sm-4 > .nav > li > a:hover a:active {color: #FFFFFF; background-color: red}")), 
  tags$style(HTML(".container-fluid {margin-left: 25px;
                  margin-right: 25px}")),
  
  

  headerPanel(list(img(src = "eyLogo.png",
                       height = 135,
                       width = 240,
                       style = "vertical-align: bottom;"))),
  
  sidebarLayout(
    navlistPanel("KICKSTARTER PREDICTOR", widths = c(4,8),
                 tabPanel("Walkthrough",
                          htmlOutput("introVid")),
                 tabPanel("General Summary",
                          tabsetPanel(
                            tabPanel(div("New Campaign Timeline",
                                     style = eyYellow),
                                     plotOutput("myTimeline"),
                                     selectInput("mainCatPlot",
                                                 div("Select Main Category",
                                                 style = tabTitle),
                                                 choices = c(Kik$mainCatPlotInput$mainCategory),
                                                 selected = "Art"),
                                     plotOutput("myPlot")),
                            tabPanel(div("Funds Ratio",
                                     style = eyYellow),
                                     div(tableOutput("myFundsRatio"),
                                     style = tabTitle)),
                            tabPanel(div("Partial Funds",
                                     style = eyYellow),
                                     selectInput("partialPlot",
                                                 div("Select Size",
                                                 style = tabTitle),
                                                 choices = c("Small",
                                                             "Mid",
                                                             "Large",
                                                             "Prem"),
                                                 selected = "Small"),
                                     plotOutput("partialFundPlot"))
                          )
                 ),
                 tabPanel("Campaign Name Analysis",
                          tabsetPanel(id = "tabsetHere",
                            tabPanel(div("Name Length",
                                     style = eyYellow),
                                     plotOutput("myCharPlot")),
                            tabPanel(div("Top Words Overall",
                                     style = eyYellow),
                                     div(tableOutput("kikNmAllOut"),
                                     style = tabTitle)),
                            tabPanel(div("Most Popular Words Main Category",
                                     style = eyYellow),
                                     plotOutput("nmTknMainPlotOut"),
                                     selectInput("tknRankMain",
                                                 div("Select Main Category",
                                                 style = tabTitle),
                                                 choices = unique(Kik$kiksrt$main_category),
                                                 selected = "Art"),
                                     div(tableOutput("tknRankMainTable"),
                                     style = tabTitle)),
                            tabPanel(div("Main Category FX Rank",
                                     style = eyYellow),
                                     selectInput("FX",
                                                 div("Select Base Currency",
                                                 style = tabTitle),
                                                 choices = unique(Kik$kiksrt$currency),
                                                 selected = "USD"),
                                     selectInput("FX2",
                                                 div("Select Compare Currency",
                                                 style = tabTitle),
                                                 choices = unique(Kik$kiksrt$currency),
                                                 selected = "GBP"),
                                     selectInput("FXCat",
                                                 div("Select Main Category",
                                                 style = tabTitle),
                                                 choices = unique(Kik$kiksrt$main_category),
                                                 selected = "Art"),
                                     div(tableOutput("FXTable"),
                                     style = tabTitle))
                          )
                 ),
                 tabPanel("About",
                          div(textOutput("aboutMe"),
                          style = tabTitle))
    ),
    mainPanel(
      
      
      
    )
  )
)

server <- function(input, output, session) {
  #showModal(test)

  
  #Campaign Name Analysis --------------------------------------------------------

  output$aboutMe <- renderText("Hello")
 
  
  
  #Top 100 Words Overall
  output$kikNmAllOut <- renderTable(Kik$kikNmAll)
  
  #Top 100 Words Main Category
  output$tknRankMainTable <- renderTable({
    input1 <- reactive({input$tknRankMain})
    myPlot <- Kik$tknRankMain(input1())
    myPlot
  })
  #wc
  output$nmTknMainPlotOut <-renderPlot({
    myCat <- reactive({input$tknRankMain})
    
    myPlot <- Kik$nmTknMainPlot(mainCat = myCat())
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


























