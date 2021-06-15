library(shiny)
library(shinyalert)
library(here)
library(shinyWidgets)
library(rsconnect)

#Palette
#More colors on 02_analysis
Shy$palEyYellow <- "color: #FFE700;"
Shy$palWhite <- "color: white"

ui <- fluidPage(
   #modalDialog(includeHTML(here::here("html","welcome.html")),
    #           easyClose = TRUE),
  
  shinyWidgets::setBackgroundColor("#1C2134"),
  tags$style(HTML(" *{border-style: none !important}")),
  tags$style(HTML(".tabbable > .nav  > li > a:link {background-color:#222A35}")),
  tags$style(HTML(".col-sm-4 {background-color: #222A35}")),
  tags$style(HTML(".navbar-brand {color: #FFFFFF")),
  tags$style(HTML(".container-fluid {margin-left: 25px;
                  margin-right: 25px}")),
  tags$style(HTML(".btn {color: blue;
                  margin: 15px 0}")),

  
  titlePanel(list(img(src = "eyLogo.png",
                       height = 135))),
  
  sidebarLayout(
    navlistPanel("KICKSTARTER PREDICTOR", widths = c(4,8),
                 tabPanel("Walkthrough",
                          htmlOutput("introVid")),
                 tabPanel("General Summary",
                          tabsetPanel(
<<<<<<< HEAD
                            tabPanel(div("New Campaign Timeline",
                                         style = Shy$palEyYellow),
                                     (actionButton("btnTimeline", label = "Click for Analysis",
                                                   icon = icon("question-circle"))),
                                     selectInput("timelineIn",
                                                 div("Select Project Size",
                                                 style = Shy$palWhite),
                                                 choices = Kik$timelineStCtInput$kikSize,
                                                 selected = "Small"),
                                     plotOutput("myTimeline"),
                                     selectInput("mainCatPlot",
                                                 div("Select Main Category",
                                                     style = Shy$palWhite),
                                                 choices = Kik$mainCatPlotInput$mainCategory,
=======
                            tabPanel("New Project Timeline",
                                     Kik$timelineStCtPlot),
                            tabPanel("Category ",
                                     selectInput("subcatPlot",
                                                 "Select Subcategory",
                                                 choices = c(Kik$subCatPlotInput$mainCategory),
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
                                                 selected = "Art"),
                                     selectInput("mainCatPlotSz",
                                                 div("Select Project Size",
                                                     style = Shy$palWhite),
                                                 choices = Kik$mainCatPlotInputSz$kikSize,
                                                 selected = "Small"),
                                     plotOutput("campaignCountCat")),
                            tabPanel(div("Funds Ratio",
                                         style = Shy$palEyYellow),
                                     div(tableOutput("myFundsRatio"),
                                         style = Shy$palWhite)),
                            tabPanel(div("Partial Funds",
                                         style = Shy$palEyYellow),
                                     selectInput("partialPlot",
                                                 div("Select Size",
                                                     style = Shy$palWhite),
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
                                                   style = Shy$palEyYellow),
                                               plotOutput("myCharPlot")),
                                      tabPanel(div("Most Popular Words Main Category",
                                                   style = Shy$palEyYellow),
                                               plotOutput("nmTknMainPlotOut"),
                                               selectInput("tknRankMain",
                                                           div("Select Main Category",
                                                               style = Shy$palWhite),
                                                           choices = c(Kik$tknCatInput$mainCategory),
                                                           selected = "Art"),
                                               div(tableOutput("tknRankMainTable"),
                                                   style = Shy$palWhite)),
                                      tabPanel(div("Main Category FX Rank",
                                                   style = Shy$palEyYellow),
                                               selectInput("fx",
                                                           div("Select Base Currency",
                                                               style = Shy$palWhite),
                                                           choices = Kik$tknFxRankInput$currency,
                                                           selected = "USD"),
                                               selectInput("fx2",
                                                           div("Select Compare Currency",
                                                               style = Shy$palWhite),
                                                           choices = Kik$tknFxRankInput$currency,
                                                           selected = "GBP"),
                                               selectInput("fxCat",
                                                           div("Select Main Category",
                                                               style = Shy$palWhite),
                                                           choices = unique(Kik$tknFxRankInputCat$mainCategory),
                                                           selected = "Art"),
                                               div(tableOutput("fxTable"),
                                                   style = Shy$palWhite))
                          )
                 ),
                 tabPanel("About",
                          div(textOutput("aboutMe"),
                              style = Shy$palWhite))
    ),
    mainPanel(
    )
  )
)

server <- function(input, output, session) {
  #showModal(test)
  
  output$aboutMe <- renderText("Hello")
  output$myFundsRatio <- renderTable(Kik$catRatioResults)
  output$myCharPlot <- renderPlot(Kik$charPlot)
  
  observeEvent(input$btnTimeline, {
    showModal(modalDialog(includeHTML(here::here("html","welcome.html")), 
                          easyClose = TRUE))
  })
  
  output$myTimeline <- renderPlot({
    input1 <- input$timelineIn
    Kik$timelineStCtPlot(input1)
  })
  
  output$tknRankMainTable <- renderTable({
    input1 <- input$tknRankMain
    Kik$tknRankMain(input1)
  })
  
  output$nmTknMainPlotOut <-renderPlot({
    myCat <- input$tknRankMain
    Kik$nmTknMainPlot(mainCat = myCat)
  })
  
  output$fxTable <- renderTable({
    myFx1 <- input$fx
    myFx2 <- input$fx2
    myCat <- input$fxCat 
    
    myTable <- Kik$tknFxRank(mainCat = myCat, baseCurr = myFx1, curr2 = myFx2)
    myTable
  })
  
  output$partialFundPlot <- renderPlot({
    inputPlot <- input$partialPlot
    Kik$partialFailPlot(inputPlot)
    
  })
  
  output$introVid <- renderUI({
    tags$iframe(src = "https://www.youtube.com/embed/hfPnq3i4Udw",
                width = 600, height = 400)
  })
  
<<<<<<< HEAD
  output$campaignCountCat <- renderPlot({
    myCat <- input$mainCatPlot
    mySize <- input$mainCatPlotSz
    Kik$mainCatPlot(mainCategory = myCat, kikSize = mySize)
=======

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
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
  })
}

shinyApp(ui, server)
























