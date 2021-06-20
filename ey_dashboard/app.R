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
            # easyClose = TRUE),
  
  shinyWidgets::setBackgroundColor("#1C2134"),
  tags$style(HTML(" *{border-style: none !important}")),
  tags$style(HTML(".tabbable > .nav  > li > a:link {background-color:#222A35}")),
  tags$style(HTML(".col-sm-2 {background-color: #222A35}")),
  tags$style(HTML(".navbar-brand {color: #FFFFFF")),
  tags$style(HTML(".container-fluid {margin-left: 25px;
                  margin-right: 25px}")),
  tags$style(HTML(".btn {color: blue;
                  margin: 15px 0}")),
  
  tags$style(HTML(".datatables > .dataTables_wrapper > .display > tbody > tr:nth-child(even) {background-color: #797878")),
  tags$style(HTML(".datatables > .dataTables_wrapper > .display > thead {background-color: #797878")),
  tags$style(HTML(".dataTables_filter {display: none}")),
  tags$style(HTML(".dataTables_length {display: none}")),
  tags$style(HTML(".datatables > .dataTables_wrapper > .display > caption {background-color: #FFE700;
                  color: black; text-align: center}")),
  tags$style(HTML(".datatables > .dataTables_wrapper > .dataTables_info {color: white")),
  tags$style(HTML(".datatables > .dataTables_wrapper > .dataTables_paginate {background-color: white}")),
  tags$style(HTML("#myTimeline {width: 80vw !important}")),
  tags$style(HTML("#campaignCountCat {width: 80vw !important}")),
  tags$style(HTML("#partialFundPlot {width: 80vw !important}")),

  
  titlePanel(list(img(src = "eyLogo.png",
                      height = 135))),
  
  sidebarLayout(
    navlistPanel("KICKSTARTER PREDICTOR", widths = c(2,8),
#                 tabPanel("Walkthrough",
 #                         htmlOutput("introVid")),
                 tabPanel("General Summary",
                          tabsetPanel(
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
                            tabPanel(div("New Campaign Timeline",
                                         style = Shy$palEyYellow),
                                     (actionButton("btnTimeline", label = "Click for Info",
                                                   icon = icon("question-circle"))),
                                     selectInput("timelineIn",
                                                 div("Select Project Size",
                                                     style = Shy$palWhite),
                                                 choices = Kik$timelineStCtInput$kikSize,
                                                 selected = "Small"),
                                     plotly::plotlyOutput("myTimeline",
                                                          width = "100%"),
                                     selectInput("mainCatPlot",
                                                 div("Select Main Category",
                                                     style = Shy$palWhite),
                                                 choices = Kik$mainCatPlotInput$mainCategory,
<<<<<<< HEAD
=======
=======
                            tabPanel("New Project Timeline",
                                     Kik$timelineStCtPlot),
                            tabPanel("Category ",
                                     selectInput("subcatPlot",
                                                 "Select Subcategory",
                                                 choices = c(Kik$subCatPlotInput$mainCategory),
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
                                                 selected = "Art"),
                                     selectInput("mainCatPlotSz",
                                                 div("Select Project Size",
                                                     style = Shy$palWhite),
                                                 choices = Kik$mainCatPlotInputSz$kikSize,
                                                 selected = "Small"),
                                     plotly::plotlyOutput("campaignCountCat")),
                            tabPanel(div("Funds Ratio",
                                         style = Shy$palEyYellow),
                                     (actionButton("btnFunRat", label = "Click for Info",
                                                   icon = icon("question-circle"))),
                                     DT::DTOutput("myFundsRatio")),
                            tabPanel(div("Partial Funds",
                                         style = Shy$palEyYellow),
                                     (actionButton("btnPartialFun", label = "Click for Info",
                                                   icon = icon("question-circle"))),
                                     selectInput("partialPlot",
                                                 div("Select Size",
                                                     style = Shy$palWhite),
                                                 choices = c("Small",
                                                             "Med",
                                                             "Large",
                                                             "Prem"),
                                                 selected = "Small"),
                                     plotly::plotlyOutput("partialFundPlot"))
                          )
                 ),
                 tabPanel("Campaign Name Analysis",
                          tabsetPanel(id = "tabsetHere",
                                      tabPanel(div("Most Popular Words Main Category",
                                                   style = Shy$palEyYellow),
                                               (actionButton("btnWCloud", label = "Click for Info",
                                                             icon = icon("question-circle"))),
                                               plotOutput("nmTknMainPlotOut"),
                                               selectInput("tknRankMain",
                                                           div("Select Main Category",
                                                               style = Shy$palWhite),
                                                           choices = c(Kik$tknCatInput$mainCategory),
                                                           selected = "Art"),
                                               div(DT::dataTableOutput("tknRankMainTable"),
                                                   style = Shy$palWhite)),
                                      tabPanel(div("Main Category FX Rank",
                                                   style = Shy$palEyYellow),
                                               (actionButton("btnFxRank", label = "Click for Info",
                                                             icon = icon("question-circle"))),
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
                                               div(DT::dataTableOutput("fxTable"),
                                                   style = Shy$palWhite))
                          )
                 ),
                 tabPanel("About",
                          div(htmlOutput("aboutMe"),
                              style = Shy$palWhite),
                          div(includeHTML(here::here("html","about.html"))),
                          style = "color: white")
    ),
    mainPanel(
    )
  )
)

server <- function(input, output, session) {
  #showModal(test)
  
  output$myFundsRatio <- DT::renderDT(Kik$catRatioResults)
  
  observeEvent(input$btnTimeline, {
    showModal(modalDialog(includeHTML(here::here("html","genSumNewCamp.html")), 
                          easyClose = TRUE))
  })
  observeEvent(input$btnFunRat, {
    showModal(modalDialog(includeHTML(here::here("html","genSumFunRat.html")), 
                          easyClose = TRUE))
  })
  observeEvent(input$btnPartialFun, {
    showModal(modalDialog(includeHTML(here::here("html","genSumPartialFun.html")), 
                          easyClose = TRUE))
  })
  observeEvent(input$btnWCloud, {
    showModal(modalDialog(includeHTML(here::here("html","nmWCloud.html")), 
                          easyClose = TRUE))
  }) 
  observeEvent(input$btnFxRank, {
    showModal(modalDialog(includeHTML(here::here("html","nmFxRank.html")), 
                          easyClose = TRUE))
  }) 
  
  
  output$myTimeline <- plotly::renderPlotly({
    input1 <- input$timelineIn
    Kik$timelineStCtPlot(input1)
  })
  output$campaignCountCat <- plotly::renderPlotly({
    myCat <- input$mainCatPlot
    mySize <- input$mainCatPlotSz
    Kik$mainCatPlot(mainCategory = myCat, kikSize = mySize)
  })
  output$tknRankMainTable <- DT::renderDataTable({
    input1 <- input$tknRankMain
    Kik$tknRankMain(input1)
  })
  output$nmTknMainPlotOut <- renderPlot({
    myCat <- input$tknRankMain
    Kik$nmTknMainPlot(mainCat = myCat)
  })
  output$fxTable <- DT::renderDataTable({
    myFx1 <- input$fx
    myFx2 <- input$fx2
    myCat <- input$fxCat 
    myTable <- Kik$tknFxRank(mainCat = myCat, baseCurr = myFx1, curr2 = myFx2)
    
  })
  output$partialFundPlot <- plotly::renderPlotly({
    inputPlot <- input$partialPlot
    Kik$partialFailPlot(inputPlot)
    
  })
  output$introVid <- renderUI({
    tags$iframe(src = "https://www.youtube.com/embed/hfPnq3i4Udw",
                width = 600, height = 400)
  })
<<<<<<< HEAD

=======
  
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
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
}

shinyApp(ui, server)
























