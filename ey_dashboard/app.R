library(shiny)
library(shinyalert)
library(here)
library(shinyWidgets)
library(rsconnect)

#Palette
Shy$eyYellow <- "color: #FFE700;"
Shy$palWhite <- "color: white"


ui <- fluidPage(
  # modalDialog(includeHTML(here::here("html","welcome.html"))),
  
  shinyWidgets::setBackgroundColor("#1C2134"),
  tags$style(HTML(" *{border-style: none !important}")),
  tags$style(HTML(".tabbable > .nav  > li > a:link {background-color:#222A35}")),
  tags$style(HTML(".col-sm-4 {background-color: #222A35}")),
  tags$style(HTML(".navbar-brand {color: #FFFFFF")),
  tags$style(HTML(".container-fluid {margin-left: 25px;
                  margin-right: 25px}")),

  
  titlePanel(list(img(src = "eyLogo.png",
                       height = 135))),
  
  sidebarLayout(
    navlistPanel("KICKSTARTER PREDICTOR", widths = c(4,8),
                 tabPanel("Walkthrough",
                          htmlOutput("introVid")),
                 tabPanel("General Summary",
                          tabsetPanel(
                            tabPanel(div("New Campaign Timeline",
                                         style = Shy$eyYellow),
                                     actionButton("btnTimeline", label = "Click for Analysis", icon = icon("question-circle")),
                                     plotOutput("myTimeline"),
                                     selectInput("mainCatPlot",
                                                 div("Select Main Category",
                                                     style = Shy$palWhite),
                                                 choices = Kik$mainCatPlotInput$mainCategory,
                                                 selected = "Art"),
                                     plotOutput("campaignCountCat")),
                            tabPanel(div("Funds Ratio",
                                         style = Shy$eyYellow),
                                     div(tableOutput("myFundsRatio"),
                                         style = Shy$palWhite)),
                            tabPanel(div("Partial Funds",
                                         style = Shy$eyYellow),
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
                                                   style = Shy$eyYellow),
                                               plotOutput("myCharPlot")),
                                      tabPanel(div("Most Popular Words Main Category",
                                                   style = Shy$eyYellow),
                                               plotOutput("nmTknMainPlotOut"),
                                               selectInput("tknRankMain",
                                                           div("Select Main Category",
                                                               style = Shy$palWhite),
                                                           choices = c(Kik$tknCatInput$mainCategory),
                                                           selected = "Art"),
                                               div(tableOutput("tknRankMainTable"),
                                                   style = Shy$palWhite)),
                                      tabPanel(div("Main Category FX Rank",
                                                   style = Shy$eyYellow),
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
  output$myTimeline <- renderPlot(Kik$timelineStCtPlot)
  output$myCharPlot <- renderPlot(Kik$charPlot)
  
  observeEvent(input$btnTimeline, {
    showModal(modalDialog(includeHTML(here::here("html","welcome.html"))))
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
  
  output$campaignCountCat <- renderPlot({
    inputPlot <- input$mainCatPlot
    Kik$mainCatPlot(inputPlot)
  })
}

shinyApp(ui, server)
























