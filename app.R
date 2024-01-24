library(shiny)
library(bslib)
library(htmltools)
library(tidyverse)
library(echarts4r)
library(reactable)
library(readxl)
library(readr)
library(rsconnect)

ui <- page_sidebar(
  title="Cancer Inequalities - SR Results",
  sidebar = sidebar(
      tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Loading...",id="loadmessage")),
      uiOutput("selectCancer"),
      uiOutput("selectYear"),
      uiOutput("selectPOC"),
      uiOutput("selectSource")
    ),
  
  navset_card_underline(
    nav_panel("Treemap",echarts4rOutput("tree")),
    nav_panel("Table",tags$div(reactableOutput("table"),
                               tags$br(),
                               tags$br(),
                               downloadButton("downloadData","Download")
                               )
              )
  )
  
)


server <- function(input, output) {
  
  data <- reactive({read_rds("inst/processed_table2.rds")})
  
  output$selectCancer <- renderUI({
    selectInput("cancer",
                "Cancer Site",
                choices = c("Overall",sort(unique(data()$cancer))),
                # multiple = T,
                selected = "Overall"
    )
  })
  
  output$selectYear <- renderUI({
    sliderInput("year",
                "Publication Year",
                min=min(data()$year),
                max=max(data()$year),
                value=c(min(data()$year),max(data()$year)),
                sep="",
                dragRange=T
    )
  })
  
  
  # output$selectYear <- renderUI({
  #   selectInput("year",
  #               "Publication Year",
  #               choices=unique(data$year)
  #   )
  # })
  
  output$selectPOC <- renderUI({
    selectInput("poc",
                "Point on Care Continuum",
                choices = c("Overall",sort(unique(data()$poc))),
                # multiple = T,
                selected = "Overall"
    )
  })

  output$selectSource <- renderUI({
    selectInput("source",
                "Data Source",
                choices = c("Overall",sort(unique(data()$source))),
                # multiple = T,
                selected = "Overall"
    )
  })

  output$tree <- renderEcharts4r({
    
    shiny::validate(need(input$cancer, 'Please select a valid cancer site'))
    shiny::validate(need(input$year, 'Please select a valid publication year'))
    shiny::validate(need(input$poc, 'Please select a valid point on care'))
    shiny::validate(need(input$source, 'Please select a valid data source'))
    
    treeFUN(data(),input$cancer,input$year,input$poc,input$source,"plot")
    
  })
  
  output$table <- renderReactable({
    
    shiny::validate(need(input$cancer, 'Please select a valid cancer site'))
    shiny::validate(need(input$year, 'Please select a valid publication year'))
    shiny::validate(need(input$poc, 'Please select a valid point on care'))
    shiny::validate(need(input$source, 'Please select a valid data source'))
    
    treeFUN(data(),input$cancer,input$year,input$poc,input$source,"table")
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("cancer_inequal_summary.csv")
    },
    content = function(file) {
      write.csv(treeFUN(data(),input$cancer,input$year,input$poc,input$source,"download"), 
                file)
    }
  )
  
}


shinyApp(ui = ui, server = server)








