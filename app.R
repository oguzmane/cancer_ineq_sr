library(shiny)
library(bslib)
library(htmltools)
library(tidyverse)
library(echarts4r)
library(reactable)
library(readxl)
library(readr)
library(rsconnect)
library(shinyWidgets)
library(bsicons)
library(DescTools)
library(sf)
library(tmap)

ui <- page_sidebar(
  title="Cancer Inequalities - SR Results",
  includeCSS("www/css.css"),
  sidebar = sidebar(
      width = 350,
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
      # actionButton("show", "Select Cancer Sites"),
      uiOutput("selectCancer"),
      uiOutput("selectPOC"),
      uiOutput("selectPub_Year"),
      uiOutput("selectStudy_Start"),
      uiOutput("selectStudy_End"),
      uiOutput("selectCountry"),
      uiOutput("selectNo_Pat"),
      uiOutput("selectSource")
    ),
  
  navset_card_underline(
    nav_panel("Treemap",
              echarts4rOutput("tree")),
    nav_panel("Time Series",
              uiOutput("time_selectMetric"),
              echarts4rOutput("time")),
    nav_panel("Map",
              uiOutput("map_selectInequality"),
              tmapOutput("map")),
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
  data_geo <- reactive({read_rds("inst/geo.rds")})
  
 
  # observeEvent(input$show, {
  #   showModal(modalDialog(
  #     title = "Important message",
  #   ))
  # })
  
  output$selectCancer <- renderUI({
    pickerInput("cancer",
                "Cancer Site",
                choices = list("Overall"=list("Cancer - General"),
                               "Breast"=list("Breast"),
                               "Digestive/Gastrointestinal"=list("Anal","Appendiceal","Bowel",
                                                                 "Bile duct cancer","Liver",
                                                                 "Oesophageal","Pancreatic","Stomach"),
                               "Endocrine"=list("Thyroid"),
                               "Eye"=list("Retinoblastoma"),
                               "Genitourinary"=list("Bladder","Kidney","Penile","Prostate",
                                                    "Testicular","Wilms tumour"),
                               "Germ Cell"=list("Germ cell tumours"),
                               "Gynaecological"=list("Cervical","Ovarian","Uterine","Vulval"),
                               "Head and Neck"=list("Head and Neck - General","Laryngeal","Salivary gland"),
                               "Haematological"=list("Lymphoma - General","Hodgkin Lymphoma",
                                                     "Non-Hodgkin Lymphoma","Leukaemia - General",
                                                     "Lymphoblastic Leukaemia",
                                                     "Myeloid Leukaemia"),
                               "Musculoskeletal"=list("Bone/sarcoma"),
                               "Neurological"=list("Brain & CNS","Neuroblastoma"),
                               "Respiratory"=list("Lung","Mesothelioma"),
                               "Skin"=list("Skin - General","Melanoma","Non-melanoma"),
                               "Other"=list("Other")),
                multiple = T,
                selected = unique(data()$cancer_group)[!is.na(unique(data()$cancer_group))], 
                options = list('actions-box' = T)
    )
  })
  
  output$selectPOC <- renderUI({
    pickerInput("poc",
                "Outcome",
                choices = list("Point on Care Continuum" = list("Awareness","Access to care",
                                                                "Diagnosis","Screening","Treatment","Experience",
                                                                "Quality of life"),
                               "Metrics" = list("Incidence","Mortality","Risk","Survival"),
                               "Other" = list("Other")),
                multiple = T,
                selected = unique(data()$poc)[!is.na(unique(data()$poc))], 
                options = list('actions-box' = T)
    )
  })
  
  output$selectPub_Year <- renderUI({
    sliderInput("pub_year",
                "Publication Year",
                min=min(data()$pub_year),
                max=max(data()$pub_year),
                value=c(min(data()$pub_year),max(data()$pub_year)),
                sep="",
                dragRange=T
    )
  })
  
  output$selectStudy_Start <- renderUI({
    sliderInput("study_start",
                "Study Start",
                min=min(data()$study_start,na.rm=T),
                max=max(data()$study_start,na.rm=T),
                value=c(min(data()$study_start,na.rm=T),
                        max(data()$study_start,na.rm=T)),
                sep="",
                dragRange=T
    )
  })
  
  output$selectStudy_End <- renderUI({
    sliderInput("study_end",
                "Study End",
                min=min(data()$study_end,na.rm=T),
                max=max(data()$study_end,na.rm=T),
                value=c(min(data()$study_end,na.rm=T),
                        max(data()$study_end,na.rm=T)),
                sep="",
                dragRange=T
    )
  })
  
  output$selectCountry <- renderUI({
    pickerInput("country",
                "Country",
                choices = sort(unique(data()$country)),
                multiple = T,
                selected = sort(unique(data()$country)[!is.na(unique(data()$country))]),
                options = list('actions-box' = T)
    )
  })
  
  output$selectNo_Pat <- renderUI({
    pickerInput("no_pat",
                "# Patients",
                choices = levels(data()$no_pat),
                multiple = T,
                selected = levels(data()$no_pat),
                options = list('actions-box' = T)
    )
  })
  
  output$selectSource <- renderUI({
    pickerInput("source",
                "Data Source",
                choices = sort(unique(data()$source)),
                multiple = T,
                selected = unique(data()$source)[!is.na(unique(data()$source))],
                options = list('actions-box' = T)
    )
  })

  output$tree <- renderEcharts4r({
    
    shiny::validate(need(input$cancer, 'Please select a valid cancer site'))
    shiny::validate(need(input$poc, 'Please select a valid outcome measure'))
    shiny::validate(need(input$no_pat, 'Please select a valid sample size'))
    shiny::validate(need(input$source, 'Please select a valid data source'))
    shiny::validate(need(input$country, 'Please select a valid country'))
    
    treeFUN(data(),input$cancer,input$pub_year,input$study_start,
            input$study_end,input$no_pat,input$poc,input$source,input$country,"plot")
    
  })

  output$time <- renderEcharts4r({
    
    shiny::validate(need(input$cancer, 'Please select a valid cancer site'))
    shiny::validate(need(input$poc, 'Please select a valid outcome measure'))
    shiny::validate(need(input$no_pat, 'Please select a valid sample size'))
    shiny::validate(need(input$source, 'Please select a valid data source'))
    shiny::validate(need(input$country, 'Please select a valid country'))
    
    timeFUN(data(),input$cancer,input$pub_year,input$study_start,
            input$study_end,input$no_pat,input$poc,input$source,input$country)
    
  })
  
  output$map_selectInequality <- renderUI({
    selectInput("map_select",
                "Select Inequality",
                choices = sort(unique(data()$inequality)),
                selected = sort(unique(data()$inequality))[1]
    )
  })
  
  output$map <- renderTmap({

    shiny::validate(need(input$cancer, 'Please select a valid cancer site'))
    shiny::validate(need(input$poc, 'Please select a valid outcome measure'))
    shiny::validate(need(input$no_pat, 'Please select a valid sample size'))
    shiny::validate(need(input$source, 'Please select a valid data source'))
    shiny::validate(need(input$country, 'Please select a valid country'))

    mapFUN(data(),data_geo(),input$cancer,input$pub_year,input$study_start,
           input$study_end,input$no_pat,input$poc,input$source,input$country,input$map_select)

  })
  
  output$table <- renderReactable({
    
    shiny::validate(need(input$cancer, 'Please select a valid cancer site'))
    shiny::validate(need(input$poc, 'Please select a valid outcome measure'))
    shiny::validate(need(input$no_pat, 'Please select a valid sample size'))
    shiny::validate(need(input$source, 'Please select a valid data source'))
    shiny::validate(need(input$country, 'Please select a valid country'))
    
    treeFUN(data(),input$cancer,input$pub_year,input$study_start,
            input$study_end,input$no_pat,input$poc,input$source,input$country,"table")
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("cancer_inequal_summary.csv")
    },
    content = function(file) {
      write.csv(treeFUN(data(),input$cancer,input$pub_year,input$study_start,
                        input$study_end,input$no_pat,input$poc,input$source,input$country,"download"), 
                file)
    }
  )
  
}


shinyApp(ui = ui, server = server)








