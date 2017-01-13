#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(RODBC)
library(dplyr)
library(plotly)
library(shiny)
library("magrittr")
library("shinydashboard")
library("DT")
library("readr")
#Open connection to SQL Server using system DSN
cn <- odbcConnect("RDSN",uid="Coho_Admin",pwd = "R0ppP1&ddzos")
#Get tagged fish from the current year using pre-written SQL View

EFishingFish <- sqlQuery(cn,"SELECT IndividualID, Survey, Year, Species, PITNumber, ForkLength, Weight, Comments, ReachName, Agency FROM dbo.tblDataIndividuals WHERE (Species = N'coho salmon') AND (Year > 2014) OR (Species = N'steelhead') AND (Year > 2014)")
EFishingFish[EFishingFish == -9999] = NA
EFishingFish<-mutate_each(EFishingFish,funs(toupper))
speciesList<-c("COHO SALMON", "STEELHEAD")
surveyList<-c("EF", "DSMT")
yearList<-c(2014,2015,2016)
EFishingFish<- transform(EFishingFish, ForkLength = as.numeric(ForkLength))
EFishingFish<- transform(EFishingFish, Weight = as.numeric(Weight))
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "QAQC Dashboard"),
  
  # Sidebar with a slider input for number of bins 
  dashboardSidebar(
      selectInput("SpeciesSelected", "Species:", speciesList, selected = "COHO SALMON"),
      selectInput("YearSelected", "Year:", yearList, selected = 2016),
      selectInput("SurveySelected", "Survey", surveyList, selected = "EF" ),
      radioButtons("AgencySelected", "Select Agency",c( "UCCE","SCWA"))
    ),
    
    # Show a plot of the generated distribution
    dashboardBody(
      plotlyOutput("lengthWeightGraph"),
      verbatimTextOutput("boxselect"),
      verbatimTextOutput("missingmeasurements")
    )
  )


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$lengthWeightGraph<-renderPlotly({
    fishGraph<- EFishingFish%>%filter(Species == input$SpeciesSelected)%>%filter(Year==input$YearSelected)%>%filter(Agency==input$AgencySelected)%>%filter(Survey==input$SurveySelected)%>% ggplot(aes(x = Weight, y = ForkLength, label= PITNumber, key = IndividualID)) + geom_point()
    ggplotly(fishGraph)
  })
  
  output$boxselect<-renderPrint({
    eventdata<-event_data("plotly_selected")
    if(is.null(eventdata)==T)return(NULL)
    mergedData<- merge(eventdata, EFishingFish, by.x = "key",by.y = "IndividualID" )
    mergedData
  }
  
    )
  output$missingmeasurements<- renderPrint({
      missingfish<-EFishingFish%>%filter(Species == input$SpeciesSelected)%>%filter(Year==input$YearSelected)%>%filter(Agency==input$AgencySelected)%>%filter(Survey==input$SurveySelected)%>%filter(is.na(Weight)|is.na(ForkLength))
      missingfish
  })  
  
  # output$DataTableSelectedPoints<-renderDataTable({
  #   event.data<-event_data("plotly_selected")
  #   if(is.null(event.data)==T)return(NULL)
  #   DT::datatable(event.data, colnames = c("Survey", "Year", "Species", "PITNumber", "ForkLength", "Weight", "Comments", "ReachName", "Agency"), rownames = FALSE, options = list(order = list(list(0, 'desc')), autoWidth = FALSE))
  #   
  #   })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

