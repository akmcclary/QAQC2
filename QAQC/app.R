

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
EFishingFish$Weight<-round(EFishingFish$Weight,2)

ui <- dashboardPage(
  dashboardHeader(title = "QAQC Dashboard"),
  

  dashboardSidebar(
      selectInput("SpeciesSelected", "Species:", speciesList, selected = "COHO SALMON"),
      selectInput("YearSelected", "Year:", yearList, selected = 2016),
      selectInput("SurveySelected", "Survey", surveyList, selected = "EF" ),
      radioButtons("AgencySelected", "Select Agency",c( "UCCE","SCWA"))
    ),
    

    dashboardBody(
      plotlyOutput("lengthWeightGraph"),
      DT::dataTableOutput("selectedFish"),
      DT::dataTableOutput("missingmeasurements")
    )
  )



server <- shinyServer(function(input, output) {
  
  output$lengthWeightGraph<-renderPlotly({
    fishGraph<- EFishingFish%>%filter(Species == input$SpeciesSelected)%>%filter(Year==input$YearSelected)%>%filter(Agency==input$AgencySelected)%>%filter(Survey==input$SurveySelected)%>% ggplot(aes(x = Weight, y = ForkLength, label= PITNumber, key = IndividualID)) + geom_point()
    ggplotly(fishGraph)
  })
  
  output$selectedFish<-renderDataTable({
    eventdata<-event_data("plotly_selected")
    if(is.null(eventdata)==T)return(NULL)
    mergedData<- merge(eventdata, EFishingFish, by.x = "key",by.y = "IndividualID" )
    mergedData<-mergedData%>%filter(x!=0)
    selectedFishTable<- mergedData[,c("ReachName", "key", "Survey", "Year", "Species","PITNumber","ForkLength", "Weight",  "Comments" )]
    DT::datatable(selectedFishTable, colnames = c("Reach", "IndividualID", "Survey", "Year", "Species","PITNumber","Fork Length", "Weight",  "Comments" ), caption = "Selected Fish", rownames = FALSE, options = list(searching = FALSE, paging= FALSE))
    
  }
  
    )
  output$missingmeasurements<- renderDataTable({
      missingfish<-EFishingFish%>%filter(Species == input$SpeciesSelected)%>%filter(Year==input$YearSelected)%>%filter(Agency==input$AgencySelected)%>%filter(Survey==input$SurveySelected)%>%filter(is.na(Weight)|is.na(ForkLength))
      fishTable<- missingfish[,c("ReachName", "IndividualID", "Survey", "Year", "Species","PITNumber","ForkLength", "Weight",  "Comments" )]
      DT::datatable(fishTable, colnames = c("Reach", "IndividualID", "Survey", "Year", "Species","PITNumber","Fork Length", "Weight",  "Comments" ), caption = "Fish with missing Fork Lengths or weights", rownames = FALSE, options = list(searching = FALSE, paging= FALSE))
     
  })  
  
  
})


shinyApp(ui = ui, server = server)

