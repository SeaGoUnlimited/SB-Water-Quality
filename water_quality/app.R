
library(shiny)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(shinydashboard)
library(plotly)

###############################################

#About: water quality parameters for 4 beaches, and several slough and creeks in the Goleta area. Goleta rain data was overlapped with this data to observe how rain events affect creeks, sloughs, and beaches. 

#Ideally, we'd like to pull the most recent water quality and rain data, so the app platform would update itself, and possibly make water quality predictions into the future.

#Goleta bacteria dataset
gol_bac <- read_csv("~/github/SB-Water-Quality/gol_bac.csv")


#Filtered for two bacteria, and lagoon water/simplify
gol_b<-gol_bac %>% 
  select(StationID,SampleDate,TestMaterial,ParameterCode,Result) 

colnames(gol_b)<-c("station","date","testmaterial","parametercode","result")

###################################################
#Better bacteria datasets

Goleta_bac_county <- read_csv("~/github/SB-Water-Quality/Goleta_bac_county.csv")

Hope_Ranch_bac <- read_csv("~/github/SB-Water-Quality/Hope_Ranch_bac.csv")

Sands_bac <- read_csv("~/github/SB-Water-Quality/Sands_bac.csv")

Arroyo_Burro_bac <- read_csv("~/github/SB-Water-Quality/Arroyo_Burro_bac.csv")

beach_bac<-rbind(Goleta_bac_county,Hope_Ranch_bac,Sands_bac,Arroyo_Burro_bac) %>% 
  select("Station Name","Description","SampleDate","parameter","Result","unit")

#reset column names, so they match what I called them below in the UI and server

colnames(beach_bac)<-c("stationid","beach","date","parametercode","result","unit")

beach_ecoli<-beach_bac %>% 
  filter(parametercode=="E. Coli")

beach_entero<-beach_bac %>% 
  filter(parametercode=="Enterococcus")

beach_total<-beach_bac %>% 
  filter(parametercode=="Total Coliforms")

beach_fecal<-beach_bac %>% 
  filter(parametercode=="Fecal Coliforms")

#Goleta chemical dataset

goleta_chem_sbck <- read_csv("~/github/SB-Water-Quality/goleta_chem_sbck.csv")
View(goleta_chem_sbck)

gol_c<-goleta_chem_sbck %>% 
  select(StationID,SampleDate,TestMaterial,ParameterCode,Result) 

colnames(gol_c)<-c("station","date","testmaterial","parametercode","result")


#########Chemical df's

gol_cond<-gol_c %>% 
  filter(parametercode=="Conductivity")

gol_temp<-gol_c %>% 
  filter(parametercode=="Temp")

gol_turb<-gol_c %>% 
  filter(parametercode=="TURB")

gol_ph<-gol_c %>% 
  filter(parametercode=="pH")


#combine bacteria and chem datasets
gol_b_c<-rbind(gol_b,gol_c)

gol_b_c$date<-as.Date(gol_b_c$date)



#####################################


#rainfall dataset
Goleta_water_district_rainfall <- read_csv("~/github/SB-Water-Quality/Goleta_water_district_rainfall.csv")

#rename 
rain<-Goleta_water_district_rainfall

#combine year,month,day into new "date" column
rain$date <- as.Date(with(rain, paste(year, month, day,sep="-")), "%Y-%m-%d")

##############################################






###########################################



ui <- dashboardPage(
  
  dashboardHeader(title="Beach Bacteria"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("E. Coli",tabName = "tab_1"),
      menuItem("Enterococcus",tabName = "tab_2"),
      menuItem("Total Coliforms",tabName = "tab_3"),
      menuItem("Fecal Coliforms",tabName = "tab_4")
      
      
      
    )
  ),
  
  dashboardBody(
    
    fluidRow(
      box(leafletOutput("MapPlot1"))),
    
    tabItems(
      tabItem(tabName = "tab_1",
              fluidRow(
                box(plotOutput("my_graph1",height = 500,width = 500)),
                box(title = "E. Coli:",
                    selectInput("beach","Beach:",choices = unique(beach_ecoli$beach)),
                    dateRangeInput("date", "Date range:",
                                   start = "2004-01-01",
                                   end   = "2017-04-01")
                )
              )),
      tabItem(tabName = "tab_2",
              fluidRow(
                box(plotOutput("my_graph2",height = 500,width = 500)),
                box(title = "Enterococcus:",
                    selectInput("beach_2","Beach:",choices = unique(beach_entero$beach)),
                    dateRangeInput("date2", "Date range:",
                                   start = "1998-01-01",
                                   end   = "2018-03-01")
                )
              )),
      tabItem(tabName = "tab_3",
              fluidRow(
                box(plotOutput("my_graph3",height = 500,width=500)),
                box(title = "Total Coliforms:",
                    
                    selectInput("beach_3","Beach:",choices = unique(beach_total$beach)),
                    
                    # Input: 
                    dateRangeInput("date3", "Date range:",
                                   start = "2004-01-01",
                                   end   = "2018-03-01"))
              )),
      tabItem(tabName = "tab_4",
              fluidRow(
                box(plotOutput("my_graph4",height = 500,width = 500)),
                box(title = "Fecal Coliforms:",
                    selectInput("beach_4","Beach:",choices = unique(beach_fecal$beach)),
                    dateRangeInput("date4", "Date range:",
                                   start = "2015-09-01",
                                   end   = "2018-02-28")
                )
              ))
      
      
      
    )
    
  )
  
)



server <- function(input,output){
  
  output$MapPlot1 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(-119.847250,34.408614,zoom = 13) %>% 
      addMarkers(lng = -119.826228, lat = 34.417057, popup="Goleta Slough") %>% 
      addMarkers(lng=-119.874244,lat=34.417040,popup="Devereux Slough")
  })
  
  
  output$my_graph1<-renderPlot({
    
    x <- beach_ecoli$result 
    p<-input$parametercode
    d<-input$date
    id<-input$beach
    
    ggplot(subset(beach_ecoli,beach==id ),aes(date,result,0.6))+
      geom_col(color="blue")+
      theme_bw()+
      ggtitle("Beach E. Coli levels")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("MPN/100 mL")+
      xlim(d)
    
    
    
  })
  
  output$my_graph2<-renderPlot({
    
    x <- beach_entero$result 
    p<-input$parametercode
    d<-input$date2
    id<-input$beach_2
    
    ggplot(subset(beach_entero,beach==id ),aes(date,result,0.4))+
      geom_col(color="green")+
      theme_bw()+
      ggtitle("Beach Enterococcus Levels")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("MPN/100 mL")+
      xlim(d)+
      geom_hline(yintercept = 104)  
      
    
    
  })
  
  output$my_graph3<-renderPlot({
    
    x <- beach_total$result 
    p<-input$parametercode
    d<-input$date3
    id<-input$beach_3
    
    ggplot(subset(beach_total,beach==id ),aes(date,result))+
      geom_col(color="black")+
      theme_bw()+
      ggtitle("Total Coliforms")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("MPN/100 mL")+
      xlim(d)+
      geom_hline(yintercept = 10000)  
    
    
  })
  
  output$my_graph4<-renderPlot({
    
    x <- beach_total$result 
    p<-input$parametercode
    d<-input$date4
    id<-input$beach_4
    
    ggplot(subset(beach_fecal,beach==id ),aes(date,result))+
      geom_col(color="brown",alpha=0.7)+
      theme_bw()+
      ggtitle("Fecal Coliforms")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("MPN/100 mL")+
      xlim(d)+
      geom_hline(yintercept = 400)  
    
    
  })
  
  
} 

shinyApp(ui = ui, server = server)

