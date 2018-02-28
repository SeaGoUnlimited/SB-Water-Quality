
library(shiny)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(shinydashboard)

###############################################


#Goleta bacteria dataset
gol_bac <- read_csv("~/github/SB-Water-Quality/gol_bac.csv")


#Filtered for two bacteria, and lagoon water/simplify
gol_b<-gol_bac %>% 
  select(StationID,SampleDate,TestMaterial,ParameterCode,Result) 

colnames(gol_b)<-c("stationid","date","testmaterial","parametercode","result")

#Goleta chemical dataset
goleta_chem_sbck <- read_csv("~/github/SB-Water-Quality/goleta_chem_sbck.csv")
View(goleta_chem_sbck)

gol_c<-goleta_chem_sbck %>% 
  select(StationID,SampleDate,TestMaterial,ParameterCode,Result) 

colnames(gol_c)<-c("stationid","date","testmaterial","parametercode","result")


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
  
  dashboardHeader(title="Water Quality App"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Bacteria",tabName = "tab_1"),
      menuItem("Chemical Parameters",tabName = "tab_2"),
      menuItem("Conductivity",tabName = "tab_3"),
      menuItem("Temperature",tabName = "tab_4")
      
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "tab_1",
              fluidRow(
                box(plotOutput("my_graph1",height = 700,width = 700)),
                box(title = "choose FIB:",
                    selectInput("parametercode","Parameter Code:",choices = unique(gol_b$parametercode)),
                    selectInput("Stationid","Station ID:",choices = unique(gol_b$stationid)),
                    dateRangeInput("date", "Date range:",
                                   start = "2002-01-01",
                                   end   = "2017-12-31")
                )
              )),
      tabItem(tabName = "tab_2",
              fluidRow(
                box(plotOutput("my_graph2",height = 700,width=700)),
                box(title = "Chemical Parameter:",
                    selectInput("chem","Parameter Code:",choices = unique(gol_c$parametercode),width = 300),
                    
                    selectInput("station","Station ID:",choices = unique(gol_c$stationid)),
                    
                    # Input: 
                    dateRangeInput("date2", "Date range:",
                                   start = "2002-01-01",
                                   end   = "2017-12-31"))
              )),
      tabItem(tabName = "tab_3",
              fluidRow(
                box(plotOutput("my_graph3",height = 700,width=700)),
                box(title = "conductivity",
                    
                    selectInput("stationId","Station ID:",choices = unique(gol_c$stationid)),
                    
                    # Input: 
                    dateRangeInput("date3", "Date range:",
                                   start = "2002-01-01",
                                   end   = "2017-12-31"))
              )),
      
      tabItem(tabName = "tab_4",
              fluidRow(
                box(plotOutput("my_graph4",height = 700,width=700)),
                box(title = "Temperature",
                    
                    selectInput("stationID","Station ID:",choices = unique(gol_temp$stationid)),
                    
                    # Input: 
                    dateRangeInput("date4", "Date range:",
                                   start = "2002-01-01",
                                   end   = "2017-12-31"))
              ))
      
      
      
    )
    
  )
  
)



server <- function(input,output){
  
  
  
  output$my_graph1<-renderPlot({
    
    x <- gol_b$result 
    p<-input$parametercode
    d<-input$date
    id<-input$Stationid
    
    ggplot(subset(gol_b_c,parametercode==p),aes(date,result,color=stationid,0.3))+
      geom_point()+
      theme_bw()+
      ggtitle("Goleta Bacteria Data")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("Result unit...")+
      coord_cartesian(ylim = c(0, 10000))+
      xlim(d)
    
    
  })
  output$my_graph2<-renderPlot({
    
    x <- gol_b_c$result 
    y<-input$chem
    d<-input$date2
    id<-input$stationid
    
    ggplot(subset(gol_b_c,parametercode==y),aes(date,result,color=stationid,0.3))+
      geom_point()+
      theme_bw()+
      ggtitle("Goleta Chem Data")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("Result unit...")+
      coord_cartesian(ylim = c(0, 10000))+
      xlim(d)
    
  })
  
  output$my_graph3<-renderPlot({
    
    
    d<-input$date3
    id<-input$stationId
    
    ggplot(subset(gol_cond,stationid==id),aes(date,result,0.4))+
      geom_point(color="blue")+
      theme_bw()+
      ggtitle("Goleta Chem Data")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("uS")+xlim(d)+
      xlim(d)+geom_smooth(method = "lm")
    
  })
  
  output$my_graph4<-renderPlot({
    
    
    d<-input$date4
    id<-input$stationID
    
    ggplot(subset(gol_temp,stationid==id),aes(date,result,0.4))+
      geom_point()+
      theme_bw()+
      ggtitle("Goleta Temp Data")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("Degrees Celcius")+
      xlim(d)+geom_smooth()
    
  })
  
} 

shinyApp(ui = ui, server = server)

