
library(shiny)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(shinydashboard)
library(dplyr)

###############################################
#About: water quality parameters for 4 beaches in the Santa Barbara area. Goleta rain data was overlapped with this data to observe how rain events affect beaches. 

#Ideally, we'd like to pull the most recent water quality and rain data, so the app platform would update itself, and possibly make water quality predictions into the future.

###################################################
#Better bacteria datasets

Goleta_bac_county <- read_csv("Goleta_bac_county.csv")

Hope_Ranch_bac <- read_csv("Hope_Ranch_bac.csv")

Sands_bac <- read_csv("Sands_bac.csv")

Arroyo_Burro_bac <- read_csv("Arroyo_Burro_bac.csv")


#Combine into one dataframe
beach_bac<-rbind(Goleta_bac_county,Hope_Ranch_bac,Sands_bac,Arroyo_Burro_bac) %>% 
  select("Station Name","Description","SampleDate","parameter","Result","unit")

#reset column names, so they match what I called them below in the UI and server

colnames(beach_bac)<-c("stationid","beach","date","parametercode","result","unit")

beach_bac$log<-(log10(beach_bac$result))

beach_bac$date<-as.Date(beach_bac$date)

##ADD RAIN DATASET AND COMBINE WITH BACTERIA DF

#rainfall dataset
Goleta_water_district_rainfall <- read_csv("Goleta_water_district_rainfall.csv")

#rename 
rain<-Goleta_water_district_rainfall 


#combine year,month,day into new "date" column
rain$date <- as.Date(with(rain, paste(year, month, day,sep="-")), "%Y-%m-%d")

# Simplify rain dataframe
r1<-rain %>% 
  select("date","dailyrain")
r1$date<-as.Date(r1$date)

#combine bacteria data frame and rain dataframe
b_r<-merge(beach_bac,r1,by="date", all=TRUE)

#b_r[is.na(b_r)]<-0

beach_ecoli<-b_r %>% 
  filter(parametercode=="E. Coli")

beach_entero<-b_r %>% 
  filter(parametercode=="Enterococcus")

beach_total<-b_r %>% 
  filter(parametercode=="Total Coliforms")

beach_fecal<-b_r %>% 
  filter(parametercode=="Fecal Coliforms")

#Divide beach bac into seprate bacteria parameters



###########################################



ui <- dashboardPage(
  
  dashboardHeader(title="Goleta Beaches: Bacteria Levels", titleWidth = 450),
  
  dashboardSidebar(
    width = 120,
    sidebarMenu(
      
      menuItem("E. Coli",tabName = "tab_1"),
      menuItem("Enterococcus",tabName = "tab_2"),
      menuItem("Total Coliforms",tabName = "tab_3"),
      menuItem("Fecal Coliforms",tabName = "tab_4")
      
      
      
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              '))),
    
    tabItems(
      tabItem(tabName = "tab_1",
              fluidRow(
                box(plotOutput("my_graph1",height = 300,width = 500), width = 8, background="blue",align="center",
                    plotOutput("rain_graph",height = 300,width = 500)),
                box(title = "E. Coli: No Defined Health Limit",
                    background = "blue",height  = NULL,
                    selectInput("beach","Beach:",choices = unique(beach_ecoli$beach)),
                    dateRangeInput("date", "Date range:", start = "2004-01-01", end   = "2017-04-01"),width = 4
                ),
                box(title = "Beach Map", 
                    leafletOutput("BeachMap1",height = 250,width = 225),width = 4,align="center")
              
              )),
      tabItem(tabName = "tab_2",
              fluidRow(
                box(plotOutput("my_graph2",height = 300,width = 500),width = 8,background="blue",align="center",
                    plotOutput("rain_graph2",height = 300,width = 500)),
                box(title = "Enterococcus: Health Standard: log (104 MPN/Liter)=2.017",
                    background = "blue",
                    selectInput("beach_2","Beach:",choices = unique(beach_entero$beach)),
                    dateRangeInput("date2", "Date range:", start = "1998-01-01", end   = "2018-03-01"),width=4
                ),
                box(title = "Beach Map", 
                    leafletOutput("BeachMap2",height = 250,width = 225),width = 4,align="center")
              )),
      tabItem(tabName = "tab_3",
              fluidRow(
                box(plotOutput("my_graph3",height = 300,width=500), width = 8,background="blue",align="center",
                    plotOutput("rain_graph3",height = 300,width = 500)),
                box(title = "Total Coliforms: Health Standard: log (10,000 MPN/Liter)=4 ",background = "blue",
                    selectInput("beach_3","Beach:",choices = unique(beach_total$beach)),
                    
                    # Input: 
                    dateRangeInput("date3", "Date range:",
                                   start = "2004-01-01",
                                   end   = "2018-03-01"),width = 4
                    ),
                box(title = "Beach Map", 
                    leafletOutput("BeachMap3",height = 250,width = 225),width = 4,align="center")
              )),
      tabItem(tabName = "tab_4",
              fluidRow(
                box(plotOutput("my_graph4",height = 300,width = 500), width = 8,background="blue",align="center",
                    plotOutput("rain_graph4",height = 300,width = 500)),
                box(title = "Fecal Coliforms: Health Standard: log(400 MPN/Liter)=2.60",background = "blue",
                    selectInput("beach_4","Beach:",choices = unique(beach_fecal$beach)),
                    dateRangeInput("date4", "Date range:",
                                   start = "2015-09-01",
                                   end   = "2018-02-28"),width=4
                ),
                box(title = "Beach Map", 
                    leafletOutput("BeachMap4",height = 250,width = 225),width = 4,align="center")
              ))
      
      
      
    )
    
  )
  
)



server <- function(input,output){
  
  
  
  output$my_graph1<-renderPlot({
    
    
    d<-input$date
    id<-input$beach
    
    
    
    
    
      ggplot(subset(beach_ecoli,beach==id ),aes(date,log,0.6))+
      geom_col(color="red")+
      theme_bw()+
      ggtitle("Beach E. Coli levels")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("log (MPN/100 mL)")+
      xlim(d)
    
   
    
  })
  
  output$BeachMap1 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(-119.81,34.408614,zoom = 11) %>% 
      addMarkers(lng = -119.8322, lat = 34.4168, popup="Goleta Beach") %>% 
      addMarkers(lng=-119.879890,lat= 34.408489,popup="Sands Beach") %>% 
      addMarkers(lng=-119.780014,lat= 34.414687,popup="Hope Ranch Beach") %>% 
      addMarkers(lng=-119.743515,lat= 34.403105,popup="Arroyo Burro Beach")
  })
  
  
  output$my_graph2<-renderPlot({
    
   
    d<-input$date2
    id<-input$beach_2
    
    ggplot(subset(beach_entero,beach==id ),aes(date,log))+
      geom_col(color="orange")+
      theme_bw()+
      ggtitle("Beach Enterococcus Levels")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("log (MPN/100 mL)")+
      xlim(d)+
      geom_hline(yintercept = 2.017033)
    
    
  })
  
  output$BeachMap2 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(-119.81,34.408614,zoom = 11) %>% 
      addMarkers(lng = -119.8322, lat = 34.4168, popup="Goleta Beach") %>% 
      addMarkers(lng=-119.879890,lat= 34.408489,popup="Sands Beach") %>% 
      addMarkers(lng=-119.780014,lat= 34.414687,popup="Hope Ranch Beach") %>% 
      addMarkers(lng=-119.743515,lat= 34.403105,popup="Arroyo Burro Beach")
  })
  
  output$my_graph3<-renderPlot({
    
    
    d<-input$date3
    id<-input$beach_3
    
    ggplot(subset(beach_total,beach==id),aes(date,log))+
      geom_col(color="black")+
      theme_bw()+
      ggtitle("Total Coliforms")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("log (MPN/100 mL)")+
      xlim(d)+
      geom_hline(yintercept = 4)  
    
    
  })
  
  output$BeachMap3 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(-119.81,34.408614,zoom = 11) %>% 
      addMarkers(lng = -119.8322, lat = 34.4168, popup="Goleta Beach") %>% 
      addMarkers(lng=-119.879890,lat= 34.408489,popup="Sands Beach") %>% 
      addMarkers(lng=-119.780014,lat= 34.414687,popup="Hope Ranch Beach") %>% 
      addMarkers(lng=-119.743515,lat= 34.403105,popup="Arroyo Burro Beach")
  })
  
  output$my_graph4<-renderPlot({
    
    
    d<-input$date4
    id<-input$beach_4
    
    ggplot(subset(beach_fecal,beach==id ),aes(date,log))+
      geom_col(color="brown",alpha=0.7)+
      theme_bw()+
      ggtitle("Fecal Coliforms")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("log (MPN/100 mL)")+
      xlim(d)+
      geom_hline(yintercept = 2.6020599)  
    
    
  })
  
  output$BeachMap4 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(-119.81,34.408614,zoom = 11) %>% 
      addMarkers(lng = -119.8322, lat = 34.4168, popup="Goleta Beach") %>% 
      addMarkers(lng=-119.879890,lat= 34.408489,popup="Sands Beach") %>% 
      addMarkers(lng=-119.780014,lat= 34.414687,popup="Hope Ranch Beach") %>% 
      addMarkers(lng=-119.743515,lat= 34.403105,popup="Arroyo Burro Beach")
  })
  
  output$rain_graph<-renderPlot({
    
    
    d<-input$date
    id<-input$beach
    
    ggplot(r1,aes(date,dailyrain,0.6))+
      geom_col(color="blue")+
      theme_bw()+
      ggtitle("Rainfall")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("Rain (inches)")+
      xlim(d)
  
  })
  
  output$rain_graph2<-renderPlot({
    
    
    d<-input$date2
    id<-input$beach
    
    ggplot(r1,aes(date,dailyrain,0.6))+
      geom_col(color="blue")+
      theme_bw()+
      ggtitle("Rainfall")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("Rain (inches)")+
      xlim(d)
    
  })
  
  output$rain_graph3<-renderPlot({
    
    
    d<-input$date3
    
    
    ggplot(r1,aes(date,dailyrain,0.6))+
      geom_col(color="blue")+
      theme_bw()+
      ggtitle("Rainfall")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("Rain (inches)")+
      xlim(d)
    
  })
  
  output$rain_graph4<-renderPlot({
    
   
    d<-input$date4
    
    ggplot(r1,aes(date,dailyrain,0.6))+
      geom_col(color="blue")+
      theme_bw()+
      ggtitle("Rainfall")+
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Sample Date")+
      ylab("Rain (inches)")+
      xlim(d)+
     expand_limits(y=0)
    
  })

}

shinyApp(ui = ui, server = server)

