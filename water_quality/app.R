
library(shiny)
library(tidyverse)
library(shinythemes)
library(leaflet)

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



# Define UI for application that draws a histogram
ui <- fluidPage(
  shinythemes::themeSelector(),
   # Application title
   titlePanel("SB Water Quality"),
   
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
     
     # Sidebar panel for inputs ----
     sidebarPanel(
       
       selectInput("parametercode","Parameter Code:",choices = unique(gol_b_c$parametercode)),
       
       selectInput("station","Station ID:",choices = unique(gol_b_c$stationid)),
       
       # Input: 
       dateRangeInput("date", "Date range:",
                      start = "2002-01-01",
                      end   = "2017-12-31")
       
      ),
      
      
    
      mainPanel(
         plotOutput("distPlot"),
         
         leafletOutput("MapPlot1")
      )
   )
)

# Define server logic 
server <- function(input, output) {
   
  output$MapPlot1 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(-119.847250,34.408614,zoom = 13) %>% 
      addMarkers(lng = -119.826228, lat = 34.417057, popup="Goleta Slough") %>% 
      addMarkers(lng=-119.874244,lat=34.417040,popup="Devereux Slough")
  })
  
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x <- gol_b_c$result 
      p<-input$parametercode
      d<-input$date
      id<-input$station
      
      # draw the histogram with the specified number of bins
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
   
}

# Run the application 
shinyApp(ui = ui, server = server)

