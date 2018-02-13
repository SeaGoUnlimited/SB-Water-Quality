
library(shiny)
library(tidyverse)
library(shinythemes)


#Goleta bacteria dataset
gol_bac <- read_csv("~/Desktop/github/SB-Water-Quality/gol_bac.csv")

#Filtered for two bacteria, and lagoon water/simplify
gol_b<-gol_bac %>% 
  select(StationID,SampleDate,TestMaterial,ParameterCode,Result) %>% 
  filter(TestMaterial=="Lagoon Water")
colnames(gol_b)<-c("stationid","date","testmaterial","parametercode","result")



#rainfall dataset
Goleta_water_district_rainfall <- read_csv("~/Desktop/github/SB-Water-Quality/Goleta_water_district_rainfall.csv")

#rename 
rain<-Goleta_water_district_rainfall

#combine year,month,day into new "date" column
rain$date <- as.Date(with(rain, paste(year, month, day,sep="-")), "%Y-%m-%d")






# Define UI for application that draws a histogram
ui <- fluidPage(
  shinythemes::themeSelector(),
   # Application title
   titlePanel("SB Water Quality"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
     # Sidebar panel for inputs ----
     sidebarPanel(
       
       selectInput("parametercode","Parameter Code:",choices = unique(gol_b$parametercode)),
       
       # Input: Slider for the number of bins ----
       sliderInput(inputId = "date",
                   label = "Year",
                   min = 2002,
                   max = 2017,
                   value = 2017)
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  #output$sb_map <- renderLeaflet ({
  #sb_map<-leaflet() %>% 
  # addTiles() %>% 
  #addMarkers(lng=-119.6982, lat=34.4208, popup="Zooming maps")}
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x <- gol_b$result 
      p<-input$parametercode
      
      # draw the histogram with the specified number of bins
      ggplot(subset(gol_b,year=date,parametercode==p),aes(x=date,y=result,color=stationid,alpha==0.3))+
        geom_point()+
        theme_bw()+
        ggtitle("Goleta Bacteria Data")+
        theme(plot.title = element_text(hjust = 0.5))+
        xlab("Sample Date")+
        ylab("MPN/100mL")+
        coord_cartesian(ylim = c(0, 10000))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

