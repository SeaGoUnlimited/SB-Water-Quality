
library(shiny)
library(tidyverse)



#Goleta bacteria dataset
gol_bac <- read_csv("~/Desktop/github/SB-Water-Quality/gol_bac.csv")

#Filtered for two bacteria, and lagoon water/simplify
gol_b<-gol_bac %>% 
  select(StationID,SampleDate,TestMaterial,ParameterCode,Result) %>% 
  filter(ParameterCode=="E. Coli"|ParameterCode=="Enterococcus") %>% 
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
   
   # Application title
   titlePanel("SB Water Quality"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("date",
                     "Date",
                     min = 2009,
                     max = 2017,
                     value = 2013)
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x <- gol_b$result 
      year<-input$date
      
      # draw the histogram with the specified number of bins
      ggplot(subset(gol_b,date==date),aes(x=date,y=result,color=stationid,size=0.1))+
        geom_point()+
        theme_bw()+
        coord_cartesian(ylim = c(0, 6000))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

