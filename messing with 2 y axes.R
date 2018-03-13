
library(shiny)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(shinydashboard)
library(plotly)
library(dplyr)


Goleta_bac_county <- read_csv("Goleta_bac_county.csv")

Hope_Ranch_bac <- read_csv("Hope_Ranch_bac.csv")

Sands_bac <- read_csv("Sands_bac.csv")

Arroyo_Burro_bac <- read_csv("Arroyo_Burro_bac.csv")


#Combine into one dataframe
beach_bac<-rbind(Goleta_bac_county,Hope_Ranch_bac,Sands_bac,Arroyo_Burro_bac) %>% 
  select("Station Name","Description","SampleDate","parameter","Result","unit")

#reset column names, so they match what I called them below in the UI and server

colnames(beach_bac)<-c("stationid","beach","date","parametercode","result","unit")

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



g<-ggplot(beach_ecoli,aes(date))

  g<-g+geom_line(aes(y=result, color=beach))+theme_classic()

  g<-g+geom_point(aes(y=dailyrain*5000,color="rain"))

  g<-g + scale_y_continuous(sec.axis = sec_axis(~.*.0002, name = "Daily Rain (inches)")) 
  
  g

  pg<-ggplotly(g)  
  
  pg
  