# Define global variables for shiny app

library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(DT)

menu_fil <- read_csv("filter_list.csv")
dt <- read_csv("input/report_check_sinc1OCT19.csv")

dt %>% 
  mutate(SALE_AMT = ifelse(is.na(SALE_AMT),0,SALE_AMT), 
         accuracy = round(SALE_AMT/FORECAST_SALE_AMT, digits = 3), 
         REQ_DATE = dmy(REQ_DATE),
         START_DATE = dmy(START_DATE), 
         END_DATE = dmy(END_DATE)) %>% 
  mutate(accuracy = ifelse(FORECAST_SALE_AMT == 0 & SALE_AMT == 0, 1, accuracy)) %>% 
  mutate(accuracy = ifelse(FORECAST_SALE_AMT == 0, round((SALE_AMT+0.5)/(FORECAST_SALE_AMT+0.5), digits = 3), accuracy)) %>% 
  select(LOCATION_CODE, PRODUCT_NAME, FORECAST_SALE_AMT, STOCK_ON_HAND_AMT, SALE_AMT, accuracy,
         MAT_CODE, REQ_DATE, START_DATE, END_DATE) -> dt