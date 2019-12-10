# Define global variables for shiny app

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(DT)

menu_fil <- read_csv("filter_list.csv")
dt <- read_csv("input/report_check_sinc1OCT19.csv")

dt %>% 
  mutate(SALE_AMT = ifelse(is.na(SALE_AMT),0,SALE_AMT), 
         accuracy = round(SALE_AMT/FORECAST_SALE_AMT*100, digits = 3),
         SKU_error = FORECAST_SALE_AMT - SALE_AMT,
         REQ_DATE = dmy(REQ_DATE),
         START_DATE = dmy(START_DATE), 
         END_DATE = dmy(END_DATE)) %>% 
  mutate(accuracy = ifelse(FORECAST_SALE_AMT == 0 & SALE_AMT == 0, 100, accuracy)) %>% 
  mutate(accuracy = ifelse(FORECAST_SALE_AMT == 0 & SALE_AMT != 0, 
                           round((SALE_AMT+1)/(FORECAST_SALE_AMT+1)*100, digits = 3), accuracy)) %>% 
  select(LOCATION_CODE, PRODUCT_NAME, FORECAST_SALE_AMT, STOCK_ON_HAND_AMT, SALE_AMT, 
         accuracy, SKU_error,
         MAT_CODE, REQ_DATE, START_DATE, END_DATE) -> dt
