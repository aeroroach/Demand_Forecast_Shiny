# Define global variables for shiny app

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(DT)

menu_fil <- read_csv("filter_list.csv")
dt <- read_csv("input/report_check_sinc1NOV19_add_3_4_update26DEC.csv")

dt %>% 
  filter(FORECAST_SALE_AMT > 0 , STOCK_ON_HAND_AMT > 0, SALE_AMT > 0, SALE_AMT_4DAY_AFTER > 0) %>% 
  mutate(REQ_DATE = dmy(REQ_DATE),
         START_DATE = dmy(START_DATE), 
         END_DATE = dmy(END_DATE), 
         SALE_AMT_4DAY_AFTER = ifelse(is.na(SALE_AMT_4DAY_AFTER),0,SALE_AMT_4DAY_AFTER), 
         SALE_AMT = ifelse(is.na(SALE_AMT),0,SALE_AMT), 
       LOCATION_CODE = as.character(LOCATION_CODE))  -> dt

dt %>% 
  mutate(prop_error = round((FORECAST_SALE_AMT - SALE_AMT)/FORECAST_SALE_AMT*100, digits = 3),
         SKU_error = FORECAST_SALE_AMT - SALE_AMT) %>% 
  select(LOCATION_CODE, PRODUCT_NAME, FORECAST_SALE_AMT, SALE_AMT, 
         prop_error, SKU_error,
         MAT_CODE, REQ_DATE, START_DATE, END_DATE) -> dt_full

dt %>% 
  mutate(prop_error = round((FORECAST_SALE_AMT - SALE_AMT_4DAY_AFTER)/FORECAST_SALE_AMT*100, digits = 3),
         SKU_error = FORECAST_SALE_AMT - SALE_AMT_4DAY_AFTER) %>% 
  select(LOCATION_CODE, PRODUCT_NAME, FORECAST_SALE_AMT, SALE_AMT_4DAY_AFTER, 
         prop_error, SKU_error,
         MAT_CODE, REQ_DATE, START_DATE, END_DATE) -> dt_buff
