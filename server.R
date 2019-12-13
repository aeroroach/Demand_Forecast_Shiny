#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

start_date <- sort(dt$START_DATE)[1]
end_date <- sort(dt$END_DATE, decreasing = T)[1]

shinyServer(function(input, output) {

# Description of filter menu -----------------------------------------------------------

  fil_input <- reactive({
    
    menu_fil %>% 
      filter(Group == input$fil_sel)
    
  })
  
  output$desc <- renderText({
    
    tmp_txt <- fil_input()
    tmp_txt <- tmp_txt$Desc
    tmp_txt
    
  })
  

# Minimum Forecast filtering ----------------------------------------------

  min_fil <- reactive({
    
    dt %>% 
      filter(FORECAST_SALE_AMT > input$fil_min)
    
  })

# Date Filtering ----------------------------------------------------------

  date_fil <- reactive({

    range_fil <- input$date_fil
    
    min_fil() %>%
      filter(REQ_DATE >= range_fil[1], REQ_DATE <= range_fil[2])

  })
  
# Filtering base on accuracy case -----------------------------------------

  user_input <- reactive({
    
    if(input$fil_sel == "All data") {
      
      date_fil()
      
    } else if(input$fil_sel == "Proper forecast") {
      
      date_fil() %>% 
        filter(accuracy == 100)
      
    } else if(input$fil_sel == "Under forecast") {
      
      date_fil() %>% 
        filter(accuracy > 100) %>% 
        arrange(desc(accuracy))
      
    } else if(input$fil_sel == "Over forecast") {
      
      date_fil() %>% 
        filter(accuracy < 100) %>% 
        arrange(desc(accuracy))
      
    } 
    
  })

# Model filtering ---------------------------------------------------------

  model_input <- reactive({
    
    user_input() %>% 
      filter(PRODUCT_NAME %in% input$model_fil)
    
  })  
  
# Plot histogram ----------------------------------------------------------

  output$his_acc <- renderPlot({
    
    model_input() %>% 
      ggplot(aes(x = accuracy)) + 
      geom_histogram(binwidth = 25, color = "white", fill = "coral2", alpha = 0.7) + 
      ylab("HS forecast records") +
      xlab("Distribution of accuracy (%)") +
      theme_minimal() +
      theme(text = element_text(size = 15)) +
      scale_x_continuous(limits = c(0,300))
    
  })
  
  output$his_res <- renderPlot({
    model_input() %>% 
      ggplot(aes(x = SKU_error)) + 
      geom_histogram(binwidth = 1, color = "white", fill = "coral2", alpha = 0.7) + 
      ylab("HS forecast records") +
      xlab("Distribution of Forecast error (SKU)") +
      theme_minimal() +
      theme(text = element_text(size = 15)) +
      scale_x_continuous(limits = c(-10,10))
  })

# Data table output -------------------------------------------------------

  output$details <- DT::renderDataTable({
    
    model_input() %>% 
      select(-STOCK_ON_HAND_AMT, -START_DATE, -END_DATE) %>% 
      DT::datatable(colnames = c("Location", "Model", "Forecast", "Actual", "Accuracy", "Error", "Mat Code", "Req. Date"))
    
  })

# Value Box ---------------------------------------------------------------
  
  output$period <- renderValueBox({
    
    valueBox(
      tags$p(paste(start_date, "to", end_date), style = "font-size: 40%;"), 
      "Sales Period Coverage",
      color = "green", icon = icon("calendar")
    )
  })
  
  output$no_record <- renderValueBox({
    
    tmp <- user_input()
    no_rec <- nrow(tmp)
    
    valueBox(
      format(no_rec, big.mark = ","), 
      "No. of Records",
      color = "green", icon = icon("bar-chart-o")
    )
  })  
  
  output$Mean_acc <- renderValueBox({
    
    tmp <- user_input()
    M_acc <- mean(tmp$accuracy)
    
    valueBox(
      round(M_acc, digits = 3), 
      "Mean Accuracy",
      color = "green", icon = icon("check-circle")
    )
  })
  
# Download button ---------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste0("Df_accuracy_", input$fil_sel, ".csv")
    },
    
    content = function(file) {
      write_csv(user_input(), file)
      
    }
    
  )
  
  
})