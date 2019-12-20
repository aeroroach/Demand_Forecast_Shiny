#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

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
  
  
# Choosing between full range or buffer -----------------------------------

  fil_full <- reactive({
    
    if(input$full_switch) {
      
      dt_full
      
    } else {
      dt_buff
    }
    
  })

# Minimum Forecast filtering ----------------------------------------------

  min_fil <- reactive({
    
    fil_full() %>% 
      filter(FORECAST_SALE_AMT >= input$fil_min)
    
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
        filter(prop_error >= -20, prop_error <= 20)
      
    } else if(input$fil_sel == "Under forecast") {
      
      date_fil() %>% 
        filter(prop_error < -20) %>% 
        arrange(prop_error)
      
    } else if(input$fil_sel == "Over forecast") {
      
      date_fil() %>% 
        filter(prop_error > 20) %>% 
        arrange(desc(prop_error))
      
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
      ggplot(aes(x = prop_error)) + 
      geom_histogram(binwidth = 25, color = "white", fill = "coral2", alpha = 0.7) + 
      ylab("HS forecast records") +
      xlab("Distribution of Forecast error (%)") +
      theme_minimal() +
      theme(text = element_text(size = 15)) +
      scale_x_continuous(limits = c(-200,200))
    
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
      select(-START_DATE, -END_DATE) %>% 
      DT::datatable(colnames = c("Location", "Model", "Forecast", "Actual", "% Error", "Error", "Mat Code", "Req. Date"))
    
  })

# Value Box ---------------------------------------------------------------
  
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
    M_acc <- mean(tmp$prop_error)
    
    valueBox(
      round(M_acc, digits = 3), 
      "Mean % Error",
      color = "red", icon = icon("check-circle")
    )
  })
  
  output$Mean_err <- renderValueBox({
    
    tmp <- user_input()
    M_err <- mean(tmp$SKU_error)
    
    valueBox(
      round(M_err, digits = 3), 
      "Mean SKU error",
      color = "red", icon = icon("check-circle")
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