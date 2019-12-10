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

# Description of filter criteria -----------------------------------------------------------

  fil_input <- reactive({
    
    menu_fil %>% 
      filter(Group == input$fil_sel)
    
  })
  
  output$desc <- renderText({
    
    tmp_txt <- fil_input()
    tmp_txt <- tmp_txt$Desc
    tmp_txt
    
  })


# Date Filtering ----------------------------------------------------------

  
  
# Filtering base on accuracy case -----------------------------------------

  user_input <- reactive({
    
    if(input$fil_sel == "All data") {
      
      dt
      
    } else if(input$fil_sel == "Proper forecast") {
      
      dt %>% 
        filter(accuracy == 1, STOCK_ON_HAND_AMT > 0)
      
    } else if(input$fil_sel == "Under forecast") {
      
      dt %>% 
        filter(accuracy > 1, STOCK_ON_HAND_AMT > 0) %>% 
        arrange(desc(accuracy))
      
    } else if(input$fil_sel == "Over forecast") {
      
      dt %>% 
        filter(accuracy < 1) %>% 
        arrange(desc(accuracy))
      
    } else if (input$fil_sel == "Uncertainty") {
      
      dt %>% 
        filter(accuracy == 1, STOCK_ON_HAND_AMT == 0)
      
    }
    
  })
  
# Plot histogram ----------------------------------------------------------

  output$his_acc <- renderPlot({
    
    user_input() %>% 
      ggplot(aes(x = accuracy)) + 
      geom_histogram(binwidth = 1, color = "grey35", fill = "coral2", alpha = 0.7) + 
      ylab("HS forecast records") +
      xlab("Distribution of accuracy") +
      theme_minimal() +
      theme(text = element_text(size = 15)) +
      scale_x_continuous(limits = c(0,25))
    
  })

# Data table output -------------------------------------------------------

  output$details <- DT::renderDataTable({
    
    user_input() %>% 
      select(-START_DATE, -END_DATE) %>% 
      DT::datatable()
    
  })

# Value Box ---------------------------------------------------------------
  
  output$period <- renderValueBox({
    
    valueBox(
      tags$p(paste(start_date, "to", end_date), style = "font-size: 40%;"), 
      "Sales Period",
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