# Load packages
x <- c("shiny","tidyr","dplyr","ggplot2","sqldf")
lapply(x, require, character.only = TRUE)
  
# Load data
setwd("~/ALFRE/MADE BY ALFREDO NASIFF FORS/R PROJECTS/Detroit Tigers Application")
detroit_data <- read.csv("AnalyticsQuestionnairePitchData.csv")

ui <- fluidPage(
  # Input()
  fluidRow(
    column(1,
           wellPanel(
             selectInput(inputId = 'Pitcher_ID',
                         label = "Please select the ID of the pitcher",
                         choices = unique(detroit_data$PitchId))
           )),
    column(2,
           wellPanel(
            uiOutput("Pitch_Type")
           )
    )
  ),
  
  # Output()
  fluidRow(
    column(1,
           wellPanel(
             textOutput("value"),
           ))
  ),
)

server <- function(input,output){
  
  output$Pitch_Type <- renderUI ({
    selectInput(
      inputId = 'Pitch_Type',
      label = "Please select the type of pitch",
      choices = unique(detroit_data[detroit_data$PitchType == input$Pitcher_ID,
                                    "PitchType"])
    )
  })
  
  output$value <- renderText({
    
    detroit_data_i <- detroit_data %>% 
      filter(PitcherId == input$Pitcher_ID & PitchType == input$Pitch_Type) %>% 
      subset(PitchCall %in% c("called_strike", "strikeout", "swinging_strike")) %>% 
      select(c(22:67)) %>% 
      summarise(across(everything(), mean))
    
    detroit_data_ii<- detroit_data %>% 
      filter(PitcherId == input$Pitcher_ID & PitchType == input$Pitch_Type) %>% 
      subset(!(PitchCall %in% c("called_strike", "strikeout", "swinging_strike"))) %>% 
      select(c(22:67)) %>% 
      summarise(across(everything(), mean))
  
    detroit_data_iii <- as.numeric(as.vector(detroit_data_i[1,]))
    
    detroit_data_iv <- as.numeric(as.vector(detroit_data_ii[1,]))
    
    vector_diff <- detroit_data_iii - detroit_data_iv
    
    each_vector_max <- pmax(detroit_data_iii, detroit_data_iv)
    
    max_perc_diff <- max((abs(vector_diff) / abs(each_vector_max)) * 100)
    
    colnumber_maxim <- which.max(max_perc_diff)
    
    colname_maxim <- colnames(detroit_data_ii[colnumber_maxim])
    
    paste("The pitcher", input$Pitcher_ID, "had a difference of",
          colnumber_maxim, "percent in", colname_maxim, "comparing the times
          the throws of", input$Pitcher_Type, "were strike vs when was a ball,
          hit or foul")
  })

}

shinyApp(ui = ui, server = server)