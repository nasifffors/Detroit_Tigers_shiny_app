# Load packages
x <- c("shiny","tidyr","dplyr")
lapply(x, require, character.only = TRUE)
  
# Load data
detroit_data <- read.csv("AnalyticsQuestionnairePitchData.csv")

ui <- fluidPage(
  # Input()
  fluidRow(
    column(1,
           wellPanel(
             selectInput(inputId = 'Pitcher_ID',
                         label = "Please select the ID of the pitcher",
                         choices = unique(detroit_data$PitcherId),
                         )
           ), 
           width = 5),
    column(2,
           wellPanel(
            uiOutput("Second_Selection"),
           ),
           width = 5,
           offset = 1),
    
  # Output()
  fluidRow(
    column(1,
           wellPanel(
             textOutput("value"),
           ),
           width = 10),
  ),
  ),
)

server <- function(input,output, session){
  
  output$Second_Selection <- renderUI(
    selectInput(
      inputId = "Pitch_Type",
      label = "Please enter the type of pitch",
      choices = unique(detroit_data[detroit_data$PitcherId == input$Pitcher_ID,
                       "PitchType"])
    )
  )
  
  output$value <- reactive({
    
    # filter the data by pitcher Id and pitch type, then subset only the rows where
    # the pith call was strike, strikeout or swinging strike, then find the mean of
    # each of the variables in columns from 22 to 67.
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
    
    # convert to vector
    detroit_data_iii <- as.numeric(as.vector(detroit_data_i[1,]))
    
    detroit_data_iv <- as.numeric(as.vector(detroit_data_ii[1,]))
    
    # save in a new vector the difference of the means between the strike pitches vs
    # the means of the non-strike pitches
    vector_diff <- detroit_data_iii - detroit_data_iv
    
    # save in a new vector the maximum value of each pair value of vectors
    # detroit_data_iii and detroit_data_iv
    each_vector_max <- pmax(detroit_data_iii, detroit_data_iv)
    
    # find the maximum percentage among all differences
    max_perc_diff <- max((abs(vector_diff) / abs(each_vector_max)) * 100)
    
    # find the column number of the maximum difference in percent
    colnumber_maxim <- which.max(max_perc_diff)
    
    # find the column name of the maximum difference in percent
    colname_maxim <- colnames(detroit_data_ii[colnumber_maxim])
    
    paste("The pitcher", input$Pitcher_ID, "had a difference of",
          colnumber_maxim, "percent in", colname_maxim, "comparing the times
          the throws of", input$Pitch_Type, "were strike vs when was a ball,
          hit or foul")
  })

}

shinyApp(ui = ui, server = server)
