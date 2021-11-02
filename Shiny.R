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
    fluidRow(
      column(1,
             wellPanel(
               plotOutput("plot"),
             ),
             width = 10),
    )
  ),
)

server <- function(input,output, session){
  
  output$Second_Selection <- renderUI(
    selectInput(
      inputId = "Pitch_Type",
      label = "Please enter the type of pitch",
      choices = unique(detroit_data$PitchType[detroit_data$PitcherId == 
                                                input$Pitcher_ID])
    )
  )
  
  PId <- reactive({input$Pitcher_ID})
  PType <- reactive({input$Pitch_Type})
  
  detroit_data_i <- reactive({
    # filter the data by pitcher Id and pitch type, then subset only the rows 
    # where the pith call was strike, strikeout or swinging strike, then find 
    # the mean of each of the variables in columns from 22 to 67.
    detroit_data %>% 
      filter(PitcherId == PId() & PitchType == PType()) %>% 
      subset(PitchCall %in% c("called_strike", "strikeout", 
                              "swinging_strike")) %>% 
      select(c(22:67)) %>% 
      summarise(across(everything(), mean))
    })
  detroit_data_ii <- reactive({
    # detroit_data_ii is equal to detroit_data_i but filtering in all throws
    # except strike, strikeout or swinging_strike
    detroit_data %>% 
      filter(PitcherId == PId() & PitchType == PType()) %>% 
      subset(!(PitchCall %in% c("called_strike", "strikeout", 
                                "swinging_strike"))) %>% 
      select(c(22:67)) %>% 
      summarise(across(everything(), mean))
  })
  detroit_data_iii <- reactive({
    # convert to vector
    as.numeric(as.vector(detroit_data_i()[1,]))
  })
  detroit_data_iv <- reactive({
    # convert to vector
    as.numeric(as.vector(detroit_data_ii()[1,]))
  })
  vector_diff <- reactive({
    # save in a new vector the difference of the means between the strike
    # pitches vs the means of the non-strike pitches
    detroit_data_iii() - detroit_data_iv()
  })
  each_vector_max <- reactive({
    # save in a new vector the maximum value of each pair value of vectors
    # detroit_data_iii and detroit_data_iv
    pmax(detroit_data_iii(), detroit_data_iv())
  })
  max_perc_diff <- reactive({
    # find the maximum percentage among all differences
    max((abs(vector_diff()) / abs(each_vector_max())) * 100)
  })
  colnumber_maxim <- reactive({
    # find the column number of the maximum difference in percent
    which.max(max_perc_diff())
  })
  colname_maxim <- reactive({
    # find the column name of the maximum difference in percent
    colnames(detroit_data_ii()[colnumber_maxim()])
  })
  
  output$value <- renderText({
    
    paste("The pitcher", PId(), "had a difference of",
          colnumber_maxim(), "percent in", colname_maxim(), "comparing the times
          the throws of", PType(), "were strike vs when was a ball,
          hit or foul")
  })
  
  output$plot <- renderPlot({
    plot(detroit_data_iii(),detroit_data_iv(), col="blue", pch=19, cex=0.75)
    text(detroit_data_iii(),detroit_data_iv(),
         labels = colnames(detroit_data[c(22:67)]),
         cex = 0.6, pos = 4, col = "red")
  })
  
}

shinyApp(ui = ui, server = server)
