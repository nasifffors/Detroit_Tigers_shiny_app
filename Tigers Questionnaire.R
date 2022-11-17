# Load packages
x <- c("shiny","tidyr","dplyr")
lapply(x, require, character.only = TRUE)

# Load data
detroit_data <- read.csv("AnalyticsQuestionnairePitchData.csv")

ui <- fluidPage(
  
  titlePanel("Detroit Tigers pitcher's analysis"),
  
  # Input()
  fluidRow(
    
    column("Pitcher ID",
           wellPanel(
             selectInput(inputId = 'Pitcher_ID',
                         label = "Please select the ID of the pitcher",
                         choices = unique(detroit_data$PitcherId),
             )
           ), 
           width = 5),
    
    column("Pitch selection",
           wellPanel(
             uiOutput("Second_Selection"),
           ),
           width = 5,
           offset = 1),
    
    # Output()
    fluidRow(
      column("Conclusion",
             wellPanel(
               textOutput("value"),
             ),
             width = 10,
             margin = 1),
    ),
    fluidRow(
      column("Plot",
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
      choices = unique(detroit_data$PitchType[detroit_data$PitcherId 
                                              == input$Pitcher_ID]),
    )
  )

  PId <- reactive({input$Pitcher_ID})
  PType <- reactive({input$Pitch_Type})
  
  # filter the data by pitcher Id and pitch type, then subset only the rows 
  # where the pith call was strike, strikeout or swinging strike, then find 
  # the mean of each of the variables in columns from 22 to 63.
  detroit_data_i <- reactive({
    detroit_data %>%
      na.omit() %>%
      dplyr::filter(PitcherId == PId() & PitchType == PType()) %>% 
      subset(PitchCall %in% c("called_strike", "strikeout", 
                              "swinging_strike")) %>%
    select(c(22:63)) %>% 
    summarise(across(everything(), mean))
  })
    
  # detroit_data_ii is equal to detroit_data_i but filtering in all throws
  # except strike, strikeout or swinging_strike
  detroit_data_ii <- reactive({
    detroit_data %>%
      na.omit() %>%
      filter(PitcherId == PId() & PitchType == PType()) %>% 
      subset(!(PitchCall %in% c("called_strike", "strikeout", 
                                "swinging_strike"))) %>% 
      select(c(22:63)) %>% 
      summarise(across(everything(), mean))
    })
  
  # convert to vector
  detroit_data_iii <- reactive ({
    as.numeric(as.vector(detroit_data_i()[1,]))
  })
  
  # convert to vector
  detroit_data_iv <- reactive({
    as.numeric(as.vector(detroit_data_ii()[1,]))
  })
  
  # save in a new vector the difference of the means between the strike
  # pitches vs the means of the non-strike pitches
  vector_diff <- reactive({
    detroit_data_iii() - detroit_data_iv()
  })
  
  # save in a new vector the maximum value of each pair value of vectors
  # detroit_data_iii and detroit_data_iv
  each_vector_max <- reactive({
    pmax(detroit_data_iii(), detroit_data_iv())
    })
  
  # find the maximum percentage among all differences
  perc_diff <- reactive({
    abs(vector_diff()) / abs(each_vector_max()) * 100
  })
  
  # find the column number of the maximum difference in percent
  colnumber_maxim <- reactive({
    which.max(perc_diff())
  })
  
  # find the column name of the maximum difference in percent
  colname_maxim <- reactive({
    colnames(detroit_data_ii()[colnumber_maxim()])
  })
  
  conclusion <- reactive({
    paste("The pitcher", PId(), "had a difference of",
          colnumber_maxim(), "percent in", colname_maxim(), "comparing the times
          the throws of", PType(), "were strike versus when was a ball,
          hit or foul")
  })
  
  output$value <- renderText({
    conclusion()
  })
  
  output$plot <- renderPlot({
    plot(perc_diff(), vector_diff(), col="blue", pch=19, cex=0.75,
         xlab = "relative value (%)", ylab = "absolute value")
    text(perc_diff(), vector_diff(),
         labels = colnames(detroit_data[c(22:63)]),
         cex = 0.6, pos = 4, col = "red")
  })
  
}

#Run the app
shinyApp(ui = ui, server = server)