
library(shiny)
library(DT)
library(tm)
library(data.table)

source("loadData.R", local = TRUE)
source("prepareData.R", local = TRUE)
source("prediction.R", local = TRUE)

print("start loading data")
dataLoaded <- FALSE
dataLoaded <- loadData()
print("done loading data")

shinyServer(
  function(input,output, session) {
    
    output$ui <- renderUI({
      if (is.null(input$input_type))
        return()
      
      switch(input$input_type,
             "text" = textInput("dynamic", "Dynamic",
                                value = INITIAL_STR),
             "numeric" =  numericInput("dynamic", "Dynamic",
                                       value = 12),
             "selectInput" = selectInput("dynamic", "Dynamic",
                                         choices = c("Option 1" = "option1",
                                                     "Option 2" = "option2"),
                                         selected = "option2"
             ),
             "selectInput (multi)" = selectInput("dynamic", "Dynamic",
                                                 choices = c("Option 1" = "option1",
                                                             "Option 2" = "option2"),
                                                 selected = c("option1", "option2"),
                                                 multiple = TRUE
             )
      )
    })
    
    output$input_type_text <- renderText({
      input$input_type
    })
    
    output$dynamic_value <- renderPrint({
      str(input$dynamic)
    })
    
    #dynamic buttons -> Update the button labels with the predications
    output$w1 <- renderUI({
      if ({length(predictions())} < 1) {return()}
      if (is.na(predictions()[1])) {return()} 
      pWord1 <- {predictions()[1]}; 
      if (pWord1 == "i") {pWord1 <- "I"}
      actionButton("actionW1", label = pWord1)
    })

    output$w2 <- renderUI({
      if (length(predictions()) < 2) {return()}
      if (is.na(predictions()[2])) {return()} 
      pWord2 <- {predictions()[2]}; if (pWord2 == "i") {pWord2 <- "I"}
      actionButton("actionW2", label = pWord2)
    })
    
    output$w3 <- renderUI({
      if (length(predictions()) < 3) {return()}
      if (is.na(predictions()[3])) {return()}      
      pWord3 <- {predictions()[3]}; if (pWord3 == "i") {pWord3 <- "I"}
      actionButton("actionW3", label = pWord3)
    })
    
    output$w4 <- renderUI({
      if (length(predictions()) < 4) {return()}
      if (is.na(predictions()[4])) {return()} 
      pWord4 <- {predictions()[4]}; if (pWord4 == "i") {pWord4 <- "I"}
      actionButton("actionw4", label = pWord4)
    })
    
    #button click events -> update the input phrase with the selected word
    observeEvent(input$actionW1, {
      if (length(predictions()) < 1) {return()}
      pWord1 <- {predictions()[1]}; 
      if (pWord1 == "i") {pWord1 <- "I"}
      updateTextInput(session, "Phrase", NULL, paste0(input$Phrase, paste0(pWord1, " ")))
    })
    
    observeEvent(input$actionW2, {
      if (length(predictions()) < 2) {return()}
      pWord2 <- {predictions()[2]}; if (pWord2 == "i") {pWord2 <- "I"}
      updateTextInput(session, "Phrase", NULL, paste0(input$Phrase, paste0(pWord2, " ")))
    })
    
    observeEvent(input$actionW3, {
      if (length(predictions()) < 3) {return()}
      pWord3 <- {predictions()[3]}; if (pWord3 == "i") {pWord3 <- "I"}
      updateTextInput(session, "Phrase", NULL, paste0(input$Phrase, paste0(pWord3, " ")))
    })
    
    observeEvent(input$actionw4, {
      if (length(predictions()) < 4) {return()}
      pWord4 <- {predictions()[4]}; if (pWord4 == "i") {pWord4 <- "I"}
      updateTextInput(session, "Phrase", NULL, paste0(input$Phrase, paste0(pWord4, " ")))
    })
    
    predictions <- reactive({
      if (!dataLoaded) {
        output$enteredPhrase <- renderPrint({"Initializing please wait."})
      }
      if (input$Phrase == "") {return()}
      else if (!input$Phrase == "") {
        validate(
          #need(length(strsplit(input$Phrase, split = " ")[[1]]) >= 2, "validate")
        )        
      }
        thePredictions <- as.character(getPredictions(input$Phrase, input$NoOfWords))
        thePredictions
    })
  }
)