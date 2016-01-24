library(shiny)

shinyUI(
  navbarPage("4Cast", windowTitle = "4Cast",
             tabPanel("the app",
                      mainPanel(
                        fluidRow(
                          column(5, 
                                 h4('the 4Cast app '),
                                 p('4Cast is a predictive text analytics app developed for easy integration into any device to make communication 
                                    easier and quicker.'),
                                 p(''),
                                 p('4Cast uses predictive analytics to allow busy people to more effectively communicate by suggesting next words 
                                    and allowing them to select a word with one click.'),
                                 p(''),
                                 p('4Cast speeds up user communication by eliminating extra keystrokes and avoiding spelling errors.'),
                                 p(''),
                                 p('Under the hood how does 4Cast work? 4Cast uses the latest data science and statistical techniques to constantly 
                                    analyze real-time and historical linguistic data from around the world to make your next word prediction.'),
                                 p('And for the curious geeks out there - our techniques include cutting-edge advances in predictive modeling, 
                                    machine learning, and data mining.'),
                                 h4('Get started with 4Cast'),
                                 tags$ol(
                                   tags$li("Click on \'the demo\' in the menu above."), 
                                   tags$li("Select the number of word predictions you want."),
                                   tags$li("And start typing.") 
                                 )
                          ),
                          column(3,
                                 tags$img(width = "763px", height = "599px", src = "diagram.png")
                                 #tags$img(width = "937px", height = "318px", src = "model.png")
                          )
                        )
                      )
             ),
             tabPanel("the demo", 
                      headerPanel ("the 4Cast demo"),
                      mainPanel(
                        br(),
                        p('Instructions:'),
                        p('1 - Enter the number of word predictions (max = 4).'),
                        textInput("NoOfWords", "", "4"),
                        p('2 - Start typing in the input box below.'),
                        p('3 - Predictions (max = 4) will appear on bottons below the text box ', tags$em("after you enter a space.")),
                        p('4 - Continue typing or click on a predicted word button.'),
                        p('5 - If you click on a word prediction button the word is added to your phrase and the next prediction is supplied.'),
                        br(),
                        textInput("Phrase","Enter your phrase - ", "", width = 1000),
                        p(tags$b("Word Predictions - ")),
                        fluidRow(
                          column(3, 
                            uiOutput("w1")
                          ),
                          column(3,
                            uiOutput("w2")
                          ),
                          column(3,
                            uiOutput("w3")
                          ),
                          column(3,
                            uiOutput("w4")
                          )
                        ),
                        br()
                      )
             ), 
             tabPanel("the tribe",
                      mainPanel(
                        p('We are a tribe of geeks who learned data science and predictive analytics from the awesome people at Coursera and Johns 
                           Hopkins University\'s Data Science specialization courses.
                           We put our new knowledge and energy into making 4Cast the leading text analytics app on the market.'),
                        p('Thx for testing 4Cast out.'),
                        p(''),
                        p('We look forward to all of your input, feedback, and suggestions for making 4Cast better.'),
                        p(''),
                        p('- the cloud9 tribe'),
                        p(''),
                        p('', tags$a(href="http://rpubs.com/ammraleigh/145904", "Product Presentation"))
                      )
             )
))
