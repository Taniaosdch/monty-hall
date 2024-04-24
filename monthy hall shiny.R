library(shiny)

door <- paste('Door', 1:3)
prize_door <- sample(door,1)
change_choice <- c("Yes", "No")

ui <- fluidPage(
  titlePanel("Monty Hall problem"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("player_choice", "Choose the door", door, selected = character(0))
    ),
    mainPanel(
      uiOutput("formula"),
      br(),
      textOutput("step_1"),
      br(),
      actionButton("shown_door_button", "2) Show the Door with a goat"),
      br(),
      textOutput("door_shown"),
      br(),
      uiOutput("change_door_button"),
      br(),
      uiOutput("change_yes_input"), 
      p(),
      uiOutput("change_no_input"),
      textOutput("yes_button_output"),
      br(),
      textOutput("no_button_output"),
      uiOutput("formula_yes")
    )
  )
)

server <- function(input, output, session) {
  show_change_button <- reactiveVal(FALSE)
  # show_choice_radio <- reactiveVal(FALSE)
  show_choice_yes <- reactiveVal(FALSE)
  show_choice_no <- reactiveVal(FALSE)
  
  output$formula <- renderUI({
    withMathJax(paste("Prior probabilities: $$\\ P(D1)=\\frac{1}{3} \\quad \\ P(D2)=\\frac{1}{3} \\quad \\ P(D3) = \\frac{1}{3}$$"))
  })
  
  observeEvent(input$player_choice, {
    output$step_1 <- renderText({
      paste("You've chosen", input$player_choice)
    })
  })
  
  doorshown <- reactive({
    ifelse(prize_door == input$player_choice, 
           sample(door[!(door %in% input$player_choice)], 1), 
           door[!(door %in% input$player_choice | door %in% prize_door)])
  })
  
  observeEvent(input$shown_door_button, {
    output$door_shown <- renderText({
      require(doorshown())
      prize_door <- sample(door,1)
      paste("The goat is behind", doorshown())
    })
    show_change_button(TRUE)
  })
  
  output$change_door_button <- renderUI({
    req(show_change_button())
    actionButton("wanna_change_door_button", "3) Wanna change your choice?")
  })
  
  observeEvent(input$wanna_change_door_button, {
    show_choice_yes(TRUE)
    show_choice_no(TRUE)
  })
  
  
  output$change_yes_input <- renderUI({
    req(show_choice_yes())
    actionButton("yes_button", "Yes")
  })
  
  output$change_no_input <- renderUI({
    req(show_choice_no())
    actionButton("no_button", "No")
  })
  
  
  observeEvent(input$yes_button, {
    output$yes_button_output <- renderText({
      paste("The car was behind", prize_door, ifelse(prize_door!=input$player_choice, "You won.", "sorry"))
    })
    output$formula_yes <- renderUI({
      require(doorshown())
      doorshown <- doorshown()
      withMathJax(paste("You have left with two choices. Probability that the car was behind", input$player_choice, "is still", "$$\\ P(D)=\\frac{1}{3}$$",
                        "Since probabilities should sum up to 1, probability that the car is behind", door[!(door %in% input$player_choice | door %in% doorshown)],
                        "is", "$$\\ P(D)=\\frac{2}{3}$$", "So it's better to change your choice."))
    })
    show_choice_no(FALSE)
  })
  
  observeEvent(input$no_button, {
    output$no_button_output <- renderText({
      paste("The car was behind", prize_door, ifelse(prize_door!=input$player_choice, "sorry", "You won."))
    })
    output$formula_yes <- renderUI({
      require(doorshown())
      doorshown <- doorshown()
      withMathJax(paste("You have left with two choices. Probability that the car was behind", input$player_choice, "is still", "$$\\ P(D)=\\frac{1}{3}$$",
                        "Since probabilities should sum up to 1, probability that the car is behind", door[!(door %in% input$player_choice | door %in% doorshown)],
                        "is", "$$\\ P(D)=\\frac{2}{3}$$", "So it's better to change your choice."))
    })
    show_choice_yes(FALSE)
  })
}

shinyApp(ui, server)