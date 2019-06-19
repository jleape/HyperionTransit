#### Log in module ###

PASSWORD <- data.frame(
  username = c("transmilenio", "jleape"), 
  password = c("mejorParaTodos", "Transmilenio3")
  )

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      textInput("userName", "Username:"),
      passwordInput("passwd", "Password:"),
      br(),
      fluidRow(
        column(6,
               actionButton("login", "Login")
               ),
        column(6,
               actionButton("demo", "Demo")
               )
      )
    )
  }
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    USER$pass
  }  
})

# Login info during session ----
output$userPanel <- renderUI({
  if (USER$Logged == TRUE) {
    fluidRow(
      column(2,
             "User: ", USER$name
      ),
      column(1, actionLink("logout", "Logout"))
    )
  }  
})

# control login
observeEvent(input$login , {
  Username <- isolate(input$userName)
  Password <- isolate(input$passwd)
  Id.username <- which(PASSWORD$username == Username)
  Id.password <- which(PASSWORD$password == Password)
  if (length(Id.username) > 0 & length(Id.password) > 0) {
    if (Id.username == Id.password) {
      USER$Logged <- TRUE
      USER$name <- Username
    }
  } else {
    USER$pass <- "User name or password failed!"
  }
})

# start demo
observeEvent(input$demo , {
  Username <- isolate("Demo")
  USER$Logged <- TRUE
  USER$name <- Username
})

# control logout
observeEvent(input$logout , {
  USER$Logged <- FALSE
  USER$pass <- ""
})