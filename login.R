loginModal <- function(failed = FALSE) {
  modalDialog(
    title = "Login",
    textInput("username", "Enter your name:"),
    passwordInput("password", "Enter your password:"),
    if (failed)
      div(tags$b("There is no registered person with that name and password. Try again.", style = "color: red;")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("loginok", "OK")
    )
  )
}

afterloginModal <- function(failed= FALSE){
  modalDialog(
    title="Welcome Back",
    tags$br(),
    "What do you want to do today?",
    tags$br(),
    fluidRow(column(3,
                    actionButton("onboardtab", "On Board Clients")),
             column(3,
                    actionButton("markettab"," Market Overview")),
             column(3,
                    actionButton("portfoliotab", "View Portfolio"))#,
             #column(3,
             #       actionLink("updateportfolio", "Update Portfolio"))
             ),
    tags$br(),
    footer = tagList(
      modalButton("Close")
      
    ))
}