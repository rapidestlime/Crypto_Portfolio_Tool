source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard","shinyWidgets","lubridate","hash","RSQLite")
loadPkgs(pkgnames)

# registerModal <- function(pwfailed = FALSE, uniquenamefailed = FALSE) {
#   modalDialog(
#     title = "Register a new account",
#     textInput('playername', "Enter name:"),
#     passwordInput("password1", "Enter a new password:"),
#     passwordInput("password2", "Confirm by re-entering the new password:"),
#     if (pwfailed)
#       div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
#     if (uniquenamefailed)
#       div(tags$b("The playername is in use. Try again.", style = "color: red;")),
#     
#     footer = tagList(
#       modalButton("Cancel"),
#       actionButton("registerok", "OK")
#     )
#   )
# }

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
                    actionLink("onboardclients", "On Board Clients")),
             column(3,
                    actionLink("marketoverview"," Market Overview")),
             column(3,
                    actionLink("viewportfolio", "View Portfolio")),
             column(3,
                    actionLink("updateportfolio", "Update Portfolio"))),
    tags$br(),
    footer = tagList(
      modalButton("Close")
    
  ))
}

# checkUniquePlayername <- function(playername){
#   #open the connection
#   conn <- dbConnect(RSQLite::SQlite(),dbpath)
#   #password could contain an SQL insertion attack
#   #Create a template for the query with placeholders for playername and password
#   querytemplate <- "SELECT * FROM riskplayer WHERE playername=?id1;"
#   query<- sqlInterpolate(conn, querytemplate,id1=playername)
#   result <- dbGetQuery(conn,query)
#   print(nrow(result))
#   # If the query is successful, result should be a dataframe with one row
#   if (nrow(result)==0){
#     FALSE
#   } else {
#     TRUE
#   }
#   #Close the connection
#   dbDisconnect(conn)
# }


#######ui########
ui <- dashboardPage(skin= "purple",
  dashboardHeader(title = "LFG PORTFOLIO TRACKING TOOL",
                  titleWidth = 450
                  ),
  dashboardSidebar(
    sidebarMenu(id="tabs",
      #https://fontawesome.com/icons?d=gallery
      menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
      menuItem("Onboarding", tabName = "onboarding", icon = icon("portal-enter")),
      menuItem("Market Overview", tabName = "market", icon = icon("binoculars")),
      menuItem("Portfolio", tabName = "portfolio", icon = icon("chart-pie"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "welcome",
              tags$div(
                tags$img(
                  src='background.png',
                  style="position: absolute; 
                         top: -50%; 
                         left: -50%; 
                         width: 150%; 
                         height: 150%;
                         display: flex;
                         z-order: 0;"
                ),
                tags$div(
                  style="z-order:1;position:absolute;font-size: 150%",
                  tags$img(
                    src='logo.png',
                    style= 
                      'margin-left: auto;
                     margin-right: auto;
                     width: 50%; 
                     height: 50%;
                     align: center;'
                  ),
                  tags$br(),
                  tags$h1(
                    'WELCOME!',
                    style='color: #000000;
                           font-family: Times New Roman;
                           align: center;'
                  ),
                  #tags$br(),
                  tags$h2(
                    'About Tool',
                    style='color: #000000;
                           font-family: Times New Roman;
                           align: center;'
                  ),
                  tags$p(
                    "This tool allows one to keep track of the current crypto market as well as managing portfolio",
                    style='color:#000000;
                           font-family:Times New Roman;
                           align:center;'
                  ),

                  tags$h2(
                    "Creators:",
                    style='color:#000000;
                           font-family:Times New Roman;
                           align:center;'
                  ),
                  tags$ul(
                    p('GOH RAY FONG'),
                    p('NICHOLAS TAN YI DA')
                  ),
                  # fluidRow(
                  #   box(width=3,
                  #       tags$h4("Logged in as:"),
                  #       htmlOutput("loggedInAs"),
                  #   ),
          
                  actionButton("login", "Login"),
                  tags$h4("Logged in as:"),
                  htmlOutput("loggedInAs", style="font-size:30;color:#D6AC18;font-family:Times New Roman;align:center;")
                ),
      #           #Second tab content
      #           tabItem(tabName = "onboarding",
      #                   tags$div(
      #                     tags$img(
      #                       src='background.png',
      #                       style="position: absolute;
      #          top: -50%;
      #          left: -50%;
      #          width: 150%;
      #          height: 150%;
      #          display: flex;
      #          z-order: 0;"
      #                     ),
      #                     tags$br(),
      #                     tags$h1(
      #                       'WELCOME!',
      #                       style='color: #000000;
      #            font-family: Times New Roman;
      #            align: center;'
      #                     ),
      #                     
      #                     
      #                     # actionButton("login", "Login"),
      #                     # tags$h4("Logged in as:"),
      #                     # htmlOutput("loggedInAs", style="font-size:30;color:#D6AC18;font-family:Times New Roman;align:center;")
      #                   )
      #                   ,
      # 
      #   #third tab content
      #   tabItem(tabName = "market",
      #           tags$div(
      #             tags$img(
      #               src='background.png',
      #               style="position: absolute;
      #    top: -50%;
      #    left: -50%;
      #    width: 150%;
      #    height: 150%;
      #    display: flex;
      #    z-order: 0;"
      #             ),
      #             tags$br(),
      #             tags$h1(
      #               'WELCOME!',
      #               style='color: #000000;
      #      font-family: Times New Roman;
      #      align: center;'
      #             ),
      #             
      #             
      #             # actionButton("login", "Login"),
      #             # tags$h4("Logged in as:"),
      #             # htmlOutput("loggedInAs", style="font-size:30;color:#D6AC18;font-family:Times New Roman;align:center;")
      #           )
      #           ,
      #   #fourth tab content
      #   tabItem(tabName = "portfolio",
      #           tags$div(
      #             tags$img(
      #               src='background.png',
      #               style="position: absolute;
      #            top: -50%;
      #            left: -50%;
      #            width: 150%;
      #            height: 150%;
      #            display: flex;
      #            z-order: 0;"
      #             ),
      #             tags$br(),
      #             tags$h1(
      #               'WELCOME!',
      #               style='color: #000000;
      #              font-family: Times New Roman;
      #              align: center;'
      #             ),
      #             
      #             
      #             # actionButton("login", "Login"),
      #             # tags$h4("Logged in as:"),
      #             # htmlOutput("loggedInAs", style="font-size:30;color:#D6AC18;font-family:Times New Roman;align:center;")
      #           )                
      #         )
      # )
              
              
      )
      
    ))))
  

################SERVER####################
server <- function(input, output, session) {
  #reactiveValues objects for storing items like the user password
  vals <- reactiveValues(password = NULL,clientid=NULL,playername=NULL,question_store = c(1:10),wrong_qns=c(),score=0,grid=list(0,0,0,0,0,0,0,0,0),query=NULL)
  timer <- reactiveVal(35)
  active <- reactiveVal(FALSE)
 
  # #Fire some code if the user clicks the Register button
  # observeEvent(input$register, {
  #   showModal(registerModal())
  # })
  # # Fire some code if the user clicks the registerok button
  # observeEvent(input$registerok, {
  #   # Check that password1 exists and it matches password2
  #   if (nchar(input$password1)==0 || (input$password1 != input$password2)) 
  #     showModal(registerModal(pwfailed = TRUE))
  #   if (checkUniquePlayername(input$playername)) 
  #     showModal(registerModal(uniquenamefailed = TRUE))
  #   #store the password and close the dialog
  #   vals$password <- input$password1
  #   print(vals$password) # for debugging
  #   vals$playername <- input$playername
  #   print(vals$playername)
  #   registerPlayer(vals$playername,vals$password)
  #   if (!is.null(vals$playername))
  #     vals$playerid <- getPlayerID(vals$playername,vals$password)
  #   print(vals$playerid) # for debugging
  #   removeModal()
  #   
  # })
  #Fire some code if the user clicks the Login button
  observeEvent(input$login, {
    showModal(loginModal(failed=FALSE))
  })
  # Fire some code if the user clicks the loginok button
  observeEvent(input$loginok, {
    if (input$username=="dennis" && input$password=="lfg") {
      
      removeModal()
      showModal(afterloginModal(failed=FALSE))
    } else {
      showModal(loginModal(failed = TRUE))
    }
  }) 
  
  # React to successful login
  output$loggedInAs <- renderUI({
    if (is.null(input$username))
      "Not logged in yet."
    else
      input$username
      
  })
  
  observeEvent(input$viewportfolio, {
    updateTabItems(session,"tabs","portfolio")
  })
  observeEvent(input$updateportfolio, {
    updateTabItems(session,"tabs","portfolio")
  })
  observeEvent(input$onboardclients, {
    updateTabItems(session,"tabs","onboarding")
  })
  observeEvent(input$marketoverview, {
    updateTabItems(session,"tabs","market")
  })
   
  
}


# fluidRow(column(3,
#                 actionButton("viewportfolio", "View Portfolio")),
#          column(3,
#                 actionButton("updateportfolio", "Update Portfolio")),
#          column(3,
#                 actionButton("onboardclients", "On Board Clients")),
#          column(3,
#                 actionButton("marketoverview"," Market Overview"))),
# tags$br(),


shinyApp(ui, server)

