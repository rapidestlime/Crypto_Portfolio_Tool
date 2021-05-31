source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard","shinyWidgets","lubridate","hash")
loadPkgs(pkgnames)






#######ui########
ui <- dashboardPage(skin= "purple",
  dashboardHeader(title = "INEVITABLE FUND TRACKING TOOL",
                  titleWidth = 450
                  ),
  dashboardSidebar(
    sidebarMenu(  
      #https://fontawesome.com/icons?d=gallery
      menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
      menuItem("Onboarding", tabName = "onboarding", icon = icon("portal-enter")),
      menuItem("Market Overview", tabName = "market", icon = icon("binoculars")),
      menuItem("Portfolio Overview", tabName = "portfolio", icon = icon("chart-pie"))
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
                  actionButton("register", "Register"),
                  actionButton("login", "Login"),
                  tags$h4("Logged in as:")
                  #htmlOutput("loggedInAs", style="font-size:30;color:#D6AC18;font-family:Times New Roman;align:center;")
                )
              )
      )
              
              
      )
      
    )
  )

################SERVER####################
server <- function(input, output, session) {
  #reactiveValues objects for storing items like the user password
  vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL,question_store = c(1:10),wrong_qns=c(),score=0,grid=list(0,0,0,0,0,0,0,0,0),query=NULL)
  timer <- reactiveVal(35)
  active <- reactiveVal(FALSE)
  
}





shinyApp(ui, server)

