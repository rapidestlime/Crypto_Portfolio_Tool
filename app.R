source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard","shinyWidgets","lubridate","hash","RSQLite")
loadPkgs(pkgnames)

source("Information.R")
source("login.R")


#######ui########
ui <- dashboardPage(skin= "purple",
  dashboardHeader(title = "LFG CAPTIAL TRACKING TOOL",
                  titleWidth = 400
                  ),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      #https://fontawesome.com/icons?d=gallery
      menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
      menuItem("Onboarding", tabName = "onboarding", icon = icon("folder-open")),
      menuItem("Market Overview", tabName = "market", icon = icon("binoculars")),
      menuItem("Portfolio Overview", tabName = "portfolio", icon = icon("chart-pie"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "welcome",
              setBackgroundImage(src='background.png',TRUE),
                tags$div(
                  style="font-size: 150%;",
                  tags$img(
                    src='main logo.png',
                    height=150,
                    width=400
                  ),
                  tags$br(),
                  tags$h1(
                    'WELCOME!',
                    style='color: #000000;
                           font-family: Times New Roman;
                           align: center;'
                  ),
                  tags$br(),
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
                  tags$h4("Logged in as:"),
                  htmlOutput("loggedInAs", style="font-size:30;color:#D6AC18;font-family:Times New Roman;align:center;")
                
              )
      ),
      
      tabItem(tabName="onboarding",
              setBackgroundImage(src='background.png',TRUE),
                fluidRow(
                  column(6,
                tags$div(
                  style="font-size: 150%;",
                  tags$img(
                    src='addclientlogo.png',
                    height=150,
                    width=400
                  ),
                  tags$h2(
                    'Client Name:',
                    style='color: #000000;
                           font-family: Times New Roman;
                           align: center;'
                  ),
                  textInput("name1",NULL),
                  tags$h2(
                    'Client Primary Wallet:',
                    style='color: #000000;
                           font-family: Times New Roman;
                           align: center;'
                  ),
                  textInput("primarywallet1",NULL,width="170%"),
                  tags$h2(
                    'Client Secondary Wallet(if any):',
                    style='color: #000000;
                           font-family: Times New Roman;
                           align: center;'
                  ),
                  textInput("secondarywallet1",NULL,width="170%"),
                  tags$h2(
                    'File Input:',
                    style='color: #000000;
                           font-family: Times New Roman;
                           align: center;'
                  ),
                  fileInput("file1", NULL,
                            accept = c(
                              ".csv")),
                  actionButton("save","SAVE"))),
                  column(6,
                  tags$img(
                    src='updateclientlogo.png',
                    height=150,
                    width=400
                  ),
                  tags$h2(
                    'Client Name:',
                    style='color: #000000;
                           font-family: Times New Roman;
                           align: center;'
                  ),
                  selectizeInput("name2",NULL,choices=retrievenamelist()),
                  tags$h2(
                    'Client Primary Wallet:',
                    style='color: #000000;
                           font-family: Times New Roman;
                           align: center;'
                  ),
                  textInput("primarywallet2",NULL,width="170%"),
                  tags$h2(
                    'Client Secondary Wallet:',
                    style='color: #000000;
                           font-family: Times New Roman;
                           align: center;'
                  ),
                  textInput("secondarywallet2",NULL,width="170%"),
                  actionButton("update","UPDATE")
                           )
                          )
              ),
      tabItem(tabName="market",
              setBackgroundImage(src='background.png',TRUE),
              actionButton("hello","hello"),
              downloadButton("downloadData", label = "Download"))
      
      
                     
      
             )    
              
             )
      
)
  

################SERVER####################
server <- function(input, output, session) {
  #reactiveValues objects for storing items like the user password
  vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL,question_store = c(1:10),wrong_qns=c(),score=0,grid=list(0,0,0,0,0,0,0,0,0),query=NULL)
  timer <- reactiveVal(35)
  active <- reactiveVal(FALSE)
  
  
  
  ##########LOGIN############
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
  ##############################
  
  
  #######TAB SWITCH AFTER LOGIN#######
  observeEvent(input$onboardtab,{updateTabItems(session, "sidebar", "onboarding")
    removeModal()})
  observeEvent(input$markettab,{updateTabItems(session, "sidebar", "market")
    removeModal()})
  observeEvent(input$portfoliotab,{updateTabItems(session, "sidebar", "portfolio")
    removeModal()})
  
  
  ###########DB DATA DUMP: FOR BACKUP PURPOSES IN CASE APP REDEPLOYMENT!#############
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ClientData", "db", sep=".")
    },
    
    content = function(file) {
      file.copy(file.path(getwd(),'ClientData.db'), file)
    },
  )  
  ###############################################################################
  
  
  ###########save client info################
  observeEvent(input$save,{
    addclientInfo(input$name1)
    if (input$primarywallet1 != '' && input$name1 != ''){ #name and priwallet cannot empty
       addclientWallet(input$name1,input$primarywallet1, input$secondarywallet1)
       updateTextInput(session,"name1", value="")
       updateTextInput(session,"primarywallet1", value="")
       updateTextInput(session,"secondarywallet1", value="")
       showModal(modalDialog(
         title = "Alert",
         "Information saved!",
         easyClose = TRUE,
         footer = NULL
       )) }
    else {showModal(modalDialog(
      title = "Alert",
      "Error saving details. Check field inputs!",
      easyClose = TRUE,
      footer = NULL
    ))}
  })
  ##############################################
  
  
  ############update client info################
  observeEvent(input$update,{
    updateclientwallet(input$name2,input$primarywallet2,input$secondarywallet2)
    if (input$primarywallet2 != '' || input$secondarywallet2 != ''){
      updateTextInput(session,"name2", value="")
      updateTextInput(session,"primarywallet2", value="")
      updateTextInput(session,"secondarywallet2", value="")
      showModal(modalDialog(
        title = "Alert",
        "Information updated!",
        easyClose = TRUE,
        footer = NULL
      ))
    }})
}





shinyApp(ui, server)

