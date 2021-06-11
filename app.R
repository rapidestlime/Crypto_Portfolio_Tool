source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard","shinyWidgets","lubridate","hash","RSQLite")
loadPkgs(pkgnames)
library(httr)
library(jsonlite)
library(plyr)
library(data.table)
library(rlist)
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
              downloadButton("downloadData", label = "Download")),
      tabItem(tabName="portfolio",
              setBackgroundImage(src='background.png',TRUE),
              uiOutput('datatables'),
              actionButton("hello","hello"),
              downloadButton("generateData", label = "Generate PDF")
      )
      
      
                     
      
             )    
              
             )
      
)
  

################SERVER####################
server <- function(input, output, session) {
  #reactiveValues objects for storing items like the user password
  vals <- reactiveValues(datatags=list(),tables=list(),query=NULL)
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
  ################################################
  
  
  
  
  
  
  #helpful links for generating multiple tables in one go (dont delete!)
  #https://stackoverflow.com/questions/41201192/r-shiny-display-multiple-plots-selected-with-checkboxgroupinput
  #https://community.rstudio.com/t/shiny-app-with-dynamic-number-of-datatables/2405/2
  #https://stackoverflow.com/questions/22842354/outputing-n-tables-in-shiny-where-n-depends-on-the-data
  #https://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time-o1
  #https://community.rstudio.com/t/shiny-app-with-dynamic-number-of-datatables/2405/8  <---important for understanding dynamic table generation!
  
  
  
  
  
  
  
  ###############data tables####################
  output$datatables <- renderUI({isolate({
    link <- 'https://api.zapper.fi/v1/protocols/balances/supported?addresses%5B%5D=0x58bbae0159117a75225e72d941dbe35ffd99f894&api_key=96e0cc51-a62e-42ca-acee-910ea7d2a241'
    test <- GET(link)
    test <- fromJSON(rawToChar(test$content))
    for (i in seq(from = 1, to = length(test$network))){
      network <- test$network[i]
      vals$datatags <- list(vals$datatags, h2(paste0(str_to_title(network),' Network')))
      for (e in ldply(test$protocols[i], data.frame)$protocol){
        link1 <- paste0(paste0('https://api.zapper.fi/v1/protocols/',e),paste0(paste0('/balances?addresses%5B%5D=0x58bbae0159117a75225e72d941dbe35ffd99f894&network=',network),'&api_key=96e0cc51-a62e-42ca-acee-910ea7d2a241'))
        data <- fromJSON(rawToChar(GET(link1)$content))
        wallet <- '0x58bbae0159117a75225e72d941dbe35ffd99f894'
        vals$datatags <- list(vals$datatags, h3(paste0(str_to_title(e),' Protocol')))
        vals$tables[[paste0(e,network)]] <- ldply(eval(parse(text=sprintf("data$'%s'$products$assets",wallet))),data.frame)
        vals$datatags <- list(vals$datatags,DT::dataTableOutput(paste0(e,network)))
      }}
    
    
    for (i in names(vals$tables)){local({
      i<-i
      print(vals$tables[[i]])
      output[[i]] <- DT::renderDataTable({vals$tables[[i]]},options = list(pageLength = 10, width="100%", scrollX = TRUE))
    })}
   return(vals$datatags) 
  }) })
    
    }





shinyApp(ui, server)

