source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard","shinyWidgets","lubridate","hash","RSQLite")
loadPkgs(pkgnames)


dbpath <- "ClientData.db"

sqlQueryGetFromSqlitePath <- function(strPath,query){
  # Use this for SELECT queries to an SQLite database
  #Connect to the SQLite database
  conn <- dbConnect(RSQLite::SQLite(), strPath)
  result <- dbGetQuery(conn,query)
  #Disconnect from the database
  dbDisconnect(conn)
  #return the result
  result
}

sqlQueryExecuteFromSqlitePath <- function(strPath,query){
  # Use this for DELETE, INSERT, or UPDATE queries to an SQLite database
  #Connect to the SQLite database
  conn <- dbConnect(RSQLite::SQLite(), strPath)
  result <- dbSendQuery(conn,query)
  dbClearResult(result)
  #Disconnect from the database
  dbDisconnect(conn)
}



######example#####
#testSqlite <- function(){
#  path <- "TestData.db"
#  query <- "SELECT * FROM LightningExtract LIMIT 10"
#  result <- sqlQueryGetFromSqlitePath(path,query)
#  result
#} 
##################




addclientInfo <- function(name){
  conn <- dbConnect(RSQLite::SQLite(), dbpath)
  querytemplate <- "INSERT INTO ClientInfo (ClientName,ClientOnboarding) VALUES (?id1,DATETIME('now','localtime'));"
  query<- sqlInterpolate(conn, querytemplate,id1=name)
  sqlQueryExecuteFromSqlitePath(dbpath,query)
}

addclientWallet <- function(name,wallet){
  conn <- dbConnect(RSQLite::SQLite(), dbpath)
  querytemplate <- "INSERT INTO ClientWallet (ClientName,ClientWallet) VALUES (?id1,?id2);"
  query<- sqlInterpolate(conn, querytemplate,id1=name,id2=wallet)
  sqlQueryExecuteFromSqlitePath(dbpath,query)
}

retrievenamelist <- function(){
  querytemplate <- "SELECT ClientName FROM ClientInfo"
  sqlQueryGetFromSqlitePath(dbpath,querytemplate)$ClientName
}

#addclientInfo('Nicholas tan')
