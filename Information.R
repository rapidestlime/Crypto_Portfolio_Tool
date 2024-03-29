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
  result
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

addclientWallet <- function(name,priwallet,secwallet,terrawallet,solanawallet){
  conn <- dbConnect(RSQLite::SQLite(), dbpath)
  querytemplate <- "INSERT INTO ClientWallet (ClientName,PriWallet,SecWallet,TerraWallet,SolanaWallet) VALUES (?id1,?id2,?id3,?id4,?id5);"
  query<- sqlInterpolate(conn, querytemplate,id1=name,id2=priwallet,id3=secwallet,id4=terrawallet,id5=solanawallet)
  sqlQueryExecuteFromSqlitePath(dbpath,query)
}

retrievenamelist <- function(){
  querytemplate <- "SELECT ClientName FROM ClientInfo"
  sqlQueryGetFromSqlitePath(dbpath,querytemplate)$ClientName
}

retrievewalletlist <- function(name){
  conn <- dbConnect(RSQLite::SQLite(), dbpath)
  querytemplate <- "SELECT * FROM ClientWallet WHERE ClientName = ?id1"
  query<- sqlInterpolate(conn, querytemplate,id1=name)
  sqlQueryGetFromSqlitePath(dbpath,query)
}

updateclientwallet <- function(name,priwallet,secwallet,terrawallet,solanawallet) {
  conn <- dbConnect(RSQLite::SQLite(), dbpath)
  
  if (priwallet != '' && secwallet == ''){
    querytemplate <- "UPDATE ClientWallet SET PriWallet = ?id1 WHERE ClientName = ?id2"
    query <- sqlInterpolate(conn, querytemplate,id1=priwallet,id2=name)
    sqlQueryExecuteFromSqlitePath(dbpath,query)
  }
  if (priwallet == '' && secwallet != ''){
    querytemplate <- "UPDATE ClientWallet SET SecWallet = ?id1 WHERE ClientName = ?id2"
    query <- sqlInterpolate(conn, querytemplate,id1=secwallet,id2=name)
    sqlQueryExecuteFromSqlitePath(dbpath,query)
  }
  if (priwallet != '' && secwallet != ''){
    querytemplate <- "UPDATE ClientWallet SET PriWallet = ?id1,SecWallet ?id2 WHERE ClientName = ?id3"
    query <- sqlInterpolate(conn, querytemplate,id1=priwallet,id2=secwallet,id3=name)
    sqlQueryExecuteFromSqlitePath(dbpath,query)
  }
  if (priwallet == '' && secwallet == ''){
  } 
  
  if (terrawallet != '' && solanawallet == ''){
    querytemplate <- "UPDATE ClientWallet SET TerraWallet = ?id1 WHERE ClientName = ?id2"
    query <- sqlInterpolate(conn, querytemplate,id1=terrawallet,id2=name)
    sqlQueryExecuteFromSqlitePath(dbpath,query)
  }
  if (terrawallet == '' && solanawallet != ''){
    querytemplate <- "UPDATE ClientWallet SET SolanaWallet = ?id1 WHERE ClientName = ?id2"
    query <- sqlInterpolate(conn, querytemplate,id1=solanawallet,id2=name)
    sqlQueryExecuteFromSqlitePath(dbpath,query)
  }
  if (terrawallet != '' && solanawallet != ''){
    querytemplate <- "UPDATE ClientWallet SET TerraWallet = ?id1,SolanaWallet = ?id2 WHERE ClientName = ?id3"
    query <- sqlInterpolate(conn, querytemplate,id1=terrawallet,id2=solanawallet,id3=name)
    sqlQueryExecuteFromSqlitePath(dbpath,query)
  }
  if (terrawallet == '' && solanawallet == ''){
  }
}

getclientAUM <- function(name){
  conn <- dbConnect(RSQLite::SQLite(), dbpath)
  querytemplate <- "SELECT * FROM ClientPerformance WHERE ClientName = ?id1"
  query<- sqlInterpolate(conn, querytemplate,id1=name)
  sqlQueryGetFromSqlitePath(dbpath,query)$ClientRefAUM
}
  
saveclientAUM <- function(newaum,name){
  conn <- dbConnect(RSQLite::SQLite(), dbpath)
  querytemplate <- "UPDATE ClientPerformance SET ClientRefAUM= ?id1 WHERE ClientName = ?id2"
  query<- sqlInterpolate(conn, querytemplate,id1=newaum,id2=name)
  sqlQueryExecuteFromSqlitePath(dbpath,query)
}