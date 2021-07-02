source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard","shinyWidgets","lubridate","hash","RSQLite")
loadPkgs(pkgnames)
library(httr)
library(jsonlite)
library(plyr)
library(data.table)
library(rlist)



getportfolio1 <- function(wallet){
  # retrieves and output list of the relevant portfolio of the particular addrress string input
  # **Sources**
  # Ethereum: Zapper, BSC: Apeboard; some from Zapper, Polygon: Apeboard,
  # Fantom: Zapper, Optimism: Zapper
  # Subject to future changes depending on provider coverage
  
  outputtags <- list() # list of tags to be outputted
  outputtables <- list() # list of data tables to be referenced at last part of function
  zapper <- c('ethereum','xdai','optimism','fantom') #zapper use
  staketype <- c('masterchef','geyser','gauge','single-staking') #zapper use
    
  
  
  
  ######### zapper networks ############
  supportedlink <- paste0(paste0('https://api.zapper.fi/v1/protocols/balances/supported?addresses%5B%5D=',priwallet),'&api_key=96e0cc51-a62e-42ca-acee-910ea7d2a241')
  supported <- GET(supportedlink)
  zappersupported <- fromJSON(rawToChar(supported$content))
  for (nw in zapper) {
  ethindex <- which(zappersupported$network == nw)
  if (ethindex != integer(0)){
    outputtags <- list(outputtags, h2(paste0(str_to_title(nw),' Network')))
    for (e in ldply(zappersupported$protocols[ethindex], data.frame)$protocol){
      datalink <- sprintf('https://api.zapper.fi/v1/protocols/%s/balances?addresses%s%s&network=%s&api_key=96e0cc51-a62e-42ca-acee-910ea7d2a241',e,'%5B%5D=',wallet,nw)
      data <- fromJSON(rawToChar(GET(datalink)$content))
      if (eval(parse(text=sprintf("data$'%s'$products"))) != list()) {
      outputtags <- list(outputtags, h3(paste0(str_to_title(e),' Protocol')))
      outputtables[[paste0(e,nw)]] <- ldply(eval(parse(text=sprintf("data$'%s'$products$assets",wallet))),data.frame) %>% select(type,category,symbol,label,protocol,price,balance,balanceUSD)
      outputtags <- list(outputtags,DT::dataTableOutput(paste0(e,nw)))
      }
    }}} 
  
  
  prevnet <- 1 # for zapper staking protocols
  prevfarm <- 1 # for zapper staking protocols
  
  for (n in zapper){
    for (s in staketype){
      datalink <- sprintf('https://api.zapper.fi/v1/staked-balance/%s?addresses%s=%s&api_key=96e0cc51-a62e-42ca-acee-910ea7d2a241&network=%s',s,'%5B%5D',wallet,n)
      data <- fromJSON(rawToChar(GET(datalink)$content))
      print(data)
      if (length(eval(parse(text=sprintf("data$'%s'",wallet)))) != list() ){
        if (n != prevnet){
          outputtags <- list(outputtags,h2(paste0(str_to_title(n),' Network (Staking)')))
          prevnet <- n
        }
        rows <- as.numeric(length(eval(parse(text=sprintf("data$'%s'$label",wallet)))))
        newdata <- eval(parse(text=sprintf("data$'%s'",wallet)))
        for (i in seq(from = 1, to = rows)){
          if (newdata$protocolDisplay[[i]] != prevfarm){
            outputtags <- list(outputtags,h3(str_to_title(newdata$protocolDisplay[[i]])))
            prevfarm <- newdata$protocolDisplay[[i]]
          }
          outputtags <- list(outputtags,h4(newdata$symbol[[i]]))
          outputtags <- list(outputtags,h5('Tokens Staked'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste(n,s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Tokens Staked' )))
          outputtables[[paste(n,s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Tokens Staked')]] <- ldply(newdata$tokens[[i]],data.frame) %>% select(symbol,price,balance,balanceUSD)
          outputtags <- list(outputtags,h5('Tokens Rewards'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste(n,s,newdata$protocolDisplay[[i]],newdata$symbol[[i]], 'Token Rewards' )))
          outputtables[[paste(n,s,newdata$protocolDisplay[[i]],newdata$symbol[[i]], 'Token Rewards' )]] <- ldply(newdata$rewardTokens[[i]],data.frame) %>% select(symbol,price,balance,balanceUSD)
        }
      }
      else{
        
      }
    }
  }
  
  
  
  
  
  ########## apeboard networks ############
  apirepo <- read.csv('API.csv',headers=TRUE)
  
  
  ######### Binance Smart Chain############
  bscwallet <- GET(paste0('https://api.apeboard.finance/wallet/bsc/',wallet))
  bscwallet <- fromJSON(rawToChar(bscwallet$content)) 
  bscwallet <- as.data.frame(bscwallet) %>% select(-id,-logo,-decimals,-address) %>% mutate(balanceUSD = price*balance)
  outputtags <- list(outputtags,h2('Binance-Smart-Chain Network'))
  outputtags <- list(outputtags,h3('Token Protocol'))
  outputtags <- list(outputtags, DT::dataTableOutput(paste('Binance-Smart-Chain','Token Protocol')) )
  outputtables[[paste('Binance-Smart-Chain','Token Protocol')]] <- bscwallet
  
  
  # function to check if json response empty
  jsonchecker <- function(data){
    elements <- names(data)
    not_empty <- c()
    for (i in elements){
      if (data[[i]] != list()){
        not_empty <- c(not_empty,i) #json data exist
      }
    }
    return(not_empty) #json data empty
  } 
  
  for (link in apirepo$bsc){
    data <- GET(paste0(link,wallet))
    data <- fromJSON(rawToChar(data$content))
    occupied <- jsonchecker(data)
    if (occupied != c()){
      protocol <- str_sub(strsplit(link,'/')[[1]][4],end=-4)
      outputtags <- list(outputtags,h3(str_to_title(protocol)))
      for (e in occupied){
        if (e == 'vaults'){
          outputtags <- list(outputtags,h4('Vaults'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(data[[e]][['tokens']][[i]])==2){
              pair <- paste0(paste0(data[[e]][['tokens']][[i]]['symbol'][[1]],'/'),data[[e]][['tokens']][[i]]['symbol'][[2]])
              outputtags <- list(outputtags,h4(pair))
            }
            else{
              outputtags <- list(outputtags,h4(data[[e]][['tokens']][[i]]['symbol'][[1]]))
            }
            outputtags <- list(outputtags,h5('Tokens Staked'))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Staked'))) 
            outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            if(eval(parse(paste0("data[[e]][['rewards']][[i]]",'$balance'))) != NULL){
              outputtags <- list(outputtags,h5('Tokens Rewards'))
              outputtags <- list(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-lpAddress,-logo,-address) %>% mutate(balanceUSD = price*balance)
            }
          }
        }
        
        if (e == 'farms'){ 
          outputtags <- list(outputtags,h4('Farms'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(data[[e]][['tokens']][[i]])==2){
              pair <- paste0(paste0(data[[e]][['tokens']][[i]]['symbol'][[1]],'/'),data[[e]][['tokens']][[i]]['symbol'][[2]])
              outputtags <- list(outputtags,h4(pair))
            }
            else{
              outputtags <- list(outputtags,h4(data[[e]][['tokens']][[i]]['symbol'][[1]]))
            }
            outputtags <- list(outputtags,h5('Tokens Staked'))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Staked'))) 
            outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            if(eval(parse(paste0("data[[e]][['rewards']][[i]]",'$balance'))) != NULL){
              outputtags <- list(outputtags,h5('Tokens Rewards'))
              outputtags <- list(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-lpAddress,-logo,-address) %>% mutate(balanceUSD = price*balance)
            }
          }
        }
        
        if (e == 'lendings'){
          # need test example  
        }
        
        if (e == 'grazes'){
          # need test example  
        }
        
        if (e == 'stakings'){
          # need test example  
        }
        
        if (e == 'positions'){
          # need test example  
        }
        
        if (e == 'borrows'){
          outputtags <- list(outputtags,h4('Borrowings'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('bsc',protocol,'Borrowings')))
          outputtables[[paste('bsc',protocol,'Borrowings')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = -price*balance)
        }
        
        if (e == 'deposits'){
          outputtags <- list(outputtags,h4('Deposits'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('bsc',protocol,'Deposits')))
          outputtables[[paste('bsc',protocol,'Deposits')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
        }
        
        if (e == 'stake'){
          # need test example 
        }
        
        if (e == 'boardroom'){
          # need test example
        }
        
        if (e == 'vault'){
          # need test example
        }
        
        if (e == 'syrup'){
          # need test example
        }
        
        if (e == 'mintings'){
          # need test example
        }
        
        if (e == 'locked'){
          # need test example
        }
      }
    }
  } 
  
  
  bsczapper <- c('1inch','harvest','bzx') #for bsc protocols outside Apeboard
  for (e in bsczapper){
    exist <- NULL
    datalink <- sprintf('https://api.zapper.fi/v1/protocols/%s/balances?addresses%s%s&network=%s&api_key=96e0cc51-a62e-42ca-acee-910ea7d2a241',e,'%5B%5D=',wallet,'binance-smart-chain ')
    data <- fromJSON(rawToChar(GET(datalink)$content))
    if (eval(parse(text=sprintf("data$'%s'$products"))) != list()) {
      outputtags <- list(outputtags, h3(paste0(str_to_title(e),' Protocol')))
      outputtables[[paste0(e,'bsc')]] <- ldply(eval(parse(text=sprintf("data$'%s'$products$assets",wallet))),data.frame)
      outputtags <- list(outputtags,DT::dataTableOutput(paste0(e,'bsc')))
    }
  }
    
    prevfarm <- 1
    
    for (s in staketype){
      datalink <- sprintf('https://api.zapper.fi/v1/staked-balance/%s?addresses%s=%s&api_key=96e0cc51-a62e-42ca-acee-910ea7d2a241&network=%s',s,'%5B%5D',wallet,'binance-smart-chain')
      data <- fromJSON(rawToChar(GET(datalink)$content))
      print(data)
      if (length(eval(parse(text=sprintf("data$'%s'",wallet)))) != list() ){
        rows <- as.numeric(length(eval(parse(text=sprintf("data$'%s'$label",wallet)))))
        newdata <- eval(parse(text=sprintf("data$'%s'",wallet)))
        for (i in seq(from = 1, to = rows)){
          if (newdata$protocol[[i]] %in% bsczapper){
            if (newdata$protocol[[i]] != prevfarm){
              outputtags <- list(outputtags,h3(str_to_title(newdata$protocolDisplay[[i]])))
              prevfarm <- newdata$protocol[[i]]
            }
            outputtags <- list(outputtags,h4(newdata$symbol[[i]]))
            outputtags <- list(outputtags,h5('Tokens Staked'))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('bsc',s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Tokens Staked' )))
            outputtables[[paste('bsc',s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Tokens Staked')]] <- ldply(newdata$tokens[[i]],data.frame) %>% select(symbol,price,balance,balanceUSD)
            outputtags <- list(outputtags,h5('Tokens Rewards'))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('bsc',s,newdata$protocolDisplay[[i]],newdata$symbol[[i]], 'Token Rewards' )))
            outputtables[[paste('bsc',s,newdata$protocolDisplay[[i]],newdata$symbol[[i]], 'Token Rewards' )]] <- ldply(newdata$rewardTokens[[i]],data.frame) %>% select(symbol,price,balance,balanceUSD)
          }
        }
      }
      else{
        
      }
    }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############### Polygon #################
  
  maticwallet <- GET(paste0('https://api.apeboard.finance/wallet/matic/',wallet))
  maticwallet <- fromJSON(rawToChar(maticwallet$content))
  outputtags <- list(outputtags,h2('Polygon Network'))
  if (maticwallet != list()){
  maticwallet <- as.data.frame(maticwallet) %>% select(-id,-logo,-decimals,-address) %>% mutate(balanceUSD = price*balance)
  
  outputtags <- list(outputtags,h3('Token Protocol'))
  outputtags <- list(outputtags, DT::dataTableOutput(paste('Polygon','Token Protocol')) )
  outputtables[[paste('Polygon','Token Protocol')]] <- maticwallet
  }
  
  
  
  for (link in apirepo$matic){
    data <- GET(paste0(link,wallet))
    data <- fromJSON(rawToChar(data$content))
    occupied <- jsonchecker(data)
    if (occupied != c()){
      protocol <- str_sub(strsplit(link,'/')[[1]][4],end=-6)
      outputtags <- list(outputtags,h3(str_to_title(protocol)))
      for (e in occupied){
        if (e == 'vaults'){
          # need test example
        }
        
        if (e == 'farms'){
          outputtags <- list(outputtags,h4('Farms'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(data[[e]][['tokens']][[i]]['symbol'])==2){
              pair <- paste0(paste0(data[[e]][['tokens']][[i]]['symbol'][[1]],'/'),data[[e]][['tokens']][[i]]['symbol'][[2]])
              outputtags <- list(outputtags,h4(pair))
            }
            else{
              outputtags <- list(outputtags,h4(data[[e]][['tokens']][[i]]['symbol'][[1]]))
            }
            outputtags <- list(outputtags,h5('Tokens Staked'))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('matic',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('matic',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = -price*balance)  
            if(eval(parse(paste0("data[[e]][['rewards']][[i]]",'$balance'))) != NULL){
              outputtags <- list(outputtags,h5('Tokens Rewards'))
              outputtags <- list(outputtags,DT::dataTableOutput(paste('matic',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('matic',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-lpAddress,-logo,-address) %>% mutate(balanceUSD = price*balance)
            }
          }
        }
        
        
        if (e == 'positions'){
          # need test example
        }
        
        if (e == 'borrows'){
          outputtags <- list(outputtags,h3('Borrowings'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('matic',protocol,'Borrowings')))
          outputtables[[paste('matic',protocol,'Borrowings')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = -price*balance)
        }
        
        if (e == 'deposits'){
          outputtags <- list(outputtags,h3('Deposits'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('matic',protocol,'Deposits')))
          outputtables[[paste('matic',protocol,'Deposits')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
        }
        
      }
    }
  }   
  
  
return(list(tags=outputtags,tables=outputtables)) 
                     
}
