source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard","shinyWidgets","lubridate","hash","RSQLite")
loadPkgs(pkgnames)
library(httr)
library(jsonlite)
library(plyr)
library(data.table)
library(rlist)





getportfolio2 <- function(terra,solana){
  # retrieves and output list of the relevant portfolio of the particular addrress string input
  # **Sources**
  # Terra: Apeboard, Solana: Apeboard
  # Subject to future changes depending on provider coverage
  
  
  outputtags <- list()
  outputtables <- list()
  
  
  #get api links
  apirepo <- read.csv('API.csv',headers=TRUE)
  
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
  
  ############# Terra ################
  if (!is.na(terra)) {
  terrawallet <- GET(paste0('https://api.apeboard.finance/wallet/terra/',terra))
  terrawallet <- fromJSON(rawToChar(terrawallet$content))
  outputtags <- list(outputtags,h2('Terra Network'))
  if (terrawallet != list()){
    terrawallet <- as.data.frame(terrawallet) %>% select(-id,-logo,-decimals,-address) %>% mutate(balanceUSD = price*balance)
    
    outputtags <- list(outputtags,h3('Token Protocol'))
    outputtags <- list(outputtags, DT::dataTableOutput(paste('Terra','Token Protocol')) )
    outputtables[[paste('Terra','Token Protocol')]] <- terrawallet
  }
  
  
  for (link in apirepo$terra){
    data <- GET(paste0(link,terra))
    data <- fromJSON(rawToChar(data$content))
    occupied <- jsonchecker(data)
    if (occupied != c()){
      protocol <- str_sub(strsplit(link,'/')[[1]][4],end=-6)
      outputtags <- list(outputtags,h3(str_to_title(protocol)))
      for (e in occupied){
        if (e == 'govStakings'){
          outputtags <- list(outputtags,h4('Governance Stakings'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,'Governance Stakings')))
          outputtables[[paste('terra',protocol,'Governance Stakings')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(price = as.double(price)) %>% mutate(balanceUSD = price*balance)
        }
        
        if (e == 'savings'){
          outputtags <- list(outputtags,h4('Savings'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,'Savings')))
          outputtables[[paste('terra',protocol,'Savings')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
        }
        
        if (e == 'delegated'){
          validatornum <- length(data[[e]][['validatorName']])
          outputtags <- list(outputtags,h4('Delegator Stakings'))
          for (i in seq(from=1, to= validatornum)){
            
            outputtags <- list(outputtags,h5(data[[e]][['validatorName']][i]))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('terra', 'protocol','Delegator Stakings',toString(i))))
            vaildatordata <- as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(rewardsbalance = data[[e]][['rewards']][[i]]['balance']) %>% mutate(balanceUSD = price*balance + price*rewardsbalance)
            outputtables[[paste('terra','protocol','Delegator Stakings',toString(i))]] <- validatordata
            
          }
          
        }
        
        if (e == 'undelegated'){
          # need to find examples pending
        }
        
        if (e == 'ancAirdrops'){
          outputtags <- list(outputtags, h4('Anchor Airdrops'))
          outputtags <- list(outputtags, DT::dataTableOutput(paste('terra',protocol,'Anchor Airdrops')))
          otuputtables[[paste('terra',protocol,'Anchor Airdrops')]] <- as.data.frame(data[[e]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price * balance)
        }
        
        if (e == 'mirAirdrops'){
          outputtags <- list(outputtags, h4('Mirror Airdrops'))
          outputtags <- list(outputtags, DT::dataTableOutput(paste('terra',protocol,'Mirror Airdrops')))
          otuputtables[[paste('terra',protocol,'Mirror Airdrops')]] <- as.data.frame(data[[e]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price * balance)
        }
        
        if (e == 'farms'){ 
          outputtags <- list(outputtags, h4('Farms'))
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
            outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('terra',protocol,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]])  %>% select(-id,-logo,-decimals,-address,-hodl) %>% mutate(balance = as.double(balance)) %>% mutate(price = as.double(price)) %>% mutate(balanceUSD = price*balance)
            if(eval(parse(paste0("data[[e]][['rewards']][[i]]",'$balance'))) != NULL){
              outputtags <- list(outputtags,h5('Tokens Rewards'))
              outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('terra',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-id,-logo,-address,-decimals)  %>% mutate(balance = as.double(balance)) %>% mutate(price = as.double(price)) %>% mutate(balanceUSD = price*balance)
            }
          }
        }
        
        
        if (e == 'positions'){
          outputtags <- list(outputtags, h4('Positions'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(data[[e]][['tokens']][[i]]['symbol'])==2){
              pair <- paste0(paste0(data[[e]][['tokens']][[i]]['symbol'][[1]],'/'),data[[e]][['tokens']][[i]]['symbol'][[2]])
              outputtags <- list(outputtags,h4(pair))
            }
            else{
              outputtags <- list(outputtags,h5(data[[e]][['tokens']][[i]]['symbol'][[1]]))
            }
            outputtags <- list(outputtags,h5('Tokens Staked'))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('terra',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]])  %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
            
          }
        }
        
        if (e == 'borrows'){
          outputtags <- list(outputtags,h4('Borrowings'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,'Borrowings')))
          outputtables[[paste('terra',protocol,'Borrowings')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = -price*balance)
          outputtags <- list(outputtags,h4('Rewards'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,'Borrowings','Rewards')))
          outputtables[[paste('terra',protocol,'Borrowings','Rewards')]] <- ldply(data[[e]][['rewards']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
        }
        
        if (e == 'deposits'){
          outputtags <- list(outputtags,h4('Deposits'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,'Deposits')))
          outputtables[[paste('terra',protocol,'Deposits')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
        }
        
      }
    }
  }
  
  }
  
  
  
  
  
  ############ Solana #################
  if (!is.na(solana)){
  
  solanawallet <- GET(paste0('https://api.apeboard.finance/wallet/solana/',solana))
  solanawallet <- fromJSON(rawToChar(solanawallet$content))
  outputtags <- list(outputtags,h2('Solana Network'))
  if (solanawallet != list()){
    solanawallet <- as.data.frame(solanawallet) %>% select(-id,-logo,-decimals,-address) %>% mutate(balanceUSD = price*balance)
    
    outputtags <- list(outputtags,h3('Token Protocol'))
    outputtags <- list(outputtags, DT::dataTableOutput(paste('Solana','Token Protocol')) )
    outputtables[[paste('Solana','Token Protocol')]] <- solanawallet
  }
  
  
  
  
  for (link in apirepo$solana){
    data <- GET(paste0(link,solana))
    data <- fromJSON(rawToChar(data$content))
    occupied <- jsonchecker(data)
    if (occupied != c()){
      protocol <- str_sub(strsplit(link,'/')[[1]][4],end=-7)
      outputtags <- list(outputtags,h3(str_to_title(protocol)))
      for (e in occupied){
        if (e == 'farms'){ 
          outputtags <- list(outputtags, h4('Farms'))
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
            outputtags <- list(outputtags,DT::dataTableOutput(paste('solana',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('solana',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]])  %>% select(-logo,-address)  %>% mutate(balanceUSD = price*balance)
            if(eval(parse(paste0("data[[e]][['rewards']][[i]]",'$balance'))) != NULL){
              outputtags <- list(outputtags,h5('Tokens Rewards'))
              outputtags <- list(outputtags,DT::dataTableOutput(paste('solana',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('solana',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
            }
          }
        }
        
        
        if (e == 'positions'){
          outputtags <- list(outputtags, h4('Positions'))
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
            outputtags <- list(outputtags,DT::dataTableOutput(paste('solana',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('solana',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]])  %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
            
          }
        }
        
        if (e == 'fusion'){
          outputtags <- list(outputtags, h4('Fusion'))
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
            outputtags <- list(outputtags,DT::dataTableOutput(paste('solana',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('solana',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]])  %>% select(-logo,-address)  %>% mutate(balanceUSD = price*balance)
            if(eval(parse(paste0("data[[e]][['rewards']][[i]]",'$balance'))) != NULL){
              outputtags <- list(outputtags,h5('Tokens Rewards'))
              outputtags <- list(outputtags,DT::dataTableOutput(paste('solana',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('solana',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
            }
          }
          
        }
        
      }
    }
  }
  
  }


return(list(tags=outputtags,tables=outputtables))  


}
