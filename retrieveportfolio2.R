library(logger)




getportfolio2 <- function(terra,solana){
  # retrieves and output list of the relevant portfolio of the particular addrress string input
  # **Sources**
  # Terra: Apeboard, Solana: Apeboard
  # Subject to future changes depending on provider coverage
  
  
  outputtags <- tagList() # list of tags to be outputted
  outputtables <- hash() # list of data tables to be referenced at last part of function
  networth <- is.numeric(0)
  
  apirepo <- read.csv('API.csv',header=TRUE)
  colnames(apirepo) <- c('bsc','matic','terra','solana')
  
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
  
  
  ############ Apeboard Networks #############
  
  ############# Terra ################
  log_info('Handling Apeboard Terra wallet')
  if (!is.na(terra)) {
  terrawallet <- GET(paste0('https://api.apeboard.finance/wallet/terra/',terra))
  terrawallet <- fromJSON(rawToChar(terrawallet$content))
  outputtags <- list(outputtags,h2('Terra Network'))
  if (length(terrawallet) != 0){
    terrawallet <- as.data.frame(terrawallet) %>% select(-id,-logo,-decimals,-address) %>% mutate(balanceUSD = price*balance)
    outputtags <- list(outputtags,h3('Token Protocol'))
    outputtags <- list(outputtags, DT::dataTableOutput(paste('Terra','Token Protocol')) )
    outputtables[[paste('Terra','Token Protocol')]] <- terrawallet
    networth <- networth + sum(terrawallet$balanceUSD)
  }
  log_info('Handling Apeboard Terra wallet completed')
  
  
  for (link in (apirepo$terra %>% na_if("") %>% na.omit)){
    log_info('Getting Apeboard Terra {link}')
    data <- GET(paste0(link,terra))
    data <- fromJSON(rawToChar(data$content))
    occupied <- jsonchecker(data)
    log_info('Checking Apeboard Terra {link} with occupied data: {occupied}')
    if (length(occupied) != 0){
      protocol <- str_sub(strsplit(link,'/')[[1]][4],end=-6)
      outputtags <- list(outputtags,h3(str_to_title(protocol)))
      log_info('Handling Apeboard Terra {protocol}')
      for (e in occupied){
        if (e == 'govStakings'){
          log_info('Handling Apeboard Terra {protocol} {e}')
          outputtags <- list(outputtags,h4('Governance Stakings'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,'Governance Stakings')))
          outputtables[[paste('terra',protocol,'Governance Stakings')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(price = as.double(price)) %>% mutate(balanceUSD = price*balance)
          networth <- networth + sum(outputtables[[paste('terra',protocol,'Governance Stakings')]]$balanceUSD)
          log_info('Handling Apeboard Terra {protocol} {e} completed')
        }
        
        if (e == 'savings'){
          log_info('Handling Apeboard Terra {protocol} {e}')
          outputtags <- list(outputtags,h4('Savings'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,'Savings')))
          outputtables[[paste('terra',protocol,'Savings')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
          networth <- networth + sum(outputtables[[paste('terra',protocol,'Savings')]]$balanceUSD)
          log_info('Handling Apeboard Terra {protocol} {e} completed')
        }
        
        if (e == 'delegated'){
          log_info('Handling Apeboard Terra {protocol} {e}')
          validatornum <- length(data[[e]][['validatorName']])
          outputtags <- list(outputtags,h4('Delegator Stakings'))
          for (i in seq(from=1, to= validatornum)){
            outputtags <- list(outputtags,h5(data[[e]][['validatorName']][i]))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('terra', 'protocol','Delegator Stakings',toString(i))))
            vaildatordata <- as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(rewardsbalance = data[[e]][['rewards']][[i]]['balance']) %>% mutate(balanceUSD = price*balance + price*rewardsbalance)
            outputtables[[paste('terra','protocol','Delegator Stakings',toString(i))]] <- validatordata
            networth <- networth + sum(validatordata$balanceUSD)
          }
          log_info('Handling Apeboard Terra {protocol} {e} completed')
        }
        
        if (e == 'undelegated'){
          # need to find examples pending
        }
        
        if (e == 'ancAirdrops'){
          log_info('Handling Apeboard Terra {protocol} {e}')
          outputtags <- list(outputtags, h4('Anchor Airdrops'))
          outputtags <- list(outputtags, DT::dataTableOutput(paste('terra',protocol,'Anchor Airdrops')))
          otuputtables[[paste('terra',protocol,'Anchor Airdrops')]] <- as.data.frame(data[[e]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price * balance)
          networth <- networth + sum(outputtables[[paste('terra',protocol,'Anchor Airdrops')]]$balanceUSD)
          log_info('Handling Apeboard Terra {protocol} {e} completed')
        }
        
        if (e == 'mirAirdrops'){
          log_info('Handling Apeboard Terra {protocol} {e}')
          outputtags <- list(outputtags, h4('Mirror Airdrops'))
          outputtags <- list(outputtags, DT::dataTableOutput(paste('terra',protocol,'Mirror Airdrops')))
          otuputtables[[paste('terra',protocol,'Mirror Airdrops')]] <- as.data.frame(data[[e]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price * balance)
          networth <- networth + sum(outputtables[[paste('terra',protocol,'Mirror Airdrops')]]$balanceUSD)
          log_info('Handling Apeboard Terra {protocol} {e}')
        }
        
        if (e == 'farms'){ 
          log_info('Handling Apeboard Terra {protocol} {e}')
          outputtags <- list(outputtags, h4('Farms'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(data[[e]][['tokens']][[i]][['symbol']])==2){
              pair <- paste(data[[e]][['tokens']][[i]][['symbol']][[1]],'/',data[[e]][['tokens']][[i]][['symbol']][[2]])
              outputtags <- list(outputtags,h4(pair))
            }
            else{
              outputtags <- list(outputtags,h4(data[[e]][['tokens']][[i]][['symbol']][[1]]))
            }
            outputtags <- list(outputtags,h5('Tokens Staked'))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('terra',protocol,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]])  %>% select(-id,-logo,-decimals,-address,-hodl) %>% mutate(balance = as.double(balance)) %>% mutate(price = as.double(price)) %>% mutate(balanceUSD = price*balance)
            networth <- networth + sum(outputtables[[paste('terra',protocol,e,pair,'Tokens Staked')]]$balanceUSD)
            log_info('Handling Apeboard Terra {protocol} {e} tokens staked data completed')
            if(exists(paste0("data[[e]][['rewards']][[i]]",'$balance'))){
              outputtags <- list(outputtags,h5('Tokens Rewards'))
              outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('terra',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-id,-logo,-address,-decimals)  %>% mutate(balance = as.double(balance)) %>% mutate(price = as.double(price)) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('terra',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard Terra {protocol} {e} tokens rewards completed')
            }
          }
          log_info('Handling Apeboard Terra {protocol} {e} overall data completed')
        }
        
        
        if (e == 'positions'){
          log_info('Handling Apeboard Terra {protocol} {e}')
          outputtags <- list(outputtags, h4('Positions'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(data[[e]][['tokens']][[i]][['symbol']])==2){
              pair <- paste(data[[e]][['tokens']][[i]][['symbol']][[1]],'/',data[[e]][['tokens']][[i]][['symbol']][[2]])
              outputtags <- list(outputtags,h4(pair))
            }
            else{
              outputtags <- list(outputtags,h5(data[[e]][['tokens']][[i]][['symbol']][[1]]))
            }
            outputtags <- list(outputtags,h5('Tokens Staked'))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('terra',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]])  %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
            networth <- networth + sum(outputtables[[paste('terra',protocol,e,pair,'Tokens Staked')]]$balanceUSD)
            log_info('Handling Apeboard Terra {protocol} {e} tokens staked completed')
            
          }
          log_info('Handling Apeboard Terra {protocol} {e} overall data completed')
        }
        
        if (e == 'borrows'){
          log_info('Handling Apeboard Terra {protocol} {e}')
          outputtags <- list(outputtags,h4('Borrowings'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,'Borrowings')))
          outputtables[[paste('terra',protocol,'Borrowings')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = -price*balance)
          networth <- networth + sum(outputtables[[paste('terra',protocol,'Borrowings')]]$balanceUSD)
          outputtags <- list(outputtags,h4('Rewards'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,'Borrowings','Rewards')))
          outputtables[[paste('terra',protocol,'Borrowings','Rewards')]] <- ldply(data[[e]][['rewards']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
          networth <- networth + sum(outputtables[[paste('terra',protocol,'Borrowings','Rewards')]]$balanceUSD)
          log_info('Handling Apeboard Terra {protocol} {e} overall data completed')
        }
        
        if (e == 'deposits'){
          log_info('Handling Apeboard Terra {protocol} {e}')
          outputtags <- list(outputtags,h4('Deposits'))
          outputtags <- list(outputtags,DT::dataTableOutput(paste('terra',protocol,'Deposits')))
          outputtables[[paste('terra',protocol,'Deposits')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
          networth <- networth + sum(outputtables[[paste('terra',protocol,'Deposits')]]$balanceUSD)
          log_info('Handling Apeboard Terra {protocol} {e} overall data completed')
        }
        
      }
    }
  }
  
  }
  log_info('Handling Apeboard Terra overall data completed!')
  
  
  
  
  
  ############ Solana #################
  if (!is.na(solana )){
  log_info('Handling Apeboard Solana Wallet')
  solanawallet <- GET(paste0('https://api.apeboard.finance/wallet/solana/',solana))
  solanawallet <- fromJSON(rawToChar(solanawallet$content))
  outputtags <- list(outputtags,h2('Solana Network'))
  if (length(solanawallet) != 0){
    solanawallet <- as.data.frame(solanawallet) %>% select(-id,-logo,-decimals,-address) %>% mutate(balanceUSD = price*balance)
    outputtags <- list(outputtags,h3('Token Protocol'))
    outputtags <- list(outputtags, DT::dataTableOutput(paste('Solana','Token Protocol')))
    outputtables[[paste('Solana','Token Protocol')]] <- solanawallet
    networth <- networth + sum(solanawallet$balanceUSD)
  }
  log_info('Handling Apeboard Solana Wallet Completed')
  
  
  
  for (link in (apirepo$solana %>% na_if("") %>% na.omit)){
    log_info('Getting Apeboard Solana {link}')
    data <- GET(paste0(link,solana))
    data <- fromJSON(rawToChar(data$content))
    occupied <- jsonchecker(data)
    log_info('Checking Apeboard Solana {link} with occupied data: {occupied}')
    if (occupied != c()){
      protocol <- str_sub(strsplit(link,'/')[[1]][4],end=-7)
      outputtags <- list(outputtags,h3(str_to_title(protocol)))
      log_info('Handling Apeboard Solana {protocol}')
      for (e in occupied){
        if (e == 'farms'){ 
          log_info('Handling Apeboard Solana {protocol} {e}')
          outputtags <- list(outputtags, h4('Farms'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(data[[e]][['tokens']][[i]][['symbol']])==2){
              pair <- paste(data[[e]][['tokens']][[i]][['symbol']][[1]],'/',data[[e]][['tokens']][[i]][['symbol']][[2]])
              outputtags <- list(outputtags,h4(pair))
            }
            else{
              outputtags <- list(outputtags,h4(data[[e]][['tokens']][[i]][['symbol']][[1]]))
            }
            outputtags <- list(outputtags,h5('Tokens Staked'))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('solana',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('solana',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]])  %>% select(-logo,-address)  %>% mutate(balanceUSD = price*balance)
            networth <- networth + sum(outputtables[[paste('solana',protocol,e,pair,'Tokens Staked')]]$balanceUSD)
            log_info('Handling Apeboard Solana {protocol} {e} tokens staked data completed')
            if(exists(paste0("data[[e]][['rewards']][[i]]",'$balance'))){
              outputtags <- list(outputtags,h5('Tokens Rewards'))
              outputtags <- list(outputtags,DT::dataTableOutput(paste('solana',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('solana',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('solana',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard Solana {protocol} {e} tokens rewards data completed')
            }
          }
          log_info('Handling Apeboard Solana {protocol} {e} overall data completed')
        }
        
        
        if (e == 'positions'){
          log_info('Handling Apeboard Solana {protocol} {e}')
          outputtags <- list(outputtags, h4('Positions'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(data[[e]][['tokens']][[i]][['symbol']])==2){
              pair <- paste(data[[e]][['tokens']][[i]][['symbol']][[1]],'/',data[[e]][['tokens']][[i]][['symbol']][[2]])
              outputtags <- list(outputtags,h4(pair))
            }
            else{
              outputtags <- list(outputtags,h4(data[[e]][['tokens']][[i]][['symbol']][[1]]))
            }
            outputtags <- list(outputtags,h5('Tokens Staked'))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('solana',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('solana',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]])  %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
            networth <- networth + sum(outputtables[[paste('solana',protocol,e,pair,'Tokens Staked')]]$balanceUSD)
          }
          log_info('Handling Apeboard Solana {protocol} {e} overall data completed')
        }
        
        if (e == 'fusion'){
          log_info('Handling Apeboard Solana {protocol} {e}')
          outputtags <- list(outputtags, h4('Fusion'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(data[[e]][['tokens']][[i]][['symbol']])==2){
              pair <- paste(data[[e]][['tokens']][[i]][['symbol']][[1]],'/',data[[e]][['tokens']][[i]][['symbol']][[2]])
              outputtags <- list(outputtags,h4(pair))
            }
            else{
              outputtags <- list(outputtags,h4(data[[e]][['tokens']][[i]][['symbol']][[1]]))
            }
            outputtags <- list(outputtags,h5('Tokens Staked'))
            outputtags <- list(outputtags,DT::dataTableOutput(paste('solana',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('solana',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]])  %>% select(-logo,-address)  %>% mutate(balanceUSD = price*balance)
            networth <- networth + sum(outputtables[[paste('solana',protocol,e,pair,'Tokens Staked')]]$balanceUSD)
            log_info('Handling Apeboard Solana {protocol} {e} tokens staked data completed')
            if(exists(paste0("data[[e]][['rewards']][[i]]",'$balance'))){
              outputtags <- list(outputtags,h5('Tokens Rewards'))
              outputtags <- list(outputtags,DT::dataTableOutput(paste('solana',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('solana',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('solana',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard Solana {protocol} {e} tokens rewards data completed')
            }
          }
          log_info('Handling Apeboard Solana {protocol} {e} overall data completed')
        }
        
      }
    }
  }
  
  
  }

log_info('Handling Apeboard Solana overall data completed')
return(hash(tags=outputtags,tables=outputtables,net=networth))
}
