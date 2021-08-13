library(logger)



getportfolio1 <- function(wallet){
  # retrieves and output list of the relevant portfolio of the particular addrress string input
  # **Sources**
  # Ethereum: Zapper, BSC: Apeboard; some from Zapper, Polygon: Apeboard,
  # Fantom: Zapper, Optimism: Zapper
  # Subject to future changes depending on provider coverage
  
  outputtags <- tagList() # list of tags to be outputted
  outputtables <- hash() # list of data tables to be referenced at last part of function
  networth <- is.numeric(0)
  zapper <- c('ethereum','xdai','optimism','fantom') #zapper use
  staketype <- c('masterchef','geyser','gauge','single-staking') #zapper use
    
  
  
  
  ######### zapper networks ############
  supportedlink <- paste0(paste0('https://api.zapper.fi/v1/protocols/balances/supported?addresses%5B%5D=',wallet),'&api_key=96e0cc51-a62e-42ca-acee-910ea7d2a241')
  supported <- GET(supportedlink)
  zappersupported <- fromJSON(rawToChar(supported$content))
  log_info('Zapper scrap successful!')
  for (nw in zapper) {
  #ethindex <- which(zappersupported$network == nw)
  if (nw %in% zappersupported$network){
    ethindex <- which(zappersupported$network == nw)
    outputtags <- tagAppendChild(outputtags, h2(paste0(str_to_title(nw),' Network')))
    log_info('Handling Zapper {nw} Network now!')
    for (e in ldply(zappersupported$protocols[ethindex], data.frame)$protocol){
      datalink <- paste('https://api.zapper.fi/v1/protocols/',e,'/balances?addresses%5B%5D=',wallet,'&network=',nw,'&api_key=96e0cc51-a62e-42ca-acee-910ea7d2a241',sep='')
      log_info('Getting Zapper {nw} Network: {e}!')
      data <- fromJSON(rawToChar(GET(datalink)$content))
      cond <- eval(parse(text=sprintf("data$'%s'$products",wallet)))
      log_info('Checking Zapper {nw} Network: {e}!')
      if (length(cond) != 0) {
      log_info('Handling Zapper {nw} Network: {e}!')
      outputtags <- tagAppendChild(outputtags, h3(paste0(str_to_title(e),' Protocol')))
      outputtables[[paste0(e,nw)]] <- ldply(eval(parse(text=sprintf("data$'%s'$products$assets",wallet))),data.frame) %>% select(type,category,symbol,price,balance,balanceUSD)
      networth <- networth + sum(outputtables[[paste0(e,nw)]]$balanceUSD)
      outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste0(e,nw)))
      log_info('Handling Zapper {nw} Network: {e} is successful!')
      }
    }}} 
  
  
  prevnet <- 1 # for zapper staking protocols
  prevfarm <- 1 # for zapper staking protocols
  
  for (n in zapper){
    for (s in staketype){
      log_info('Getting Zapper {n} Network Stake Type {s}!')
      datalink <- sprintf('https://api.zapper.fi/v1/staked-balance/%s?addresses%s=%s&api_key=96e0cc51-a62e-42ca-acee-910ea7d2a241&network=%s',s,'%5B%5D',wallet,n)
      data <- fromJSON(rawToChar(GET(datalink)$content))
      log_info('Checking Zapper {n} Network Stake Type {s}!')
      if (length(eval(parse(text=sprintf("data$'%s'",wallet)))) != 0 ){
        log_info('Handling Zapper {n} Network Stake Type {s}!')
        if (n != prevnet){
          outputtags <- tagAppendChild(outputtags,h2(paste0(str_to_title(n),'Network (Staking)')))
          prevnet <- n
        }
        rows <- as.numeric(length(eval(parse(text=sprintf("data$'%s'$label",wallet)))))
        newdata <- eval(parse(text=sprintf("data$'%s'",wallet)))
        for (i in seq(from = 1, to = rows)){
          if (newdata$protocolDisplay[[i]] != prevfarm){
            outputtags <- tagAppendChild(outputtags,h3(str_to_title(newdata$protocolDisplay[[i]])))
            prevfarm <- newdata$protocolDisplay[[i]]
          }
          outputtags <- tagAppendChild(outputtags,h4(newdata$symbol[[i]]))
          outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
          outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste(n,s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Tokens Staked' )))
          outputtables[[paste(n,s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Tokens Staked')]] <- as.data.frame(newdata$tokens[[i]]) %>% select(symbol,price,balance,balanceUSD)
          networth <- networth + sum(as.data.frame(newdata$tokens[[i]])$balanceUSD)
          outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
          outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste(n,s,newdata$protocolDisplay[[i]],newdata$symbol[[i]], 'Token Rewards' )))
          outputtables[[paste(n,s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Token Rewards' )]] <- as.data.frame(newdata$rewardTokens[[i]]) %>% select(symbol,price,balance,balanceUSD)
          networth <- networth + sum(as.data.frame(newdata$rewardTokens[[i]])$balanceUSD)
        }
        log_info('Handling Zapper {n} Network Stake Type {s} successful!')
      }
      else{
        log_info('Nothing in Zapper {n} Network Stake Type {s}!')
      }
    }
  }
  
  
  
  
  
  ########## apeboard networks ############
  apirepo <- read.csv('API.csv',header=TRUE)
  colnames(apirepo) <- c('bsc','matic','terra','solana')
  credentials <- add_headers(origin= 'https://apeboard.finance',            #for apeboard api access credentials
                             passcode= '5a102a34f60fa7ec9d643a8a0e72cab9', 
                             referer= 'https://apeboard.finance/')
  
  log_info('Reading API.csv file successful!')
  
  ######### Binance Smart Chain############
  log_info('Handling Apeboard BSC Wallet!')
  bscwallet <- GET(paste0("https://api.apeboard.finance/wallet/bsc/",wallet),credentials)
  bscwallet <- fromJSON(rawToChar(bscwallet$content)) 
  bscwallet <- as.data.frame(bscwallet) %>% select(-logo,-decimals,-address,-extensions,-source,-createdAt,-modifiedAt) %>% mutate(balanceUSD = price*balance)
  outputtags <- tagAppendChild(outputtags,h2('Binance-Smart-Chain Network'))
  outputtags <- tagAppendChild(outputtags,h3('Token Protocol'))
  outputtags <- tagAppendChild(outputtags, DT::dataTableOutput(paste('Binance-Smart-Chain','Token Protocol')))
  outputtables[[paste('Binance-Smart-Chain','Token Protocol')]] <- bscwallet
  networth <- networth + sum(bscwallet$balanceUSD)
  log_info('Handling Apeboard BSC Wallet successful!')
  
  # function to check if json response empty
  jsonchecker <- function(data){
    elements <- names(data)
    not_empty <- c()
    for (i in elements){
      if (length(data[[i]]) != 0){
        not_empty <- c(not_empty,i) #json data exist
      }
    }
    return(not_empty) #json data empty
  } 
  
  for (link in apirepo$bsc){
    log_info('Handling Apeboard BSC {link}')
    data <- GET(paste0(link,wallet),credentials)
    data <- fromJSON(rawToChar(data$content))
    occupied <- jsonchecker(data)
    log_info('Checking Apeboard BSC {link} with occupied data {occupied}')
    if (length(occupied) != 0){
      protocol <- str_sub(strsplit(link,'/')[[1]][4],end=-4)
      outputtags <- tagAppendChild(outputtags,h3(str_to_title(protocol)))
      log_info('Handling Apeboard BSC {protocol}')
      for (e in occupied){
        if (e == 'vaults'){
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Vaults'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              pair <- paste(tokens[['symbol']][[1]],'/',tokens[['symbol']][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              pair <- tokens[['symbol']][[1]]
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Staked'))) 
            print(data[[e]][['tokens']][[i]])
            #as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            data1 <- as.data.frame(tokens) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]] <- data1
            networth <- networth + sum(outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]]$balanceUSD)
            log_info('Handling Apeboard BSC {protocol} {e} tokens staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]","$balance"))){
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard BSC {protocol} {e} tokens rewards data successful')
            }
          }
          log_info('Handling Apeboard BSC {protocol} {e} overall successful')
        }
        
        if (e == 'farms'){
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Farms'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              pair <- paste(tokens[['symbol']][[1]],'/',tokens[['symbol']][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              pair <- tokens[['symbol']][[1]]
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Staked'))) 
            outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(tokens) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            networth <- networth + sum(outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]]$balanceUSD)
            log_info('Handling Apeboard BSC {protocol} {e} tokens staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]","$balance" ))){
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard BSC {protocol} {e} tokens rewards data successful')
            }
          }
          log_info('Handling Apeboard BSC {protocol} {e} overall data successful')
        }
        
        if (e == 'lendings'){
          # need test example (e.g. alphahomora cannot find)
        }
        
        if (e == 'grazes'){
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Grazing'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              pair <- paste(tokens[['symbol']][[1]],'/',tokens['symbol'][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              pair <- tokens[['symbol']][[1]]
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Staked'))) 
            #as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            data1 <- as.data.frame(tokens) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]] <- data1
            networth <- networth + sum(data1$balanceUSD)
            log_info('Handling Apeboard BSC {protocol} {e} tokens staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]","$balance"))){
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard BSC {protocol} {e} tokens rewards data successful')
            }
          }
          log_info('Handling Apeboard BSC {protocol} {e} overall data successful')
        }
        
        if (e == 'stakings'){
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Stakings'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              pair <- paste(tokens[['symbol']][[1]],'/',tokens[['symbol']][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              pair <- tokens[['symbol']][[1]]
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Staked'))) 
            #as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            data1 <- as.data.frame(tokens) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]] <- data1
            networth <- networth + sum(data1$balanceUSD)
            log_info('Handling Apeboard BSC {protocol} {e} tokens staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]","$balance"))){
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard BSC {protocol} {e} tokens rewards data successful')
            }
          }
          log_info('Handling Apeboard BSC {protocol} {e} overall data successful')
        }
        
        if (e == 'positions'){
          # need test example (rewards for badgerbsc cannot find)
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Positions'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              pair <- paste(tokens[['symbol']][[1]],'/',tokens[['symbol']][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              pair <- tokens[['symbol']][[1]]
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Staked'))) 
            #print(data[[e]][['tokens']][[i]])
            #as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            data1 <- as.data.frame(tokens) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]] <- data1
            networth <- networth + sum(data1$balanceUSD)
            log_info('Handling Apeboard BSC {protocol} {e} tokens staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]","$balance"))){
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard BSC {protocol} {e} tokens rewards data successful')
            }
          }
          log_info('Handling Apeboard BSC {protocol} {e} overall data successful')
        }
        
        if (e == 'borrows'){
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Borrowings'))
          outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,'Borrowings')))
          outputtables[[paste('bsc',protocol,'Borrowings')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = -price*balance)
          networth <- networth + sum(outputtables[[paste('bsc',protocol,'Borrowings')]]$balanceUSD)
          log_info('Handling Apeboard BSC {protocol} {e} overall data successful')
        }
        
        if (e == 'deposits'){
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Deposits'))
          outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,'Deposits')))
          outputtables[[paste('bsc',protocol,'Deposits')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
          networth <- networth + sum(outputtables[[paste('bsc',protocol,'Deposits')]]$balanceUSD)
          log_info('Handling Apeboard BSC {protocol} {e} overall data successful')
        }
        
        if (e == 'stake'){
          # need test example ellipsis
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Stake'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              print('test61')
              pair <- paste(tokens[['symbol']][[1]],'/',tokens[['symbol']][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              print('test62')
              pair <- tokens[['symbol']][[1]]
              print('test62')
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Staked'))) 
            #print(data[[e]][['tokens']][[i]])
            #as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            data1 <- as.data.frame(tokens) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]] <- data1
            networth <- networth + sum(data1$balanceUSD)
            log_info('Handling Apeboard BSC {protocol} {e} tokens staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]","$balance"))){
              print('test8')
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard BSC {protocol} {e} tokens rewards data successful')
            }
          }
          log_info('Handling Apeboard BSC {protocol} {e} overalldata successful')
        }
        
        if (e == 'boardroom'){
          # need test example (e.g. MDEX)
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Boardroom'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              pair <- paste(tokens[['symbol']][[1]],'/',tokens[['symbol']][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              pair <- tokens[['symbol']][[1]]
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Staked'))) 
            #print(data[[e]][['tokens']][[i]])
            #as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            data1 <- as.data.frame(tokens) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]] <- data1
            networth <- networth + sum(data1$balanceUSD)
            log_info('Handling Apeboard BSC {protocol} {e} tokens staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]","$balance"))){
              print('test8')
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard BSC {protocol} {e} tokens rewards data successful')
            }
          }
          log_info('Handling Apeboard BSC {protocol} {e} overall data successful')
        }
        
        if (e == 'vault'){
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Vault'))
          outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e)))
          outputtables[[paste('bsc',protocol,e)]] <- as.data.frame(data[[e]][['tokens']]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
          networth <- networth + sum(outputtables[[paste('bsc',protocol,e)]]$balanceUSD)
          log_info('Handling Apeboard BSC {protocol} {e} overall data successful')
        }
        
        if (e == 'syrup'){
          # need test example (e,g. pancakeswap)???? ---- need further tests :(
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Syrup'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              pair <- paste(tokens[['symbol']][[1]],'/',tokens[['symbol']][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              pair <- tokens[['symbol']][[1]]
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Staked'))) 
            #print(data[[e]][['tokens']][[i]])
            #as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            data1 <- as.data.frame(tokens) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]] <- data1
            networth <- networth + sum(data1$balanceUSD)
            log_info('Handling Apeboard BSC {protocol} {e} tokens staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]","$balance"))){
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard BSC {protocol} {e} tokens rewards data successful')
            }
          }
          log_info('Handling Apeboard BSC {protocol} {e} overall data successful')
        }
        
        if (e == 'mintings'){
          # need test example (e.g. venus) ?????
        }
        
        if (e == 'locked'){
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Locked'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              pair <- paste(tokens[['symbol']][[1]],'/',tokens[['symbol']][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              pair <- tokens[['symbol']][[1]]
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Staked'))) 
            #print(data[[e]][['tokens']][[i]])
            #as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            data1 <- as.data.frame(tokens) %>% select(-address,-logo) %>% mutate(balanceUSD = as.numeric(price)*as.numeric(balance))
            outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]] <- data1
            networth <- networth + sum(data1$balanceUSD)
            log_info('Handling Apeboard BSC {protocol} {e} tokens staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]","$balance"))){
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = as.numeric(price)*as.numeric(balance))
              networth <- networth + sum(outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard BSC {protocol} {e} tokens rewards data successful')
            }
          }
          log_info('Handling Apeboard BSC {protocol} {e} overall data successful')
        }
        
        if (e == 'pools'){
          log_info('Handling Apeboard BSC {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Pools'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              pair <- paste(tokens[['symbol']][[1]],'/',tokens[['symbol']][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              pair <- tokens[['symbol']][[1]]
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Staked'))) 
            #print(data[[e]][['tokens']][[i]])
            #as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            data1 <- as.data.frame(tokens) %>% select(-address,-logo) %>% mutate(balanceUSD = price*balance)
            outputtables[[paste('bsc',protocol,e,pair,'Tokens Staked')]] <- data1
            networth <- networth + sum(data1$balanceUSD)
            log_info('Handling Apeboard BSC {protocol} {e} tokens staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]","$balance"))){
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('bsc',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard BSC {protocol} {e} tokens reward data successful')
            }
          }
          log_info('Handling Apeboard BSC {protocol} overall data successful')
        }
      }
    }
  } 
  
  
  bsczapper <- c('1inch','harvest','bzx') #for bsc protocols outside Apeboard
  for (e in bsczapper){
    log_info('Getting Zapper BSC {e}')
    datalink <- sprintf('https://api.zapper.fi/v1/protocols/%s/balances?addresses%s%s&network=%s&api_key=96e0cc51-a62e-42ca-acee-910ea7d2a241',e,'%5B%5D=',wallet,'binance-smart-chain')
    data <- fromJSON(rawToChar(GET(datalink)$content))
    log_info('Checking Zapper BSC {e}')
    if (exists(sprintf(text="data$'%s'$products",wallet))) {
      log_info('Handling Zapper BSC {e}')
      outputtags <- tagAppendChild(outputtags, h3(paste0(str_to_title(e),' Protocol')))
      outputtables[[paste0(e,'bsc')]] <- ldply(eval(parse(text=sprintf("data$'%s'$products$assets",wallet))),data.frame)
      networth <- networth + sum(outputtables[[paste(e,'bsc')]]$balanceUSD)
      outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste0(e,'bsc')))
      log_info('Handling Zapper BSC {e} successful')
    }
  }
    
    prevfarm <- 1
    
    for (s in staketype){
      log_info('Getting Zapper BSC stake type: {s}')
      datalink <- sprintf('https://api.zapper.fi/v1/staked-balance/%s?addresses%s=%s&api_key=96e0cc51-a62e-42ca-acee-910ea7d2a241&network=%s',s,'%5B%5D',wallet,'binance-smart-chain')
      data <- fromJSON(rawToChar(GET(datalink)$content))
      log_info('Checking Zapper BSC stake type {s}')
      if (length(eval(parse(text=sprintf("data$'%s'",wallet)))) != 0){
        log_info('Handling Zapper BSC stake type {s}')
        rows <- as.numeric(length(eval(parse(text=sprintf("data$'%s'$label",wallet)))))
        newdata <- eval(parse(text=sprintf("data$'%s'",wallet)))
        log_info('Checking if Zapper BSC networks in data...')
        for (i in seq(from = 1, to = rows)){
          if (newdata$protocol[[i]] %in% bsczapper){
            if (newdata$protocol[[i]] != prevfarm){
              outputtags <- tagAppendChild(outputtags,h3(str_to_title(newdata$protocolDisplay[[i]])))
              prevfarm <- newdata$protocol[[i]]
            }
            outputtags <- tagAppendChild(outputtags,h4(newdata$symbol[[i]]))
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Tokens Staked')))
            outputtables[[paste('bsc',s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Tokens Staked')]] <- ldply(newdata$tokens[[i]],data.frame) %>% select(symbol,price,balance,balanceUSD)
            networth <- networth + sum(outputtables[[paste('bsc',s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Tokens Staked')]]$balanceUSD)
            outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('bsc',s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Token Rewards')))
            outputtables[[paste('bsc',s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Token Rewards')]] <- ldply(newdata$rewardTokens[[i]],data.frame) %>% select(symbol,price,balance,balanceUSD)
            networth <- networth + sum(outputtables[[paste('bsc',s,newdata$protocolDisplay[[i]],newdata$symbol[[i]],'Tokens Rewards')]]$balanceUSD)
          }
        }
      }
      else{
        log_info('Nothing found in Zapper BSC stake type {s}')  
      }
      log_info('Handling Zapper BSC stake type {s} completed')
    }
  
  
   log_info('Handling BSC overall data completed!')
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############### Polygon #################
  log_info('Handling Apeboard Polygon wallet')
  maticwallet <- GET(paste0('https://api.apeboard.finance/wallet/matic/',wallet),credentials)
  maticwallet <- fromJSON(rawToChar(maticwallet$content))
  outputtags <- tagAppendChild(outputtags,h2('Polygon Network'))
  print(maticwallet)
  if (length(maticwallet) != 0){
  maticwallet <- as.data.frame(maticwallet) %>% select(-id,-logo,-decimals,-address) %>% mutate(balanceUSD = price*balance) 
  outputtags <- tagAppendChild(outputtags,h3('Token Protocol'))
  outputtags <- tagAppendChild(outputtags, DT::dataTableOutput(paste('Polygon','Token Protocol')))
  outputtables[[paste('Polygon','Token Protocol')]] <- maticwallet
  networth <- networth + sum(maticwallet$balanceUSD)
  }
  log_info('Handling Apeboard Polygon wallet completed')
  
  
  for (link in (apirepo$matic %>% na_if("") %>% na.omit)){
    log_info('Getting Apeboard Polygon {link}')
    data <- GET(paste0(link,wallet),credentials)
    data <- fromJSON(rawToChar(data$content))
    occupied <- jsonchecker(data)
    log_info('Checking Apeboard Polygon {link} with occupied data: {occupied}')
    if (length(occupied) != 0){
      protocol <- str_sub(strsplit(link,'/')[[1]][4],end=-6)
      outputtags <- tagAppendChild(outputtags,h3(str_to_title(protocol)))
      log_info('Handling Apeboard Polygon {protocol}')
      for (e in occupied){
        if (e == 'vaults'){
          # need test example polycat
          log_info('Handling Apeboard Polygon {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Vaults'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              pair <- paste(data[[e]][['tokens']][[i]][['symbol']][[1]],'/',data[[e]][['tokens']][[i]][['symbol']][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              pair <- data[[e]][['tokens']][[i]]['symbol'][[1]]
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('matic',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('matic',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
            networth <- networth + sum(outputtables[[paste('matic',protocol,e,pair,'Tokens Staked')]]$balanceUSD)
            log_info('Handling Apeboard Polygon {protocol} {e} token staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]",'$balance'))){
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('matic',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('matic',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('matic',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard Polygon {protocol} {e} token rewards data successful')
            }
          }
          log_info('Handling Apeboard Polygon {protocol} {e} overall data successful')
        }
        
        if (e == 'farms'){
          log_info('Handling Apeboard Polygon {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Farms'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              pair <- paste(data[[e]][['tokens']][[i]][['symbol']][[1]],'/',data[[e]][['tokens']][[i]][['symbol']][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              pair <- data[[e]][['tokens']][[i]]['symbol'][[1]]
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('matic',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('matic',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
            networth <- networth + sum(outputtables[[paste('matic',protocol,e,pair,'Tokens Staked')]]$balanceUSD)
            log_info('Handling Apeboard Polygon {protocol} {e} token staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]",'$balance'))){
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('matic',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('matic',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('matic',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard Polygon {protocol} {e} token rewards data successful')
            }
          }
          log_info('Handling Apeboard Polygon {protocol} {e} overall data successful')
        }
        
        
        if (e == 'positions'){
          # need test example quickswap
          log_info('Handling Apeboard Polygon {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h4('Positions'))
          amt <- length(data[[e]][['tokens']])
          for (i in seq(from=1, to=amt)){
            tokens <- data[[e]][['tokens']][[i]]
            if (length(tokens[['symbol']])==2){
              pair <- paste(data[[e]][['tokens']][[i]][['symbol']][[1]],'/',data[[e]][['tokens']][[i]][['symbol']][[2]])
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            else{
              pair <- data[[e]][['tokens']][[i]]['symbol'][[1]]
              outputtags <- tagAppendChild(outputtags,h4(pair))
            }
            outputtags <- tagAppendChild(outputtags,h5('Tokens Staked'))
            outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('matic',protocol,e,pair,'Tokens Staked')))
            outputtables[[paste('matic',protocol,e,pair,'Tokens Staked')]] <- as.data.frame(data[[e]][['tokens']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
            networth <- networth + sum(outputtables[[paste('matic',protocol,e,pair,'Tokens Staked')]]$balanceUSD)
            log_info('Handling Apeboard Polygon {protocol} {e} token staked data successful')
            if(exists(paste0("data[[e]][['rewards']][[i]]",'$balance'))){
              outputtags <- tagAppendChild(outputtags,h5('Tokens Rewards'))
              outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('matic',protocol,e,pair,'Tokens Rewards')))
              outputtables[[paste('matic',protocol,e,pair,'Tokens Rewards')]] <- as.data.frame(data[[e]][['rewards']][[i]]) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
              networth <- networth + sum(outputtables[[paste('matic',protocol,e,pair,'Tokens Rewards')]]$balanceUSD)
              log_info('Handling Apeboard Polygon {protocol} {e} token rewards data successful')
            }
          }
          log_info('Handling Apeboard Polygon {protocol} {e} overall data successful')
        }
        
        if (e == 'borrows'){
          log_info('Handling Apeboard Polygon {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h3('Borrowings'))
          outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('matic',protocol,'Borrowings')))
          outputtables[[paste('matic',protocol,'Borrowings')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = -price*balance)
          networth <- networth + sum(outputtables[[paste('matic',protocol,'Borrowings')]]$balanceUSD)
          log_info('Handling Apeboard Polygon {protocol} {e} overall data successful')
        }
        
        if (e == 'deposits'){
          log_info('Handling Apeboard Polygon {protocol} {e}')
          outputtags <- tagAppendChild(outputtags,h3('Deposits'))
          outputtags <- tagAppendChild(outputtags,DT::dataTableOutput(paste('matic',protocol,'Deposits')))
          outputtables[[paste('matic',protocol,'Deposits')]] <- ldply(data[[e]][['tokens']],data.frame) %>% select(-logo,-address) %>% mutate(balanceUSD = price*balance)
          networth <- networth + sum(outputtables[[paste('matic',protocol,'Deposits')]]$balanceUSD)
          log_info('Handling Apeboard Polygon {protocol} {e} overall data successful')
        }
        
      }
    }
  }   
log_info('Handling Apeboard Polygon overall data completed')  

return(hash(tags=outputtags,tables=outputtables,net=networth)) 
                     
}


 

########testing part##########3
#data <- getportfolio1('0x58bbae0159117a75225e72d941dbe35ffd99f894')
#data$tags

