ExtractData <- function(file_name) {
  #import .csv file
  import <- read.csv(file_name, header = FALSE)
  
  #assign names to the value of the columns of the dataframe
  names(import) <- c("TimeStamp", "Symbol", "Quantity", "Price")
  
  return(import)
}

#function for returning the maximum time gap for a given symbol
MaxTimeGap <- function(file, symbol) {

  #extract time stamps of a given symbol
  time_stamp <- file[with(file, file$Symbol == symbol),]$TimeStamp
  time_stamp_def1 <- c(time_stamp[1], time_stamp)
  time_stamp <- c(time_stamp, time_stamp[length(time_stamp)])
  
  #calculate the largest difference between the time stamps
  max_time_gap <- max(time_stamp - time_stamp_def1)
  
  return(max_time_gap)
}

#function for extracting the total volume for sales of a symbol
Volume <- function(file, symbol) {
  
  #extract quantities of sales for a given symbol
  quantities <- file[with(file, file$Symbol == symbol),]$Quantity
  #take the sum across all quantities
  volume <- sum(quantities)
  return(volume)
  
}

#function for extracting the maximum sales price of a symbol
MaxTradePrice <- function(file, symbol) {
  
  prices <- file[with(file, file$Symbol == symbol),]$Price
  max_trade_price <- max(prices)
  return(max_trade_price)
  
}

#function for calculating the weighted average price of a symbol
WeightedAveragePrice <- function(file, symbol) {
  
  #take the ordered cross-product of a symbol's quantity and prices
  quantities <- file[with(file, file$Symbol == symbol),]$Quantity
  prices <- file[with(file, file$Symbol == symbol),]$Price

  exchange <- crossprod(quantities, prices)
  #divide this total exchange amount by the volume of the symbol traded
  volume <- sum(quantities)
  
  weighted_average_price <- trunc(exchange/volume)
  
  return(weighted_average_price)
  
}

#function which returns a .csv file of desired data given an input's filename
Output <- function(file_name) {
  
  #extract .csv data
  file <- ExtractData(file_name)
  #transform data into character and numeric types
  for (i in 1:ncol(file)) {
    file[,i] <- sapply(file[,i], as.character)
  }
  for (i in c(1,3,4)) {
    file[,i] <- sapply(file[,i], as.numeric)
  }
  
  unique_symbols <- unique(file$Symbol)
  
  output <- NULL
  
  #generate desired data
  for (i in unique_symbols) {
    vector <- c(i, MaxTimeGap(file, i), Volume(file, i), MaxTradePrice(file, i), WeightedAveragePrice(file, i))
    output <- rbind(output, vector)
  }
  
  output <- as.data.frame(output)
  
  #transform data into character and numeric data types and format the output file
  for (i in 1:ncol(file)) {
    output[,i] <- sapply(output[,i], as.character)
  }
  for (i in 2:5) {
    output[,i] <- sapply(output[,i], as.numeric)
  }
  rownames(output) <- NULL
  names(output) <- c("Symbol", "MaxTimeGap", "Volume", "MaxTradePrice", "WeightedAveragePrice")
  
  write.csv(output, "output.csv", row.names = FALSE)
  
}
