library(tidyverse)
library(lubridate)
if(!'data.table' %in% installed.packages()[,1]) install.packages('data.table')

# Function to fetch google stock data
google_stocks <- function(sym, current = TRUE, sy = 2005, sm = 1, sd = 1, ey, em, ed)
{
  # sy, sm, sd, ey, em, ed correspond to
  # start year, start month, start day, end year, end month, and end day
  
  # If TRUE, use the date as the enddate
  if(current){
    system_time <- as.character(Sys.time())
    ey <- as.numeric(substr(system_time, start = 1, stop = 4))
    em <- as.numeric(substr(system_time, start = 6, stop = 7))
    ed <- as.numeric(substr(system_time, start = 9, stop = 10))
  }
  
  require(data.table)
  #http://finance.google.com/finance/historical?q=AAPL&startdate=Oct+18+2012&enddate=Oct+13+2017&output=csv
  # Fetch data from google
  google_out = tryCatch(
    suppressWarnings(
      fread(paste0("http://finance.google.com/finance/historical",
                   "?q=", sym,
                   "&startdate=", paste(sm, sd, sy, sep = "+"),
                   "&enddate=", paste(em, ed, ey, sep = "+"),
                   "&output=csv"), sep = ",")),
    error = function(e) NULL
  )
  
  # If successful, rename first column
  if(!is.null(google_out)){
    names(google_out)[1] = "Date"
  }
  
  return(google_out)
}

# Test it out
#http://finance.google.com/finance?q=SWX%3AVW&sq=vw&sp=2&ei=LR-AWtjrGNSMswGKw5moBQ
apple_data = google_stocks('AAPL')
vw_data = google_stocks('SWX%3AVW')
tesla_data = google_stocks('tsla')
dai_data = google_stocks('DAI')
ibm_data = google_stocks('ibm')

apple_label <- rep("Apple", length(apple_data$Date))
apple_data$label <- apple_label

vw_label <- rep("VW", length(vw_data$Date))
vw_data$label <- vw_label

daimler_label <- rep("Daimler", length(dai_data$Date))
dai_data$label <- daimler_label

tesla_label <- rep("Tesla", length(tesla_data$Date))
tesla_data$label <- tesla_label

ibm_label <- rep("IBM", length(ibm_data$Date))
ibm_data$label <- ibm_label

shares_dac <- rbind(vw_data, dai_data, apple_data, ibm_data, tesla_data)
shares_dac$datum <- as.factor(shares_dac$Date)

shares_dax_order <- shares_dac[with(shares_dac, order(datum)),]
save(shares_dax_order, file="shares_dax_order.rda")
load("shares_dax_order.rda")

shares_dax_order$YM <- format(dmy(shares_dax_order$Date), "%Y-%m")


#plotting
ggplot(shares_dax_order, aes(Date, Close, color = label)) +geom_line() + guides(color=guide_legend("Marke"))

shares_dax_order$YM <- format(shares_dax_order$Date, "%Y-%m")
ggplot(shares_dax_order, aes(YM, Close)) +geom_boxplot()
p <- ggplot(shares_dax_order, aes(YM, Close, fill=label)) 
p <- p + geom_boxplot() 
p <- p + theme(axis.text.x = element_text(angle=45))
p <- p + scale_x_discrete(name ="Zeitraum")
p <- p + scale_y_discrete(name ="Schlusskurs")
p

ggsave("shares_order.png", width = 30, height = 20, units = "cm")


ggplot(apple_data, aes(Date, Close, group =1)) + geom_line() 
apple_data$YM <- format(dmy(apple_data$Date), "%Y-%m")
ggplot(apple_data, aes(YM, Close)) + geom_boxplot()

