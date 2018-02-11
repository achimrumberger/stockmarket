Stockmarket Google Finance
================

Get and plot historical stokmarket data from Google Finance
-----------------------------------------------------------

As the yahoo finance no longer seems to work, an alternative using google data.

``` r
# necessary libraries
library(data.table)
library(ggplot2)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday,
    ##     week, yday, year

    ## The following object is masked from 'package:base':
    ## 
    ##     date

Function to get google finance data
-----------------------------------

Use the 'fread' function from 'data.table'

``` r
google_stocks <- function(sym, current = TRUE, sy = 2005, sm = 1, sd = 1, ey, em, ed)
{
  # sy, sm, sd: start year, start month and start day
  # ey, em, ed: end year, end month, and end day
  
  # If TRUE, use the date as the enddate
  if(current){
    system_time <- as.character(Sys.time())
    ey <- as.numeric(substr(system_time, start = 1, stop = 4))
    em <- as.numeric(substr(system_time, start = 6, stop = 7))
    ed <- as.numeric(substr(system_time, start = 9, stop = 10))
  }
  
  require(data.table)
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
```

I want to read data from VW, Tesla, Daimler IBM and Apple
---------------------------------------------------------

Label the data and combine them to one big data.frame
-----------------------------------------------------

``` r
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
shares_dax_order$YM <- format(dmy(shares_dax_order$Date), "%Y-%m")
```

Save the data for further processing
------------------------------------

``` r
save(shares_dax_order, file="shares_dax_order.rda")
load("shares_dax_order.rda")
```

And do the Plots
----------------

![](finance_google_files/figure-markdown_github-ascii_identifiers/pressure-1.png) \#\# and save the plot as well

``` r
ggsave("shares_order.png", width = 30, height = 20, units = "cm")
```