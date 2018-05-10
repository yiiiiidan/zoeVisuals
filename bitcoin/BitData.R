setwd("~/git/zoe/r")
source(file.path(paste0(getwd(),"/header.R")))
setwd(importdir)

######################## Load in Libraries #########################################

library(dplyr)
library(riingo)
riingo_set_token("2a549fe19b8290a218f4cc24bdd47de781d13659")

######################## Start Program Here ########################################
# Import the data for 2018
data.this.year <- riingo_crypto_prices("btcusd", start_date = "2018-01-01", end_date = Sys.Date()) %>% 
  select(date, close)
data.this.year <- as.data.frame(data.this.year)
data.this.year$date <- as.character(data.this.year$date)

# Import the data before 2018
data <- read.csv("market.csv", as.is = T)
BTC <- filter(data, symbol == "BTC") %>%
  select(`date`, `close`)
BTC.2016 <- filter(BTC, substr(BTC$date, 0, 4) <= "2017")

# Merge
BTC.merge <- rbind(BTC.2016, data.this.year)

# Split the data yearly
BTC_year <- split(BTC.merge, factor(substr(BTC.merge$date, 0, 4)))

# Function for computing annual returns
con_to_return <- function(df){
  new = df
  for (i in 2:length(df)){
    for (j in 1:nrow(df[[i]])){
      new[[i]][j, 2] <- (df[[i]][j, 2] - 
                           df[[i-1]][nrow(df[[i-1]]), 2])/df[[i-1]][nrow(df[[i-1]]), 2]
    }
  }
  return(new)
}

# Dataframe containing the returns
BTC_return <- con_to_return(BTC_year)
df_all <- rbind(BTC_return[[2]], BTC_return[[3]], BTC_return[[4]], 
                BTC_return[[5]], BTC_return[[6]])
df_three <- rbind(BTC_return[[2]], BTC_return[[3]], BTC_return[[4]])
df_four <- rbind(BTC_return[[2]], BTC_return[[3]], BTC_return[[4]], BTC_return[[5]])

# Annotation
source_string <- c("Source: Selected Exchanges")
note_string <- c("Note: The rate of return was computed based on the last day of the previous year")
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), 
                        y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), 
                        y = unit(0.15, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
