setwd(file.path(paste0(getwd(),"/zoeVisuals")))
source(file.path(paste0(getwd(),"/header.R")))
setwd("Users/jimmy/data/")

######################## Load in Libraries #########################################

library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)

######################## Start Program Here ########################################
# Read data from yahoo finance
sp500 <- new.env()
sp_data <- getSymbols("^GSPC", env = sp500, src = 'yahoo',
                      from = as.Date("2012-01-01"), to = as.Date(today()))
gspc <- data.frame(sp500$GSPC) 
gspc_date <- gspc %>%
  mutate(`date` = as.Date(rownames(gspc)), `close` = `GSPC.Close`) %>% 
  select(`date`, `close`)

# Split the data according to the year
gspc_split <- split(gspc_date, factor(substr(gspc_date$date, 0, 4)))

# Make a function to compute returns
con_to_return_index <- function(df){
  new = df
  for (i in 2:length(df)){
    for (j in 1:nrow(df[[i]])){
      new[[i]][j, 2] <- (df[[i]][j, 2] - 
                           df[[i-1]][nrow(df[[i-1]]), 2])/df[[i-1]][nrow(df[[i-1]]), 2]
    }
  }
  return(new)
}

# Return a dataframe
stocks <- con_to_return_index(gspc_split)
df <- rbind(data.frame(date= as.Date("2013-01-01"), close = 0),stocks[[2]], 
            data.frame(date= as.Date("2014-01-01"), close = 0),stocks[[3]], 
            data.frame(date= as.Date("2015-01-01"), close = 0),stocks[[4]], 
            data.frame(date= as.Date("2016-01-01"), close = 0),stocks[[5]], 
            data.frame(date= as.Date("2017-01-01"), close = 0),stocks[[6]], 
            data.frame(date= as.Date("2018-01-01"), close = 0),stocks[[7]])
today <- format(df$date[nrow(df)], "%b %d, %Y")
last <- df$close[length(df$close)]
df$size <- c(rep(1, 253*5-1), rep(2, nrow(df)-(253*5-1)))

#Annotation
source_string <- c("Source: Yahoo Finance (https://finance.yahoo.com)")
note_string <- c("Note: The rate of return was computed based on the last day of the previous year")
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), 
                        y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), 
                        y = unit(0.15, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))

#Create the first plot
snp_plot_first <- function(){
    for (i in 1:60){
      plot <- ggplot(df[1:(1264/60*(i-1)),], aes(x = as.Date(yday(date), "1970-01-01"), y = close, 
                                                 color = factor(year(date))[1:(1264/60*(i-1))])) +
      geom_line() +
      scale_x_date(date_breaks="months", date_labels="%b", limits = as.Date(c('1970-01-01','1971-01-02'))) +
      labs(x="Month",colour="") +
      geom_hline(yintercept = 0) +
      scale_y_continuous(label = percent, limits = c(-0.10, 0.30)) +
      ggtitle(paste0("S&P 500 Index Return (2013 - ", today, ")")) +
      xlab("Month of the Year") +
      ylab("Return (%)") +
      zoe_theme +
      scale_colour_manual(values=cbPalette)+
      theme(legend.text=element_text(size=14), axis.text = element_text(size = 10)) +
      guides(color = guide_legend(override.aes = list(size=3)))
    
     ##Add source - anotation
      my_gtable   <- ggplot_gtable(ggplot_build(plot))
      my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
      my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
    
      ## Save the plot
      if (i <= 9){
        ## Save the plot
        ggsave(paste0("0", i, ".jpeg"), device = 'jpeg', plot = my_gtable, width = 15, height = 12, units = "cm")
      } else{
        ggsave(paste0(i, ".jpeg"), device = 'jpeg', plot = my_gtable, width = 15, height = 12, units = "cm")  
      }
    }
}

# Second function
snp_plot_second <- function(){
  for (i in 1:30){
    plot <- ggplot(df[1:(1264+(nrow(df)-1264)/30*(i)),], aes(x = as.Date(yday(date), "1970-01-01"), y = close,
                                                group = factor(year(date))[1:(1264+(nrow(df)-1264)/30*(i))],
                                                color = factor(year(date))[1:(1264+(nrow(df)-1264)/30*(i))],
                                                size = size[1:(1264+(nrow(df)-1264)/30*(i))])) +
      geom_line() +
      scale_x_date(date_breaks="months", date_labels="%b", limits = as.Date(c('1970-01-01','1971-01-02'))) +
      labs(x="Month",colour="") +
      geom_hline(yintercept = 0) +
      scale_y_continuous(label = percent, limits = c(-0.10, 0.30)) +
      ggtitle(paste0("S&P 500 Index Return (2013 - ", today, ")")) +
      xlab("Month of the Year") +
      ylab("Return (%)") +
      zoe_theme +
      scale_colour_manual(values=cbPalette_grey) +
      scale_size(range=c(0.5, 1.3), guide=FALSE) +
      theme(legend.text=element_text(size=14), axis.text = element_text(size = 10)) +
      guides(color = guide_legend(override.aes = list(size=3))) 

    
    # Add source - anotation
    my_gtable   <- ggplot_gtable(ggplot_build(plot))
    my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
    my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
    # Save the plot
    ggsave(paste0(i + 60, ".jpeg"), device = 'jpeg', plot = my_gtable, width = 15, height = 12, units = "cm")
  }
  
    # Last plot
    this.date <- df$date[nrow(df)]
    plot <- ggplot(df, aes(x = as.Date(yday(date), "1970-01-01"), y = close,
                                              group = factor(year(date)),
                                              color = factor(year(date)),
                                              size = size)) +
      geom_line() +
      scale_x_date(date_breaks="months", date_labels="%b", limits = as.Date(c('1970-01-01','1971-01-02'))) +
      labs(x="Month", colour="") +
      geom_hline(yintercept = 0) +
      scale_y_continuous(label = percent, limits = c(-0.10, 0.30)) +
      ggtitle(paste0("S&P 500 Index Return (2013 - ", today, ")")) +
      xlab("Month of the Year") +
      ylab("Return (%)") +
      zoe_theme +
      scale_colour_manual(values=cbPalette_grey) +
      scale_size(range=c(0.5, 1.3), guide=FALSE) +
      annotate("text", x = as.Date(yday(today()+60), "1970-01-01"), y = last-0.0125, 
             label = paste0(round(df[nrow(df), 2]*100,2)  , "% (",
                            format(df$date[nrow(df)], "%b %d"), ")"), size = 4) +
      geom_point(aes(x = as.Date(yday(this.date), "1970-01-01"), y = last), color = "black") +
      theme(legend.text=element_text(size=14), axis.text = element_text(size = 10)) +
      guides(color = guide_legend(override.aes = list(size=3)))
  

      ##Add source - anotation
      my_gtable   <- ggplot_gtable(ggplot_build(plot))
      my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
      my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
      ## Save the plot
      ggsave("91.jpeg", device = 'jpeg', plot = my_gtable, width = 15, height = 12, units = "cm")
}
snp_plot_first()
snp_plot_second()
