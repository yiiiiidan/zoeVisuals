source(file.path(paste0(getwd(),"/BitData.R")))
setwd("~/git/zoe/r")

######################## Load in Libraries #########################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)

######################## Start Program Here ########################################
#Plot for three year
bit_3years <- function(){
  for (i in 1: 50){
    plot <- ggplot(df_three[1:nrow(df_three)*(1/50)*i,], aes(x = as.Date(yday(date), "1970-01-01"), y = close, 
                                                             color = factor(year(date)))) +
      geom_line(size = 1) +
      scale_x_date(date_breaks="months", date_labels="%b", limits = as.Date(c('1970-01-01','1970-12-31'))) +
      labs(x="Month",colour="") +
      geom_hline(yintercept = 0) +
      scale_y_continuous(label = percent, limits = c(-0.6, 1.5)) +
      ggtitle("Annual Return of Bitcoin")+
      xlab("Month of the Year") +
      ylab("Return (%)") +
      zoe_theme +
      scale_colour_manual(values=cbPalette)
    
    my_gtable   <- ggplot_gtable(ggplot_build(plot))
    my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
    my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
    if (i <=9){
      ggsave(paste0("0", i, ".jpeg"), device = 'jpeg', plot = my_gtable, width = 15, height = 12, units = "cm")
      
    } else {
      ggsave(paste0(i, ".jpeg"), device = 'jpeg', plot = my_gtable, width = 15, height = 12, units = "cm")
    }
  }

}

#plot for 4years
bit_4years <- function(){
  for (i in 1:30){
    start_po <- nrow(df_three)
    how_many <- nrow(df_four) - start_po
    plot <- ggplot(df_four[1:(start_po + (how_many/30) *i), ],
                   aes(x = as.Date(yday(date), "1970-01-01"), y = close, 
                       color = factor(year(date)))) +
      geom_line(size = 1) +
      scale_x_date(date_breaks="months", date_labels="%b", limits = as.Date(c('1970-01-01','1970-12-31'))) +
      labs(x="Month",colour="") +
      geom_hline(yintercept = 0) +
      scale_y_continuous(label = percent, limits = c(-0.6, 1.5 + (18.5/30)*i)) +
      ggtitle("Annual Return of Bitcoin")+
      xlab("Month of the Year") +
      ylab("Return (%)") +
      zoe_theme +
      scale_colour_manual(values=cbPalette)
    
    my_gtable   <- ggplot_gtable(ggplot_build(plot))
    my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
    my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
    ## Save the plot
    ggsave(paste0(i+50, ".jpeg"), device = 'jpeg', plot = my_gtable, width = 15, height = 12, units = "cm")
  }
}


# Highlight the last year
df_all$size <- c(rep(1, nrow(rbind(BTC_return[[2]], BTC_return[[3]], BTC_return[[4]], BTC_return[[5]]))),
             rep(2, nrow(BTC_return[[6]])))

bit_all <- function(){
  start_po <- nrow(df_four)
  how_many <- nrow(df_all) - start_po
  for (i in 1:18){
  plot <- ggplot(df_all[1:start_po + (how_many/19 * i),], aes(x = as.Date(yday(date), "1970-01-01"), y = close, 
                       group = factor(year(date)), color = factor(year(date)), size = size)) +
    geom_line() +
    scale_x_date(date_breaks="months", date_labels="%b", limits = as.Date(c('1970-01-01','1971-01-02'))) +
    labs(x="Month",colour="") +
    geom_hline(yintercept = 0) +
    scale_y_continuous(label = percent) +
    ggtitle("Annual Return of Bitcoin") +
    xlab("Month of the Year") +
    ylab("Return (%)") +
    zoe_theme +
    scale_colour_manual(values=cbPalette_grey_bit) +
    scale_size(range=c(0.5, 1.3), guide=FALSE) 

  
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  ## Save the plot
  ggsave(paste0(i+80, ".jpeg"), device = 'jpeg', plot = my_gtable, width = 15, height = 12, units = "cm")
  }
}

bit_last <- function(){
  last <- df_all$close[nrow(df_all)]
  this.date <- df_all$date[nrow(df_all)]
  start_po <- nrow(df_four)
  how_many <- nrow(df_all) - start_po
    plot <- ggplot(df_all, aes(x = as.Date(yday(date), "1970-01-01"), y = close, 
                                                                group = factor(year(date)), color = factor(year(date)), size = size)) +
      geom_line() +
      scale_x_date(date_breaks="months", date_labels="%b", limits = as.Date(c('1970-01-01','1971-01-02'))) +
      labs(x="Month",colour="") +
      geom_hline(yintercept = 0) +
      scale_y_continuous(label = percent) +
      ggtitle("Annual Return of Bitcoin") +
      xlab("Month of the Year") +
      ylab("Return (%)") +
      zoe_theme +
      scale_colour_manual(values=cbPalette_grey_bit) +
      scale_size(range=c(0.5, 1.3), guide=FALSE) +
      annotate("text", x = as.Date(yday(today()+55), "1970-01-01"), y = last-0.0125, 
               label = paste0(round(df_all[nrow(df_all), 2]*100,2)  , "% (",
                              format(as.Date(df_all$date[nrow(df_all)]), "%b %d"), ")"), size = 3) +
      geom_point(aes(x = as.Date(yday(this.date), "1970-01-01"), y = last), color = "black") 
    
    
    my_gtable   <- ggplot_gtable(ggplot_build(plot))
    my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
    my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
    ## Save the plot
    ggsave(paste0(99, ".jpeg"), device = 'jpeg', plot = my_gtable, width = 15, height = 12, units = "cm")
  }