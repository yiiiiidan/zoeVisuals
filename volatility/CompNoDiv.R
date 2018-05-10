######## Load in header file  ############

setwd("~/git/zoe/r/")
source("header.R")
setwd(importdir)

######## Load in Libraries    ############

library(readxl)
library(dplyr)
library(scales)
library(gtable)
library(grid)
library(gridExtra)

######## Start Program Here   ############

# Bring in the data 
shil <- read_excel(paste0(importdir, "ie_data.xls"), 
                   sheet = "Data")

# Name the columns
colnames(shil) <- c("date", "price", "div", "earnings", "CPI", "data_frac",
                    "long_irate", "real_price", "real_div", "real_earn", "cape")

# Remove first 6 rows
shil <- shil[7:nrow(shil),]

# Let`s get the real returns
shil$real_price <- as.numeric(shil$real_price)
shil$real_div <- as.numeric(shil$real_div)
shil <- shil %>% 
  select(`date`, `real_price`, `real_div`)

# Split the data according to year
shil.year <- cbind(shil, year = substring(shil$date, 0, 4))
shil.year$year <- as.factor(shil.year$year)
shil.split <- split(shil.year, shil.year$year)
shil.split <- shil.split[1:length(shil.split)-1]

# Function to get first element
first.ele <- function(df){
  return(df[1,])
}

# Function to Calculate yearly returns
per.return <- function(list){
  per <- rep(NA, 20)
  for (i in 1:20){
    per[i] <- (first.ele(list[[i+1]])$real_price/
                 first.ele(list[[1]])$real_price)^(1/i)-1
  }
  return(per)
}

# A vector containing compound returns for 127 years
return <- c()
for (k in 1:127){
  return <- append(return, per.return(shil.split[k:(20+k)]))
}

# Make a data frame with ID and Year
return.df <- data.frame(ID=rep(seq(1,127), each = 20), year= rep(rep(1:20), 127), return)

# Set up annotation
setwd(exportdir)
source_string <- c("Source: http://www.econ.yale.edu/~shil/data.htm (zoefin.com)")
note_string <- c("Note: Nominal price was adjusted to real price based on CPI. Dividend not included.")
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), 
                        y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), 
                        y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))

setwd(exportdir)

# Save frames
for (i in 10:28){
  
  # Except the last frame
  if (i != 28){
    plot <- ggplot(data = return.df, aes(x=year, y=return, color = factor(ID))) +
      
      # Aesthetics
      geom_line() +
      zoe_theme +
      scale_x_continuous(breaks = seq(1:20), limits = c(1, i-8)) +
      scale_y_continuous(label = percent, breaks = c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5), limits = c(-0.45,0.52)) +
      xlab("Time Horizon in Years") +
      ylab("Real return (%)") +
      ggtitle("Compound Annual Returns (Stocks)\n(1872~2017)") +
      geom_segment(aes(x=20, y=-0.043, xend=20, yend=0.093), size=0.4, color="black") +
      geom_segment(aes(x=1, y=-0.42, xend=1, yend=0.46), size=0.4, color="black") +
      geom_hline(yintercept = 0) +
      scale_color_discrete(guide = F)
    
    # Add source - anotation
    my_gtable   <- ggplot_gtable(ggplot_build(plot))
    my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
    my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
    
    # Save the plot
    ggsave(paste0(i, ".jpeg"), device = 'jpeg', plot = my_gtable, width = 15, height = 13, units = "cm")
    
  } else { # The last frame
    plot <- ggplot(data = return.df, aes(x=year, y=return, color = factor(ID))) +
      # Aesthetics
      geom_line() +
      zoe_theme +
      scale_x_continuous(breaks = seq(1:20), limits = c(1, i-8)) +
      scale_y_continuous(label = percent, breaks = c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5), limits = c(-0.45,0.52)) +
      xlab("Time Horizon in Years") +
      ylab("Real return (%)") +
      ggtitle("Compound Annual Returns (Stocks)\n(1872~2017)") +
      geom_hline(yintercept=0) +
      geom_segment(aes(x=20, y=-0.043, xend=20, yend=0.093), size=0.4, color="black") +
      geom_segment(aes(x=1, y=-0.42, xend=1, yend=0.46), size=0.4, color="black") +
      scale_color_discrete(guide = F) +
      
      #Add dots and annotation for year 1
      geom_point(aes(x = 1, y = max(return.df[,3])), color = "blue") +
      geom_point(aes(x = 1, y = min(return.df[,3])), color = "red") +
      geom_point(aes(x = 1, y = max(return.df[-1221,3])), color = "blue") +
      geom_point(aes(x = 1, y = min(return.df[-1201,3])), color = "red") +
      annotate("text", x=2.7, y=max(return.df[,3]),
               label = "46%(1935),", size = 3) +
      annotate("text", x=5.3, y=max(return.df[-1221,3]), 
               label = "45%(1933)", size = 3) +
      annotate("text", x=2.7, y=min(return.df[,3]),
               label = "-42%(1931)", size = 3) +
      annotate("text", x=2.7, y=min(return.df[-1201,3]),
               label = "-37%(1917)", size = 3) +

      
      #Add dots and annotation for year 20
      geom_point(aes(x = 20, y = max(return.df[return.df[,2] == 20,][,3])), color = "blue") +
      geom_point(aes(x = 20, y = min(return.df[return.df[,2] == 20,][,3])), color = "blue") +
      annotate("text", x=19.4, y=max(return.df[return.df[,2] == 20,][,3])+0.03,
               label = "9.3%", size = 3.25) +
      annotate("text", x=19.33, y=min(return.df[return.df[,2] == 20,][,3])-0.03,
               label = "-4.3%", size = 3.25) 
    
    # Add source - anotation
    my_gtable   <- ggplot_gtable(ggplot_build(plot))
    my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
    my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
    
    # Save the last frame
    ggsave(paste0(i, ".jpeg"), device = 'jpeg', plot = my_gtable, width = 15, height = 13, units = "cm")
  }
}