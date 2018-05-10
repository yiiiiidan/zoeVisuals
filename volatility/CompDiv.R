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
stern <- read_excel(paste0(importdir, "histretSP.xls"), sheet = "Returns by year")

# Clean up the dataframe 
stern <- stern[18:107,1:2]
stern <- as.data.frame(stern)
stern[,2] <- as.numeric(stern[,2])
colnames(stern) <- c("year", "snp")
stern[,2] <- rep(100, nrow(stern)) + stern[,2]*100

# Function to calculate compound returns
compound <- function(df){
  per <- rep(NA, 20)
  for (i in 1:20){
    per[i] <- (prod(df[1:i,2]))^(1/i) 
  }
  return(per)
}

# Get compound returns
return <- c()
for (i in 1:70){
  return <- append(return, (compound(stern[i:(19+i),])-100)/100)
}

# Generate a data frame with the returns and ID, year.
df <- data.frame(ID=rep(seq(1,70), each = 20), year=rep(1:20, 70),return=return)

# Set up annotaion
setwd(exportdir)
source_string <- c("Source: http://pages.stern.nyu.edu/~adamodar/ (zoefin.com)")
note_string <- c("Note: Nominal price and dividend were adjusted to real price and dividend based on CPI.")
note2_string <- c("The total rate of return includes dividend reinvestment.")
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), 
                        y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), 
                        y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
note2_grob   <- textGrob(note2_string, x = (unit(0.5, "strwidth", note2_string) + unit(0.2, "inches")), 
                         y = unit(0.1, "inches"),
                         gp =gpar(fontfamily = "my_font", fontsize = 8))

# Create a plot
for (i in  10:28){
  
  # Except the last frame
  if (i!=28){
    plot <- ggplot(data = df, aes(x=year, y=return, color=factor(ID))) +
      # Aesthetics
      geom_line() +
      zoe_theme +
      scale_x_continuous(breaks = seq(1:20), limits = c(1, i-8)) +
      scale_y_continuous(label = percent, breaks = c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5)) +
      xlab("Time Horizon in Years") +
      ylab("Real return (%)") +
      ggtitle("Compound Annual Returns (Stocks)\n(1928~2017)") +
      geom_hline(yintercept = 0) +
      geom_segment(aes(x=1, y=-0.438, xend=1, yend=0.526), size = 0.4, color = "black") +
      geom_segment(aes(x=20, y=0.237, xend=20, yend=0.1769), size = 0.4, color = "black") +
      scale_color_discrete(guide = F) 

    ## Save the plot
    # Add source - anotation
    my_gtable   <- ggplot_gtable(ggplot_build(plot))
    my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
    my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
    my_gtable   <- arrangeGrob(my_gtable, bottom = note2_grob)
  
    # Save the plot
    ggsave(paste0(i, ".jpeg"), device = 'jpeg', plot = my_gtable, width = 15, height = 13, units = "cm")
    
  } else{ # This is the last frame
    plot <- ggplot(data = df, aes(x=year, y=return, color=factor(ID))) +
      # Aesthetics
      geom_line() +
      zoe_theme +
      scale_x_continuous(breaks = seq(1:20), limits = c(1, i-8)) +
      scale_y_continuous(label = percent, breaks = c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5)) +
      xlab("Time Horizon in Years") +
      ylab("Real return (%)") +
      ggtitle("Compound Annual Returns (Stocks)\n(1928~2017)") +
      geom_hline(yintercept = 0) +
      geom_segment(aes(x=1, y=-0.448, xend=1, yend=0.516), size = 0.4, color = "black") +
      geom_segment(aes(x=20, y=0.013, xend=20, yend=0.166), size = 0.4, color = "black") +
      scale_color_discrete(guide = F) +
      
      # Add points
      geom_point(aes(x = 1, y = max(df[,3])), color = "blue", size=1.5) +
      geom_point(aes(x = 1, y = min(df[,3])), color = "red", size=1.5) +
      geom_point(aes(x = 20, y = max(df[df[,2]==20,][,3])), color = "blue", size=1.5) +
      geom_point(aes(x = 20, y = min(df[df[,2]==20,][,3])), color = "red", size=1.5) +
      
      # Add Texts 
      annotate("text", x=3, y=max(df[,3]),
               label = paste0(as.character(round(max(df[,3]),3)*100), "%", " (1954)"), size = 3.25) +
      annotate("text", x=3, y=min(df[,3]),
               label = paste0(as.character(round(min(df[,3]),3)*100), "%", " (1931)"), size = 3.25) +
      annotate("text", x=19.5, y=max(df[df[,2]==20,][,3])+0.037,
               label = paste0(as.character(round(max(df[df[,2]==20,][,3]*100),2)), "%"), size = 3.25) +
      annotate("text", x=19.5, y=min(df[df[,2]==20,][,3])-0.045,
               label = paste0(as.character(round(min(df[df[,2]==20,][,3]*100),2)), "%"), size = 3.25) 
    
    # Add source - anotation
    my_gtable   <- ggplot_gtable(ggplot_build(plot))
    my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
    my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
    my_gtable   <- arrangeGrob(my_gtable, bottom = note2_grob)
    
    # Save the plot
    ggsave(paste0(i, ".jpeg"), device = 'jpeg', plot = my_gtable, width = 15, height = 13, units = "cm")
    
    }
}