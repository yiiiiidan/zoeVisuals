# Set dataroot location
dataroot <- "/Users/jimmy/data/zoe/"

# Set dataset libraries
localdir <- paste0(dataroot, "datasets/")

# Set the import/export directories
importdir <- paste0(localdir, "import/")
exportdir <- paste0(localdir, "export/")

# Set the programroot location
programroot <- "~/zoe/git/zoe/r"

# Set options  
# This option is used to prevent strings from being imported as factors
options(StringsAsFactors=FALSE)

# Set my font for MAC (just download and install OTF fonts then use quartzFonts)
quartzFonts(my_font = quartzFont(c("Libre Baskerville", 
                                   "Libre Baskerville Bold", 
                                   "Libre Baskerville Italic",
                                   "Libre Baskerville Bold")))
require(ggplot2)

# Make a theme that matches the OfDollarsAndData.com blog
zoe_theme <- theme(
  plot.title       = element_text(family = "my_font", size = 16, face = "bold", hjust = 0.5, margin = margin(0, 0, 10, 0)),
  axis.title.y     = element_text(face = "bold", size = 15, family = "my_font", margin = margin(0, 10, 0, 0)),
  axis.text.y      = element_text(color = "black"), 
  axis.ticks.y     = element_line(color = "black"),
  axis.text.x      = element_text(color = "black"),
  axis.ticks.x     = element_line(color = "black"),
  axis.title.x     = element_text(face = "bold", size = 16, family = "my_font", margin = margin(10, 0, 0, 0)),
  axis.line.x      = element_line(color = "black"),
  axis.line.y      = element_line(color = "black"),
  legend.key       = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border     = element_blank(),
  panel.background = element_blank(),
  plot.caption     = element_text(hjust = 0, family = "my_font", size = 8))


#Set Pallette
cbPalette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#A65628", "#984EA3", "#FF7F00")
cbPalette_grey <- c(rep("grey88",5), "#D55E00")
cbPalette_grey_bit <- c(rep("grey88",4), "#D55E00")