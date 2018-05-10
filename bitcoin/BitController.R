setwd("~/git/zoe/r")
source(file.path(paste0(getwd(),"/BitCoin.R")))
setwd(exportdir)
#####################################################

bit_3years()
bit_4years()
bit_all()
bit_last()

