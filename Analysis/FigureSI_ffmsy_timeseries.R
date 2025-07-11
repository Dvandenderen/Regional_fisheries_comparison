# load libraries
library(ggplot2)

# load F/FMS data
source('Analysis/NWEU_F_Fmsy.R') # source NW Europe
source('Analysis/NWAT_F_Fmsy.R') # source Northwest Atlantic
source('Analysis/ALAS_F_Fmsy.R') # source alaska

library(gridExtra)

pdf("Output/Figure_SI_ffmsy.pdf", width = 9, height = 4.5)
grid.arrange(NWEU, NWAT, ALA, nrow = 1)
dev.off()
