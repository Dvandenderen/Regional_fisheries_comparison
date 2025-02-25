
# ------------------------------------------------------------------------------
# create bar plot and radar plots on:
# fishing operations, fleet efficiency, environmental indicators, and control and safety
# ------------------------------------------------------------------------------

# load libraries
library(fmsb)

# create combined bar plot
pdf("Output/Figure_4sub_fishing_operations_barplot.pdf",width = 9.5,height = 10)
layout(matrix(c(1:16), nrow = 4, ncol = 4, byrow = TRUE))

# get fishing operations (data taken from extended Data Table in manuscript)
land <- matrix(data= c(128,0.46, 9420,393,120,0.7,84, 4.3,6.4,20,6.8,36,0.3,40,
                       285,1.25,15115,990,274,0.1,15,28.6,3.9,14,6.5,30,1.8,NA,
                       123,0.22, 6626,452,163,0.6,15,  20,2.8,10, 16,24,0.3,6),ncol=3)
rownames(land) <- c("KW sea days", "Sea days","Fleet size","Fuel use",
                    "Discards","Mammal biomass","Demersal biomass","% of area bottom trawled",
                    "Landings (kg) per liter fuel","Landings (kg) per KW sea day","Value of landings/Fuel costs","Income (1000 US$)",
                    "Fatal injuries per year","Observer days at sea (x1000)")
colnames(land) <- c("Alaska","NW EU","NE Am.")

barplot(t(land[1,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,300),yaxt="n",ylab="Million KW sea days")
axis(2,c(0,150,300),las=1)

barplot(t(land[2,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,1.3),yaxt="n",ylab="Million sea days")
axis(2,c(0,0.65,1.3),las=1)

barplot(t(land[3,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,16000),yaxt="n",ylab="Fleet size (nb)")
axis(2,c(0,8000,16000),las=1)

barplot(t(land[4,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,1000),yaxt="n",ylab="Fuel use (million liter)")
axis(2,c(0,500,1000),las=1)

barplot(t(land[5,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,300),yaxt="n",ylab="Discards (1000 MT)")
axis(2,c(0,150,300),las=1)

barplot(t(land[6,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,1),yaxt="n",ylab="Mammals (MT per km2)")
axis(2,c(0,0.5,1),las=1)

barplot(t(land[7,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,90),yaxt="n",ylab="Demersal fish (MT per km2)")
axis(2,c(0,45,90),las=1)

barplot(t(land[8,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,30),yaxt="n",ylab="% area bottom trawled")
axis(2,c(0,15,30),las=1)

barplot(t(land[9,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,7),yaxt="n",ylab="Landings (kg) per liter fuel")
axis(2,c(0,3.5,7),las=1)

barplot(t(land[10,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,16),yaxt="n",ylab="Landings (kg) per KW sea day")
axis(2,c(0,8,16),las=1)

barplot(t(land[11,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,20),yaxt="n",ylab="Value of landings/Fuel cost")
axis(2,c(0,10,20),las=1)

barplot(t(land[12,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,40),yaxt="n",ylab="Income (1000 US$)")
axis(2,c(0,20,40),las=1)

barplot(t(land[13,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,2),yaxt="n",ylab="Fatal injuries per year")
axis(2,c(0,1,2),las=1)

barplot(t(land[14,]), beside = T,space=c(0,0.05,0.05),
        col=c("#FFE5E5","#F2F2F2","#E5E5FF"),border=c("red","black","blue"),
        ylim=c(0,40),yaxt="n",ylab="Observer days at sea (x1000)")
axis(2,c(0,20,40),las=1)

dev.off()

# ------------------------------------------------------------------------------
# get radar plot for fishing operations

data <- t(as.data.frame(matrix(c(2521,128,0.46,9420,393,
                                 3876,285,1.25,15115,990,
                                 1281,123,0.22,6626,452),nrow=5)))

colnames(data) <- c("Landings","Effort", "Sea days","Fleet size","Fuel use")
rownames(data) <- c("Alaska","NW EU", "NW Atlantic")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot
data <- rbind(as.numeric(c(4000,300,1.3,16000,1000)) , rep(0,5) , data)

# plot with default options:
data <- as.data.frame(data)
pdf("Output/Figure_4sub_radar_fishing.pdf",width = 5,height = 5)

# Fill colors
areas <- c("#FF000026","#00000026","#0000FF26")

radarchart(data,pcol=c("red","black","blue"),pfcol = areas)
dev.off()

# ------------------------------------------------------------------------------
# get radar plot for efficiency
data <- t(as.data.frame(matrix(c(2.381,6.4,20,6.8,36,
                                 5.021,3.9,14,6.5,30,
                                 4.412,2.8,10,16,24),nrow=5)))

colnames(data) <- c("Value","Landings l-1","Landings eff-1","Value/fuel cost","Income")
rownames(data) <- c("Alaska","NW EU", "NW Atlantic")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot
data <- rbind(as.numeric(c(5.000,6.5,22,20,38)) , rep(0,5) , data)

# plot with default options:
data <- as.data.frame(data)
pdf("Output/Figure_4sub_radar_efficiency.pdf",width = 5,height = 5)
areas <- c("#FF000026","#00000026","#0000FF26")

radarchart(data,pcol=c("red","black","blue"),pfcol = areas)
dev.off()

