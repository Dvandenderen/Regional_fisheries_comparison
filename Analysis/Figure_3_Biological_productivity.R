
library(ggplot2)

# source F/FMSY data
source('Analysis/NWEU_Weighted_F_Fmsy.R')
source('Analysis/NWAT_Weighted_F_Fmsy.R')
source('Analysis/ALAS_Weighted_F_Fmsy.R')

# - 
# SAU output is created outside github
# -
# Europe
# reg1 <- read.csv("Data/Sea around us/Europe/SAU MEOW 41 v50-0.csv")
# reg2 <- read.csv("Data/Sea around us/Europe/SAU MEOW 85 v50-0.csv")
# reg3 <- read.csv("Data/Sea around us/Europe/SAU MEOW 97 v50-0.csv")
# NWEU <- rbind(reg1,reg2,reg3)
# NWEU <- subset(NWEU,NWEU$year > 1979)
# save(NWEU,file="Data/Sea around us/NWEU_SAU.Rdata")
load("Data/Sea around us/NWEU_SAU.Rdata")
catchEU    <- aggregate(NWEU$tonnes,by=list(NWEU$year),FUN=sum)
catchEU$prod <- (305)
catchEU$corC <- catchEU$x / (catchEU$prod*9*10^6) * 1000
NWEUR <- cbind(NWEUR,catchEU [match(NWEUR$years,catchEU$Group.1),c(4)])
colnames(NWEUR)[ncol(NWEUR)] <- "corC"

# ALA
# reg1 <- read.csv("Data/Sea around us/Alaska/SAU MEOW 17 v50-0.csv")
# reg2 <- read.csv("Data/Sea around us/Alaska/SAU MEOW 172 v50-0.csv")
# reg3 <- read.csv("Data/Sea around us/Alaska/SAU MEOW 177 v50-0.csv")
# ALA <- rbind(reg1,reg2,reg3)
# ALA <- subset(ALA,ALA$year > 1979)
# save(ALA,file="Data/Sea around us/ALA_SAU_old.Rdata")
# ALA <- read.csv("Data/Sea around us/SAU EEZ 959 v50-1.csv")
# save(ALA,file="Data/Sea around us/ALA_SAU.Rdata") US only

load("Data/Sea around us/ALA_SAU.Rdata")
catchALA    <- aggregate(ALA$tonnes,by=list(ALA$year),FUN=sum)
catchALA$prod <- (404) # new value without russia
catchALA$corC <- catchALA$x/ (catchALA$prod*9*10^6) * 1000
Alaska <- cbind(Alaska,catchALA [match(Alaska$years,catchALA$Group.1),c(4)])
colnames(Alaska)[ncol(Alaska)] <- "corC"

# NW Atlantic
# reg1 <- read.csv("Data/Sea around us/NeNo/SAU MEOW 229 v50-0.csv")
# reg2 <- read.csv("Data/Sea around us/NeNo/SAU MEOW 92 v50-0.csv")
# reg3 <- read.csv("Data/Sea around us/NeNo/SAU MEOW 55 v50-0.csv")
# NWA <- rbind(reg1,reg2,reg3)
# NWA <- subset(NWA,NWA$year > 1979)
# 
# # update scallop dregde discards
# reg2 <- subset(NWA,NWA$gear_type == "dredge" & NWA$fishing_entity == "USA" & 
#                  NWA$common_name=="American sea scallop")
# reg2 <- subset(reg2,reg2$catch_type == "Landings")
# reg2 <- aggregate(tonnes~year,data=reg2,FUN=sum)
# reg2 <- data.frame(area_name = "US_update", area_type = "meow", year=reg2$year, scientific_name="Placopecten magellanicus",
#                    common_name = "American sea scallop", functional_group = "Other demersal invertebrates",
#                    commercial_group = "Molluscs", fishing_entity = "USA", fishing_sector = NA, catch_type = "Discards",
#                    reporting_status ="corrected_dvd", gear_type ="dredge", end_use_type ="", 
#                    tonnes = 0.22*reg2$tonnes/(1-0.22),landed_value=NA)
# NWA <- subset(NWA,!(NWA$gear_type == "dredge" & NWA$fishing_entity == "USA" & 
#                       NWA$common_name=="American sea scallop" & NWA$catch_type == "Discards"))
# NWA <- rbind(NWA,reg2)
# save(NWA,file="Data/Sea around us/NWA_SAU.Rdata")
load("Data/Sea around us/NWA_SAU.Rdata")
catchNeNo   <- aggregate(NWA$tonnes,by=list(NWA$year),FUN=sum)
catchNeNo$prod <- (146)
catchNeNo$corC <- catchNeNo$x/ (catchNeNo$prod*9*10^6) * 1000
NEUS <- cbind(NEUS,catchNeNo [match(NEUS$years,catchNeNo$Group.1),c(4)])
colnames(NEUS)[ncol(NEUS)] <- "corC"

alldat <- rbind(NEUS,NWEUR,Alaska)
alldat <- subset(alldat, alldat$years < 2020)
alldat <- subset(alldat,!(alldat$reg=="NEUS" & alldat$years %in% c(1980,1981)))

mod1 <- (lm(alldat$value~alldat$corC))
summary(mod1)
int <- mod1$coefficients[1]
slope <- mod1$coefficients[2]
cor(alldat$value,alldat$corC)

# plot 1
dat <- rbind(catchEU,catchALA,catchNeNo)
dat <- subset(dat,dat$Group.1 > 2000)
dat$x <- (dat$x)/1000
dat$prod <- dat$prod
dat$prod <- dat$prod * 9  # change NPP in million C per year to million ww per year

# get the catch per NPP predictions (y-a)/b = x (model fit is done in subplot B)
C_NPP_qur <- (0.25- int)/slope # catch per npp for 0.25 F/Fmsy
C_NPP_half <- (0.5-int)/slope # catch per npp for 0.5 F/Fmsy
C_NPP_thqu <- (0.75-int)/slope # catch per npp for 0.75 F/Fmsy
C_NPP_one  <- (1- int)/slope   # catch per npp for 1 F/Fmsy
C_NPP_onq  <- (1.25-int)/slope   # catch per npp for 1.25 F/Fmsy

qurFMSY  <- data.frame(minmax  =c(900,4500),
                       catch   =c(900,4500)*10^6 * C_NPP_qur/1000/1000)
halfFMSY <- data.frame(minmax  =c(900,4500),
                       catch   =c(900,4500)*10^6 * C_NPP_half/1000/1000)
thquFMSY <- data.frame(minmax  =c(900,4500),
                       catch   =c(900,4500)*10^6 * C_NPP_thqu/1000/1000)
oneFMSY  <- data.frame(minmax  =c(900,4200),
                       catch   =c(900,4200)*10^6 * C_NPP_one/1000/1000)
onqFMSY <- data.frame(minmax   =c(900,3500),
                       catch   =c(900,3500)*10^6 * C_NPP_onq/1000/1000)

plot1 <- ggplot() +   
  geom_line(data=qurFMSY,aes(x=minmax,y=catch),color="lightgrey",linetype="dashed")+
  geom_line(data=halfFMSY,aes(x=minmax,y=catch),color="lightgrey",linetype="dashed")+  
  geom_line(data=thquFMSY,aes(x=minmax,y=catch),color="lightgrey",linetype="dashed")+
  geom_line(data=oneFMSY,aes(x=minmax,y=catch),color="lightgrey",linetype="dashed") +  
  geom_line(data=onqFMSY,aes(x=minmax,y=catch),color="lightgrey",linetype="dashed") +
  geom_boxplot(data=dat, aes(x=prod, y=x,group= as.factor(prod),
                                      col = as.factor(prod),fill = as.factor(prod)))+
  labs(title="",x="Mean NPP (million tonnes ww per year)", y = "Catch (1000 tonnes ww per year)")+
  scale_fill_manual(values =c("#0000FF26","#ffca0026","#FF000026"))+ 
  scale_color_manual(values =c("blue","black","red"))+ theme_classic() + 
  theme(legend.position = "none") + 
  annotate(geom="text", x=c(3000,3400,3800,4100,4300,4400), y=c(5900,5800,5100,4200,3000,1600), 
           label=c("F/Fmsy=","1.25","1.0","0.75","0.50","0.25"), color="darkgrey",size=2)

# catch predictions
305*9*10^6 * C_NPP_half/1000/1000 # NWEU
305*9*10^6 * C_NPP_one/1000/1000 # NWEU
146*9*10^6 * C_NPP_half/1000/1000 # NWA
146*9*10^6 * C_NPP_one/1000/1000 # NWA
404*9*10^6 * C_NPP_half/1000/1000 # ALA
404*9*10^6 * C_NPP_one/1000/1000 # ALA

######
plot2 <- ggplot(alldat, aes(x=corC, y=value))
plot2 <- plot2 + geom_smooth(method='lm', formula= y~x,colour="darkgrey",fill = "lightgrey") + 
                 geom_point(aes(colour = factor(reg),fill=factor(reg),shape= factor(reg)))+
  ylim(c(0,2))+labs(x = "Catch per NPP \n (per mille)", y= "Biomass-weighted F/Fmsy")+
  scale_fill_manual(values =c("#FF000026","#0000FF26","#ffca0026")) + 
  scale_color_manual(values =c("red","blue","black"))+ 
  scale_shape_manual(values =c(21,22,24)) + 
  theme_classic() + theme(legend.position = "none")

# plot 3
tot <- rbind(sumEU,sumneus,sumala)
tot$region <- c("NW EU","NW Atlantic","Alaska")

plot3 <- ggplot(data=tot,aes(x=region,y=meanfmsy,col=region)) +geom_point() + labs(x="",y="Fmsy")+
  geom_errorbar(ymin=tot$conf1,ymax= tot$conf2,width = 0.2) + ylim(c(0,0.5))+
  scale_color_manual(values=c("red","blue","black")) + theme_classic() + theme(legend.position = "none")

pdf("Output/Figure_3_catchprod.pdf",
    height = 4, width = 9)
cowplot::plot_grid(plot1,plot2,plot3, labels = "AUTO",label_fontface = "plain", align = "h",nrow = 1)
dev.off()


# potential productivity
# catch per NPP / F/Fmsy
alldat$PFP <- alldat$corC / alldat$value
aggregate(alldat$PFP,by=list(alldat$reg),FUN=mean)
