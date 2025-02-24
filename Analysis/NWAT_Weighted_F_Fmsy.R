stock1 <- openxlsx::read.xlsx("Data/NOAA_stocks/Assessment_TimeSeries_Data_Part_1.xlsx",1,colNames = F)
stock2 <- openxlsx::read.xlsx("Data/NOAA_stocks/Assessment_TimeSeries_Data_Part_2.xlsx",1,colNames = F)
stock3 <- openxlsx::read.xlsx("Data/NOAA_stocks/Assessment_TimeSeries_Data_Part_3.xlsx",1,colNames = F)
stock4 <- openxlsx::read.xlsx("Data/NOAA_stocks/Assessment_TimeSeries_Data_Part_4.xlsx",1,colNames = F)

stock <- cbind(stock1,stock2,stock3,stock4)

stockid <- unique(as.character(stock[1,]))
stockid <- stockid[-c(1:2)]

Fspec <- which(stock[1,] == stockid[1] & stock[3,] == "Fmort")
Fmo <- if(length(Fspec) > 0){as.numeric(stock[6:166,Fspec])} else {rep(NA,161)}
Fmo_udes <- if(length(Fspec) > 0){stock[4,Fspec]} else {NA}
Fmo_u    <- if(length(Fspec) > 0){stock[5,Fspec]} else {NA}

Aspec    <- which(stock[1,] == stockid[1] & stock[3,] == "Abundance")
Abu      <- if(length(Aspec) > 0){as.numeric(stock[6:166,Aspec])} else {rep(NA,161)}
Abu_udes <- if(length(Aspec) > 0){stock[4,Aspec]} else {NA}
Abu_u    <- if(length(Aspec) > 0){stock[5,Aspec]} else {NA}

stockdat <- data.frame(Fmort = Fmo, Abundance = Abu, stocklong = stockid[1], 
                       unitF_des = Fmo_udes, unitF = Fmo_u,
                       unitA_des = Abu_udes, unitA = Abu_u, Year = 1872:2032)

for (j in 2:length(stockid)){
  Fspec <- which(stock[1,] == stockid[j] & stock[3,] == "Fmort")
  Fmo <- if(length(Fspec) > 0){as.numeric(stock[6:166,Fspec])} else {rep(NA,161)}
  Fmo_udes <- if(length(Fspec) > 0){stock[4,Fspec]} else {NA}
  Fmo_u    <- if(length(Fspec) > 0){stock[5,Fspec]} else {NA}
  
  Aspec    <- which(stock[1,] == stockid[j] & stock[3,] == "Abundance")
  Abu      <- if(length(Aspec) > 0){as.numeric(stock[6:166,Aspec])} else {rep(NA,161)}
  Abu_udes <- if(length(Aspec) > 0){stock[4,Aspec]} else {NA}
  Abu_u    <- if(length(Aspec) > 0){stock[5,Aspec]} else {NA}
  
  stock_sub <- data.frame(Fmort = Fmo, Abundance = Abu, stocklong = stockid[j], 
                          unitF_des = Fmo_udes, unitF = Fmo_u,
                          unitA_des = Abu_udes, unitA = Abu_u, Year = 1872:2032)
  stockdat <- rbind(stockdat,stock_sub)
}

stockdat <- subset(stockdat,stockdat$Year %in% c(1980:2021))

# load estimated Fmsy
fmsystock <- read.csv("Data/NOAA_stocks/FishstockFMSY_NEUS.csv")
NEUS <- subset(stockdat,stockdat$stocklong %in% fmsystock$US_name)
NEUS <- cbind(NEUS,fmsystock[match(NEUS$stocklong,fmsystock$US_name),
                                c("stockid","Fmsy")])
NEUS$Bio <- NEUS$Abundance
NEUS$Bio <- ifelse(grepl("Female",NEUS$unitA_des),NEUS$Bio*2,NEUS$Bio)

NEUS <- subset(NEUS,NEUS$unitA == "Metric Tons")
NEUS <- subset(NEUS,!(NEUS$unitA %in% c("Catch / Biomass for 30cm+ fish","Catch / Survey Biomass")))
NEUS <- subset(NEUS,!(NEUS$stocklong == "Sea scallop - Northwestern Atlantic Coast"))

#
# load RAM data to obtain stocks from Nova Scotia
#ram <- readRDS('C:/Users/danie/Documents/Online for git/DemFish_trawl_noGIT/ramlegacy/4.44/RLSADB v4.44/DB Files With Assessment Data/v4.44.rds')

# select all stocks that will be included
#stock_ID <- c("AMPL4VWX","COD4X","COD4X5Yb","CUSK4X","HAD4X5Y","HERR4VWX",
#              "POLL4VWX","POLL4VWX5","SDOG4VWX5","SHAKE4VWX","WHAKE4VWX5")

# check what is available
#ramdata   <- ram$timeseries_values_views
#ramunits  <- ram$timeseries_units_views
#rammethod <- ram$assessment
#
#tot <- data.frame(F = NA, stocklong = NA, year = NA, stockid = NA, FdivFmsy = NA, Bio= NA)
#
#for(j in 1:length(stock_ID)){
  # get biomass
#  spec <- subset(ramdata,ramdata$stockid == stock_ID[j] & ramdata$year %in% c(1980:2010))
#  specunits <- subset(ramunits,ramunits$stockid == stock_ID[j])
#  spec$SSB <- ifelse(specunits$SSB %in% c("E00","E00eggs","E00larvae","relative","E00pups","index"), NA,spec$SSB) # all other values in metric tonnes (1000 kg)
#  spec$TB  <- ifelse(specunits$TB %in% c("index","relative"), NA,spec$TB) # all other values in metric tonnes (1000 kg)
#  spec$TB  <- ifelse((is.na(spec$TBbest) &  is.na(spec$TB)), spec$TB, spec$TBbest)
#  spec$Bio <- ifelse(is.na(spec$TB),spec$SSB,spec$TB)
  
#  tot <- rbind(tot, spec[,c("F","stocklong","year","stockid","FdivFmsy","Bio")])
#}

# tot <- tot[-1,]  
# colnames(tot) <- c("Fmort","stocklong","Year","stockid","FoverFMSY","Bio")
# tot$Fmsy <- tot$Fmort/tot$FoverFMSY

# only silver hake scotian shelf and BoF has F/FMSY data and only between 1994-2010
# ignore canadian part

# get biomass-weighted Fmsy
test <- aggregate(list(NEUS$Fmsy,NEUS$Bio),by=list(NEUS$stocklong),FUN=mean,na.rm=T)
colnames(test) <- c("stock","Fmsy","Bio")
var    <- Hmisc::wtd.var(test$Fmsy,test$Bio)
value  <- sum(test$Fmsy*test$Bio)/sum(test$Bio)
stderr <- sqrt(var/nrow(test))
tstat  <- value/stderr ## not mx - mu
alpha  <- 1 - 0.95 # 0.95 conf.level
cint   <- qt(1 - alpha/2, nrow(test)-1)
cint   <- tstat + c(-cint, cint)
conf   <- cint * stderr
sumneus <- data.frame(meanfmsy=value,conf1=conf[1],conf2=conf[2])

# and create time series
tr <- NEUS
tr$FoverFMSY <- tr$Fmort / tr$Fmsy
tr <- subset(tr, !(is.na(tr$FoverFMSY)))
tr$FoverFMSY_weight <- tr$Bio * tr$FoverFMSY

# average per group
reg  <- data.frame(reg = c(rep("NEUS",42)))
reg$value <- NA
reg$numb <- NA
reg$overexplot <- NA
reg$var <- NA
reg$conf1 <- NA
reg$conf2 <- NA
years <- 1980:2021 ; reg$years <- 1980:2021

for (j in 1:42){
  daty <- subset(tr,tr$Year == years[j])
  daty <- subset(daty,!(is.na(daty$FoverFMSY)) & !(is.na(daty$Bio)))
  reg$numb[j] <- nrow(daty)
  reg$overexplot[j] <- length(which(daty$FoverFMSY > 1.1))
  reg$var[j] <- Hmisc::wtd.var(daty$FoverFMSY,daty$Bio)
  reg$value[j] <- sum(daty$FoverFMSY*daty$Bio)/sum(daty$Bio)
  stderr <- sqrt(reg$var[j]/reg$numb[j])
  tstat <- reg$value[j]/stderr ## not mx - mu
  alpha <- 1 - 0.95 # 0.95 conf.level
  cint <- qt(1 - alpha/2, reg$numb[j]-1)
  cint <- tstat + c(-cint, cint)
  conf <- cint * stderr
  reg$conf1[j] <- conf[1]
  reg$conf2[j] <- conf[2]
}

NEUS <- reg

rm(list=setdiff(ls(), c("NWEUR","NEUS","sumneus","sumEU")))