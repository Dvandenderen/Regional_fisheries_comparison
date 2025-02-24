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
fmsystock <- read.csv("Data/NOAA_stocks/FishstockFMSY_alaska.csv")
alaska <- subset(stockdat,stockdat$stocklong %in% fmsystock$US_name)
alaska <- cbind(alaska,fmsystock[match(alaska$stocklong,fmsystock$US_name),
                                c("stockid","Fmsy")])
alaska$Bio <- alaska$Abundance
alaska$Bio <- ifelse(grepl("Female",alaska$unitA_des),alaska$Bio*2,alaska$Bio)
alaska$Bio <- ifelse(alaska$unitA == "Thousand Metric Tons",alaska$Bio*1000,alaska$Bio)

# get biomass weighted FMSY
test <- aggregate(list(alaska$Fmsy,alaska$Bio),by=list(alaska$stocklong),FUN=mean,na.rm=T)
colnames(test) <- c("stock","Fmsy","Bio")
var    <- Hmisc::wtd.var(test$Fmsy,test$Bio)
value  <- sum(test$Fmsy*test$Bio)/sum(test$Bio)
stderr <- sqrt(var/nrow(test))
tstat  <- value/stderr ## not mx - mu
alpha  <- 1 - 0.95 # 0.95 conf.level
cint   <- qt(1 - alpha/2, nrow(test)-1)
cint   <- tstat + c(-cint, cint)
conf   <- cint * stderr
sumala <- data.frame(meanfmsy=value,conf1=conf[1],conf2=conf[2])

# now create time series
tr <- alaska
tr$FoverFMSY <- tr$Fmort / tr$Fmsy
tr <- subset(tr, !(is.na(tr$FoverFMSY)))
tr$FoverFMSY_weight <- tr$Bio * tr$FoverFMSY

# average per group
reg  <- data.frame(reg = c(rep("Alaska",42)))
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

Alaska <- reg

rm(list=setdiff(ls(), c("Alaska","NWEUR","NEUS","sumEU","sumala","sumneus")))
