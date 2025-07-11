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

tr <- alaska
tr$FoverFMSY <- tr$Fmort / tr$Fmsy
tr <- subset(tr, !(is.na(tr$FoverFMSY)))
tr <- subset(tr,tr$Year %in% c(1980:2019))

avg_tr <- tr %>%
  group_by(Year) %>%
  summarise(
    mean_F = mean(FoverFMSY, na.rm = TRUE),
    se = sd(FoverFMSY, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_F - 1.96 * se,
    upper = mean_F + 1.96 * se
  )

ALA <- ggplot() +
  geom_line(data = tr, aes(x = Year, y = FoverFMSY, group = stocklong), alpha = 0.1) +  # group lines
  geom_ribbon(data = avg_tr, aes(x = Year, ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +  # CI ribbon
  geom_line(data = avg_tr, aes(x = Year, y = mean_F), color = "red", size = 1.2) +  # mean line
  theme(legend.position = "none")+theme_classic()+coord_cartesian(ylim = c(0, 4.5))+labs(title = "C) Alaska")

rm(list=setdiff(ls(), c("NWEU","NWAT","ALA")))
