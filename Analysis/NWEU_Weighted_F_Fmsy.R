
tr <- read.csv("Data/ICES_stocks/StockAssessmentGraphs.csv")

stBS <- c("cod.27.22-24", "her.27.20-24","her.27.25-2932","her.27.28","ple.27.21-23","sol.27.20-24",
          "spr.27.22-32")
stCS <-   c("cod.27.6a","cod.27.7e-k","had.27.6b","had.27.7a","had.27.7b-k","her.27.irls",
          "her.27.nirs","lez.27.4a6a","nep.fu.11","nep.fu.12","nep.fu.14","nep.fu.15",
          "nep.fu.16","nep.fu.17","nep.fu.19","nep.fu.2021","nep.fu.22","ple.27.7a",
          "sol.27.7a","sol.27.7e","sol.27.7fg","whg.27.7a","whg.27.7b-ce-k")
stNS <- c("cod.27.47d20","had.27.46a20","her.27.3a47d","nep.fu.6","nep.fu.7",
          "nep.fu.8","nep.fu.9","nop.27.3a4","ple.27.420","ple.27.7d","pok.27.3a46",
          "pra.27.3a4a","san.sa.1r", "san.sa.2r", "san.sa.3r","san.sa.4", "sol.27.4",
          "spr.27.3a4","tur.27.4", "whg.27.47d","wit.27.3a47d")
          
tr <- subset(tr,tr$FishStock %in% c(stBS,stNS,stCS))
tr <- subset(tr,tr$FishingPressureDescription == "F")
tr <- subset(tr,!(tr$StockSizeDescription == "B_index"))
tr <- subset(tr,!(tr$FishStock =="pra.27.3a4a" )) # remove remaining invertebrates

tr$FoverFMSY <- tr$FishingPressure / tr$FMSY
tr <- subset(tr, !(is.na(tr$FoverFMSY)))
tr$FoverFMSY_weight <- tr$StockSize * tr$FoverFMSY

# get stocksize weighted FMSY
test <- aggregate(list(tr$FMSY,tr$StockSize),by=list(tr$FishStock),FUN=mean,na.rm=T)
colnames(test) <- c("stock","Fmsy","Bio")
var    <- Hmisc::wtd.var(test$Fmsy,test$Bio)
value  <- sum(test$Fmsy*test$Bio)/sum(test$Bio)
stderr <- sqrt(var/nrow(test))
tstat  <- value/stderr ## not mx - mu
alpha  <- 1 - 0.95 # 0.95 conf.level
cint   <- qt(1 - alpha/2, nrow(test)-1)
cint   <- tstat + c(-cint, cint)
conf   <- cint * stderr
sumEU <- data.frame(meanfmsy=value,conf1=conf[1],conf2=conf[2])

# and create time series
reg  <- data.frame(reg = c(rep("W_Europe",42)))
reg$value <- NA
reg$numb <- NA
reg$overexplot <- NA
reg$var <- NA
reg$conf1 <- NA
reg$conf2 <- NA
years <- 1980:2021 ; reg$years <- 1980:2021

for (j in 1:42){
  daty <- subset(tr,tr$Year == years[j])
  daty <- subset(daty,!(is.na(daty$FoverFMSY)) & !(is.na(daty$StockSize)))
  reg$numb[j] <- nrow(daty)
  reg$overexplot[j] <- length(which(daty$FoverFMSY > 1.1))
  reg$var[j] <- Hmisc::wtd.var(daty$FoverFMSY,daty$StockSize)
  reg$value[j] <- sum(daty$FoverFMSY*daty$StockSize)/sum(daty$StockSize)
  stderr <- sqrt(reg$var[j]/reg$numb[j])
  tstat <- reg$value[j]/stderr ## not mx - mu
  alpha <- 1 - 0.95 # 0.95 conf.level
  cint <- qt(1 - alpha/2, reg$numb[j]-1)
  cint <- tstat + c(-cint, cint)
  conf <- cint * stderr
  reg$conf1[j] <- conf[1]
  reg$conf2[j] <- conf[2]
}

# https://stackoverflow.com/questions/37973240/how-to-create-a-confidence-interval-for-a-weighted-average-of-areas-under-the-ro
# weighted.ttest.ci <- function(x, weights, conf.level = 0.95) {
#   require(Hmisc)
#   nx <- length(x)
#   df <- nx - 1
#   vx <- wtd.var(x, weights, normwt = TRUE) ## From Hmisc
#   mx <- weighted.mean(x, weights)
#   stderr <- sqrt(vx/nx)
#   tstat <- mx/stderr ## not mx - mu
#   alpha <- 1 - conf.level
#   cint <- qt(1 - alpha/2, df)
#   cint <- tstat + c(-cint, cint)
#   cint * stderr
# }
NWEUR <- reg

rm(list=setdiff(ls(), c("NWEUR","sumEU")))