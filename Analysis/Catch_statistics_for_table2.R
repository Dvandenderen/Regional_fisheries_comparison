load("Data/Sea around us/NWEU_SAU.Rdata")
load("Data/Sea around us/ALA_SAU.Rdata")
load("Data/Sea around us/NWA_SAU.Rdata")

regions <- list(ALA, NWEU, NWA) # or NWA or ALA

# fishing nations
nb_fishing_nat <- data.frame(region=c("ALA","NWEU","NWA"),nb=NA)
for (j in 1:3){
  reg <- regions[[j]]
  reg <- subset(reg,reg$year %in% c(2010:2019))
  fnations <- aggregate(reg$tonnes,by=list(reg$fishing_entity),FUN=sum)
  fnations$perc <- fnations$x / sum(fnations$x) * 100
  fnations <- fnations[order(-fnations$perc),] 
  fnations$cumb <- cumsum(fnations$perc)
  nb_fishing_nat[j,2] <- which(fnations$cumb > 95)[1]
}
nb_fishing_nat

# get landing diversity
land_div <- data.frame(region=c("ALA","NWEU","NWA"),nb=NA)
for (j in 1:3){
  reg <- regions[[j]]
  reg <- subset(reg,reg$year %in% c(2010:2019))
  regland <- subset(reg,reg$catch_type =="Landings")
  shan <- aggregate(regland$tonnes,by=list(regland$scientific_name),FUN=sum)
  land_div[j,2] <- vegan::diversity(shan$x,"simpson")
}
land_div 

# get value diversity
value_div <- data.frame(region=c("ALA","NWEU","NWA"),nb=NA)
for (j in 1:3){
  reg <- regions[[j]]
  reg <- subset(reg,reg$year %in% c(2010:2019))
  regland <- subset(reg,reg$catch_type =="Landings")
  shan <- aggregate(regland$landed_value,by=list(regland$scientific_name),FUN=sum)
value_div[j,2] <- vegan::diversity(shan$x,"simpson")
}
value_div 

# landings dem/pel/inv
demidx <- c("Medium demersals (30 - 89 cm)","Large sharks (>=90 cm)","Small to medium flatfishes (<90 cm)",
            "Large demersals (>=90 cm)","Small to medium flatfishes (<90 cm)","Medium bathydemersals (30 - 89 cm)",
            "Large rays (>=90 cm)","Large reef assoc. fish (>=90 cm)","Large benthopelagics (>=90 cm)",
            "Medium benthopelagics (30 - 89 cm)","Small to medium sharks (<90 cm)","Large flatfishes (>=90 cm)",
            "Medium reef assoc. fish (30 - 89 cm)","Small reef assoc. fish (<30 cm)","Small benthopelagics (<30 cm)",
            "Large bathydemersals (>=90 cm)","Small demersals (<30 cm)","Small to medium rays (<90 cm)")
invidx <- c("Shrimps","Other demersal invertebrates","Cephalopods","Lobsters, crabs")            
pelidx <- c("Medium pelagics (30 - 89 cm)","Small pelagics (<30 cm)","Large bathypelagics (>=90 cm)",
            "Large pelagics (>=90 cm)","Medium bathypelagics (30 - 89 cm)","Small bathypelagics (<30 cm)")

landings <- data.frame(region=c("ALA","NWEU","NWA"),
                       dem=NA,inv=NA,pel=NA,tot=NA)
for (j in 1:3){
  reg <- regions[[j]]
  reg <- subset(reg,reg$year %in% c(2010:2019))
  reg$func <- NA
  reg$func <- ifelse(reg$functional_group %in% demidx,"dem",reg$func)
  reg$func <- ifelse(reg$functional_group %in% invidx,"inv",reg$func)
  reg$func <- ifelse(reg$functional_group %in% pelidx,"pel",reg$func)
  land    <- aggregate(reg$tonnes,by=list(reg$year,reg$catch_type,reg$func),FUN=sum)
  land    <- subset(land,land$Group.2 == "Landings")
  land    <- aggregate(land$x,by=list(land$Group.3),FUN=mean)
  landings[j,2:4]  <- land$x/1000
  landings[j,5] <- sum(landings[j,2:4])
}

# values dem/pel/inv
values <- data.frame(region=c("ALA","NWEU","NWA"),
                       dem=NA,inv=NA,pel=NA,tot=NA)
for (j in 1:3){
  reg <- regions[[j]]
  reg <- subset(reg,reg$year %in% c(2010:2019))
  reg$func <- NA
  reg$func <- ifelse(reg$functional_group %in% demidx,"dem",reg$func)
  reg$func <- ifelse(reg$functional_group %in% invidx,"inv",reg$func)
  reg$func <- ifelse(reg$functional_group %in% pelidx,"pel",reg$func)
  value    <- aggregate(reg$landed_value,by=list(reg$year,reg$func),FUN=sum,na.rm=T)
  value    <- aggregate(value$x,by=list(value$Group.2),FUN=mean)
  value$x_stand  <- value$x/(catch$x*10^6)
  values[j,2:4]  <- value$x / 10^6
  values[j,5] <- sum(values[j,2:4])
}
values

values[,2:5]/landings[,2:5]

# selectivity dem/pel/inv
selectivity <- data.frame(region=c("ALA","NWEU","NWA"),
                     dem=NA,inv=NA,pel=NA)
discards    <- data.frame(region=c("ALA","NWEU","NWA"),
                          dem=NA,inv=NA,pel=NA,tot=NA)

for (j in 1:3){
  reg <- regions[[j]]
  reg <- subset(reg,reg$year %in% c(2010:2019))
  reg$func <- NA
  reg$func <- ifelse(reg$functional_group %in% demidx,"dem",reg$func)
  reg$func <- ifelse(reg$functional_group %in% invidx,"inv",reg$func)
  reg$func <- ifelse(reg$functional_group %in% pelidx,"pel",reg$func)
  selec    <- aggregate(reg$tonnes,by=list(reg$year,reg$func,reg$catch_type),FUN=sum,na.rm=T)
  selec    <- aggregate(selec$x,by=list(selec$Group.2,selec$Group.3),FUN=mean)
  selec2    <- data.frame(group = selec[1:3,1],discards = selec[1:3,3],landings= selec[4:6,3])
  selec2[,2:3] <- selec2[,2:3]/1000
  selectivity[j,2:4] <- selec2$discards/(selec2$discards+selec2$landings)*100
  discs  <- subset(reg, reg$catch_type == "Discards")
  discs  <- aggregate(discs$tonnes,by=list(discs$func,discs$year),FUN=sum,na.rm=T)
  discards[j,2:4] <- aggregate(discs$x,by=list(discs$Group.1),FUN=mean,na.rm=T)[,2]/1000
  discards[j,5] <- sum(discards[j,2:4])
}
selectivity
discards

# check selectivity per gear type 
# if needed, define for which region
j <- NA
reg <- regions[[j]]
reg <- subset(reg,reg$year %in% c(2010:2019))
selec    <- aggregate(reg$tonnes,by=list(reg$year,reg$gear_type,reg$catch_type),FUN=sum,na.rm=T)
selec    <- aggregate(selec$x,by=list(selec$Group.2,selec$Group.3),FUN=mean)
tr <- subset(selec,selec$Group.2 == "Landings")
tr2 <- subset(selec,selec$Group.2 == "Discards")
tr <- cbind(tr,tr2[match(tr$Group.1,tr2$Group.1),c(3)])
tr <- tr[,c(1,3,4)]
colnames(tr) <- c("Group","Landings","Discards")
tr$Discards[is.na(tr$Discards)] <- 0
tr$frac <- tr$Discards/(tr$Discards+tr$Landings)*100
tr[order(tr$Landings),]

# The SAU data reported unusually high discard rates in U.S. scallop dredge fisheries, 
# with discard fractions (discards ÷ total catch × 100) listed at 50–70%. 
# We cross-checked these values against NOAA’s National Bycatch Report and 
# recalculated the fraction, finding it to be closer to 22%. The SAU data were 
# adjusted accordingly for this fishery type 
# code shows it worked
reg <- regions[[3]]
reg$uni <- paste(reg$gear_type,reg$fishing_entity,reg$common_name,reg$year)
reg <- subset(reg,reg$gear_type == "dredge" & reg$fishing_entity == "USA" & reg$common_name=="American sea scallop")
selec    <- aggregate(reg$tonnes,by=list(reg$year,reg$catch_type),FUN=sum,na.rm=T)
#selec    <- aggregate(selec$x,by=list(selec$Group.2,selec$Group.3),FUN=mean)
tr <- subset(selec,selec$Group.2 == "Landings")
tr2 <- subset(selec,selec$Group.2 == "Discards")
tr <- cbind(tr,tr2[match(tr$Group.1,tr2$Group.1),c(3)])
tr <- tr[,c(1,3,4)]
colnames(tr) <- c("Year","Landings","Discards")
tr$Discards[is.na(tr$Discards)] <- 0
tr$frac <- tr$Discards/(tr$Discards+tr$Landings)*100
tr[order(-tr$Landings),]

# ------------------------------------------------------------------------------
# get common species
reg <- regions[[2]] # NWEU
reg <- subset(reg, reg$year %in% c(2010:2020))
catchEU    <- aggregate(reg$tonnes,by=list(reg$common_name),FUN=sum)
catchEU[order(catchEU$x), ]
catchEU    <- aggregate(reg$landed_value,by=list(reg$common_name),FUN=sum,na.rm=T)
catchEU[order(catchEU$x), ]

reg <- regions[[3]] # NWA
reg <- subset(reg, reg$year %in% c(2010:2020))
catchNWA    <- aggregate(reg$tonnes,by=list(reg$common_name),FUN=sum)
catchNWA[order(catchNWA$x), ]
catchNWA    <- aggregate(reg$landed_value,by=list(reg$common_name),FUN=sum,na.rm=T)
catchNWA[order(catchNWA$x), ]

reg <- regions[[1]] # ALA
reg <- subset(reg, reg$year %in% c(2010:2020))
catchALA    <- aggregate(reg$tonnes,by=list(reg$common_name),FUN=sum)
catchALA[order(catchALA$x), ]
catchALA    <- aggregate(reg$landed_value,by=list(reg$common_name),FUN=sum,na.rm=T)
catchALA[order(catchALA$x), ]
