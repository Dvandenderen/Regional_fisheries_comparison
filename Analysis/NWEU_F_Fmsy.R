
library(ggplot2)
library(dplyr)

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
tr <- subset(tr,tr$Year %in% c(1980:2021))
ggplot()+geom_line(data=tr, aes(x=Year,y=FoverFMSY,col=FishStock,group=FishStock))+theme_classic()+
  theme(legend.position = "none")

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

NWEU <- ggplot() +
  geom_line(data = tr, aes(x = Year, y = FoverFMSY, group = FishStock), alpha = 0.1) +  # group lines
  geom_ribbon(data = avg_tr, aes(x = Year, ymin = lower, ymax = upper), fill = "black", alpha = 0.1) +  # CI ribbon
  geom_line(data = avg_tr, aes(x = Year, y = mean_F), color = "black", size = 1.2) +  # mean line
  theme(legend.position = "none")+theme_classic() + coord_cartesian(ylim = c(0, 4.5))+labs(title="A) NW Europe")


rm(list=setdiff(ls(), c("NWEU")))