library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(classInt)
source("preprocess.R")

# Import Data ----
Data = preprocess()
PL_Data = Data$PL        #Party-List
Const_Data = Data$Const  #Constituency


# Party-List ----
## Local Rate ----
pl_lr <- function(PL_Data){
  BJT <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_BJT)) + 
    geom_sf() +
    ggtitle("Party-List: Bhumjaithai Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "skyblue", "#0F1599"),
                        breaks = c(0.1, 0.2),
                        guide = guide_coloursteps(even.steps = FALSE,
                                                  show.limits = TRUE,
                                                  title = "Local Rate"))
  
  CT <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_CT)) + 
    geom_sf() +
    ggtitle("Party-List: Chartthaipattana Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#E90080"),
                      breaks = c(0.1),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  DEM <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_Democrat)) +
    geom_sf() + 
    ggtitle("Party-List: Democrat Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#AFEEEE", "#00A1F1"),
                      breaks = c(0.1, 0.2),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  MFP <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_MFP)) + 
    geom_sf() +
    ggtitle("Party-List: Move Forward Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#FFDAB9", "#FFD700", "#FFA500", "#EF771E"),
                      breaks = c(0.3, 0.4, 0.5),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  PCC <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_Prachachat)) +
    geom_sf() + 
    ggtitle("Party-List: Prachachat Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#BA810D"),
                      breaks = c(0.25),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  PPRP <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_PPRP)) +
    geom_sf() + 
    ggtitle("Party-List: Palang Pracharath Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#4061A6"),
                      breaks = c(0.05),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  PT <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_PheuThai)) +
    geom_sf() + 
    ggtitle("Party-List: Pheu Thai Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#FA8072", "#E30613"),
                      breaks = c(0.3, 0.4, 0.5),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  TST <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_TST)) +
    geom_sf() + 
    ggtitle("Party-List: Thai Sang Thai Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#65408F"),
                      breaks = c(0.05),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  UTNP <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_UTNP)) +
    geom_sf() + 
    ggtitle("Party-List: United Thai Nation Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "skyblue", "blue", "#0000CD"),
                      breaks = c(0.1, 0.2, 0.3),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))  
  
  return(list(BJT, CT, DEM, MFP, PCC, PPRP, PT, TST, UTNP))
}
plLR_out <- pl_lr(PL_Data)
plLR_out[[1]] # BJT
plLR_out[[2]] # CT
plLR_out[[3]] # DEM
plLR_out[[4]] # MFP
plLR_out[[5]] # PCC
plLR_out[[6]] # PPRP
plLR_out[[7]] # PT
plLR_out[[8]] # TST
plLR_out[[9]] # UTNP

## Votes ----
pl_v <- function(PL_Data){
  BJT <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_BJT)) +
    geom_sf() +
    ggtitle("Party-List: Bhumjaithai Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "skyblue", "blue", "#0F1599"),
                      breaks = c(10000, 30000, 50000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  CT <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_CT)) +
    geom_sf() +
    ggtitle("Party-List: Chartthaipattana Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#E90080"),
                      breaks = c(30000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  DEM <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_Democrat)) +
    geom_sf() +
    ggtitle("Party-List: Democrat Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#AFEEEE", "#00A1F1"),
                      breaks = c(10000, 20000, 50000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  MFP <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_MFP)) +
    geom_sf() +
    ggtitle("Party-List: Move Forward Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("#FFDAB9", "#FFD700", "#FFA500", "#EF771E"),
                      breaks = c(100000, 200000, 300000, 400000, 600000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  PCC <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_Prachachat)) +
    geom_sf() +
    ggtitle("Party-List: Prachachat Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#BA810D"),
                      breaks = c(75000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  PPRP <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_PPRP)) +
    geom_sf() +
    ggtitle("Party-List: Palang Pracharath Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "lightblue", "#4061A6"),
                      breaks = c(10000, 20000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  
  PT <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_PheuThai)) +
    geom_sf() +
    ggtitle("Party-List: Pheu Thai Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#FA8072", "#E30613"),
                      breaks = c(100000, 200000, 300000, 400000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  TST <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_TST)) +
    geom_sf() +
    ggtitle("Party-List: Thai Sang Thai Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#65408F"),
                      breaks = c(10000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  UTNP <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_UTNP)) +
    geom_sf() +
    ggtitle("Party-List: United Thai Nation Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "skyblue", "blue", "#0000CD"),
                      breaks = c(25000, 50000, 75000, 100000, 200000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))  
  
  return(list(BJT, CT, DEM, MFP, PCC, PPRP, PT, TST, UTNP))
}
plV_out <- pl_v(PL_Data)
plV_out[[1]] # BJT
plV_out[[2]] # CT
plV_out[[3]] # DEM
plV_out[[4]] # MFP
plV_out[[5]] # PCC
plV_out[[6]] # PPRP
plV_out[[7]] # PT
plV_out[[8]] # TST
plV_out[[9]] # UTNP

## Compare how to classify ----
MFP_PL <- PL_Data %>% 
  select(ADM1_EN, Local_Rate_MFP, Votes_MFP, Global_Rate_MFP, geometry)
  
### Equal Interval ----
classIntervals(MFP_PL$Votes_MFP, n = 4, style = "equal")$brks
MFP_PL$EI_votes <- cut(MFP_PL$Votes_MFP, 
                       breaks = classIntervals(MFP_PL$Votes_MFP, n = 4, style = "equal")$brks,
                       include.lowest = TRUE,
                       labels = c("30,550.0 - 423,079.2", "423,079.3 - 815,608.5", 
                                  "815,608.6 - 1,208,137.8", "1,208,137.9 - 1,600,667.0"))
summary(MFP_PL$EI_votes)
ggplot(data = MFP_PL, aes(geom="sf", fill=EI_votes)) +
  geom_sf() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  scale_fill_manual(values = c("#FFDAB9", "#FFD700", "#FFA500", "#EF771E"),
                    breaks = levels(MFP_PL$EI_votes),
                    guide = guide_legend(title = "Equal Interval of Votes"))

### Quantile ----
classIntervals(MFP_PL$Votes_MFP, n = 4, style = "quantile")$brks
MFP_PL$Q_votes <- cut(MFP_PL$Votes_MFP, 
                      breaks = classIntervals(MFP_PL$Votes_MFP, n = 4, style = "quantile")$brks,
                      include.lowest = TRUE,
                      labels = c("30,550 - 87,981", "87,982 - 137,539", 
                                 "137,540 -224,197", "224,198 - 1,600,667"))
summary(MFP_PL$Q_votes)
ggplot(data = MFP_PL, aes(geom="sf", fill=Q_votes)) +
  geom_sf() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  scale_fill_manual(values = c("#FFDAB9", "#FFD700", "#FFA500", "#EF771E"),
                    breaks = levels(MFP_PL$Q_votes),
                    guide = guide_legend(title = "Quantile of Votes"))


### Maximum breaks ----
# classIntervals(MFP_PL$Votes_MFP, 
#                n = 4, 
#                style = "fixed",
#                fixedBreaks = c(min(MFP_PL$Votes_MFP), 100,000, 200,000, ))$brks
# 
# MFP_PL$MB_votes <- cut(MFP_PL$Votes_MFP,
#                       breaks = c(100000, 200000, 300000, 400000, 600000),
#                       include.lowest = TRUE)
# 
# ggplot(data = MFP_PL, aes(geom="sf", fill=MB_votes)) +
#   geom_sf() +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         rect = element_blank()) +
#   scale_fill_manual(colours = c("#FFDAB9", "#FFD700", "#FFA500", "#EF771E"),
#                     breaks = c(100000, 200000, 300000, 400000, 600000),
#                     guide = guide_coloursteps(title = "Maximum Breaks of Votes"))



# Constituency ----
# Only consider local rate
const_lr <- function(Const_Data){
  BJT <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_BJT)) +
    geom_sf() + 
    ggtitle("Constituency: Bhumjaithai Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "skyblue", "#0F1599"),
                      breaks = c(0.2, 0.4),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
    
  CT <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_CT)) +
    geom_sf() + 
    ggtitle("Constituency: Chartthaipattana Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#E90080"),
                      breaks = c(0.2, 0.4),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate")) 
    
  DEM <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_Democrat)) +
    geom_sf() + 
    ggtitle("Constituency: Democrat Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#AFEEEE", "#00A1F1"),
                      breaks = c(0.25, 0.5),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  MFP <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_MFP)) +
    geom_sf() + 
    ggtitle("Constituency: Move Forward Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#FFDAB9", "#FFD700", "#FFA500", "#EF771E"),
                      breaks = c(0.125, 0.25, 0.375),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
    
  PCC <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_Prachachat)) +
    geom_sf() + 
    ggtitle("Constituency: Prachachat Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#BA810D"),
                      breaks = c(0.25),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  PPRP <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_PPRP)) +
    geom_sf() + 
    ggtitle("Constituency: Palang Pracharath Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "lightblue", "#4061A6"),
                      breaks = c(0.2, 0.4),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
    
  PT <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_PheuThai)) +
    geom_sf() + 
    ggtitle("Constituency: Pheu Thai Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#FA8072", "#E30613"),
                      breaks = c(0.125, 0.25, 0.375),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))

  TST <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_TST)) +
    geom_sf() + 
    ggtitle("Constituency: Thai Sang Thai Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "#65408F"),
                      breaks = c(0.2),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
    
  UTNP <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_UTNP)) +
    geom_sf() + 
    ggtitle("Constituency: United Thai Nation Party") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_stepsn(colours = c("white", "skyblue", "blue", "#0000CD"),
                      breaks = c(0.125, 0.25, 0.375),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  return(list(BJT, CT, DEM, MFP, PCC, PPRP, PT, TST, UTNP))
}
const_out <- const_lr(Const_Data)
const_out[[1]] # BJT
const_out[[2]] # CP
const_out[[3]] # DEM
const_out[[4]] # MFP
const_out[[5]] # PCC
const_out[[6]] # PPRP
const_out[[7]] # PT
const_out[[8]] # TST
const_out[[9]] # UTNP


# # Plot radius of specific point
# centroid <- st_centroid(PL_Data) 
# 
# ## BJT 
# centroid$geometry[centroid$ADM1_EN == "Uthai Thani"] #(99.47819 15.35016)
# ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_BJT)) +
#   geom_sf() + 
#   # geom_point(aes(99.47819, 15.35016), size=1, shape=1,  color="#ff0000") +
#   ggtitle("Constituency: Bhumjaithai Party") +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         rect = element_blank()) +
#   scale_fill_stepsn(colours = c("white", "skyblue", "#0F1599"),
#                     breaks = c(0.2, 0.4),
#                     guide = guide_coloursteps(even.steps = FALSE,
#                                               show.limits = TRUE,
#                                               title = "Local Rate"))