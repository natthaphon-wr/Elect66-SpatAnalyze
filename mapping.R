library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(gridExtra)
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
    scale_fill_stepsn(colours = c("white", "skyblue", "#0F1599"),
                        breaks = c(0.1, 0.2),
                        guide = guide_coloursteps(even.steps = FALSE,
                                                  show.limits = TRUE,
                                                  title = "Local Rate"))
  
  CT <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_CT)) + 
    geom_sf() +
    ggtitle("Party-List: Chartthaipattana Party") +
    scale_fill_stepsn(colours = c("white", "#E90080"),
                      breaks = c(0.1),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  DEM <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_Democrat)) +
    geom_sf() + 
    ggtitle("Party-List: Democrat Party") +
    scale_fill_stepsn(colours = c("white", "#AFEEEE", "#00A1F1"),
                      breaks = c(0.1, 0.2),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  MFP <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_MFP)) + 
    geom_sf() +
    ggtitle("Party-List: Move Forward Party") +
    scale_fill_stepsn(colours = c("white", "#FFDAB9", "#FFD700", "#FFA500", "#EF771E"),
                      breaks = c(0.3, 0.4, 0.5),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  PCC <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_Prachachat)) +
    geom_sf() + 
    ggtitle("Party-List: Prachachat Party") +
    scale_fill_stepsn(colours = c("white", "#BA810D"),
                      breaks = c(0.25),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  PPRP <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_PPRP)) +
    geom_sf() + 
    ggtitle("Party-List: Palang Pracharath Party") +
    scale_fill_stepsn(colours = c("white", "#4061A6"),
                      breaks = c(0.05),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  PT <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_PheuThai)) +
    geom_sf() + 
    ggtitle("Party-List: Pheu Thai Party") +
    scale_fill_stepsn(colours = c("white", "#FA8072", "#E30613"),
                      breaks = c(0.3, 0.4, 0.5),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  TST <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_TST)) +
    geom_sf() + 
    ggtitle("Party-List: Thai Sang Thai Party") +
    scale_fill_stepsn(colours = c("white", "#65408F"),
                      breaks = c(0.05),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  UTNP <- ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_UTNP)) +
    geom_sf() + 
    ggtitle("Party-List: United Thai Nation Party") +
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
    scale_fill_stepsn(colours = c("white", "skyblue", "blue", "#0F1599"),
                      breaks = c(10000, 30000, 50000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  CT <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_CT)) +
    geom_sf() +
    ggtitle("Party-List: Chartthaipattana Party") +
    scale_fill_stepsn(colours = c("white", "#E90080"),
                      breaks = c(30000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  DEM <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_Democrat)) +
    geom_sf() +
    ggtitle("Party-List: Democrat Party") +
    scale_fill_stepsn(colours = c("white", "#AFEEEE", "#00A1F1"),
                      breaks = c(10000, 20000, 50000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  MFP <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_MFP)) +
    geom_sf() +
    ggtitle("Party-List: Move Forward Party") +
    scale_fill_stepsn(colours = c("#FFDAB9", "#FFD700", "#FFA500", "#EF771E"),
                      breaks = c(100000, 200000, 300000, 400000, 600000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  PCC <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_Prachachat)) +
    geom_sf() +
    ggtitle("Party-List: Prachachat Party") +
    scale_fill_stepsn(colours = c("white", "#BA810D"),
                      breaks = c(75000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  PPRP <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_PPRP)) +
    geom_sf() +
    ggtitle("Party-List: Palang Pracharath Party") +
    scale_fill_stepsn(colours = c("white", "lightblue", "#4061A6"),
                      breaks = c(10000, 20000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  
  PT <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_PheuThai)) +
    geom_sf() +
    ggtitle("Party-List: Pheu Thai Party") +
    scale_fill_stepsn(colours = c("white", "#FA8072", "#E30613"),
                      breaks = c(100000, 200000, 300000, 400000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  TST <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_TST)) +
    geom_sf() +
    ggtitle("Party-List: Thai Sang Thai Party") +
    scale_fill_stepsn(colours = c("white", "#65408F"),
                      breaks = c(10000),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Votes",
                                                barheight = unit(3, "in")))
  
  UTNP <- ggplot(data = PL_Data, aes(geom="sf", fill=Votes_UTNP)) +
    geom_sf() +
    ggtitle("Party-List: United Thai Nation Party") +
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


# Constituency ----
# Only consider local rate
const_lr <- function(PL_Data){
  BJT <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_BJT)) +
    geom_sf() + 
    ggtitle("Constituency: Bhumjaithai Party") +
    scale_fill_stepsn(colours = c("white", "skyblue", "#0F1599"),
                      breaks = c(0.2, 0.4),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
    
  CT <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_CT)) +
    geom_sf() + 
    ggtitle("Constituency: Chartthaipattana Party") +
    scale_fill_stepsn(colours = c("white", "#E90080"),
                      breaks = c(0.2, 0.4),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate")) 
    
  DEM <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_Democrat)) +
    geom_sf() + 
    ggtitle("Constituency: Democrat Party") +
    scale_fill_stepsn(colours = c("white", "#AFEEEE", "#00A1F1"),
                      breaks = c(0.25, 0.5),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  MFP <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_MFP)) +
    geom_sf() + 
    ggtitle("Constituency: Move Forward Party") +
    scale_fill_stepsn(colours = c("white", "#FFDAB9", "#FFD700", "#FFA500", "#EF771E"),
                      breaks = c(0.125, 0.25, 0.375),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
    
  PCC <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_Prachachat)) +
    geom_sf() + 
    ggtitle("Constituency: Prachachat Party") +
    scale_fill_stepsn(colours = c("white", "#BA810D"),
                      breaks = c(0.25),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  PPRP <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_PPRP)) +
    geom_sf() + 
    ggtitle("Constituency: Palang Pracharath Party") +
    scale_fill_stepsn(colours = c("white", "lightblue", "#4061A6"),
                      breaks = c(0.2, 0.4),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
    
  PT <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_PheuThai)) +
    geom_sf() + 
    ggtitle("Constituency: Pheu Thai Party") +
    scale_fill_stepsn(colours = c("white", "#FA8072", "#E30613"),
                      breaks = c(0.125, 0.25, 0.375),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))

  TST <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_TST)) +
    geom_sf() + 
    ggtitle("Constituency: Thai Sang Thai Party") +
    scale_fill_stepsn(colours = c("white", "#65408F"),
                      breaks = c(0.2),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
    
  UTNP <- ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_UTNP)) +
    geom_sf() + 
    ggtitle("Constituency: United Thai Nation Party") +
    scale_fill_stepsn(colours = c("white", "skyblue", "blue", "#0000CD"),
                      breaks = c(0.125, 0.25, 0.375),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE,
                                                title = "Local Rate"))
  
  return(list(BJT, CT, DEM, MFP, PCC, PPRP, PT, TST, UTNP))
}
const_out <- const_lr(PL_Data)
const_out[[1]] # BJT
const_out[[2]] # CT
const_out[[3]] # DEM
const_out[[4]] # MFP
const_out[[5]] # PCC
const_out[[6]] # PPRP
const_out[[7]] # PT
const_out[[8]] # TST
const_out[[9]] # UTNP
