library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(spdep)
# library(tmap)
source("preprocess.R")

# Load Data ----
Data = preprocess()
PL_Data = Data$PL
Const_Data = Data$Const


# Core Functions -----
LR_party <- function(Data, Party){
  if (Party == "MFP"){
    LR <- Data %>% 
      select(Province = ADM1_EN, Local_Rate = Local_Rate_MFP, geometry)
  }else if(Party == "PT"){
    LR <- Data %>% 
      select(Province = ADM1_EN, Local_Rate = Local_Rate_PheuThai, geometry)
  }else if(Party == "UTNP"){
    LR <- Data %>% 
      select(Province = ADM1_EN, Local_Rate = Local_Rate_UTNP, geometry)
  }else if(Party == "BJT"){
    LR <- Data %>% 
      select(Province = ADM1_EN, Local_Rate = Local_Rate_BJT, geometry)
  }else if(Party == "DEM"){
    LR <- Data %>% 
      select(Province = ADM1_EN, Local_Rate = Local_Rate_Democrat, geometry)
  }else if(Party == "PPRP"){
    LR <- Data %>% 
      select(Province = ADM1_EN, Local_Rate = Local_Rate_PPRP, geometry)
  }
  
  return(LR)
}

autoCorr <- function(data, nb, plot=TRUE){
  # Calculate lag in every province
  lw <- nb2listw(nb, style="W", zero.policy=TRUE) #list weight 
  data$lag <- lag.listw(lw, data$Local_Rate)      #lag value
  
  # Lag vs Local Rate
  if (plot){
    reg <- lm(lag ~ Local_Rate, data)               #linear regression
    plot(lag ~ Local_Rate, data, pch=21, asp=1, las=1, col = "black", bg="grey",
         xlab = "Local Rate",
         ylab = "Adjacency Local Rate") +
      abline(reg, col = "blue") +
      abline(v = mean(data$Local_Rate), lty=3, col = "grey") +
      abline(h = mean(data$Local_Rate), lty=3, col = "grey")
  }
    
  
  # Moran's I coefficient and MC
  moran <- moran(data$Local_Rate, listw = lw, n = length(nb), S0 = Szero(lw))
  MC <- moran.mc(data$Local_Rate, lw, nsim = 1000)

  return(MC)
}

# Define Adjacency ----
# Can use both PL_Data or Const_Data, just need geometry column

## Contiguous Adjacency ----
nb_con <- poly2nb(PL_Data, queen=TRUE)
# nb_con[[3]]
# PL_Data$ADM1_EN[3]
# PL_Data$ADM1_EN[nb_con[[3]]]

# Add Phangna and Phuket to be neighborhood
nb_con[[which(PL_Data$ADM1_EN == "Phuket")]] <- c(which(PL_Data$ADM1_EN == "Phangnga"))
nb_con[[which(PL_Data$ADM1_EN == "Phangnga")]] <- append(nb_con[[which(PL_Data$ADM1_EN == "Phangnga")]], 
                                                     which(PL_Data$ADM1_EN == "Phuket"))
PL_Data$ADM1_EN[nb_con[[which(PL_Data$ADM1_EN == "Phuket")]]]
PL_Data$ADM1_EN[nb_con[[which(PL_Data$ADM1_EN == "Phangnga")]]]

## Distance based Adjacency ----
# Under assumption that attribute are constant over geometry, 
#   centroid can represent as that province.
# centroid <- st_centroid(PL_Data) 
# nb_dist  <-  dnearneigh(centroid, 0, 100)  # 0-100 km
# PL_Data$ADM1_EN[3]
# nb_dist[[3]] |> length()
# nb_dist[[3]]
# PL_Data$ADM1_EN[nb_dist[[3]]]
# 
# # Perform experiment on different distances neighborhood in each
# #   - data: PL_Data or Const_Data
# #   - party: MFP, PT, UTNP, BJT, DEM
# 
# distance_nb <- function(data, party, distStart, distMax, distStep){
#   # Extract data for that party
#   data_party <- LR_party(data, party)
#   
#   # Autocorrelation that neighborhood based on distance
#   df_distMoran <- data.frame(distance=numeric(), morans=numeric(), pValue=numeric())
#   centroid <- st_centroid(PL_Data) 
#   for (i in seq(distStart, distMax, distStep)){
#     nb_dist  <-  dnearneigh(centroid, 0, i)  # 0-i km
#     MC <- autoCorr(data_party, nb_dist, plot=FALSE)
#     df_distMoran <- rbind(df_distMoran, data.frame(distance = i, 
#                                                    moran = MC$statistic,
#                                                    pValue = MC$p.value))
#   }
#   
#   return(df_distMoran)
# }


# Experiment: Party-List ----

## MFP ----
PL_MFP <- LR_party(PL_Data, "MFP")
PL_MFP_MC <- autoCorr(PL_MFP, nb_con)
plot(PL_MFP_MC, las=1)  #MC plot density
PL_MFP_MC$p.value       #pseudo p-value
PL_MFP_MC$statistic     #Moran's statistics

## Pheu Thai ----
PL_PT <- LR_party(PL_Data, "PT")
PL_PT_MC <- autoCorr(PL_PT, nb_con)
plot(PL_PT_MC, las=1)  #MC plot density
PL_PT_MC$p.value       #pseudo p-value
PL_PT_MC$statistic     #Moran's statistics

## UTNP ----
PL_UTNP <- LR_party(PL_Data, "UTNP")
PL_UTNP_MC <- autoCorr(PL_UTNP, nb_con)
plot(PL_UTNP_MC, las=1)  #MC plot density
PL_UTNP_MC$p.value       #pseudo p-value
PL_UTNP_MC$statistic     #Moran's statistics


# ## Distance based Adjacency
# ### MFP
# moran_MFP = distance_nb(PL_Data, "MFP", distStart=100, distMax=1000, distStep=100)
# ggplot(moran_MFP, aes(x=distance, y=moran)) +
#   geom_point() +
#   geom_hline(yintercept = 0) +
#   ylim(-1, 1)
# 
# 
# ### Pheu Thai
# moran_PT = distance_nb(PL_Data, "PT", distStart=100, distMax=1000, distStep=100)
# ggplot(moran_PT, aes(x=distance, y=moran)) +
#   geom_point() +
#   geom_hline(yintercept = 0) +
#   ylim(-1, 1)
# 
# ### UTNP
# moran_UTNP = distance_nb(PL_Data, "UTNP", distStart=100, distMax=1000, distStep=100)
# eggplot(moran_UTNP, aes(x=distance, y=moran)) +
#   geom_point() +
#   geom_hline(yintercept = 0) +
#   ylim(-1, 1)


# Experiment: Constituency ----

## MFP ----
Const_MFP <- LR_party(Const_Data, "MFP")
Const_MFP_MC <- autoCorr(Const_MFP, nb_con)
plot(Const_MFP_MC, las=1)  #MC plot density
Const_MFP_MC$p.value       #pseudo p-value
Const_MFP_MC$statistic     #Moran's statistics

## Pheu Thai ----
Const_PT <- LR_party(Const_Data, "PT")
Const_PT_MC <- autoCorr(Const_PT, nb_con)
plot(Const_PT_MC, las=1)  #MC plot density
Const_PT_MC$p.value       #pseudo p-value
Const_PT_MC$statistic     #Moran's statistics

## UTNP ----
Const_UTNP <- LR_party(Const_Data, "UTNP")
Const_UTNP_MC <- autoCorr(Const_UTNP, nb_con)
plot(Const_UTNP_MC, las=1)  #MC plot density
Const_UTNP_MC$p.value       #pseudo p-value
Const_UTNP_MC$statistic     #Moran's statistics

## BJT ----
Const_BJT <- LR_party(Const_Data, "BJT")
Const_BJT_MC <- autoCorr(Const_BJT, nb_con)
plot(Const_BJT_MC, las=1)  #MC plot density
Const_BJT_MC$p.value       #pseudo p-value
Const_BJT_MC$statistic     #Moran's statistics

## DEM ----
Const_DEM <- LR_party(Const_Data, "DEM")
Const_DEM_MC <- autoCorr(Const_DEM, nb_con)
plot(Const_DEM_MC, las=1)  #MC plot density
Const_DEM_MC$p.value       #pseudo p-value
Const_DEM_MC$statistic     #Moran's statistics

## PPRP ----
Const_PPRP <- LR_party(Const_Data, "PPRP")
Const_PPRP_MC <- autoCorr(Const_PPRP, nb_con)
plot(Const_PPRP_MC, las=1)  #MC plot density
Const_PPRP_MC$p.value       #pseudo p-value
Const_PPRP_MC$statistic     #Moran's statistics
