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

autoCorr <- function(data, nb, plot=TRUE, lisa=FALSE){
  # Calculate lag in every province
  lw <- nb2listw(nb, style="W", zero.policy=TRUE) #list weight 
  data$lag <- lag.listw(lw, data$Local_Rate)      #lag value
  
  # For LISA 
  if (lisa){
    avg_LR <- mean(data$Local_Rate)
    data <- data %>%
      mutate(LISA = case_when(
        Local_Rate<avg_LR & lag<avg_LR ~ "Low-Low",
        Local_Rate>avg_LR & lag<avg_LR ~ "High-Low",
        Local_Rate<avg_LR & lag>avg_LR ~ "Low-High",
        Local_Rate>avg_LR & lag>avg_LR ~ "High-High",
      ))
    # print(data)
    
    if (plot){
      reg <- lm(lag ~ Local_Rate, data)
      ggplot(data=data, aes(x=Local_Rate, y=lag, color = LISA)) +
        geom_point()
    }
    
  }else{
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

### Visualize for blog ----
PL_Data$ADM1_EN[nb_con[[which(PL_Data$ADM1_EN == "Bangkok")]]]
nb_con[[which(PL_Data$ADM1_EN == "Bangkok")]]

# Spatial Data
Spatial_thai <- st_read('SpatialData/tha_cod-ab.shp')
Spatial_thai <- Spatial_thai %>% 
  select(Province = ADM1_EN, geometry)

Spatial_thai$Ncont <- NA
Spatial_thai$Ncont[which(PL_Data$ADM1_EN == "Bangkok")] <- "Bangkok"
Spatial_thai$Ncont[nb_con[[which(PL_Data$ADM1_EN == "Bangkok")]]] <- "Neighbor"
Spatial_thai$Ncont[is.na(Spatial_thai$Ncont)] <- "Other"

ggplot(data = Spatial_thai, aes(geom="sf", fill=Ncont)) +
  geom_sf() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  scale_fill_manual(breaks = c("Bangkok", "Neighbor", "Other"),
                    values = c("black", "red", "white")) +
  labs(fill = "Bangkok's Neighbors",
       title = "Contiguous Neighborhood") 



## Distance based Adjacency ----
# Under assumption that attribute are constant over geometry, 
#   centroid can represent as that province.
centroid <- st_centroid(PL_Data)
nb_dist  <-  dnearneigh(centroid, 0, 100)  # 0-100 km

Spatial_thai$Ndist <- NA
Spatial_thai$Ndist[which(PL_Data$ADM1_EN == "Bangkok")] <- "Bangkok"
Spatial_thai$Ndist[nb_dist[[which(PL_Data$ADM1_EN == "Bangkok")]]] <- "Neighbor"
Spatial_thai$Ndist[is.na(Spatial_thai$Ndist)] <- "Other"

ggplot(data = Spatial_thai, aes(geom="sf", fill=Ndist)) +
  geom_sf() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  scale_fill_manual(breaks = c("Bangkok", "Neighbor", "Other"),
                    values = c("black", "red", "white")) +
  labs(fill = "Bangkok's Neighbors",
       title = "Distance Band Neighborhood with 100 km") 

## KNN ----
centroid <- st_centroid(PL_Data)
knn <-  knearneigh(centroid, k=4)
nb_knn <- knn2nb(knn)

Spatial_thai$Nknn <- NA
Spatial_thai$Nknn[which(PL_Data$ADM1_EN == "Bangkok")] <- "Bangkok"
Spatial_thai$Nknn[nb_knn[[which(PL_Data$ADM1_EN == "Bangkok")]]] <- "Neighbor"
Spatial_thai$Nknn[is.na(Spatial_thai$Nknn)] <- "Other"

ggplot(data = Spatial_thai, aes(geom="sf", fill=Nknn)) +
  geom_sf() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  scale_fill_manual(breaks = c("Bangkok", "Neighbor", "Other"),
                    values = c("black", "red", "white")) +
  labs(fill = "Bangkok's Neighbors",
       title = "K-Nearest Neighborhood with k=4") 


# Experiment: Party-List ----

## MFP ----
PL_MFP <- LR_party(PL_Data, "MFP")
PL_MFP_MC <- autoCorr(PL_MFP, nb_con)
plot(PL_MFP_MC, las=1)  #MC plot density
PL_MFP_MC$p.value       #pseudo p-value
PL_MFP_MC$statistic     #Moran's statistics

PL_MFP_MC <- autoCorr(PL_MFP, nb_con, lisa=TRUE)

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




# LISA for blog ----
PL_MFP <- LR_party(PL_Data, "MFP")
# PL_MFP_MC <- autoCorr(PL_MFP, nb_con)

lw <- nb2listw(nb_con, style="W", zero.policy=TRUE) #list weight 
PL_MFP$lag <- lag.listw(lw, PL_MFP$Local_Rate)      #lag value

avg_LR <- mean(PL_MFP$Local_Rate)
PL_MFP <- PL_MFP %>%
  mutate(LISA = case_when(
    Local_Rate<avg_LR & lag<avg_LR ~ "Low-Low",
    Local_Rate>avg_LR & lag<avg_LR ~ "High-Low",
    Local_Rate<avg_LR & lag>avg_LR ~ "Low-High",
    Local_Rate>avg_LR & lag>avg_LR ~ "High-High",
  ))

ggplot() +
  geom_point(data=PL_MFP, aes(x=Local_Rate, y=lag, color = LISA)) +
  scale_color_manual(values = c("#c80064", "#54bebe", "#98d1d1", "#df979e")) +
  geom_smooth(aes(lag, Local_Rate), PL_MFP, method = "lm", se=FALSE) +
  geom_hline(aes(yintercept = avg_LR), PL_MFP, color = "grey") +
  geom_vline(aes(xintercept = avg_LR), PL_MFP, color = "grey") +
  labs(title = "MFP Vote's Rate with LISA", 
       x = "Vote's Rate",
       y = "Lag Vote's Rate")

ggplot() +
  geom_point(data=PL_MFP, aes(x=Local_Rate, y=lag)) +
  geom_smooth(aes(lag, Local_Rate), PL_MFP, method = "lm", se=FALSE) +
  geom_hline(aes(yintercept = avg_LR), PL_MFP, color = "grey") +
  geom_vline(aes(xintercept = avg_LR), PL_MFP, color = "grey") +
  labs(title = "MFP Vote's Rate", 
       x = "Vote's Rate",
       y = "Lag Vote's Rate")


