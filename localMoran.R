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

local_moran <- function(data, p_sig, nb){
  # Perform Monte Carlo for Local Moran's I
  MC_LocalI <- localmoran_perm(data$Local_Rate, lw, nsim = 10000, zero.policy = TRUE)
  MC_LocalI.df <- as.data.frame(MC_LocalI)  #results of local moran's I with various value
  data$p  <- MC_LocalI.df$`Pr(folded) Sim`  #add psedo p-value to df
  
  # Group p-value to identify significant lvl (all types)
  data$p_factor <- cut(data$p, 
                       breaks = c(0, 0.001, 0.01, 0.05, 0.5),  # 0.1%, 1%, 5%, 50%, >50%
                       labels = c("0-0.001", "0.001-0.01", "0.01-0.05", "0.05-0.5"))
  plt_sigLVL <- ggplot(data = data, aes(geom="sf", fill=p_factor)) +
    geom_sf() + 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_manual(values = c("#DE2D26","#FC9272", "#FEE0D2", "grey90")) +
    labs(fill="P-value")
  
  # Significant Adjustment from cutoff value
  data$I_sig <- hotspot(MC_LocalI, Prname="Pr(folded) Sim", cutoff = p_sig, p.adjust = "none")
  data$I_sig <- factor(data$I_sig, levels=c("High-High","Low-Low", "Low-High", "High-Low", 
                                            "Not Significant"))
  data$I_sig[is.na(data$I_sig)] <- "Not Significant"
  
  # Plotting
  plt_I_sig <- ggplot(data=data, aes(geom="sf", fill=I_sig)) +
    geom_sf() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    scale_fill_manual(breaks = c("High-High","Low-Low", "Low-High", "High-Low", 
                                 "Not Significant"),
                      values = c("#FF0000", "#0000FF", "#a7adf9", "#f4ada8","#ededed")) +
    labs(fill = "Correlation Characteristics",
         title = paste0("Significant Level at P-value <= ", as.character(p_sig))) 
  
  return(list(plt_sigLVL, plt_I_sig, data, MC_LocalI.df))
}


# Contigious adjacency ----
nb <- poly2nb(PL_Data, queen=TRUE)

# Add Phangna and Phuket to be neighborhood
nb[[which(PL_Data$ADM1_EN == "Phuket")]] <- c(which(PL_Data$ADM1_EN == "Phangnga"))
nb[[which(PL_Data$ADM1_EN == "Phangnga")]] <- append(nb[[which(PL_Data$ADM1_EN == "Phangnga")]], 
                                                     which(PL_Data$ADM1_EN == "Phuket"))
PL_Data$ADM1_EN[nb[[which(PL_Data$ADM1_EN == "Phuket")]]]
PL_Data$ADM1_EN[nb[[which(PL_Data$ADM1_EN == "Phangnga")]]]

# Assign equal weight
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

# Experiment: Party-List ----
## MFP ----
PL_MFP <- LR_party(PL_Data, "MFP")
PL_MFP_res <- local_moran(PL_MFP, 0.05, nb)
PL_MFP_res[[1]]               #significant level in each province
PL_MFP_res[[2]]               #local moran's I that are significant
PL_MFP_df <- PL_MFP_res[[3]] 
PL_MFP_MC <- PL_MFP_res[[4]] 

## Pheu Thai ----
PL_PT <- LR_party(PL_Data, "PT")
PL_PT_res <- local_moran(PL_PT, 0.01, nb)
PL_PT_res[[1]]
PL_PT_res[[2]]

## UTNP ----
PL_UTNP <- LR_party(PL_Data, "UTNP")
PL_UTNP_res <- local_moran(PL_UTNP, 0.01, nb)
PL_UTNP_res[[1]]
PL_UTNP_res[[2]]


# Experiment: Constituency ----
## MFP ----
Const_MFP <- LR_party(Const_Data, "MFP")
Const_MFP_res <- local_moran(Const_MFP, 0.05, nb)
Const_MFP_res[[1]]
Const_MFP_res[[2]]

## Pheu Thai ----
Const_PT <- LR_party(Const_Data, "PT")
Const_PT_res <- local_moran(Const_PT, 0.01, nb)
Const_PT_res[[1]]
Const_PT_res[[2]]

## UTNP ----
Const_UTNP <- LR_party(Const_Data, "UTNP")
Const_UTNP_res <- local_moran(Const_UTNP, 0.05, nb)
Const_UTNP_res[[1]]
Const_UTNP_res[[2]]

## BJT ----
Const_BJT <- LR_party(Const_Data, "BJT")
Const_BJT_res <- local_moran(Const_BJT, 0.05, nb)
Const_BJT_res[[1]]
Const_BJT_res[[2]]

## DEM ----
Const_DEM <- LR_party(Const_Data, "DEM")
Const_DEM_res <- local_moran(Const_DEM, 0.01, nb)
Const_DEM_res[[1]]
Const_DEM_res[[2]]

## PPRP ----
Const_PPRP <- LR_party(Const_Data, "PPRP")
Const_PPRP_res <- local_moran(Const_PPRP, 0.05, nb)
Const_PPRP_res[[1]]
Const_PPRP_res[[2]]
