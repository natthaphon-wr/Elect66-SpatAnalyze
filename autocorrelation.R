library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
source("preprocess.R")

# Load Data ----
Data = preprocess()
PL_Data = Data$PL
Const_Data = Data$Const


# Functions -----
# Choose Data (PL or Const) and Party (MFP, PT, UTNP, BJT, DEM)
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
  }
  
  return(LR)
}




# Contiguous Adjacency ---- 
library(spdep)
nb_con <- poly2nb(PL_Data, queen=TRUE)
nb_con[[3]]
PL_Data$ADM1_EN[3]
PL_Data$ADM1_EN[nb_con[[3]]]
# Note that Phuket is no neighborhood

# Assign equal weight to each neighborhood
lw_con <- nb2listw(nb_con, style="W", zero.policy=TRUE) #list weight 

## Party-List -----
# MFP, PheuThai, UTNP



PL_MFP = select_party(PL_Data, "MFP")




### MFP ----
MFP <- PL_Data %>% 
  select(ADM1_EN, Local_Rate_MFP, geometry)

MFP$lag <- lag.listw(lw_con, MFP$Local_Rate_MFP)  #lag value
MFP_lm <- lm(lag ~ Local_Rate_MFP, MFP)           #linear regression

plot(lag ~ Local_Rate_MFP, MFP, pch=21, asp=1, las=1, col = "grey40", bg="grey80")
abline(MFP_lm, col="blue")
abline(v = mean(MFP$Local_Rate_MFP), lty=3, col = "grey80")
abline(h = mean(MFP$Local_Rate_MFP), lty=3, col = "grey80")

# Moran's I coefficient and MC
moran <- moran(MFP$Local_Rate_MFP, listw = lw_con, n = length(nb_con), S0 = Szero(lw_con))
MC <- moran.mc(MFP$Local_Rate_MFP, lw_con, nsim = 1000)
MC
plot(MC, las=1)
MC$p.value    #pseudo p-value
MC$statistic  #moran's statistics

## Constituency ----



