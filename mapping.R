library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
source("preprocess.R")
source("party_win.R")

# Import Votes and Rate Data ----
Data = preprocess()
PL_Data = Data$PL
Const_Data = Data$Const


# Party-List ----
## Local Rate ----
ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_MFP)) + 
  geom_sf() +
  scale_fill_viridis_c(direction = -1, option="A")
ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_BJT)) + 
  geom_sf() +
  scale_fill_viridis_c(direction = -1, option="A")
ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_PheuThai)) + 
  geom_sf() +
  scale_fill_viridis_c(direction = -1, option="A")
ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_Democrat)) + 
  geom_sf() +
  scale_fill_viridis_c(direction = -1, option="A")
ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_CT)) + 
  geom_sf() +
  scale_fill_viridis_c(direction = -1, option="A")
ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_UTNP)) + 
  geom_sf() +
  scale_fill_viridis_c(direction = -1, option="A")
ggplot(data = PL_Data, aes(geom="sf", fill=Local_Rate_Prachachat)) + 
  geom_sf() +
  scale_fill_viridis_c(direction = -1, option="A")

## Votes ----
# ggplot(data = PL_Data, aes(geom="sf", fill=log(Votes_BJT))) + 
#   geom_sf() +
#   scale_fill_viridis_c(direction = -1, option="A")

ggplot(data = PL_Data, aes(geom="sf", fill=Votes_MFP)) + 
  geom_sf() +
  scale_fill_viridis_c(direction = -1, option="A")


# ggplot(data = PL_Data, aes(geom="sf", fill=Votes_MFP)) + 
#   geom_sf() +
#   scale_fill_stepsn(colors = c("#D73027", "#FC8D59", "#FEE08B", 
#                                "#D9EF8B", "#91CF60") ,
#                     breaks = c(100000))




