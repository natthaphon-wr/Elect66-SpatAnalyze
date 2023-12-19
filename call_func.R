library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
source("preprocess.R")
source("party_win.R")

# Votes and Rate Data
Data = preprocess()
PL_Data = Data$PL
Const_Data = Data$Const
tm_shape(PL_Data) + tm_polygons(col="Local_Rate_MFP")


# Party Win Data
Data_win = party_win()
PL_win = Data_win$PL
Const_win = Data_win$Const
ggplot(data = PL_win, aes(fill = Party)) + geom_sf()
ggplot(data = Const_win, aes(fill = Party)) +  geom_sf()


