library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
source("preprocess.R")

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
ggplot(data = PL_Data, aes(geom="sf", fill=Votes_BJT)) +
  geom_sf() +
  scale_fill_stepsn(colours = c("white", "skyblue", "blue", "#0F1599"),
                    breaks = c(10000, 30000, 50000),
                    values = scales::rescale(c(10000, 30000, 50000), c(0,1)),
                    guide = guide_coloursteps(even.steps = FALSE,
                                              show.limits = TRUE,
                                              title = "Votes BJT",
                                              barheight = unit(2.2, "in"),
                                              barwidth = unit(0.15, "in")))

ggplot(data = PL_Data, aes(geom="sf", fill=Votes_Democrat)) +
  geom_sf() +
  scale_fill_stepsn(colours = c("white", "#AFEEEE", "#00A1F1"),
                    breaks = c(10000, 20000, 50000),
                    values = scales::rescale(c(10000, 20000, 50000), c(0,1)),
                    guide = guide_coloursteps(even.steps = FALSE,
                                              show.limits = TRUE,
                                              title = "Votes Democrat",
                                              barheight = unit(2.2, "in"),
                                              barwidth = unit(0.15, "in")))

ggplot(data = PL_Data, aes(geom="sf", fill=Votes_MFP)) +
  geom_sf() +
  scale_fill_stepsn(colours = c("#FFDAB9", "#FFD700", "#FFA500", "#EF771E"),
                    breaks = c(100000, 200000, 300000, 400000, 600000),
                    values = scales::rescale(c(100000, 200000, 300000, 400000, 600000), c(0,1)),
                    guide = guide_coloursteps(even.steps = FALSE,
                                              show.limits = TRUE,
                                              title = "Votes MFP",
                                              barheight = unit(3, "in"),
                                              barwidth = unit(0.15, "in")))

ggplot(data = PL_Data, aes(geom="sf", fill=Votes_PheuThai)) +
  geom_sf() +
  scale_fill_stepsn(colours = c("white", "#FA8072", "#E30613"),
                    breaks = c(100000, 200000, 300000, 500000),
                    values = scales::rescale(c(100000, 200000, 300000, 500000), c(0,1)),
                    guide = guide_coloursteps(even.steps = FALSE,
                                              show.limits = TRUE,
                                              title = "Votes PheuThai",
                                              barheight = unit(3, "in"),
                                              barwidth = unit(0.15, "in")))

ggplot(data = PL_Data, aes(geom="sf", fill=Votes_TST)) +
  geom_sf() +
  scale_fill_stepsn(colours = c("white", "#65408F"),
                    breaks = c(10000, 50000),
                    values = scales::rescale(c(10000, 50000), c(0,1)),
                    guide = guide_coloursteps(even.steps = FALSE,
                                              show.limits = TRUE,
                                              title = "Votes TST",
                                              barheight = unit(2.25, "in"),
                                              barwidth = unit(0.15, "in")))

ggplot(data = PL_Data, aes(geom="sf", fill=Votes_UTNP)) +
  geom_sf() +
  scale_fill_stepsn(colours = c("white", "skyblue", "blue", "#0000CD"),
                    breaks = c(25000, 50000, 75000, 100000, 200000),
                    values = scales::rescale(c(25000, 50000, 75000, 100000, 200000), c(0,1)),
                    guide = guide_coloursteps(even.steps = FALSE,
                                              show.limits = TRUE,
                                              title = "Votes UTNP",
                                              barheight = unit(3, "in"),
                                              barwidth = unit(0.15, "in")))


# Constituency ----
# Only consider local rate

ggplot(data = Const_Data, aes(geom="sf", fill=Local_Rate_BJT)) +
  geom_sf() + 
  scale_fill_viridis_c(direction = -1, option="A")



