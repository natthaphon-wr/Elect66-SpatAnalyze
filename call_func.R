library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
source("preprocess.R")
source("election_winner.R")

# Votes and Rate Data ----
Data = preprocess()
PL_Data = Data$PL
Const_Data = Data$Const



# Party Win Data ----
Data_win = election_winner()
PL_winner = Data_win$PL
Const_winner = Data_win$Const

ggplot(data = PL_winner, aes(fill = Party)) + 
  geom_sf() + 
  scale_fill_manual(
    label = c("Move Forward Party", "Pheu Thai Party", 
              "Prachachat Party", "United Thai Nation Party"),
    values = c("#EF771E", "#E30613", "#BA810D", "#0000CD")) +
  ggtitle("Winner in Each Province in Party-List") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())
  
  
ggplot(data = Const_winner, aes(fill = Party)) +  
  geom_sf() +
  scale_fill_manual(
    label = c("Bhumjaithai Party", "Chartthaipattana Party", "Democrat Party",
              "Move Forward Party", "Pheu Thai Party", "Palang Pracharath Party",
              "Prachachat Party", "Thai Sang Thai Party", "United Thai Nation Party"),
    values = c("#0F1599", "#E90080", "#00A1F1", "#EF771E", "#E30613", "#4061A6", 
               "#BA810D", "#65408F", "#0000CD")) +
  ggtitle("Winner in Each Province in Constituency") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())


