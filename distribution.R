library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
library(gridExtra)
library(grid.extra)
source("preprocess.R")

# Votes and Rate Data
Data = preprocess()
PL_Data = Data$PL
Const_Data = Data$Const

histogram_LR <- function(Data){
  plt_BJT <- ggplot(data = Data, aes(Local_Rate_BJT)) + 
    geom_histogram(bins=20, fill="#0F1599") +
    lims(x=c(0,1), y=c(0, 40))
  
  plt_CT <- ggplot(data = Data, aes(Local_Rate_CT)) + 
    geom_histogram(bins=20, fill="#E90080") +
    lims(x=c(0,1), y=c(0, 40))
  
  plt_Dem <- ggplot(data = Data, aes(Local_Rate_Democrat)) + 
    geom_histogram(bins=20, fill="#00A1F1") +
    lims(x=c(0,1), y=c(0, 40))
  
  plt_MFP <- ggplot(data = Data, aes(Local_Rate_MFP)) + 
    geom_histogram(bins=20, fill="#EF771E") +
    lims(x=c(0,1), y=c(0, 40))
  
  plt_PPRP <- ggplot(data = Data, aes(Local_Rate_PPRP)) + 
    geom_histogram(bins=20, fill="#4061A6") +
    lims(x=c(0,1), y=c(0, 40))
  
  plt_PT <- ggplot(data = Data, aes(Local_Rate_PheuThai)) + 
    geom_histogram(bins=20, fill="#E30613") +
    lims(x=c(0,1), y=c(0, 40))
  
  plt_P <- ggplot(data = Data, aes(Local_Rate_Prachachat)) + 
    geom_histogram(bins=20, fill="#BA810D") +
    lims(x=c(0,1), y=c(0, 40))

  plt_TST <- ggplot(data = Data, aes(Local_Rate_TST)) + 
    geom_histogram(bins=20, fill="#65408F") +
    lims(x=c(0,1), y=c(0, 40))

  plt_UTNP <- ggplot(data = Data, aes(Local_Rate_UTNP)) + 
    geom_histogram(bins=20, fill="#0000CD") +
    lims(x=c(0,1), y=c(0, 40))
  
  grid.arrange(plt_BJT, plt_CT, plt_Dem, plt_MFP, plt_P, plt_PPRP, plt_PT, plt_TST, plt_UTNP,
               nrow=3, ncol=3)
}

histogram_LR(PL_Data)
histogram_LR(Const_Data)


