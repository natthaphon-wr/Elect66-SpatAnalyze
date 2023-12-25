library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


# Preprocess Data ----
Party_List <- read_csv("Party-List.csv")
Constituency <- read_csv("constituency.csv") 
PL_GlobalVotes <- 37522746+1509836+482303
Const_GlobalVotes <- 37190071+1457899+866885

# Number of voters in each province
PL_Province <- Party_List %>% 
  group_by(Province) %>% 
  summarise(Local_Votes = sum(Votes))
Const_Province <- Constituency %>% 
  group_by(Province) %>% 
  summarise(Local_Votes = sum(Votes))

## Party-List ----
PL_election <- Party_List %>%
  group_by(Province, Party) %>%
  summarise(Votes = sum(Votes)) %>% 
  left_join(PL_Province, by = c("Province" = "Province")) %>% 
  mutate(Local_Rate = round(Votes/Local_Votes,4)) %>% 
  select(-Local_Votes) %>% 
  pivot_wider(names_from = Party,
              names_prefix = "PL_",
              values_from = c(Votes, Local_Rate))
PL_election <- replace(PL_election, is.na(PL_election), 0)
sum(is.na(PL_election))

## Constituency ----
Const_election <- Constituency %>%
  group_by(Province, Party) %>%
  summarise(Votes = sum(Votes)) %>% 
  left_join(Const_Province, by = c("Province" = "Province")) %>% 
  mutate(Local_Rate = round(Votes/Local_Votes,4)) %>% 
  select(-Local_Votes) %>% 
  pivot_wider(names_from = Party,
              names_prefix = "Const_",
              values_from = c(Votes, Local_Rate))
Const_election <- replace(Const_election, is.na(Const_election), 0)
sum(is.na(Const_election))

## Joining Data ----
Data <- left_join(PL_election, Const_election, by = "Province")
Data <- Data %>% 
  filter(Province != "Bangkok")


# Party-List and Constituency -----
## MFP ----
corr_MFP <- cor(Data$Local_Rate_PL_MFP, Data$Local_Rate_Const_MFP, method = "pearson")
ggplot(Data, aes(Local_Rate_PL_MFP, Local_Rate_Const_MFP)) +
  geom_point() +
  labs(title = "MFP's Correlative Votes in Each Province",
       x = "Party-List  Vote's Rate ",
       y = "Constituency Vote's Rate") +
  xlim(0.2, 0.6) +
  ylim(0, 0.6)

## Pheu Thai ----
corr_PT <- cor(Data$Local_Rate_PL_PheuThai, Data$Local_Rate_Const_PheuThai, method = "pearson")
ggplot(Data, aes(Local_Rate_PL_PheuThai, Local_Rate_Const_PheuThai)) +
  geom_point() +
  labs(title = "Pheu Thai's Correlative Votes in Each Province",
       x = "Party-List  Vote's Rate ",
       y = "Constituency Vote's Rate") +
  xlim(0, 0.6) +
  ylim(0, 0.6)

## UTNP ----
corr_UTNP <- cor(Data$Local_Rate_PL_UTNP, Data$Local_Rate_Const_UTNP, method = "pearson")
ggplot(Data, aes(Local_Rate_PL_UTNP, Local_Rate_Const_UTNP)) +
  geom_point() +
  labs(title = "UTNP's Correlative Votes in Each Province",
       x = "Party-List  Vote's Rate ",
       y = "Constituency Vote's Rate") +
  xlim(0, 0.5) +
  ylim(0, 0.5)


# Pary-List b/w party ----
# Exclude Bangkok that is outlier.
## PT  and MFP ----
corr_MFP_PT <- cor(Data$Votes_PL_MFP, Data$Votes_PL_PheuThai, method = "pearson")
ggplot(Data, aes(Votes_PL_MFP, Votes_PL_PheuThai)) +
  geom_point() +
  labs(title = "Party-List Votes Correlation in Each Province",
       x = "MFP Votes",
       y = "Pheu Thai Votes") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

## PT  and UTNP ----
corr_PT_UTNP <- cor(Data$Votes_PL_PheuThai, Data$Votes_PL_UTNP, method = "pearson")
ggplot(Data, aes(Votes_PL_PheuThai, Votes_PL_UTNP)) +
  geom_point() +
  labs(title = "Party-List Votes Correlation in Each Province",
       x = "Pheu Thai Votes",
       y = "UTNP Votes") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

## MFP  and UTNP ----
corr_MFP_UTNP <- cor(Data$Votes_PL_MFP, Data$Votes_PL_UTNP, method = "pearson")
ggplot(Data, aes(Votes_PL_MFP, Votes_PL_UTNP)) +
  geom_point() +
  labs(title = "Party-List Votes Correlation in Each Province",
       x = "MFP Votes",
       y = "UTNP Votes") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)
  
