# There are 9 parties
# - BJT
# - CT
# - Democrat
# - MFP
# - PPRP
# - PheuThai
# - Prachachat
# - TST
# - UTNP

preprocess <- function(){
  Party_List <- read_csv("Party-List.csv")
  Constituency <- read_csv("constituency.csv") # 
  
  # Number of Total Voters from Election Commission including others party and vote-no
  PL_GlobalVotes <- 37522746+1509836+482303
  Const_GlobalVotes <- 37190071+1457899+866885
  
  # Number of voters in each province
  PL_Province <- Party_List %>% 
    group_by(Province) %>% 
    summarise(Local_Votes = sum(Votes))
  Const_Province <- Constituency %>% 
    group_by(Province) %>% 
    summarise(Local_Votes = sum(Votes))
  
  # Party-List
  PL_election <- Party_List %>%
    group_by(Province, Party) %>%
    summarise(Votes = sum(Votes)) %>% 
    left_join(PL_Province, by = c("Province" = "Province")) %>% 
    mutate(Global_Rate = Votes/PL_GlobalVotes,
           Local_Rate = Votes/Local_Votes) %>% 
    select(-Local_Votes) %>% 
    pivot_wider(names_from = Party,
                values_from = c(Votes, Global_Rate, Local_Rate))
  PL_election <- replace(PL_election, is.na(PL_election), 0)
  # sum(is.na(PL_election))
  
  # Constituency
  Const_election <- Constituency %>% 
    group_by(Province, Party) %>% 
    summarise(Votes = sum(Votes)) %>% 
    left_join(PL_Province, by = c("Province" = "Province")) %>% 
    mutate(Local_Rate = Votes/Local_Votes) %>% 
    select(-Local_Votes) %>% 
    pivot_wider(names_from = Party,
                values_from = c(Votes, Local_Rate))
  
  Const_election <- replace(Const_election, is.na(Const_election), 0)
  # sum(is.na(Const_election))
  
  # Spatial Data
  Spatial_thai <- st_read('SpatialData/tha_cod-ab.shp')
  Spatial_thai <- Spatial_thai %>% 
    select(ADM1_EN, geometry)
  
  # Merge Spatial and Election Data
  PL_Data <- Spatial_thai %>%
    left_join(PL_election, by = c("ADM1_EN" = "Province"))
  Const_Data <- Spatial_thai %>%
    left_join(Const_election, by = c("ADM1_EN" = "Province"))
  
  
  ## Try simple map ----
  # tm_shape(PL_Data) + tm_polygons(col="Global_Rate_MFP")
  
  return(list(PL = PL_Data, Const = Const_Data))
}