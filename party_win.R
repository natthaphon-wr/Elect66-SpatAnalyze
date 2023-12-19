party_win <- function(){
  Party_List <- read_csv("Party-List.csv")
  Constituency <- read_csv("constituency.csv") 
  
  # Number of voters in each province
  PL_Province <- Party_List %>% 
    group_by(Province, Party) %>% 
    summarise(Local_Votes = sum(Votes))
  Const_Province <- Constituency %>% 
    group_by(Province, Party) %>% 
    summarise(Local_Votes = sum(Votes))
  
  # Conclude election data
  PL_ElectWin <- PL_Province %>% 
    group_by(Province) %>% 
    summarise(Max_Votes = max(Local_Votes)) %>% 
    left_join(PL_Province, join_by(Province==Province, Max_Votes==Local_Votes))
  Const_ElectWin <- Const_Province %>% 
    group_by(Province) %>% 
    summarise(Max_Votes = max(Local_Votes)) %>% 
    left_join(Const_Province, join_by(Province==Province, Max_Votes==Local_Votes))
  
  # Spatial Data
  Spatial_thai <- st_read('SpatialData/tha_cod-ab.shp')
  Spatial_thai <- Spatial_thai %>% 
    select(ADM1_EN, geometry)
  
  # Merge Spatial and Election Data
  PL_Win <- Spatial_thai %>%
    left_join(PL_ElectWin, by = c("ADM1_EN" = "Province"))
  Const_Win <- Spatial_thai %>%
    left_join(Const_ElectWin, by = c("ADM1_EN" = "Province"))
  

  return(list(PL = PL_Win, Const = Const_Win))
}