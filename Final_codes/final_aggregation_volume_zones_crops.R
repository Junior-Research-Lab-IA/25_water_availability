library(dplyr)
library (data.table)


zones <- c('zoneA','zoneB','zoneC')
combinations <- read.csv("climate_irrigsurf_combinations.csv", header = TRUE, sep = ";")

results <- list()
k <- 1


for (zone in zones) {
  for (i in 1:nrow(combinations)){
    gcm        <- combinations$gcm[i]
    rcm        <- combinations$rcm[i]
    rcp        <- combinations$rcp[i]
    propirrig  <- combinations$propirrig[i]
    landuse    <- combinations$landuse[i]
      
    df_irrigdose_vineyards <- fread(paste0("irrig_climate_", zone, "_", gcm, "_", rcm, "_", rcp, "_", zone, "_Vineyards.csv"), select = c("year", "total_irrigation"), header = TRUE, sep = ",")
      setnames(df_irrigdose_vineyards, "total_irrigation", "irrigdose_vineyards")
    df_irrigdose_orchards <- fread(paste0("irrig_climate_", zone, "_", gcm, "_", rcm, "_", rcp, "_", zone, "_orchads.csv"), select = c("year", "total_irrigation"), header = TRUE, sep = ",")
      setnames(df_irrigdose_orchards, "total_irrigation", "irrigdose_orchards")
    df_irrigdose_vegetables <- fread(paste0("irrig_climate_", zone, "_", gcm, "_", rcm, "_", rcp, "_", zone, "_vegetables.csv"), select = c("year", "total_irrigation"), header = TRUE, sep = ",")
      setnames(df_irrigdose_vegetables, "total_irrigation", "irrigdose_vegetables")
    df_irrigdose <- df_irrigdose_vineyards %>%
      left_join(df_irrigdose_orchards, by = c("year"))%>%
      left_join(df_irrigdose_vegetables, by = c("year"))
    
    df_irrigsurf <- read.csv(paste0("irrigsurf_", zone, "_", propirrig, "_", landuse, ".csv") ,header = TRUE, sep = ",")
      
    df_merged <- df_irrigdose %>%
      left_join(df_irrigsurf, by = "year")
    df_merged$irrigvolume_vineyards <- df_merged$irrigdose_vineyards * df_merged$vineyards
    df_merged$irrigvolume_orchards <- df_merged$irrigdose_orchards * df_merged$orchards
    df_merged$irrigvolume_vegetables <- df_merged$irrigdose_vegetables * df_merged$vegetables
      
      
    df_merged <- df_merged %>%
      mutate(
        gcm = gcm,
        rcm = rcm,
        rcp = rcp,
        propirrig = propirrig,
        landuse = landuse
      )
      
    results[[k]] <- df_merged
    k <- k + 1
  }
  df_irrigvolume_byzone <- bind_rows(results)
  results <- list()
  k <-1
  df_irrigvolume_byzone <- select(df_irrigvolume_byzone, gcm, rcm, rcp, propirrig, landuse, year, irrigvolume_vineyards, irrigvolume_orchards, irrigvolume_vegetables)
  write.csv(df_irrigvolume_byzone, paste0("irrigvolume_",zone,".csv") ,row.names = FALSE)
}
  

df_irrigvolume_zoneA <- read.csv(paste0("irrigvolume_zoneA.csv"),header = TRUE, sep = ",")%>%
  rename(
    irrigvolume_vineyards_zoneA = irrigvolume_vineyards ,
    irrigvolume_orchards_zoneA = irrigvolume_orchards ,
    irrigvolume_vegetables_zoneA = irrigvolume_vegetables
  )
df_irrigvolume_zoneB <- read.csv(paste0("irrigvolume_zoneB.csv"),header = TRUE, sep = ",")%>%
  rename(
    irrigvolume_vineyards_zoneB = irrigvolume_vineyards ,
    irrigvolume_orchards_zoneB = irrigvolume_orchards ,
    irrigvolume_vegetables_zoneB = irrigvolume_vegetables
  )
df_irrigvolume_zoneC <- read.csv(paste0("irrigvolume_zoneC.csv"),header = TRUE, sep = ",")%>%
  rename(
    irrigvolume_vineyards_zoneC = irrigvolume_vineyards ,
    irrigvolume_orchards_zoneC = irrigvolume_orchards ,
    irrigvolume_vegetables_zoneC = irrigvolume_vegetables
  )
  
df_irrigvolume <- df_irrigvolume_zoneA %>%
  left_join(df_irrigvolume_zoneB, by = c("gcm", "rcm", "rcp", "propirrig", "landuse", "year")) %>%
  left_join(df_irrigvolume_zoneC, by = c("gcm", "rcm", "rcp", "propirrig", "landuse", "year"))
  
df_irrigvolume$irrigvolume_vineyards <- df_irrigvolume$irrigvolume_vineyards_zoneA + df_irrigvolume$irrigvolume_vineyards_zoneB + df_irrigvolume$irrigvolume_vineyards_zoneC
df_irrigvolume$irrigvolume_orchards <- df_irrigvolume$irrigvolume_orchards_zoneA + df_irrigvolume$irrigvolume_orchards_zoneB + df_irrigvolume$irrigvolume_orchards_zoneC
df_irrigvolume$irrigvolume_vegetables <- df_irrigvolume$irrigvolume_vegetables_zoneA + df_irrigvolume$irrigvolume_vegetables_zoneB + df_irrigvolume$irrigvolume_vegetables_zoneC

df_irrigvolume_bycrop <- select(df_irrigvolume, gcm, rcm, rcp, propirrig, landuse, year, irrigvolume_vineyards, irrigvolume_orchards, irrigvolume_vegetables)

write.csv(df_irrigvolume_bycrop, paste0("irrigvolume_bycrop.csv") ,row.names = FALSE)

df_irrigvolume_total <- select(df_irrigvolume_bycrop, gcm, rcm, rcp, propirrig, landuse, year)
df_irrigvolume_total$irrigvolume <- df_irrigvolume_bycrop$irrigvolume_vineyards + df_irrigvolume_bycrop$irrigvolume_orchards + df_irrigvolume_bycrop$irrigvolume_vegetables

write.csv(df_irrigvolume_total, "irrigvolume_forqualypso.csv" ,row.names = FALSE)
  
  
    
  

