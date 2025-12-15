library(dplyr)


zone <- "zoneC"

landuse_scenarios <- c('annualsdominating', 'constante', 'perennialsdominating')
propirrig_scenarios <- c('constante', 'decrease', 'increase')

# Boucle for pour afficher chaque fruit
for (lu_scenario in landuse_scenarios) {
  for (pi_scenario in propirrig_scenarios) {
    landuse <- read.csv(paste0("landuse_",zone,"_", lu_scenario, ".csv"),header = TRUE, sep = ",", dec = ".")
    propirrig <- read.csv(paste0("prop_irrigsurf_", pi_scenario, ".csv"),header = TRUE, sep = ",", dec = ".")
    irrigsurface <- select(landuse, year)
    irrigsurface$vineyards <- landuse$vineyards * propirrig$vineyards_orchards
    irrigsurface$orchards <- landuse$orchards * propirrig$vineyards_orchards
    irrigsurface$vegetables <- landuse$vegetables * propirrig$vegetables
    write.csv(irrigsurface, paste0("irrigsurf_",zone, "_irrig", pi_scenario, "_", lu_scenario,".csv") ,row.names = FALSE)
  }
}


