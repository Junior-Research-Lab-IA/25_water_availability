library(readr)
library(dplyr)
library(CropWat)

## =========================
## 1. SOIL DATA BY ZONE
## =========================
soil <- read_delim(
  "soildata.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),
  col_types = cols()  # let readr guess types
)

soil
names(soil)
# "Zone" "soil_depth_max" "soil_depth_min" "soil_depth"
# "soil_awc_max" "soil_awc_min" "soil_awc"
soil <- soil %>%
  mutate(
    soil_depth = as.numeric(soil_depth),  # already in meters
    AWC        = as.numeric(soil_awc)     # already in mm
  )

## =========================
## 2. CROP CHOICE: VEGETABLES,vineyards, orchards
## =========================

cropcode <- c(
  "vineyard" = "SB2023-vignes",
  "orchad" = "SB2023-vergers",
  "vegetable"= "FAO-VEGETABL"
)
# Sowing date for vegetables (adapt if needed)
sowing_date_tbl <- data.frame(
  Crop = c("FAO-VEGETABL"),
  Date = c("02-15")   # MM-DD
)

target_crops <- names(cropcode)  # "Vineyards", "Orchards", "Vegetables"

# We will use: soil_depth (m) and AWC (mm) per zone

zones <- c("A", "B", "C")

#function to read climate files

read_climate <- function(file) {
  readr::read_csv(file, show_col_types = FALSE) %>%
    mutate(
      Date = as.Date(time),
      ETP  = evapo,   # Potential evapotranspiration (mm/day)
      Ptot = pr       # Precipitation (mm/day)
    )
}
## =========================
## 3. IRRIGATION PERIOD
## =========================

# irrigation dates 15/03 – 16/10 for 1951–2100
irrig_date <- as.Date(character())  # empty Date vector

for (year in 2023:2075) {
  start_date <- as.Date(paste(year, "03", "15", sep = "-"))
  end_date   <- as.Date(paste(year, "10", "16", sep = "-"))
  irrig_date <- c(irrig_date, seq.Date(start_date, end_date, by = "day"))
}

# irrigation rule
irrig_1pc_RAW <- CW_irrig_fun_factory(
  RAW_ratio = 1,
  apply_Dr  = TRUE,
  dates_irrig = irrig_date
)
## =========================
## 4. LOOP OVER ZONES AND CLIMATE SCENARIOS
## =========================
if (!dir.exists("Output")) dir.create("Output")
all_results <- list()

for (z in zones) {

  # 5.1 soil parameters for this zone
  soil_z <- soil %>% filter(Zone == z) %>% slice(1)
  depth_z <- soil_z$soil_depth
  AWC_z   <- soil_z$AWC

  # 5.2 climate file for this zone (adjust pattern to your filenames)
  clim_files <- list.files(
    path = "climate_files",
    pattern = paste0("climate_zone", z, ".*\\.csv$"),
    full.names = TRUE
  )

  if (length(clim_files) == 0) {
    warning("No climate file found for zone ", z, " – skipping.")
    next
  }

  # If you have several scenarios per zone, loop over them:
  for (clim_file in clim_files) {

    # Extract climate scenario name from file name
    scenario_name <- tools::file_path_sans_ext(basename(clim_file))

    climate <- read_climate(clim_file)

    # 5.3 loop over crops
    for (crop_label in target_crops) {

      crop_id <- cropcode[[crop_label]]

      ## Create CropWat input
      if (grepl("FAO", crop_id)) {

        sow_date <- sowing_date_tbl$Date[sowing_date_tbl$Crop == crop_id]

        cw_input <- CW_create_input(
          crop        = crop_id,
          DatesR      = climate$Date,
          ETo         = climate$ETP,
          P           = climate$Ptot,
          soil_depth  = depth_z,
          AWC         = AWC_z,
          sowing_date = sow_date
        )

      } else {

        cw_input <- CW_create_input(
          crop       = crop_id,
          DatesR     = climate$Date,
          ETo        = climate$ETP,
          P          = climate$Ptot,
          soil_depth = depth_z,
          AWC        = AWC_z
        )
      }

      X0_initial_state <- CW_create_state(cw_input = cw_input)

      cw_output <- CW_run_simulation(
        X0_initial_state,
        cw_input,
        FUN_IRRIG = irrig_1pc_RAW
      )

      # Aggregate irrigation demand
      water_demand <- cw_output %>%
        mutate(
          year  = format(DatesR, "%Y"),
          month = format(DatesR, "%m")
        ) %>%
        group_by(year) %>%
        summarise(
          total_irrigation  = sum(Ir),
          spring_irrigation = sum(Ir[month %in% c("03","04","05")]),
          summer_irrigation = sum(Ir[month %in% c("06","07","08","09")]),
          .groups = "drop"
        ) %>%
        mutate(
          Zone     = z,
          Crop     = crop_label,
          Scenario = scenario_name
        )

      # Filename that includes scenario + zone + crop
      out_file <- paste0(
        "Output/",
        scenario_name,
        "_zone", z,
        "_", crop_label,
        ".csv"
      )

      write.csv(water_demand, out_file, row.names = FALSE)

      all_results[[length(all_results) + 1]] <- water_demand
    } # end crop loop
  }   # end climate scenario loop
}     # end zone loop

# Combined output
if (length(all_results) > 0) {
  final_all_zones <- bind_rows(all_results)
  write.csv(final_all_zones, "Output/water_demand_all_zones.csv", row.names = FALSE)
}
