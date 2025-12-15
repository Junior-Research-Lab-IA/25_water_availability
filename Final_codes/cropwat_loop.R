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

soil <- soil %>%
  mutate(
    soil_depth = as.numeric(soil_depth),  # already in meters
    AWC        = as.numeric(soil_awc)     # already in mm
  )

## =========================
## 2. CROP CHOICE: VEGETABLES, VINEYARDS, ORCHARDS
## =========================

cropcode <- c(
  "Vineyards"  = "SB2023-vignes",
  "orchads"    = "SB2023-vergers",
  "vegetables" = "FAO-VEGETABL"
)

# For vegetables: two sowing dates (spring + summer)
veg_sowing_dates <- c("02-15",  # spring cycle (mid-Feb)
                      "06-01")  # summer cycle (early June)

target_crops <- names(cropcode)  # "Vineyards", "orchads", "vegetables"

zones <- c("A", "B", "C")

## =========================
## 3. READ CLIMATE
## =========================
read_climate <- function(file) {
  readr::read_csv(file, show_col_types = FALSE) %>%
    mutate(
      Date = as.Date(time),
      ETP  = evapo,   # Potential evapotranspiration (mm/day)
      Ptot = pr       # Precipitation (mm/day)
    )
}

## =========================
## 4. IRRIGATION PERIOD
## =========================

# irrigation dates 15/03 – 16/10 for 2023–2075
irrig_date <- as.Date(character())

for (year in 2023:2075) {
  start_date <- as.Date(paste(year, "03", "15", sep = "-"))
  end_date   <- as.Date(paste(year, "10", "16", sep = "-"))
  irrig_date <- c(irrig_date, seq.Date(start_date, end_date, by = "day"))
}

irrig_1pc_RAW <- CW_irrig_fun_factory(
  RAW_ratio   = 1,
  apply_Dr    = TRUE,
  dates_irrig = irrig_date
)

## =========================
## 5. LOOP OVER ZONES + CLIMATE SCENARIOS + CROPS
## =========================

if (!dir.exists("Output")) dir.create("Output")
all_results <- list()

for (z in zones) {

  # 5.1 soil parameters for this zone
  soil_z <- soil %>% filter(Zone == z) %>% slice(1)
  depth_z <- soil_z$soil_depth
  AWC_z   <- soil_z$AWC

  # 5.2 all climate files for this zone
  clim_files <- list.files(
    path   = "climate_files",
    pattern = paste0("climate_zone", z, ".*\\.csv$"),
    full.names = TRUE
  )

  if (length(clim_files) == 0) {
    warning("No climate file found for zone ", z, " – skipping.")
    next
  }

  for (clim_file in clim_files) {

    # Scenario name from file name (without extension)
    scenario_name <- tools::file_path_sans_ext(basename(clim_file))

    climate <- read_climate(clim_file)

    # 5.3 loop over crops
    for (crop_label in target_crops) {

      crop_id <- cropcode[[crop_label]]

      ## ===========
      ## CASE 1: VEGETABLES (two sowing dates)
      ## ===========
      if (crop_id == "FAO-VEGETABL") {

        veg_runs <- list()

        for (sd in veg_sowing_dates) {

          cw_input <- CW_create_input(
            crop        = crop_id,
            DatesR      = climate$Date,
            ETo         = climate$ETP,
            P           = climate$Ptot,
            soil_depth  = depth_z,
            AWC         = AWC_z,
            sowing_date = sd      # different sowing date each loop
          )

          X0_initial_state <- CW_create_state(cw_input = cw_input)

          cw_output <- CW_run_simulation(
            X0_initial_state,
            cw_input,
            FUN_IRRIG = irrig_1pc_RAW
          )

          wd_one <- cw_output %>%
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
            )

          veg_runs[[length(veg_runs) + 1]] <- wd_one
        }

        # Combine the two vegetable cycles in ONE table per year
        water_demand <- bind_rows(veg_runs) %>%
          group_by(year) %>%
          summarise(
            total_irrigation  = sum(total_irrigation),
            spring_irrigation = sum(spring_irrigation),
            summer_irrigation = sum(summer_irrigation),
            .groups = "drop"
          ) %>%
          mutate(
            Zone     = z,
            Crop     = crop_label,
            Scenario = scenario_name
          )

        ## ===========
        ## CASE 2: OTHER CROPS (vines, orchards) – one cycle
        ## ===========
      } else {

        cw_input <- CW_create_input(
          crop       = crop_id,
          DatesR     = climate$Date,
          ETo        = climate$ETP,
          P          = climate$Ptot,
          soil_depth = depth_z,
          AWC        = AWC_z
        )

        X0_initial_state <- CW_create_state(cw_input = cw_input)

        cw_output <- CW_run_simulation(
          X0_initial_state,
          cw_input,
          FUN_IRRIG = irrig_1pc_RAW
        )

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
      }

      ## ===========
      ## 6. SAVE OUTPUT (filenames start with "irrig_")
      ## ===========

      out_file <- paste0(
        "Output/",
        "irrig_",
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

## =========================
## 7. GLOBAL COMBINED TABLE
## =========================
if (length(all_results) > 0) {
  final_all_zones <- bind_rows(all_results)
  write.csv(final_all_zones, "Output/irrig_water_demand_all_zones.csv", row.names = FALSE)
}

