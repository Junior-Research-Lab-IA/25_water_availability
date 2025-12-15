#---- QUALYPSO CODE ----

#-- Load required packages

library(QUALYPSO)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)


#-- Loading of the dataset (must contain RCP, GCM, RCM, Land use scenarios, Irrigated areas scenarios, Volume of irrigation, Year)

data_complete <- read_csv("irrigvolume_forqualypso.csv")  #rename the csv by the real name of the dataset file
# Note that in this code : 
# gcm = GCM
# rcm = RCM
# rcp = RCP
# Land use scenarios : landuse
# Irrigated areas : propirrig
# Volume of irrigation : irrigvolume
# Year = year

#Combining GCM and RCM for each combination
data <- data_complete %>%
  unite("gcm_rcm", gcm, rcm, sep = "_", remove = TRUE)

#Check structure of the dataset
str(data)


# QUALYPSO needs a matric (scenarios x years)
# Each row = a unique combination of the scenario factors
# Each column = a year

#--BUILD THE Y MATRIX (RESPONSE VARIABLE)

# Select the output variable
var_to_analyse <- "irrigvolume"   #rename the output variable according to the coumn name in the csv file

# Build the Y matrix
Y <- data %>%
  select(gcm_rcm , rcp, propirrig, landuse, irrigvolume, year) %>%
  pivot_wider(names_from = year, values_from = irrigvolume) %>%
  arrange(gcm_rcm, rcp, propirrig, landuse)

# Extract the numeric matrix only
Y_mat <- as.matrix(Y %>% select(-gcm_rcm, -rcp, -propirrig, -landuse))


#-- BUILD THE SCENARIO AVAILABILITY TABLE
#QUALYPSO requires an availability table for each row of Y :

# scenAvail = the factors that produce the scenario row
# At the same time, the scenario factors are renamed for the future plots
scenAvail <- Y %>%
  select(gcm_rcm, rcp, propirrig, landuse)%>%
  mutate(
    rcp = factor(
      rcp,
      levels = c("rcp26", "rcp45", "rcp85"),
      labels = c("RCP 2.6", "RCP 4.5", "RCP 8.5")
    )
  )%>%
   mutate(
    gcm_rcm = factor(
      gcm_rcm,
      levels = c("CM5_ALADIN63", "CM5_KNMI", "ICHEC_KNMI", "ICHEC_SMHI", "MPI_CCLM4", "MPI_REMO"),
      labels = c("CM5-ALADIN63", "CM5-KNMI", "ICHEC-KNMI", "ICHEC-SMHI", "MPI-CCLM4", "MPI-REMO")
    ) 
  )%>%
  mutate(
    propirrig = factor(
      propirrig,
      levels = c("irrigconstante", "irrigdecrease", "irrigincrease"),
      labels = c("Constant scenario", "Decrease scenario", "Increase scenario")
    ) 
  )%>%
  mutate(
    landuse = factor(
      landuse,
      levels = c("annualsdominating", "constante", "perennialsdominating"),
      labels = c("Annuals dominating scenario", "Constant scenario", "Perennials dominating scenario")
    ) 
  )


#-- RUN QUALYPSO

listOption <- list(
  typeChangeVariable = "rel",  # relative change through time
  ANOVAmethod = "QUALYPSO"     # use QUALYPSO variance decomposition
)

QUALYPSO_result <- QUALYPSO(
  Y = Y_mat,
  scenAvail = as.data.frame(scenAvail),
  X = as.numeric(colnames(Y_mat)),  # years
  listOption = listOption
)

#This object now contains all uncertainty decomposition : RCM-GCM effect, RCP effect, Land use effect, Irrigated areas effect, Interactions, Total uncertainty, Mean change


#-- PLOTS

# Choice of margin and font size
par(mar = c(5, 4, 4, 2))
par(cex = 0.9)


#- RCP effect on total water demand
palette(c("#FF8C00","#0072B2", "#009C13"))  # curve's colors
plotQUALYPSOeffect(
  QUALYPSO_result,
  nameEff = "rcp",
  xlab = "Year",
  main = "Relative effect of RCPs on total irrigation water demand"
)


#- GCM and RCM effects
palette(c("#FF8C00","#0272E2", "#009C13","#30D5C8", "#F1E102", "#D55E00"))
plotQUALYPSOeffect(
  QUALYPSO_result, 
  nameEff = "gcm_rcm",
  xlab = "Year", 
  main = "Effect of GCM-RCM combinations on water demand"
)


#- Proportion of irrigated areas effect
palette(c("#0072B2", "#FF8C00", "#009C13"))
plotQUALYPSOeffect(
  QUALYPSO_result, 
  nameEff = "propirrig",
  xlab = "Year", 
  main = "Effect of the proportion of irrigated areas on water demand"
)


#- Land use effect
palette(c("#FF8C00","#0072B2", "#009C13"))
plotQUALYPSOeffect(
  QUALYPSO_result, 
  nameEff = "landuse",
  xlab = "Year", 
  main = "Effect of land use on water demand"
)


#-- PLOT UNCERTAINTY DECOMPOSITION (GLOBAL VIEW)

# Change of names for the plotting
QUALYPSO_result$namesEff <- c(
  "GCM–RCM combinations",
  "RCPs",
  "Irrigated areas scenarios",
  "Land use scenarios"
)

# Plot
plotQUALYPSOMeanChangeAndUncertainties(
  QUALYPSO_result,
  xlab = "Year",
  main = "Total uncertainties in future irrigation water demand"
)


#-- PLOT CONTRIBUTION OF EACH EFFECT AT HORIZON 2075

# Identifying the final year (2075)
final_year_index <- ncol(Y_mat)
final_year <- as.numeric(colnames(Y_mat)[final_year_index])

# Finding the corresponding line of this year 2075 in DECOMPVAR
row_2075 <- which(QUALYPSO_result$Xfut == final_year)

# Converting the line in a dataframe that can be used by ggplot
unc_2075 <- data.frame(
  effect = colnames(QUALYPSO_result$DECOMPVAR),
  relVar = as.numeric(QUALYPSO_result$DECOMPVAR[row_2075, ])
)

# Change of names for the plotting
effect <- c(
  "GCM–RCM combinations",
  "RCPs",
  "Irrigated areas scenarios",
  "Land use scenarios",
  "Internal var.",
  "Residual var."
)

# Avoiding an overlapping with a return to line
unc_2075$effect_wrapped <- str_wrap(unc_2075$effect, width = 17)

# Plot ggplot
ggplot(unc_2075, aes(x = effect_wrapped, y = relVar)) +
  geom_bar(stat = "identity") +
  labs(title = "Uncertainty contributions in 2075",
       y = "Relative variance contribution", 
       x = "Effect") +
  theme_minimal()


#-- EXTRACT AND SAVE RESULTS

# Extracting the uncertainties matrix
unc_table <- QUALYPSO_result$DECOMPVAR

# Converting it in a ddataframe + adding the "Year" column
unc_table <- unc_table %>% 
  as.data.frame() %>% 
  mutate(year = QUALYPSO_result$Xfut) %>% 
  relocate(year)

# Saving it as a csv
write_csv(unc_table, "uncertainty_results.csv")
