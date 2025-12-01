library(ggplot2)
library(dplyr)
library(tidyr)

data(L0123001, package = "airGR")
InputsModel <- CreateInputsModel(
  RunModel_GR4J,
  DatesR = BasinObs$DatesR,
  Precip = BasinObs$P,
  PotEvap = BasinObs$E
)
Ind_Run <- seq(
  which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1985-01-01"),
  which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "2012-12-31")
)
RunOptions <- CreateRunOptions(
  RunModel_GR4J,
  InputsModel = InputsModel,
  IndPeriod_Run = Ind_Run,
  IndPeriod_WarmUp = (Ind_Run[1] - 365):(Ind_Run[1] - 1)
)
Param <- c(257.237556, 1.012237, 88.234673, 2.207958)
OutputsModel <- RunModel_GR4J(
  InputsModel = InputsModel,
  RunOptions = RunOptions,
  Param = Param
)
Qsim <- OutputsModel$Qsim
Qobs <- BasinObs$Qmm[Ind_Run]

uncertainty_table <- predict_uncertainty_calibration(Qobs, Qsim)


Qsim_uncertain <- predict_uncertainty(Qsim, uncertainty_table)

# Plot the result of uncertainty estimation
plot_uncertainty_calibration(Qsim_uncertain, Qobs)

# Plot the time series with uncertainty intervals
ts_selection <- 1:365
plot_uncertainty_time_series(
  Qsim_uncertain[ts_selection, ],
  BasinObs$DatesR[Ind_Run[ts_selection]]
)

# Plot the time series with uncertainty intervals compared with observations
plot_uncertainty_time_series(
  Qsim_uncertain[ts_selection, ],
  BasinObs$DatesR[Ind_Run[ts_selection]],
  Qobs[ts_selection]
)
