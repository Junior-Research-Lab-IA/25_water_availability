# Fetch available data on DRIAS and save the file in the package

pkgload::load_all()
cfg <- loadConfig()

df_drias <- drias_list_remote()

readr::write_tsv(df_drias,
                 file.path(system.file("extdata", package = "airGRccia"),
                             "drias_scenarios_variables.tsv"))
