devtools::install_github("agrdatasci/ag5Tools", build_vignettes = TRUE)

library(ag5Tools)

## Download Solar Radiation ##

ag5Tools::ag5_download(variable = "precipitation_flux",
                       #statistic = "night_time_minimum",
                       day = "all",
                       month = "all",
                       year = 2022,
                       path = "D:/cenavarro/Workspace/era5/" # Folder to save the data
)


## Download Tmin ##

ag5Tools::ag5_download(variable = "2m_temperature",
                       statistic = "24_hour_minimum",
                       day = "all",
                       month = "all",
                       year = 2022,
                       path = "D:/cenavarro/Workspace/era5/" # Folder to save the data
)

## Download Tmax ##
ag5Tools::ag5_download(variable = "2m_temperature",
                       statistic = "24_hour_maximum",
                       day = "all",
                       month = "all",
                       year = 2022,
                       path = "D:/cenavarro/Workspace/era5/" # Folder to save the data
)
