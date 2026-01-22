# Luscinia luscinia GeoPressureR

Migration position estimates from ambient pressure data recorded with multisensor data loggers built in Lund for thrush nightingales (Luscinia luscinia) breeding nearby Lund, Sweden.

LOGGER TYPE: CAnMove multi-sensor loggers (Lund University)

Raw data is provided for the loggers' accelerometer and pressure sensors.

Labelled data indicating migratory flights and stopovers is also provided.

With our type of data, GeopressureR uses entire hours of flight.

We considered one hour of flight in GeopressureR if the bird's accelerometer recorded at least 4 out of 6 values with accelerometer values > 4 (scale: 0 to 5) within one hour.

A minimum of 1 hour and 40 minutes of activity with values > 4 was needed to be considered a migratory flight (2 hours of flight in GeopressureR), otherwise, the short flight is treated as different altitudes. 


## Project structure :file_folder:

Following the recommendations of [rrrpkg](https://github.com/ropensci/rrrpkg), the project contains:
1. Standard description files at the root (`DESCRIPTION`, `.Rproj`, `README.md`, `LICENCES`,...).
2. `data/` folder containing the raw geolocator data, the pressure and light labelled files and the data generated with the code from `analysis/`. Note that you could instead keep the geolocator and labelization files separately in a `raw-data/` folder, following `usethis()` standard.
3. `analysis/` contains all the `.R` code used for your project.
4. `report/` reads the data generated and produces sharable results (figures, html page, manuscript, etc...).
<details>
  <summary>See directory tree</summary>

```
GeoPressureTemplate
├── DESCRIPTION          		                # project metadata and dependencies
├── README.md            		                # top-level description of content and guide to users
├── GeoPressureTemplate.Rproj               # R project file
├── data                                    # Folder structured by order of use
│   ├── 0_tag                               # Folder with raw geolocator data grouped by gdl_id
│   │   ├── 18LX
│   │   │   ├── 18LX_20180725.acceleration
│   │   │   ├── 18LX_20180725.glf
│   │   │   ├── 18LX_20180725.pressure 
│   │   │   └── ...
│   │   └── 22BT
│   │       └── ...
│   ├── 1_pressure                          # Data generated with analyis/1-pressure.R
│   │   ├── 18LX_pressure_prob.Rdata
│   │   └── labels
│   │       ├── 18LX_act_pres-labeled.csv
│   │       ├── 18LX_act_pres.csv
│   │       └── ...                    
│   ├── 2_light                             # Data generated with analyis/2-light.R
│   │   ├── 18LX_light_prob.Rdata
│   │   └── labels
│   │       ├── 18LX_light-labeled.csv
│   │       ├── 18LX_light.csv
│   │       └── ...    
│   ├── 3_static                            # Data generated with analyis/3-static.R
│   │   ├── 18LX_static_prob.Rdata
│   │   └── ...
│   ├── 4_basic_graph                       # Data generated with analyis/3-basic_graph.R
│   │   ├── 18LX_basic_graph.Rdata
│   │   └── ...
│   ├── 5_wind_graph
│   │   └── ERA5_wind
│   │       ├──
│   │       └── ...
│   └── gpr_settings.xlsx
├── analysis                                # R code used to analyse your data. Follow the order
│   ├── 1-pressure.R
│   ├── 2-light.R
│   ├── 3-static.R
│   ├── 4-basic-graph.R
│   ├── 5-1-wind-graph_download.R
│   ├── 5-2-wind-graph_create.R
│   ├── 5-3-wind-graph_analyse.R
│   └── 99-combined.R
└── reports                                 # Generate HTML report to be shared (see below for details)
│   ├── _basic_trajectory.Rmd
│   ├── _site.yml
│   ├── _technical_details.Rmd
│   ├── basic_trajectory
│   │   └── 18LX.html
│   ├── technical_details
│   │   └── 18LX.html
│   ├── index.Rmd
│   └── make_reports.R
└── docs                                      # Folder where your reports will be served as a website on Github Page
    └── ...
```
</details>

This repository was generated based on [GeoPressureTemplate (v1.3)](https://github.com/Rafnuss/GeoPressureTemplate).
