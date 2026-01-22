###
# See https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-workflow.html
###

library(GeoPressureR)

rm(list=ls())
## OPTION 1: Run workflow step-by-step for a single tag
id <- "AC0" # Run a single tag
geopressuretemplate_config(id)
tag <- geopressuretemplate_tag(id)
graph <- geopressuretemplate_graph(id)
geopressuretemplate_pressurepath(id)

geopressuretemplate(id)

# OPTION 2: Run entire workflow for all tags
list_id <- tail(names(yaml::yaml.load_file("config.yml", eval.expr = FALSE)), -1)

#list_id <- list_id[list_id != "91A"]

for (id in list_id){
  cli::cli_h1("Run for {id}")
  geopressuretemplate_pressurepath(id) # geopressuretemplate(id)
}


#----- checking the outcome
load(glue::glue("./data/interim/{id}.RData"))

plot_path(path_most_likely,  plot_leaflet = F)

plot_pressurepath(pressurepath_most_likely, type = "histogram")

plot(tag, type = "map_pressure")
plot(tag, type = "map_light")

plot(marginal,  path = path_most_likely)

# check map differences between pressure and marginal probability:
geopressureviz(tag, marginal=marginal, path=path_most_likely)

plot_path(path_simulation, plot_leaflet = F)

path_most_likely <- graph_most_likely(graph)






###### Geopressure path preparation #####
#This will create a separate .rds file for each pressurepath_most_likely found.
library(dplyr)
rdata_files <- list.files("data/interim", pattern = "\\.RData$", full.names = TRUE)

rdata_files<- rdata_files[grepl("data/interim/AC0\\.RData$", rdata_files)]

for (file in rdata_files) {
  tmp_env <- new.env()
  load(file, envir = tmp_env)

  if ("pressurepath_most_likely" %in% ls(tmp_env)) {
    ppath <- as.data.frame(tmp_env$pressurepath_most_likely)

    if (is.data.frame(ppath)) {
      out_file <- sub("\\.RData$", "_pressurepath_most_likely.xlsx", basename(file))
      writexl::write_xlsx(ppath, file.path("data/interim", out_file))
      message("Saved ", out_file)
    } else {
      warning("pressurepath_most_likely in ", basename(file), " is not a data frame.")
    }

  } else {
    warning("pressurepath_most_likely not found in ", basename(file))
  }
}
