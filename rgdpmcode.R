#A projectben egy eu interaktív térképet próbálok megvalósítani amelyben gdp alapján mutatom be az Európai unió országait
#A felhasznált adatok az Eurostat-tól származnak
library("eurostat")
library("tidyverse")
library("giscoR")
library("sf")
citation("eurostat")
#GiscoR - https://doi.org/10.5281/zenodo.4317946, https://ropengov.github.io/giscoR/

gdp_data <- get_eurostat("nama_10_gdp")
gdp_data <- label_eurostat(gdp_data)
Eu_codes <- eu_countries$name

gdp_dataf <- gdp_data %>% filter(geo %in% Eu_codes & time == "2021-01-01") %>% select(geo, values)

gisco_set_cache_dir("./data/mapcache", install=TRUE)

EUnuts <- gisco_get_nuts(
  year = "2021",
  epsg = "3035",
  resolution = "03",
  nuts_level = "1"
)

country_lines <- EUnuts %>% 
