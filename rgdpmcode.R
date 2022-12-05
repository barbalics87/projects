#A projectben egy eu térképet próbálok megvalósítani amelyben különböző adatok alapján mutatom be az Európai unió országait
#A felhasznált adatok az Eurostat-tól származnak
library("eurostat")
library("tidyverse")
library("giscoR")
library("sf")
citation("eurostat")
#GiscoR - https://doi.org/10.5281/zenodo.4317946, https://ropengov.github.io/giscoR/
#Adat segédlet: GDP - "nama_10_gdp", eu_countries$code, "2021-01-01", "CP_MEUR", "B1GQ", "GDP per country", c(0, 500000, 1000000, 1500000, 2000000, 2500000, 3000000, 4000000, 5000000, 6000000)
#GDP per capita: "nama_10_pc", eu_countries$code, "20**-01-01", "CP_EUR_HAB", "B1GQ", "GDP per capita", c(0, 10000, 20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000)
#GNI - "nama_10_pp", eu_countries$code, "20**-01-01", "CP_PPS_EU27_2020_HAB", "GNI per capita PPS", c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 60000)
#HICP - "prc_hicp_aind", eu_countries$code, "2021-01-01", "INX_A_AVG", "CP00", "HICP per country", c(0, 5, 10, 15, 20, 25, 30, 40, 50, 60)
#function - dat: adat amit felhasználunk, fg: ország filter, ft: időszak filt, fu: unit filt, fi: item filt, title: plot megnevezése, breaksf: értékek felosztása  

EUmape <- function(dat, fg, ft, fu, fi, title, breaksf){
  if (missing(fi)){
    eudata <- get_eurostat(dat) %>% filter(geo %in% fg & time == ft, unit == fu) %>% select(geo, values)
  }else{
    eudata <- get_eurostat(dat) %>% filter(geo %in% fg & time == ft, unit == fu & .[[2]] == fi) %>% select(geo, values)
  }
  
  gisco_set_cache_dir("./data/mapcache", install=TRUE, overwrite = TRUE)
  
  EUnuts <- gisco_get_nuts(
    country = fg,
    year = "2021",
    epsg = "3035",
    resolution = "03",
    nuts_level = "0"
  )
  
  country_lines <- EUnuts %>% 
    group_by(
      CNTR_CODE
    ) %>%
    summarise(n = n()) %>%
    st_cast("MULTILINESTRING")
  
  EUnuts.sf <- merge(EUnuts,
                     eudata,
                     by.x = "NUTS_ID",
                     by.y = "geo",
  )
  
  EUnuts.sf$values_cut <- cut(EUnuts.sf$values,
                              breaks = breaksf,
                              dig.lab = 5
  )
  
  labs_plot <- prettyNum(breaksf[-1], big.mark = ",")
  
  pal <- hcl.colors(length(breaksf) - 1, "Lajolla")
  
  ggplot(EUnuts.sf) +
    geom_sf(aes(fill = values_cut), linewidth = 0, color = NA, alpha = 0.9) +
    geom_sf(data = country_lines, col = "black", linewidth = 0.1) +
    coord_sf(
      xlim = c(2377294, 7453440),
      ylim = c(1313597, 5628510)
    ) +
    labs(
      title = paste(title, ft),
      subtitle = "NUT-0 level",
      caption = paste0(
        "Source: Eurostat, ", gisco_attributions(),
        "\nBased on Milos Popovic: https://milospopovic.net/how-to-make-choropleth-map-in-r/"
      )
    )+
    scale_fill_manual(
      name =  "Aggregates map for incomes & exports",
      values = pal,
      labels = labs_plot,
      drop = FALSE,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = 0.5,
        keywidth = 2.5,
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = .5,
        nrow = 1,
        byrow = TRUE,
        reverse = FALSE,
        label.position = "bottom"
      )
    ) +
    theme_void() +
    # Theme
    theme(
      plot.title = element_text(
        size = 20, color = pal[length(pal) - 1],
        hjust = 0.5, vjust = 0
      ),
      plot.subtitle = element_text(
        size = 12,
        color = pal[length(pal) - 1],
        hjust = 0.5, vjust = - 5, face = "bold"
      ),
      plot.caption = element_text(
        size = 9, color = "grey60",
        hjust = 0.5, vjust = 0,
        margin = margin(t = 5, b = 10)
      ),
      legend.text = element_text(
        size = 10,
        color = "grey20"
      ),
      legend.title = element_text(
        size = 11,
        color = "grey20"
      ),
      legend.position = "bottom"
    )
}

EUmape("nama_10_gdp", eu_countries$code, "2021-01-01", "CP_MEUR", "B1GQ", "GDP per country", c(0, 500000, 1000000, 1500000, 2000000, 2500000, 3000000, 4000000, 5000000, 6000000))
EUmape("nama_10_pc", eu_countries$code, "2021-01-01", "CP_EUR_HAB", "B1GQ", "GDP per capita", c(0, 10000, 20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000))
EUmape("nama_10_pp", eu_countries$code, "2020-01-01", "CP_PPS_EU27_2020_HAB", , "GNI per capita PPS", c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 60000))
EUmape("prc_hicp_aind", eu_countries$code, "2021-01-01", "INX_A_AVG", "CP00", "HICP per country", c(0, 100, 105, 110, 113, 115, 120, 125, 150, 250))
