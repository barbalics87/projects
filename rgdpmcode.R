#A projectben egy eu térképet próbálok megvalósítani amelyben gdp alapján mutatom be az Európai unió országait
#A felhasznált adatok az Eurostat-tól származnak
library("eurostat")
library("tidyverse")
library("giscoR")
library("sf")
citation("eurostat")
#GiscoR - https://doi.org/10.5281/zenodo.4317946, https://ropengov.github.io/giscoR/

gdp_data <- get_eurostat("nama_10_gdp")
Eu_codes <- eu_countries$code

gdp_dataf <- gdp_data %>% filter(geo %in% Eu_codes & time == "2021-01-01" & unit == "CP_MEUR") %>% 
select(geo, values) %>% aggregate(values~geo, sum)
gdp_dataf$values <- as.numeric(as.character(gdp_dataf$values)) / 100000


gisco_set_cache_dir("./data/mapcache", install=TRUE)

EUnuts <- gisco_get_nuts(
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
                  gdp_dataf,
                  by.x = "NUTS_ID",
                  by.y = "geo",
)

br <- c(0, 1, 5, 10, 20, 50, 75, 100, 200, 500)

EUnuts.sf$values_cut <- cut(EUnuts.sf$values,
                           breaks = br,
                           dig.lab = 5
)

labs_plot <- prettyNum(br[-1], big.mark = ",")

pal <- hcl.colors(length(br) - 1, "Lajolla")

ggplot(EUnuts.sf) +
  geom_sf(aes(fill = values_cut), linewidth = 0, color = NA, alpha = 0.9) +
  geom_sf(data = country_lines, col = "black", linewidth = 0.1) +
  coord_sf(
    xlim = c(2377294, 7453440),
    ylim = c(1313597, 5628510)
  ) +
  labs(
    title = "GDP 2021 1^11EUR térkép",
    subtitle = "NUT-0 level",
    caption = paste0(
      "Source: Eurostat, ", gisco_attributions(),
      "\nBased on Milos Popovic: https://milospopovic.net/how-to-make-choropleth-map-in-r/"
    )
  )
scale_fill_manual(
  name = "GDP per country",
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
      hjust = 0.5, vjust = -6
    ),
    plot.subtitle = element_text(
      size = 14,
      color = pal[length(pal) - 1],
      hjust = 0.5, vjust = -10, face = "bold"
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