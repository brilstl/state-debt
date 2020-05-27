## Packages -----
library(magrittr)

## World ----

## download:: ne_countries(scale = 50, returnclass = "sf") %>% select(name, iso_a2) %>% st_write("euromap.csv", layer_options = "GEOMETRY=AS_WKT")

euro_map <-  sf::st_read("euromap.csv") %>% 
        sf::st_set_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## read data ----

### Eurostat data ----
tsabble <- vroom::vroom("https://ec.europa.eu/eurostat/databrowser-backend/api/extraction/2.0/LIVE/true/tsv/TEINA230", delim = "\t")


### Tidy (1) ----
tsabble <- tsabble %>%
        tidyr::separate(
                col = c(1), 
                c("na_item", 
                  "sector", 
                  "geo", 
                  "unit",
                  "name"), 
                sep = ",") %>%
        dplyr::select(-c(1:3))

## Tidy (2) ----

tsabble <- tsabble %>% 
        tidyr::pivot_longer(-c(name, unit), 
                     names_to = "period", 
                     values_to = "value") %>%
        dplyr::mutate(period = tsibble::yearquarter(period),
               unit = stringr::str_sub(unit, -3 , -1)) %>%
        tidyr::pivot_wider(
                names_from = unit,
                values_from = value
        )

## Tidy (3) ----

tsabble <- tsabble %>%
        dplyr::filter(!grepl("EU", name)) %>%
        dplyr::filter(!grepl("EA", name))


## center (FRANCE, NORWAY, CROATIA)----
## With the help of: bboxfinder.com/

center <- sf::st_centroid(euro_map)

### Save points for France, Norway and Croatia -----

sfc_fr <- sf::st_sfc(sf::st_point(c(2.373047,47.100045)), crs = sf::st_crs(center))
sfc_no <- sf::st_sfc(sf::st_point(c(8.085938,60.392148)), crs = sf::st_crs(center))
sfc_co <- sf::st_sfc(sf::st_point(c(16.655273,45.640928)), crs = sf::st_crs(center))

### Reset points in tibble -----

center[center$name == "France", ] <- sf::st_set_geometry(center[center$name == "France", ],  sfc_fr)
center[center$name == "Norway", ] <- sf::st_set_geometry(center[center$name == "Norway", ],  sfc_no)
center[center$name == "Croatia", ] <- sf::st_set_geometry(center[center$name == "Croatia", ],  sfc_co)

## Save lat and long ----

center <- center %>%
        dplyr::mutate(lat = unlist(purrr::map(.$geometry,1)),
                      long = unlist(purrr::map(.$geometry,2))) %>%
        dplyr::select(iso_a2, lat, long, name) %>%
        sf::st_set_geometry(NULL)

## Tidy (4) -----

tsabble <- tsabble %>%
        dplyr::mutate(name = dplyr::case_when(
                name %in% c("UK") ~ "GB",
                name %in% c("EL") ~ "GR",
                TRUE ~ as.character(name)),
               GDP = GDP/100,
               EUR = EUR * 1e6,
               EUR = EUR / 1e9) %>%
        dplyr::rename(iso_a2 = name,
               `Euros` = EUR,
               `GDP ratio` = GDP)

tsabble <- tsabble %>%
        dplyr::filter(!iso_a2 %in% c("MT"))

tsabble <- tsabble %>%
        dplyr::left_join(center)

## USE TSABBLE for geom_point ----

euro_map <- euro_map %>%
        ggplot2::ggplot() + 
        ggplot2::geom_sf(fill = "transparent",
                color = "grey65",
                size = 0.6) +
        ggplot2::coord_sf(expand = FALSE)

## Save empty tibble ----

selected_points <- tsabble[0, ]

str(selected_points)

dad_row <- (tsabble %>% dplyr::distinct(period) %>% nrow(.)) - (tsabble %>% dplyr::distinct(period) %>% nrow(.))

                       