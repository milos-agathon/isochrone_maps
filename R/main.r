######################################
# Mapping travel catchment areas
# Milos Popovic 2023/10/10
######################################

install.packages("remotes")
remotes::install_github(
    "GIScience/openrouteservice-r"
)

libs <- c(
    "tidyverse", "openrouteservice",
    "sf", "leaflet", "maptiles",
    "tidyterra"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libs == F)){
    install.packages(
        libs[!installed_libs]
    )
}

invisible(
    lapply(
        libs,
        library,
        character.only = T
    )
)

# 1. DEFINE MAIN PARAMETERS

openrouteservice::ors_profile()
lat <- 52.377
lon <- 4.897
api_key <- "************************************" # PLEASE INSERT HERE YOUR API KEY

# 2. QUERY

coords <- data.frame(lon, lat)

cycling_ams <- openrouteservice::ors_isochrones(
    locations = coords,
    profile = "cycling-regular",
    range = 3600,
    interval = 600,
    api_key = api_key,
    output = "sf"
)

# 3. DATA TRANSFORMATION

sf::sf_use_s2(F)
cycling_ams$mins <- cycling_ams$value / 60
cycling_ams$mins <- factor(
    cycling_ams$mins
)

cycling_ams_cropped <- cycling_ams |>
    dplyr::group_by(mins) |>
    sf::st_intersection() |>
    dplyr::ungroup()

# 4. INTERACTIVE MAP OF CYCLING CATCHMENT AREA

pal_fact <- leaflet::colorFactor(
    "RdPu",
    domain = cycling_ams_cropped$mins,
    reverse = T,
    na.color = "transparent"
)

leaflet::leaflet(
    cycling_ams_cropped
) |>
leaflet::addPolygons(
    fill = T,
    stroke = T,
    color = pal_fact,
    weight = .3,
    fillColor = ~pal_fact(mins),
    fillOpacity = .3
) |>
leaflet::addProviderTiles(
    "CartoDB.Positron"
) |>
leaflet::addLegend(
    "bottomright",
    pal = pal_fact,
    values = cycling_ams_cropped$mins,
    labels = cycling_ams_cropped$mins,
    opacity = .5,
    title = "Cycling distance in Amsterdam"
)

# 5. STATIC MAP OF CYCLING CATCHMENT AREA
cycling_ams_merc <- sf::st_transform(
    cycling_ams_cropped,
    3857
)

ams_layer <- maptiles::get_tiles(
    cycling_ams_merc,
    provider = "CartoDB.Positron",
    zoom = 11
)


cycling_map <- ggplot() +
    tidyterra::geom_spatraster_rgb(
        data = ams_layer
    ) +
    geom_sf(
        data = cycling_ams_merc,
        aes(
            fill = factor(mins),
            color = factor(mins),
            geometry = geometry
        ),
        size = .2,
        alpha = .5,
        inherit.aes = F
    ) +
    scale_fill_manual(
        name = "Minutes",
        values = hcl.colors(
            6, "RdPu"
        )
    ) +
    scale_color_manual(
        values = hcl.colors(
            6, "RdPu"
        )
    ) +
    guides(
        color = "none",
        fill = guide_legend(
            nrow = 1,
            byrow = T,
            keyheight = unit(5, "mm"),
            keywidth = unit(5, "mm"),
            title.position = "top",
            label.position = "bottom",
            label.hjust = .5
        )
    ) +
    theme_void() +
    theme(
        legend.position = "top",
        plot.margin = unit(
            c(
                t = 0, r = 0,
                b = 0, l = 0
            ), "lines"
        )
    ) +
    labs(
        title = "Cycling distance in Amsterdam"
    )

# 6. QUERY MULTIPLE TRAVEL MODES

openrouteservice::ors_profile()

travel_modes <- c(
    "foot-walking",
    "cycling-regular",
    "driving-car"
)

travel_list <- c()

for(mode in travel_modes){
    travel_list[[mode]] <- {
        travel_ams <- openrouteservice::ors_isochrones(
            locations = coords,
            profile = mode,
            range = 1800,
            interval = 1800,
            api_key = api_key,
            output = "sf"
        )
    travel_ams
    }
}

travel_mode_ams <- do.call(
    rbind, travel_list
)

# 7. PANEL MAP OF TRAVEL MODES

travel_mode_ams$mode <- factor(
    rownames(travel_mode_ams),
    labels = c(
        "cycling",
        "driving",
        "walking"
    )
)

travel_ams_merc <- sf::st_transform(
    travel_mode_ams,
    3857
)

travel_map <- ggplot() +
    tidyterra::geom_spatraster_rgb(
        data = ams_layer
    ) +
    geom_sf(
        data = travel_ams_merc,
        aes(
            fill = factor(mode),
            color = factor(mode),
            geometry = geometry
        ),
        size = .2,
        alpha = .5,
        inherit.aes = F
    ) +
    scale_fill_manual(
        name = "Travel mode",
        values = hcl.colors(
            3, "Set 2"
        )
    ) +
    scale_color_manual(
        values = hcl.colors(
            3, "Set 2"
        )
    ) +
    facet_wrap(~mode) +
    theme_void() +
    theme(
        legend.position = "none",
        plot.margin = unit(
            c(
                t = 0, r = 0,
                b = 0, l = 0
            ), "lines"
        ),
        plot.title = element_text(
            size = 14,
            face = "bold",
            color = "grey10",
            hjust = .5
        ),
        strip.text = element_text(
            size = 12,
            color = "grey40",
            hjust = .5
        )
    ) +
    labs(
        title = "Travel distance in Amsterdam"
    )
