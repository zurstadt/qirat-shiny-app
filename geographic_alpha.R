# Geographic Explorer - Extracted from app.R
# This module contains all UI, server, and helper code for the Geographic Explorer tab.
# To re-enable, source this file and integrate the UI/server components back into app.R.

# ============================================================================
# CONSTANTS (must match app.R or be passed in)
# ============================================================================
# DB_PATH <- "data/iqsa_bibliography.db"
# SCHOLAR_ROUTES_PATH <- "routes/all_scholar_routes.json"
# SEA_ROUTES_GIS_PATH <- "routes/sea_routes_gis.json"
# SEA_ROUTE_GEOMETRIES_PATH <- "routes/sea_route_geometries.json"
# MEDITERRANEAN_PORTS_PATH <- "routes/mediterranean_ports.json"

# ============================================================================
# CSS (add to tags$style in UI)
# ============================================================================
geo_css <- "
      /* Geographic Explorer styles */
      .geo-stat-card {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 8px;
        padding: 20px;
        text-align: center;
        border: 1px solid #dee2e6;
      }

      .geo-stat-number {
        font-size: 32px;
        font-weight: bold;
        color: #001158;
      }

      .geo-stat-label {
        font-size: 12px;
        color: #666;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
"

# ============================================================================
# UI PANEL (insert as a tabPanel in navbarPage)
# ============================================================================
geo_explorer_ui <- function() {
  tabPanel(
    title = tagList(icon("map"), "Geographic Explorer"),
    value = "geographic_explorer",
    br(),

    div(class = "card",
      div(class = "card-header", icon("route"), " Scholar Mobility Patterns"),
      div(class = "card-body",

        # Map at top (primary element)
        h4(class = "section-header-bold", "Regional Map"),
        p(style = "font-size: 0.9em; color: #666;",
          "Scholar travel routes displayed on load. Click subregions for statistics. Dotted line = Mašriq/Maġrib boundary."),
        leafletOutput("geo_travel_map", height = "550px"),

        hr(),

        # Search/Filter controls
        h4(class = "section-header-bold", "Filters"),
        fluidRow(
          column(2, selectInput("geo_mobility_filter", "Mobility:",
            choices = c("All" = "all",
                       "Sedentary" = "sedentary",
                       "Local Traveler" = "local-traveler",
                       "Extensive Local" = "extensive-local",
                       "Inter-Regional" = "inter-regional"))),
          column(2, selectInput("geo_meta_region", "Region:",
            choices = c("All" = "all",
                       "Mašriq (East)" = "mašriq",
                       "Maġrib (West)" = "maġrib",
                       "Inter-Regional" = "inter"))),
          column(2, selectInput("geo_home_subregion", "Home Subregion:",
            choices = c("All" = "all", "al-ʿirāq", "al-šām", "al-ʾandalus", "egypt",
                       "ǧibāl-ṭabaristān", "ʾifrīqiyyah", "fārs", "ḫurāsān", "ḥiǧāz"))),
          column(2, sliderInput("geo_century_filter", "Century (AH):", min = 4, max = 7, value = c(4, 7), step = 1)),
          column(2, sliderInput("geo_subregions_filter", "Subregions:", min = 1, max = 7, value = c(1, 7), step = 1)),
          column(2, actionButton("geo_reset", "Reset", icon = icon("redo"), class = "btn-secondary", style = "margin-top: 25px;"))
        ),

        hr(),

        # Mobility Statistics Summary (reactive to filters)
        h4(class = "section-header-bold", "Mobility Statistics"),
        p(style = "font-size: 0.85em; color: #666;", uiOutput("geo_filter_summary_inline")),
        fluidRow(
          column(3, uiOutput("geo_stat_sedentary")),
          column(3, uiOutput("geo_stat_local")),
          column(3, uiOutput("geo_stat_extensive")),
          column(3, uiOutput("geo_stat_interregional"))
        ),

        hr(),

        # Key Questions Answered
        h4(class = "section-header-bold", "Key Findings"),
        uiOutput("geo_key_findings"),

        hr(),

        # Author mobility table with search
        h4(class = "section-header-bold", "Author Mobility Database"),
        fluidRow(
          column(6,
            textInput("geo_author_search", "Search Author:",
              placeholder = "Shatibi, شاطبي, or šāṭibī..."),
            tags$small(class = "text-muted", style = "display: block; margin-top: -10px;",
              "Arabic, digraphs (dh, gh, sh), or transliteration")),
          column(3, uiOutput("geo_author_match"))
        ),

        # Author mobility table
        DTOutput("geo_author_mobility_table")
      )
    )
  )
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Load scholar routes from database (using normalized schema with aliases)
load_scholar_routes_from_db <- function(db_path = DB_PATH) {
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))

    query <- "
      SELECT
        author_id, author_name, death_century, regionality,
        resolved_place, latitude, longitude, sequence_order
      FROM author_itineraries
      ORDER BY author_id, sequence_order
    "
    df <- dbGetQuery(con, query)
    if (nrow(df) == 0) return(NULL)

    scholar_list <- list()
    for (aid in unique(df$author_id)) {
      author_df <- df[df$author_id == aid, ]
      if (nrow(author_df) < 2) next

      author_name <- author_df$author_name[1]
      scholar_list[[author_name]] <- list(
        author = author_name,
        regionality = author_df$regionality[1] %||% "unknown",
        death_century = as.numeric(author_df$death_century[1]) %||% NA,
        places_sequence = author_df$resolved_place,
        coords = data.frame(
          place = author_df$resolved_place,
          lat = author_df$latitude,
          lon = author_df$longitude
        ),
        total_distance_km = 0,
        uses_sea_routes = FALSE
      )
    }
    scholar_list
  }, error = function(e) {
    message("Scholar routes from DB error: ", e$message)
    NULL
  })
}

# Load scholar routes - prefer JSON (has path waypoints for route visualization)
load_scholar_routes <- function(json_path = SCHOLAR_ROUTES_PATH, db_path = DB_PATH) {
  if (file.exists(json_path)) {
    tryCatch({
      routes_json <- fromJSON(json_path, simplifyVector = FALSE)

      scholar_list <- lapply(names(routes_json), function(name) {
        scholar <- routes_json[[name]]
        list(
          author = scholar$author,
          regionality = scholar$regionality %||% "unknown",
          death_century = as.numeric(scholar$death_century) %||% NA,
          places_sequence = scholar$places_sequence %||% character(0),
          routes = scholar$routes %||% list(),
          total_distance_km = scholar$total_distance_km %||% 0,
          uses_sea_routes = scholar$uses_sea_routes %||% FALSE
        )
      })
      names(scholar_list) <- names(routes_json)
      message("Loaded ", length(scholar_list), " scholar routes from JSON with path data")
      return(scholar_list)
    }, error = function(e) {
      message("JSON loading error: ", e$message, " - falling back to database")
    })
  }

  routes <- load_scholar_routes_from_db(db_path)
  if (!is.null(routes) && length(routes) > 0) {
    message("Loaded ", length(routes), " scholar routes from database (no path waypoints)")
    return(routes)
  }

  message("No routes data available")
  NULL
}

# Load GIS-generated sea route geometries (coastal-following paths)
load_gis_sea_routes <- function(json_path = SEA_ROUTES_GIS_PATH) {
  tryCatch({
    if (!file.exists(json_path)) {
      message("GIS sea routes file not found: ", json_path)
      return(list())
    }

    data <- fromJSON(json_path, simplifyVector = FALSE)
    routes <- list()

    for (feature in data$features) {
      props <- feature$properties
      geom <- feature$geometry
      coords <- geom$coordinates

      route_key <- paste0(tolower(props$from), "_", tolower(props$to))
      reverse_key <- paste0(tolower(props$to), "_", tolower(props$from))

      coord_matrix <- do.call(rbind, lapply(coords, function(c) c(c[[1]], c[[2]])))

      routes[[route_key]] <- list(
        from = props$from,
        to = props$to,
        coordinates = coord_matrix,
        distance_km = props$distance_km
      )

      routes[[reverse_key]] <- list(
        from = props$to,
        to = props$from,
        coordinates = coord_matrix[nrow(coord_matrix):1, ],
        distance_km = props$distance_km
      )
    }

    message("Loaded ", length(data$features), " GIS sea routes with coastal geometries")
    return(routes)
  }, error = function(e) {
    message("Error loading GIS sea routes: ", e$message)
    return(list())
  })
}

# Extract coordinate portion from Thurayya code
extract_coords_from_code <- function(code) {
  match <- regmatches(code, regexpr("[0-9]{3}[EW][0-9]{3}N", code))
  if (length(match) > 0) return(match) else return(NULL)
}

# Load sea route geometries with multiple lookup keys
load_sea_route_geometries <- function(json_path = SEA_ROUTE_GEOMETRIES_PATH) {
  tryCatch({
    if (!file.exists(json_path)) {
      message("Sea route geometries file not found: ", json_path)
      return(list())
    }

    data <- fromJSON(json_path, simplifyVector = FALSE)
    routes <- list()

    for (key in names(data)) {
      route <- data[[key]]
      coords <- route$coordinates

      if (!is.null(coords) && length(coords) > 0) {
        coord_matrix <- do.call(rbind, lapply(coords, function(c) {
          if (is.list(c)) c(c[[1]], c[[2]]) else c
        }))

        from_code <- route$from_code
        to_code <- route$to_code
        from_coords <- extract_coords_from_code(from_code)
        to_coords <- extract_coords_from_code(to_code)
        from_base <- sub("_[SRW]$", "", from_code)
        to_base <- sub("_[SRW]$", "", to_code)

        route_data <- list(
          from_code = from_code,
          to_code = to_code,
          from_name = route$from_name,
          to_name = route$to_name,
          coordinates = coord_matrix,
          distance_km = route$distance_km
        )

        routes[[paste0(from_code, "_TO_", to_code)]] <- route_data
        routes[[paste0(from_base, "_TO_", to_base)]] <- route_data
        if (!is.null(from_coords) && !is.null(to_coords)) {
          routes[[paste0(from_coords, "_TO_", to_coords)]] <- route_data
        }
        routes[[paste0(tolower(route$from_name), "_TO_", tolower(route$to_name))]] <- route_data

        reverse_data <- route_data
        reverse_data$coordinates <- coord_matrix[nrow(coord_matrix):1, , drop = FALSE]
        routes[[paste0(to_code, "_TO_", from_code)]] <- reverse_data
        routes[[paste0(to_base, "_TO_", from_base)]] <- reverse_data
        if (!is.null(from_coords) && !is.null(to_coords)) {
          routes[[paste0(to_coords, "_TO_", from_coords)]] <- reverse_data
        }
        routes[[paste0(tolower(route$to_name), "_TO_", tolower(route$from_name))]] <- reverse_data
      }
    }

    message("Loaded ", length(data), " sea route geometries with coordinate-based lookup")
    return(routes)
  }, error = function(e) {
    message("Error loading GIS sea routes: ", e$message)
    return(list())
  })
}

# Load Mediterranean port locations
load_mediterranean_ports <- function(json_path = MEDITERRANEAN_PORTS_PATH) {
  tryCatch({
    if (!file.exists(json_path)) {
      message("Mediterranean ports file not found: ", json_path)
      return(NULL)
    }

    data <- fromJSON(json_path, simplifyVector = FALSE)
    ports_df <- do.call(rbind, lapply(data$features, function(f) {
      data.frame(
        name = f$properties$name,
        thurayya_code = f$properties$thurayya_code,
        lon = f$geometry$coordinates[[1]],
        lat = f$geometry$coordinates[[2]],
        stringsAsFactors = FALSE
      )
    }))

    message("Loaded ", nrow(ports_df), " Mediterranean ports")
    return(ports_df)
  }, error = function(e) {
    message("Error loading Mediterranean ports: ", e$message)
    return(NULL)
  })
}

# Load city coordinates from database
load_city_coordinates <- function(db_path = DB_PATH) {
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))

    query <- "
      SELECT
        place_name_latin as city,
        latitude as lat,
        longitude as lon,
        region
      FROM places
      WHERE latitude IS NOT NULL
    "
    dbGetQuery(con, query)
  }, error = function(e) {
    message("City coordinates loading error: ", e$message)
    NULL
  })
}

# Parse coordinates from Thurayya code
parse_thurayya_coords <- function(code) {
  if (is.na(code) || code == "" || is.null(code)) return(NULL)

  match <- regmatches(code, regexec("_(\\d{3})([EW])(\\d{3})([NS])_", code))[[1]]
  if (length(match) < 5) return(NULL)

  lon <- as.numeric(match[2]) / 10
  if (match[3] == "W") lon <- -lon

  lat <- as.numeric(match[4]) / 10
  if (match[5] == "S") lat <- -lat

  list(lon = lon, lat = lat)
}

# Extract city pairs from scholar journeys
extract_city_pairs <- function(scholar_routes) {
  pairs <- list()

  for (scholar_name in names(scholar_routes)) {
    scholar <- scholar_routes[[scholar_name]]
    places <- unlist(scholar$places_sequence)

    if (length(places) < 2) next

    for (i in 1:(length(places) - 1)) {
      city1 <- as.character(places[i])
      city2 <- as.character(places[i + 1])
      pair <- sort(c(city1, city2))
      key <- paste(pair, collapse = "|")

      if (is.null(pairs[[key]])) {
        pairs[[key]] <- list(
          from = pair[1],
          to = pair[2],
          scholars = character(0),
          centuries = numeric(0),
          regionalities = character(0)
        )
      }

      pairs[[key]]$scholars <- c(pairs[[key]]$scholars, scholar$author)
      pairs[[key]]$centuries <- c(pairs[[key]]$centuries, scholar$death_century)
      pairs[[key]]$regionalities <- c(pairs[[key]]$regionalities, scholar$regionality)
    }
  }

  if (length(pairs) == 0) return(NULL)

  pairs_df <- do.call(rbind, lapply(names(pairs), function(key) {
    p <- pairs[[key]]
    has_cross_regional <- any(grepl("visits", p$regionalities, ignore.case = TRUE))
    edge_regionality <- if (has_cross_regional) {
      "cross-regional"
    } else {
      names(sort(table(p$regionalities), decreasing = TRUE)[1])
    }

    data.frame(
      from = p$from,
      to = p$to,
      count = length(p$scholars),
      scholars = paste(p$scholars, collapse = "; "),
      primary_century = as.numeric(names(sort(table(p$centuries), decreasing = TRUE)[1])),
      primary_regionality = edge_regionality,
      stringsAsFactors = FALSE
    )
  }))

  pairs_df %>% arrange(desc(count))
}

# Count city visit frequencies
count_city_visits <- function(scholar_routes) {
  city_data <- list()

  for (scholar_name in names(scholar_routes)) {
    scholar <- scholar_routes[[scholar_name]]
    places <- unlist(scholar$places_sequence)

    for (city in places) {
      if (is.null(city_data[[city]])) {
        city_data[[city]] <- list(
          visits = 0,
          scholars = character(0),
          centuries = numeric(0),
          regionalities = character(0)
        )
      }

      city_data[[city]]$visits <- city_data[[city]]$visits + 1
      city_data[[city]]$scholars <- c(city_data[[city]]$scholars, scholar$author)
      city_data[[city]]$centuries <- c(city_data[[city]]$centuries, scholar$death_century)
      city_data[[city]]$regionalities <- c(city_data[[city]]$regionalities, scholar$regionality)
    }
  }

  if (length(city_data) == 0) return(NULL)

  cities_df <- do.call(rbind, lapply(names(city_data), function(city) {
    c <- city_data[[city]]
    data.frame(
      city = city,
      visits = c$visits,
      scholars = paste(unique(c$scholars), collapse = "; "),
      n_scholars = length(unique(c$scholars)),
      primary_century = as.numeric(names(sort(table(c$centuries), decreasing = TRUE)[1])),
      primary_regionality = names(sort(table(c$regionalities), decreasing = TRUE)[1]),
      stringsAsFactors = FALSE
    )
  }))

  cities_df %>% arrange(desc(visits))
}

# Filter scholar routes by criteria
filter_scholar_routes <- function(scholar_routes, century_range, regionality) {
  filtered <- scholar_routes

  filtered <- filtered[sapply(filtered, function(s) {
    cent <- s$death_century
    !is.na(cent) && cent >= century_range[1] && cent <= century_range[2]
  })]

  if (regionality != "all") {
    filtered <- filtered[sapply(filtered, function(s) {
      reg <- s$regionality
      if (regionality == "mashriq") {
        reg %in% c("mašriq", "mašriq visits maġrib")
      } else if (regionality == "maghrib") {
        reg %in% c("maġrib", "maġrib visits mašriq")
      } else if (regionality == "cross") {
        grepl("visits", reg)
      } else {
        TRUE
      }
    })]
  }

  filtered
}

# Get edge color based on regionality
get_edge_color <- function(regionality) {
  if (grepl("visits", regionality, ignore.case = TRUE)) {
    return("#CC79A7")
  } else if (regionality == "maġrib") {
    return("#56B4E9")
  } else if (regionality == "mašriq") {
    return("#E69F00")
  }
  "#666666"
}

# Map Thurayya regions to Mašriq/Maġrib
get_region_category <- function(thurayya_region) {
  maghrib_regions <- c(
    "al-Andalus (Spain)", "al-Maġrib", "Miṣr (Egypt)", "Barqaŧ (Lybia)"
  )
  if (thurayya_region %in% maghrib_regions) {
    return("Maġrib")
  }
  return("Mašriq")
}

# Subregion rectangles (matching GIF animations)
get_subregion_rects <- function() {
  data.frame(
    subregion = c("al-ʾandalus", "egypt", "ʾifrīqiyyah", "al-šām", "al-ʿirāq",
                  "ǧibāl-ṭabaristān", "fārs", "ḫurāsān", "ḥiǧāz"),
    xmin = c(-9.5, 25.0, -8.0, 34.0, 42.0, 48.0, 47.0, 57.0, 36.5),
    xmax = c(-2.0, 35.0, 13.0, 42.0, 48.0, 54.0, 57.0, 70.0, 43.0),
    ymin = c(36.0, 22.0, 30.0, 29.5, 29.0, 32.0, 26.0, 32.0, 20.0),
    ymax = c(43.0, 32.0, 37.5, 37.5, 37.5, 42.0, 32.0, 42.0, 29.0),
    center_lon = c(-4.0, 31.2, 3.0, 36.3, 44.4, 51.0, 52.0, 60.0, 39.8),
    center_lat = c(37.9, 30.0, 35.5, 33.5, 33.3, 36.5, 29.6, 36.0, 24.5),
    meta_region = c("maġrib", "maġrib", "maġrib", "mašriq", "mašriq",
                    "mašriq", "mašriq", "mašriq", "mašriq"),
    color = c("#56B4E9", "#56B4E9", "#56B4E9", "#E69F00", "#E69F00",
              "#E69F00", "#E69F00", "#E69F00", "#E69F00"),
    stringsAsFactors = FALSE
  )
}

# Mashriq-Maghrib dividing line coordinates
get_dividing_line <- function() {
  data.frame(
    lon = c(32.5, 32.5, 30.0),
    lat = c(20.0, 31.5, 45.0)
  )
}

# Get all Thurayya regions grouped by Mašriq/Maġrib
get_thurayya_region_groups <- function() {
  list(
    "Maġrib" = c(
      "al-Andalus (Spain)", "al-Maġrib", "Miṣr (Egypt)", "Barqaŧ (Lybia)"
    ),
    "Mašriq" = c(
      "al-Šām (Greater Syria)", "al-ʿIrāq", "Aqūr (al-Jazīraŧ)",
      "al-Jibāl", "Ḫurāsān", "Fārs(or Fāris)", "Kirmān",
      "Mā-warāʾ-l-nahr (Transoxiana)", "al-Daylam", "Jazīraŧ al-ʿarab",
      "Sijistān (Sīstān)", "al-Sind", "Ḫūzistān (al-Ahwāz)"
    )
  )
}

# Create rectangular subregion polygons for leaflet
create_subregion_polygons <- function() {
  rects <- get_subregion_rects()

  polygons_list <- lapply(1:nrow(rects), function(i) {
    coords <- matrix(c(
      rects$xmin[i], rects$ymin[i],
      rects$xmax[i], rects$ymin[i],
      rects$xmax[i], rects$ymax[i],
      rects$xmin[i], rects$ymax[i],
      rects$xmin[i], rects$ymin[i]
    ), ncol = 2, byrow = TRUE)

    st_sf(
      subregion = rects$subregion[i],
      meta_region = rects$meta_region[i],
      color = rects$color[i],
      center_lon = rects$center_lon[i],
      center_lat = rects$center_lat[i],
      geometry = st_sfc(st_polygon(list(coords)), crs = 4326)
    )
  })

  do.call(rbind, polygons_list)
}

# Legacy function kept for compatibility
create_region_polygons <- function(city_coords) {
  create_subregion_polygons()
}

# Combined geographic data loader
load_geographic_data <- function() {
  list(
    scholar_routes = load_scholar_routes(),
    city_coords = load_city_coordinates(),
    gis_sea_routes = load_gis_sea_routes(),
    sea_route_geometries = load_sea_route_geometries(),
    mediterranean_ports = load_mediterranean_ports()
  )
}

# ============================================================================
# SERVER LOGIC (call inside server function)
# ============================================================================
# To re-enable: call geo_explorer_server(input, output, session, rv, DB_PATH)
# inside the main server function, and add geo_data to rv:
#   rv$geo_data <- load_geographic_data()

geo_explorer_server <- function(input, output, session, rv, DB_PATH) {

  # City pairs (edges) reactive
  geo_city_pairs <- reactive({
    routes <- geo_filtered_routes()
    if (length(routes) == 0) return(NULL)
    extract_city_pairs(routes)
  })

  # City visits (nodes) reactive
  geo_city_visits <- reactive({
    routes <- geo_filtered_routes()
    if (length(routes) == 0) return(NULL)
    count_city_visits(routes)
  })

  # Normalize city name for coordinate lookup
  normalize_city_name <- function(name) {
    name <- tolower(name)
    name <- gsub("[ʾʿ]", "", name)
    name <- gsub("^al-", "", name)
    name <- gsub("^al ", "", name)
    name
  }

  # Base map output
  output$geo_travel_map <- renderLeaflet({
    rects <- get_subregion_rects()
    dividing_line <- get_dividing_line()

    map <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       options = providerTileOptions(opacity = 0.5)) %>%
      setView(lng = 35, lat = 32, zoom = 4)

    for (i in 1:nrow(rects)) {
      map <- map %>%
        addRectangles(
          lng1 = rects$xmin[i], lat1 = rects$ymin[i],
          lng2 = rects$xmax[i], lat2 = rects$ymax[i],
          fillColor = rects$color[i],
          fillOpacity = 0.2,
          color = rects$color[i],
          weight = 2,
          opacity = 0.7,
          layerId = paste0("rect_", rects$subregion[i]),
          group = "subregions"
        ) %>%
        addLabelOnlyMarkers(
          lng = rects$center_lon[i], lat = rects$center_lat[i],
          label = rects$subregion[i],
          labelOptions = labelOptions(
            noHide = TRUE, direction = "center", textOnly = TRUE,
            style = list("font-size" = "11px", "font-weight" = "bold", "color" = rects$color[i])
          )
        )
    }

    map <- map %>%
      addPolylines(
        lng = dividing_line$lon, lat = dividing_line$lat,
        color = "black", weight = 3, opacity = 0.8,
        dashArray = "10, 10",
        popup = "Mašriq / Maġrib Boundary",
        group = "dividing_line"
      )

    map
  })

  # Load mobility data from database
  geo_mobility_data <- reactive({
    tryCatch({
      con <- dbConnect(SQLite(), DB_PATH)
      on.exit(dbDisconnect(con))

      authors <- dbGetQuery(con, "
        SELECT author_id,
               COALESCE(author_name_canonical, author_name) as author_name,
               author_name_arabic, death_century,
               regionality, home_subregion, subregions_visited,
               inter_regional, total_distance_km, mobility_category
        FROM authors
        WHERE home_subregion IS NOT NULL
          AND in_range = 'T'
      ")

      subregion_stats <- dbGetQuery(con, "
        SELECT home_subregion as subregion,
               COUNT(*) as n_authors,
               SUM(CASE WHEN mobility_category = 'sedentary' THEN 1 ELSE 0 END) as sedentary,
               SUM(CASE WHEN mobility_category = 'local-traveler' THEN 1 ELSE 0 END) as local_traveler,
               SUM(CASE WHEN mobility_category = 'extensive-local' THEN 1 ELSE 0 END) as extensive,
               SUM(CASE WHEN mobility_category = 'inter-regional' THEN 1 ELSE 0 END) as inter_regional,
               AVG(total_distance_km) as avg_distance,
               AVG(subregions_visited) as avg_subregions
        FROM authors
        WHERE home_subregion IS NOT NULL
          AND in_range = 'T'
        GROUP BY home_subregion
      ")

      list(authors = authors, subregion_stats = subregion_stats)
    }, error = function(e) {
      message("Error loading mobility data: ", e$message)
      NULL
    })
  })

  # Handle clicks on subregion rectangles
  observeEvent(input$geo_travel_map_shape_click, {
    click <- input$geo_travel_map_shape_click
    if (is.null(click) || is.null(click$id)) return()

    if (!startsWith(click$id, "rect_")) return()
    subregion <- sub("^rect_", "", click$id)

    mobility_data <- geo_mobility_data()
    if (is.null(mobility_data)) return()

    subregion_stats <- mobility_data$subregion_stats
    rects <- get_subregion_rects()

    stats <- subregion_stats[subregion_stats$subregion == subregion, ]
    rect_info <- rects[rects$subregion == subregion, ]

    if (nrow(rect_info) == 0) return()

    n_authors <- if (nrow(stats) > 0) stats$n_authors else 0
    sedentary <- if (nrow(stats) > 0) stats$sedentary else 0
    local_trav <- if (nrow(stats) > 0) stats$local_traveler else 0
    extensive <- if (nrow(stats) > 0) stats$extensive else 0
    inter_reg <- if (nrow(stats) > 0) stats$inter_regional else 0
    avg_dist <- if (nrow(stats) > 0 && !is.na(stats$avg_distance)) round(stats$avg_distance) else 0
    avg_subs <- if (nrow(stats) > 0 && !is.na(stats$avg_subregions)) round(stats$avg_subregions, 1) else 0

    fill_color <- rect_info$color[1]
    meta <- rect_info$meta_region[1]

    popup_html <- paste0(
      "<div style='min-width: 220px;'>",
      "<strong style='font-size: 1.2em;'>", subregion, "</strong><br/>",
      "<span style='color: ", fill_color, "; font-weight: bold;'>",
      ifelse(meta == "maġrib", "Maġrib", "Mašriq"), "</span>",
      "<hr style='margin: 8px 0;'/>",
      "<strong>", n_authors, "</strong> scholars from here<br/>",
      "<hr style='margin: 8px 0;'/>",
      "<em>Mobility breakdown:</em><br/>",
      "&nbsp;&nbsp;Sedentary: ", sedentary, "<br/>",
      "&nbsp;&nbsp;Local travelers: ", local_trav, "<br/>",
      "&nbsp;&nbsp;Extensive local: ", extensive, "<br/>",
      "&nbsp;&nbsp;Inter-regional: ", inter_reg, "<br/>",
      "<hr style='margin: 8px 0;'/>",
      "<em>Avg distance:</em> ", format(avg_dist, big.mark = ","), " km<br/>",
      "<em>Avg subregions:</em> ", avg_subs,
      "</div>"
    )

    leafletProxy("geo_travel_map") %>%
      clearPopups() %>%
      addPopups(
        lng = click$lng, lat = click$lat,
        popup = popup_html
      )
  })

  # Filtered routes reactive
  geo_filtered_routes <- reactive({
    routes <- rv$geo_data$scholar_routes
    mobility_data <- geo_mobility_data()

    if (is.null(routes) || is.null(mobility_data)) return(list())

    authors_df <- mobility_data$authors

    if (!is.null(input$geo_mobility_filter) && input$geo_mobility_filter != "all") {
      authors_df <- authors_df[authors_df$mobility_category == input$geo_mobility_filter, ]
    }

    if (!is.null(input$geo_meta_region) && input$geo_meta_region != "all") {
      if (input$geo_meta_region == "inter") {
        authors_df <- authors_df[authors_df$inter_regional == 1, ]
      } else {
        authors_df <- authors_df[authors_df$regionality == input$geo_meta_region, ]
      }
    }

    if (!is.null(input$geo_home_subregion) && input$geo_home_subregion != "all") {
      authors_df <- authors_df[authors_df$home_subregion == input$geo_home_subregion, ]
    }

    if (!is.null(input$geo_century_filter)) {
      authors_df <- authors_df[!is.na(authors_df$death_century) &
                               authors_df$death_century >= input$geo_century_filter[1] &
                               authors_df$death_century <= input$geo_century_filter[2], ]
    }

    if (!is.null(input$geo_subregions_filter)) {
      authors_df <- authors_df[!is.na(authors_df$subregions_visited) &
                               authors_df$subregions_visited >= input$geo_subregions_filter[1] &
                               authors_df$subregions_visited <= input$geo_subregions_filter[2], ]
    }

    matching_authors <- tolower(authors_df$author_name)
    routes[tolower(names(routes)) %in% matching_authors]
  })

  # Draw filtered routes on the map
  observe({
    req(rv$geo_data$scholar_routes)
    req(rv$geo_data$city_coords)

    isolate({
      input$geo_mobility_filter
      input$geo_meta_region
      input$geo_home_subregion
      input$geo_century_filter
    })

    routes <- geo_filtered_routes()
    coords <- rv$geo_data$city_coords
    gis_sea_routes <- rv$geo_data$gis_sea_routes
    sea_route_geoms <- rv$geo_data$sea_route_geometries

    coords_lookup <- coords
    coords_lookup$city_norm <- sapply(coords_lookup$city, normalize_city_name)

    proxy <- leafletProxy("geo_travel_map") %>%
      clearGroup("routes")

    if (length(routes) == 0) return()

    path_routes_drawn <- 0
    fallback_routes_drawn <- 0
    gis_sea_routes_drawn <- 0

    lookup_sea_route <- function(from_code, to_code) {
      if (is.null(sea_route_geoms) || length(sea_route_geoms) == 0) return(NULL)
      from_coords <- extract_coords_from_code(from_code)
      to_coords <- extract_coords_from_code(to_code)
      if (!is.null(from_coords) && !is.null(to_coords)) {
        key <- paste0(from_coords, "_TO_", to_coords)
        if (!is.null(sea_route_geoms[[key]])) return(sea_route_geoms[[key]])
      }
      key <- paste0(from_code, "_TO_", to_code)
      if (!is.null(sea_route_geoms[[key]])) return(sea_route_geoms[[key]])
      from_base <- sub("_[SRW]$", "", from_code)
      to_base <- sub("_[SRW]$", "", to_code)
      key <- paste0(from_base, "_TO_", to_base)
      if (!is.null(sea_route_geoms[[key]])) return(sea_route_geoms[[key]])
      return(NULL)
    }

    for (scholar_name in names(routes)) {
      scholar <- routes[[scholar_name]]
      places <- unlist(scholar$places_sequence)
      regionality <- scholar$regionality
      scholar_routes <- scholar$routes

      if (length(places) < 2) next

      route_color <- get_edge_color(regionality)

      if (!is.null(scholar_routes) && length(scholar_routes) > 0) {
        for (route in scholar_routes) {
          if (!is.null(route$path) && length(route$path) >= 2) {
            for (seg_i in 1:(length(route$path) - 1)) {
              from_code <- route$path[seg_i]
              to_code <- route$path[seg_i + 1]

              gis_segment <- lookup_sea_route(from_code, to_code)

              if (!is.null(gis_segment) && !is.null(gis_segment$coordinates)) {
                seg_coords <- gis_segment$coordinates
                proxy <- proxy %>%
                  addPolylines(
                    lng = seg_coords[, 1],
                    lat = seg_coords[, 2],
                    color = "#0066CC",
                    weight = 2,
                    opacity = 0.7,
                    popup = paste0("<strong>", scholar_name, "</strong><br/>",
                                  gis_segment$from_name, " -> ", gis_segment$to_name,
                                  " (", round(gis_segment$distance_km, 0), " km coastal)"),
                    group = "routes"
                  )
                gis_sea_routes_drawn <- gis_sea_routes_drawn + 1
              } else {
                from_parsed <- parse_thurayya_coords(from_code)
                to_parsed <- parse_thurayya_coords(to_code)

                if (!is.null(from_parsed) && !is.null(to_parsed)) {
                  proxy <- proxy %>%
                    addPolylines(
                      lng = c(from_parsed$lon, to_parsed$lon),
                      lat = c(from_parsed$lat, to_parsed$lat),
                      color = route_color,
                      weight = 1.5,
                      opacity = 0.6,
                      popup = paste0("<strong>", scholar_name, "</strong><br/>",
                                    route$from, " -> ", route$to),
                      group = "routes"
                    )
                  path_routes_drawn <- path_routes_drawn + 1
                }
              }
            }
            next
          }

          from_norm <- normalize_city_name(route$from %||% "")
          to_norm <- normalize_city_name(route$to %||% "")

          from_idx <- which(coords_lookup$city_norm == from_norm)
          to_idx <- which(coords_lookup$city_norm == to_norm)

          if (length(from_idx) == 0) {
            from_idx <- which(startsWith(coords_lookup$city_norm, substr(from_norm, 1, 4)))
          }
          if (length(to_idx) == 0) {
            to_idx <- which(startsWith(coords_lookup$city_norm, substr(to_norm, 1, 4)))
          }

          if (length(from_idx) > 0 && length(to_idx) > 0) {
            proxy <- proxy %>%
              addPolylines(
                lng = c(coords_lookup$lon[from_idx[1]], coords_lookup$lon[to_idx[1]]),
                lat = c(coords_lookup$lat[from_idx[1]], coords_lookup$lat[to_idx[1]]),
                color = route_color,
                weight = 1.5,
                opacity = 0.6,
                dashArray = "5,5",
                popup = paste0("<strong>", scholar_name, "</strong><br/>",
                              route$from, " -> ", route$to, " (direct)"),
                group = "routes"
              )
            fallback_routes_drawn <- fallback_routes_drawn + 1
          }
        }
      } else {
        for (j in 1:(length(places) - 1)) {
          from_name <- places[j]
          to_name <- places[j + 1]
          from_norm <- normalize_city_name(from_name)
          to_norm <- normalize_city_name(to_name)

          from_idx <- which(coords_lookup$city_norm == from_norm)
          to_idx <- which(coords_lookup$city_norm == to_norm)

          if (length(from_idx) == 0) {
            from_idx <- which(startsWith(coords_lookup$city_norm, substr(from_norm, 1, 4)))
          }
          if (length(to_idx) == 0) {
            to_idx <- which(startsWith(coords_lookup$city_norm, substr(to_norm, 1, 4)))
          }

          if (length(from_idx) > 0 && length(to_idx) > 0) {
            proxy <- proxy %>%
              addPolylines(
                lng = c(coords_lookup$lon[from_idx[1]], coords_lookup$lon[to_idx[1]]),
                lat = c(coords_lookup$lat[from_idx[1]], coords_lookup$lat[to_idx[1]]),
                color = route_color,
                weight = 1.5,
                opacity = 0.6,
                dashArray = "5,5",
                popup = paste0("<strong>", scholar_name, "</strong><br/>",
                              from_name, " -> ", to_name, " (direct)"),
                group = "routes"
              )
            fallback_routes_drawn <- fallback_routes_drawn + 1
          }
        }
      }
    }

    message("Routes drawn: ", path_routes_drawn, " with paths, ", gis_sea_routes_drawn, " GIS coastal, ", fallback_routes_drawn, " fallback straight lines")
  })

  # Initial route display on data load
  observeEvent(rv$geo_data$scholar_routes, {
    req(rv$geo_data$scholar_routes)
    req(rv$geo_data$city_coords)

    shinyjs::delay(800, {
      routes <- rv$geo_data$scholar_routes
      coords <- rv$geo_data$city_coords
      sea_route_geoms <- rv$geo_data$sea_route_geometries

      coords_lookup <- coords
      coords_lookup$city_norm <- sapply(coords_lookup$city, normalize_city_name)

      proxy <- leafletProxy("geo_travel_map") %>%
        clearGroup("routes")

      if (length(routes) == 0) return()

      for (scholar_name in names(routes)) {
        scholar <- routes[[scholar_name]]
        places <- unlist(scholar$places_sequence)
        regionality <- scholar$regionality
        scholar_routes <- scholar$routes

        if (length(places) < 2) next

        route_color <- get_edge_color(regionality)

        if (!is.null(scholar_routes) && length(scholar_routes) > 0) {
          for (route in scholar_routes) {
            if (!is.null(route$path) && length(route$path) >= 2) {
              for (seg_i in 1:(length(route$path) - 1)) {
                from_code <- route$path[seg_i]
                to_code <- route$path[seg_i + 1]

                from_parsed <- parse_thurayya_coords(from_code)
                to_parsed <- parse_thurayya_coords(to_code)

                if (!is.null(from_parsed) && !is.null(to_parsed)) {
                  proxy <- proxy %>%
                    addPolylines(
                      lng = c(from_parsed$lon, to_parsed$lon),
                      lat = c(from_parsed$lat, to_parsed$lat),
                      color = route_color,
                      weight = 1.5,
                      opacity = 0.6,
                      popup = paste0("<strong>", scholar_name, "</strong><br/>",
                                    route$from, " -> ", route$to),
                      group = "routes"
                    )
                }
              }
            }
          }
        }
      }
      message("Initial routes drawn on map load")
    })
  }, once = TRUE, ignoreNULL = TRUE)

  # Filtered authors reactive
  geo_filtered_authors <- reactive({
    mobility_data <- geo_mobility_data()
    if (is.null(mobility_data)) return(NULL)

    authors <- mobility_data$authors

    if (!is.null(input$geo_region_filter) && input$geo_region_filter != "all") {
      reg <- input$geo_region_filter
      if (reg == "inter-regional") {
        authors <- authors[authors$inter_regional == 1 |
                          grepl("visits", authors$regionality, ignore.case = TRUE), ]
      } else if (reg == "maġrib") {
        authors <- authors[grepl("^maġrib", authors$regionality, ignore.case = TRUE), ]
      } else if (reg == "mašriq") {
        authors <- authors[grepl("^mašriq", authors$regionality, ignore.case = TRUE), ]
      }
    }

    if (!is.null(input$geo_mobility_filter) && input$geo_mobility_filter != "all") {
      authors <- authors[authors$mobility_category == input$geo_mobility_filter, ]
    }

    if (!is.null(input$geo_century_filter)) {
      authors <- authors[!is.na(authors$death_century) &
                         authors$death_century >= input$geo_century_filter[1] &
                         authors$death_century <= input$geo_century_filter[2], ]
    }

    if (!is.null(input$geo_subregions_filter)) {
      authors <- authors[!is.na(authors$subregions_visited) &
                         authors$subregions_visited >= input$geo_subregions_filter[1] &
                         authors$subregions_visited <= input$geo_subregions_filter[2], ]
    }

    authors
  })

  # Filter summary inline
  output$geo_filter_summary_inline <- renderUI({
    authors <- geo_filtered_authors()
    if (is.null(authors)) return(NULL)
    total_data <- geo_mobility_data()
    total <- if (!is.null(total_data)) nrow(total_data$authors) else 0
    HTML(paste0("Showing <strong>", nrow(authors), "</strong> of ", total, " scholars"))
  })

  # Mobility statistic cards
  output$geo_stat_sedentary <- renderUI({
    authors <- geo_filtered_authors()
    if (is.null(authors) || nrow(authors) == 0) return(div(class = "geo-stat-card", "\u2014"))
    n <- sum(authors$mobility_category == "sedentary", na.rm = TRUE)
    pct <- if (nrow(authors) > 0) round(100 * n / nrow(authors)) else 0
    div(class = "geo-stat-card", style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;",
      div(class = "geo-stat-number", style = "font-size: 2em; font-weight: bold; color: #666;", n),
      div(class = "geo-stat-label", "Sedentary"),
      p(style = "font-size: 0.85em; color: #888; margin-top: 5px;",
        paste0(pct, "% stayed in home subregion"))
    )
  })

  output$geo_stat_local <- renderUI({
    authors <- geo_filtered_authors()
    if (is.null(authors) || nrow(authors) == 0) return(div(class = "geo-stat-card", "\u2014"))
    n <- sum(authors$mobility_category == "local-traveler", na.rm = TRUE)
    pct <- if (nrow(authors) > 0) round(100 * n / nrow(authors)) else 0
    div(class = "geo-stat-card", style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;",
      div(class = "geo-stat-number", style = "font-size: 2em; font-weight: bold; color: #56B4E9;", n),
      div(class = "geo-stat-label", "Local Travelers"),
      p(style = "font-size: 0.85em; color: #888; margin-top: 5px;",
        paste0(pct, "% visited 2 subregions"))
    )
  })

  output$geo_stat_extensive <- renderUI({
    authors <- geo_filtered_authors()
    if (is.null(authors) || nrow(authors) == 0) return(div(class = "geo-stat-card", "\u2014"))
    n <- sum(authors$mobility_category == "extensive-local", na.rm = TRUE)
    pct <- if (nrow(authors) > 0) round(100 * n / nrow(authors)) else 0
    div(class = "geo-stat-card", style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;",
      div(class = "geo-stat-number", style = "font-size: 2em; font-weight: bold; color: #E69F00;", n),
      div(class = "geo-stat-label", "Extensive Local"),
      p(style = "font-size: 0.85em; color: #888; margin-top: 5px;",
        paste0(pct, "% visited 3+ subregions"))
    )
  })

  output$geo_stat_interregional <- renderUI({
    authors <- geo_filtered_authors()
    if (is.null(authors) || nrow(authors) == 0) return(div(class = "geo-stat-card", "\u2014"))
    n <- sum(authors$mobility_category == "inter-regional", na.rm = TRUE)
    pct <- if (nrow(authors) > 0) round(100 * n / nrow(authors)) else 0
    div(class = "geo-stat-card", style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px; border: 2px solid #CC79A7;",
      div(class = "geo-stat-number", style = "font-size: 2em; font-weight: bold; color: #CC79A7;", n),
      div(class = "geo-stat-label", "Inter-Regional"),
      p(style = "font-size: 0.85em; color: #888; margin-top: 5px;",
        paste0(pct, "% crossed Mashriq/Maghrib boundary"))
    )
  })

  # Key findings output
  output$geo_key_findings <- renderUI({
    mobility_data <- geo_mobility_data()
    if (is.null(mobility_data)) return(div("Loading findings..."))

    authors <- mobility_data$authors
    n_total <- nrow(authors)

    n_sedentary <- sum(authors$mobility_category == "sedentary", na.rm = TRUE)
    n_interreg <- sum(authors$inter_regional == 1, na.rm = TRUE)
    top_traveler <- authors[which.max(authors$subregions_visited), ]
    avg_subs_home <- mean(authors$subregions_visited[authors$inter_regional == 0], na.rm = TRUE)
    avg_subs_inter <- mean(authors$subregions_visited[authors$inter_regional == 1], na.rm = TRUE)
    mashriq_dist <- mean(authors$total_distance_km[authors$regionality == "mashriq"], na.rm = TRUE)
    maghrib_dist <- mean(authors$total_distance_km[authors$regionality == "maghrib"], na.rm = TRUE)

    div(
      tags$ul(style = "list-style-type: none; padding-left: 0;",
        tags$li(style = "margin-bottom: 10px;",
          icon("home", style = "color: #666;"), " ",
          tags$strong(paste0(round(100 * n_sedentary / n_total), "%")),
          " of scholars stayed in their home subregion (sedentary)"
        ),
        tags$li(style = "margin-bottom: 10px;",
          icon("exchange-alt", style = "color: #CC79A7;"), " ",
          tags$strong(n_interreg),
          " scholars crossed the Mashriq/Maghrib boundary"
        ),
        tags$li(style = "margin-bottom: 10px;",
          icon("trophy", style = "color: #E69F00;"), " ",
          "Most mobile: ", tags$strong(top_traveler$author_name),
          " (", top_traveler$subregions_visited, " subregions)"
        ),
        tags$li(style = "margin-bottom: 10px;",
          icon("chart-bar", style = "color: #56B4E9;"), " ",
          "Inter-regional scholars visited ", tags$strong(round(avg_subs_inter, 1)),
          " subregions on average vs ", tags$strong(round(avg_subs_home, 1)), " for home-region scholars"
        ),
        tags$li(style = "margin-bottom: 10px;",
          icon("road", style = "color: #009E73;"), " ",
          "Avg travel distance: Mashriq ", tags$strong(format(round(mashriq_dist), big.mark = ",")), " km",
          " | Maghrib ", tags$strong(format(round(maghrib_dist), big.mark = ",")), " km"
        )
      )
    )
  })

  # Author search match
  output$geo_author_match <- renderUI({
    search_term <- input$geo_author_search
    if (is.null(search_term) || nchar(search_term) < 2) return(NULL)

    mobility_data <- geo_mobility_data()
    if (is.null(mobility_data)) return(NULL)

    authors <- mobility_data$authors
    match_idx <- elastic_match_vec(search_term, authors$author_name, authors$author_name_arabic)
    matches <- authors[match_idx, ]

    if (nrow(matches) == 0) {
      return(div(style = "padding: 10px; color: #888;", "No matches found"))
    }

    match <- matches[1, ]
    formatted_name <- format_camel_case(match$author_name, "author")

    div(style = "padding: 10px; background: #f8f9fa; border-radius: 4px; margin-top: 5px;",
      tags$strong(formatted_name), br(),
      "Home: ", match$home_subregion, " | ",
      "Subregions: ", match$subregions_visited, " | ",
      "Category: ", match$mobility_category,
      if (!is.na(match$total_distance_km) && match$total_distance_km > 0) {
        paste0(" | Distance: ", format(round(match$total_distance_km), big.mark = ","), " km")
      },
      if (nrow(matches) > 1) {
        tags$small(style = "display: block; margin-top: 5px; color: #666;",
          paste0("+ ", nrow(matches) - 1, " more match", ifelse(nrow(matches) > 2, "es", "")))
      }
    )
  })

  # Author mobility data table
  output$geo_author_mobility_table <- renderDT({
    mobility_data <- geo_mobility_data()
    if (is.null(mobility_data)) return(NULL)

    authors <- mobility_data$authors

    if (!is.null(input$geo_author_search) && nchar(input$geo_author_search) >= 2) {
      match_idx <- elastic_match_vec(input$geo_author_search, authors$author_name, authors$author_name_arabic)
      authors <- authors[match_idx, ]
    }

    if (!is.null(input$geo_mobility_filter) && input$geo_mobility_filter != "all") {
      authors <- authors[authors$mobility_category == input$geo_mobility_filter, ]
    }

    if (!is.null(input$geo_home_subregion) && input$geo_home_subregion != "all") {
      authors <- authors[authors$home_subregion == input$geo_home_subregion, ]
    }

    if (!is.null(input$geo_century_filter)) {
      authors <- authors[!is.na(authors$death_century) &
                         authors$death_century >= input$geo_century_filter[1] &
                         authors$death_century <= input$geo_century_filter[2], ]
    }

    display_df <- authors %>%
      mutate(
        Author = sapply(author_name, function(x) format_camel_case(x, "author")),
        Century = death_century,
        Origin = sapply(regionality, function(r) {
          if (is.na(r) || r == "") return("\u2014")
          words <- strsplit(r, " ")[[1]]
          paste(sapply(words, function(w) {
            paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
          }), collapse = " ")
        }),
        Subregions = subregions_visited,
        `Distance (km)` = ifelse(is.na(total_distance_km) | total_distance_km == 0, "\u2014",
                                  format(round(total_distance_km), big.mark = ",")),
        Category = mobility_category
      ) %>%
      select(Author, Century, Origin, Subregions, `Distance (km)`, Category)

    datatable(
      display_df,
      options = list(
        pageLength = 10,
        dom = 'frtip',
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = "compact stripe",
      selection = "single"
    )
  })

  # Row selection observer
  observeEvent(input$geo_author_mobility_table_rows_selected, {
    selected_row <- input$geo_author_mobility_table_rows_selected
    if (is.null(selected_row) || length(selected_row) == 0) return()

    mobility_data <- geo_mobility_data()
    if (is.null(mobility_data)) return()

    authors <- mobility_data$authors

    if (!is.null(input$geo_mobility_filter) && input$geo_mobility_filter != "all") {
      authors <- authors[authors$mobility_category == input$geo_mobility_filter, ]
    }
    if (!is.null(input$geo_home_subregion) && input$geo_home_subregion != "all") {
      authors <- authors[authors$home_subregion == input$geo_home_subregion, ]
    }
    if (!is.null(input$geo_century_filter)) {
      authors <- authors[!is.na(authors$death_century) &
                         authors$death_century >= input$geo_century_filter[1] &
                         authors$death_century <= input$geo_century_filter[2], ]
    }

    if (selected_row <= nrow(authors)) {
      selected_author <- authors$author_name[selected_row]
      updateTextInput(session, "geo_author_search", value = selected_author)
    }
  })

  # Reset filters
  observeEvent(input$geo_reset, {
    updateSelectInput(session, "geo_mobility_filter", selected = "all")
    updateSelectInput(session, "geo_meta_region", selected = "all")
    updateSelectInput(session, "geo_home_subregion", selected = "all")
    updateSliderInput(session, "geo_century_filter", value = c(4, 7))
    updateSliderInput(session, "geo_subregions_filter", value = c(1, 7))
    updateTextInput(session, "geo_author_search", value = "")
  })

  # Filter summary
  output$geo_filter_summary <- renderUI({
    routes <- geo_filtered_routes()
    n_routes <- length(routes)
    n_total <- length(rv$geo_data$scholar_routes)

    if (n_routes == n_total) {
      div(style = "padding: 10px; margin-top: 25px;",
        tags$strong(n_routes), " scholars shown (all)"
      )
    } else {
      div(style = "padding: 10px; margin-top: 25px; color: #0072B2;",
        tags$strong(n_routes), " of ", n_total, " scholars match filters"
      )
    }
  })

  # Regional stats helper
  calculate_thurayya_stats <- function(routes, city_coords) {
    tryCatch({
      con <- dbConnect(SQLite(), DB_PATH)
      on.exit(dbDisconnect(con))

      author_places <- dbGetQuery(con, "
        SELECT ap.author_id, ap.place_name, a.death_century
        FROM author_places ap
        LEFT JOIN authors a ON ap.author_id = a.author_id
      ")

      if (nrow(author_places) == 0 || is.null(city_coords) || nrow(city_coords) == 0) {
        return(NULL)
      }

      region_lookup <- setNames(city_coords$region, sapply(city_coords$city, normalize_city_name))
      author_places$place_norm <- sapply(author_places$place_name, normalize_city_name)
      author_places$region <- region_lookup[author_places$place_norm]
      matched <- author_places[!is.na(author_places$region), ]

      if (nrow(matched) == 0) return(NULL)

      all_regions <- unique(matched$region)
      region_stats <- lapply(all_regions, function(region) {
        region_data <- matched[matched$region == region, ]
        unique_authors <- unique(region_data$author_id)
        centuries <- region_data$death_century[match(unique_authors, region_data$author_id)]
        centuries <- centuries[!is.na(centuries)]

        century_counts <- if (length(centuries) > 0) {
          table(factor(centuries, levels = 4:9))
        } else {
          table(factor(integer(0), levels = 4:9))
        }

        list(
          region = region,
          total = length(unique_authors),
          by_century = as.list(century_counts)
        )
      })
      names(region_stats) <- all_regions
      region_stats
    }, error = function(e) {
      message("Error calculating Thurayya stats: ", e$message)
      NULL
    })
  }
}
