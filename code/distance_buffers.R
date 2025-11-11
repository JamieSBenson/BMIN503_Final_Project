### This collection of functions calculates pairwise distances from every clinic observation to every overdose point, and defines a set of buffers to count them within

####### 1) CREATE THE POINTS FOR ANALYSIS ###############
create_changepoint <- function(clinics_df, ods_df) {
  clinics_changepoint <- clinics_df %>%
    group_by(group) %>%
    mutate(
      first_open = min(year),
      last_open = max(year),
      open = 1,
      count = n()
    ) %>%
    ungroup() %>%
    select(
      group,
      year,
      count,
      first_open,
      last_open,
      open,
      lat,
      lon,
      d,
      geometry
    ) %>%
    distinct() %>%
    pivot_wider(
      id_cols = c(
        group,
        count,
        first_open,
        last_open,
        lat,
        lon,
        d,
        geometry
      ),
      names_from = year,
      values_from = open,
      values_fill = 0
    ) %>%
    pivot_longer(
      !c(
        group,
        count,
        first_open,
        last_open,
        lat,
        lon,
        d,
        geometry
      ),
      names_to = "year",
      values_to = "open"
    ) %>%
    arrange(group, year) %>%
    mutate(
      close = (open - 1) * -1, # For closure calculations
      period = case_when(
        year >= first_open ~ 1,
        year < first_open ~ 0
      ),
      period_closure = case_when(
        year > last_open ~ 1,
        year <= last_open ~ 0
      ),
      time_point = as.numeric(year) - first_open,
      time_point_closure = as.numeric(year) - last_open
    ) %>%
    st_as_sf()

  clinics_changepoint <- clinics_changepoint %>%
    filter(
      (year >= min(ods_df$year, na.rm = T)) &
        (year <= max(ods_df$year, na.rm = T))
    ) # Keep only the clinic observations which occur during the range of time we have OD data

  return(clinics_changepoint)
}

create_unit_points <- function(clinics_df) {
  unit_points <- clinics_df %>%
    distinct(group, lat, lon) %>%
    st_as_sf(coords = c("lon", "lat"), crs = projcrs) %>%
    st_transform(crs = projcrs) %>%
    mutate(
      lon = st_coordinates(geometry)[, 1],
      lat = st_coordinates(geometry)[, 2]
    )

  return(unit_points)
}

##### 2) CALCULATE DISTANCES AND BUFFERS FROM EACH OD TO EACH CLINIC
# Function to create spatial grid for chunking
create_spatial_chunks <- function(points, chunk_size = 50) {
  bbox <- st_bbox(points)
  n_points <- nrow(points)
  n_chunks_needed <- ceiling(n_points / chunk_size)
  grid_size <- ceiling(sqrt(n_chunks_needed))

  x_breaks <- seq(bbox[1], bbox[3], length.out = grid_size + 1)
  y_breaks <- seq(bbox[2], bbox[4], length.out = grid_size + 1)

  coords <- st_coordinates(points)
  x_idx <- findInterval(coords[, 1], x_breaks, rightmost.closed = TRUE)
  y_idx <- findInterval(coords[, 2], y_breaks, rightmost.closed = TRUE)

  points$chunk_id <- paste0(x_idx, "_", y_idx)
  return(points)
}

create_search_poly <- function(chunk, od_data, max_distance) {
  chunk_bbox <- st_bbox(chunk)
  units(max_distance) <- NULL
  # If we were not using a projected CRS, e.g. using EPSG 4269 / NAD83, we would need to calculate this in decimal degrees. I initially did this, but have changed to use EPSG:5070 for more consistent area calculations. I am keeping the code to do this in decimal degrees though, in case I need it in the future!
  # adjust xmin/xmax with longitude scaling
  # chunk_bbox["xmin"] <- chunk_bbox["xmin"] -
  #   (max_distance / 6370000) * (180 / pi) / cos(chunk_bbox["ymin"] * pi / 180)
  # chunk_bbox["xmax"] <- chunk_bbox["xmax"] +
  #   (max_distance / 6370000) * (180 / pi) / cos(chunk_bbox["ymax"] * pi / 180)
  # chunk_bbox["ymin"] <- chunk_bbox["ymin"] -
  #   (max_distance / 6370000) * (180 / pi)
  # chunk_bbox["ymax"] <- chunk_bbox["ymax"] +
  #   (max_distance / 6370000) * (180 / pi)

  chunk_bbox[c("xmin", "ymin")] <- chunk_bbox[c("xmin", "ymin")] - max_distance
  chunk_bbox[c("xmax", "ymax")] <- chunk_bbox[c("xmax", "ymax")] + max_distance

  # return as sfc polygon in desired CRS
  search_poly <- st_as_sfc(chunk_bbox) %>%
    st_transform(crs = st_crs(od_data))

  return(search_poly)
}


# Main function: calculate distances for each clinic year
process_clinic_distances <- function(
  clinic_chunk,
  nearby_ods,
  city_years,
  buffers,
  max_distance
) {
  nearby_ods <- nearby_ods %>% st_as_sf()

  if (nrow(nearby_ods) == 0) {
    # Return empty results structure
    empty_counts <- expand_grid(
      group = clinic_chunk$group,
      year = city_years,
      buffer = buffers
    ) %>%
      mutate(
        od_count = 0L
      )

    empty_distances <- expand_grid(
      group = clinic_chunk$group,
      year = city_years
    ) %>%
      mutate(
        mean_distance = NA_real_,
        median_distance = NA_real_,
        min_distance = NA_real_,
        count_within_max = 0L
      )

    return(list(counts = empty_counts, distances = empty_distances))
  }

  # Process each clinic in chunk
  results <- map(seq_len(nrow(clinic_chunk)), function(i) {
    clinic_point <- clinic_chunk[i, ]

    # Calculate distances to all nearby crimes ONCE
    search_buffer <- st_buffer(clinic_point, max_distance)
    clinic_ods <- st_filter(nearby_ods, search_buffer)

    if (nrow(clinic_ods) == 0) {
      empty_counts <- expand_grid(
        group = clinic_point$group,
        year = city_years,
        buffer = buffers
      ) %>%
        mutate(
          od_count = 0L
        )

      empty_distances <- expand_grid(
        group = clinic_point$group,
        year = city_years
      ) %>%
        mutate(
          mean_distance = NA_real_,
          median_distance = NA_real_,
          min_distance = NA_real_,
          count_within_max = 0L
        )

      return(list(counts = empty_counts, distances = empty_distances))
    }

    # ensure sf uses S2 geodesics for distance & buffer
    sf::sf_use_s2(TRUE)

    # keep everything in lon/lat WGS84
    st_crs(clinic_point) # should be EPSG:4326
    st_crs(clinic_ods) # same

    # SINGLE DISTANCE CALCULATION - this is our "source of truth"
    all_distances <- as.numeric(st_distance(clinic_point, clinic_ods)[1, ])

    # Add distances as column to crime data for easy processing
    clinic_ods$distance <- all_distances

    # Process by year to get both counts and distance stats
    year_results <- map(unique(city_years), function(yr) {
      year_ods <- clinic_ods[clinic_ods$year == yr, ]

      if (nrow(year_ods) == 0) {
        empty_counts <- data.frame(
          group = clinic_point$group,
          year = yr,
          buffer = buffers,
          od_count = 0L
        )

        empty_distances <- data.frame(
          group = clinic_point$group,
          year = yr,
          mean_distance = NA_real_,
          median_distance = NA_real_,
          min_distance = NA_real_,
          count_within_max = 0L
        )

        return(list(counts = empty_counts, distances = empty_distances))
      }

      # DERIVE BUFFER COUNTS from distances (no spatial operations!)
      buffer_counts <- map_dfr(buffers, function(buf) {
        units(buf) <- NULL
        within_buffer <- year_ods$distance <= buf &
          year_ods$distance > (buf - min(buffers)) # The second condition (buf-min(buffer)) means that we are only including crimes between the 200ft ring we are in, and not double-counting
        ods_in_buffer <- year_ods[within_buffer, ]

        data.frame(
          group = clinic_point$group,
          year = yr,
          buffer = buf,
          od_count = nrow(ods_in_buffer)
        )
      })

      # DERIVE DISTANCE STATS from the same distance data
      distance_stats <- data.frame(
        group = clinic_point$group,
        year = yr,
        mean_distance = mean(year_ods$distance, na.rm = T),
        median_distance = median(year_ods$distance, na.rm = T),
        min_distance = min(year_ods$distance, na.rm = T),
        count_within_max = length(year_ods$distance)
      )

      return(list(counts = buffer_counts, distances = distance_stats))
    })

    # Combine results for this clinic
    clinic_counts <- map_dfr(year_results, ~ .x$counts)
    clinic_distances <- map_dfr(year_results, ~ .x$distances)

    return(list(counts = clinic_counts, distances = clinic_distances))
  })

  # Combine all clinic results in this chunk
  chunk_counts <- map_dfr(results, ~ .x$counts)
  chunk_distances <- map_dfr(results, ~ .x$distances)

  out <- left_join(chunk_counts, chunk_distances, by = join_by(group, year))

  # Set units, calculate area
  out$buffer <- set_units(out$buffer, m)
  out$count_within_max <- set_units(out$count_within_max, overdoses)
  out$od_count <- set_units(out$od_count, overdoses)
  out$area <- set_units(
    ((pi * out$buffer^2)) -
      ((pi * ((out$buffer - set_units(min(buffers), m))^2))),
    km^2
  )
  out <- out %>% mutate(across(ends_with("distance"), ~ set_units(.x, m)))

  return(out)
}

#### Now, merge back with the original clinics data by year! ######
merge_distance_changepoint <- function(changept_df, distances_df, clinics_df) {
  changept_df %>%
    distinct(group, year, .keep_all = TRUE) %>% # This should already be one row per clinic per year, but just in case
    mutate(year = as.numeric(year)) %>%
    right_join(distances_df, by = c("group", "year")) %>%
    left_join(
      clinics_df %>% # This is to bring back in the clinic address, names, keys, etc, for inclusion / exclusion and mapping info
        st_drop_geometry() %>%
        distinct(group, .keep_all = TRUE) %>%
        select(
          -c(
            lat,
            lon,
            d
          )
        ),
      by = c("group", "year")
    )
}
