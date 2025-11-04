read_clinic_geo <- function(df){
  
clinics_sf <- read_sf("data/ArcGIS/Output_Combined_DF_Geocoded/Combined_df_geocoded.shp") %>% # Load in the geocoded results from ArcGIS
  st_transform(crs=projcrs) %>%
  mutate(clinic_id = paste0(USER_year,USER_clini)) # Rename the clinic ID to match the clinic data
clinics_sf2 <- read_sf("data/ArcGIS-2024/Geocoded/Geocode_2024.shp") %>% # Load in the geocoded results from ArcGIS for 2024 file
  st_transform(crs=projcrs) %>%
  mutate(clinic_id = as.character(USER_clini)) # Rename the clinic ID to match the clinic data
clinics_sf <- bind_rows(clinics_sf,clinics_sf2)

clinics <- right_join(clinics_sf,df %>% mutate(clinic_id = paste0(year,clinic_id)),by="clinic_id") %>%
  select(clinic_id,X,Y.x,Status,Score,Match_type,Place_addr,StAddr,SubAddr,City.x,State,Subregion,Region,RegionAbbr,Country,CntryName,year:MWS,-City.y) %>%
  rename(lon = X,
         lat = Y.x,
         City = City.x)

return(clinics)
}

prob_link_clean_addr <- function(df){
  # 1) Normalize fields (do not drop geometry yet)
  clin_norm <- df %>%
    mutate(
      Subregion = ifelse(is.na(Subregion) | Subregion == "", "Unknown", Subregion),
      Name_Combine = str_to_lower(Name_Combine %||% ""),
      Name1 = str_to_lower(Name1 %||% ""),
      Name2 = str_to_lower(Name2 %||% ""),
      StAddr = str_to_lower(StAddr %||% ""),
      Street1 = str_to_lower(Street1 %||% ""),
      Address_Combine = str_to_lower(Address_Combine %||% ""),
      Phone1_clean = str_replace_all(Phone1 %>% coalesce(""), "[^0-9]", ""),
      URL1_clean = str_replace_all(URL1 %>% coalesce(""), "^(https?://)?(www\\.)?", "") %>% str_remove_all("/.*$"),
      Keys_clean = str_replace_all(Keys %||% "", "[^a-z0-9 ]", "")
    ) %>%
    # ensure geometry is sfc (already should be), then set CRS to WGS84 lon/lat if not set
    st_as_sf()
  
  if (is.null(st_crs(df))) st_crs(df) <- 4326L
  
  # Use s2/geodesic distance by keeping geographic coordinates for distance computations,
  # Choose an equal-area / metric CRS that is appropriate for USA-wide data: use EPSG:5070 (US National Albers)
  clin_proj <- st_transform(clin_norm, 5070)  
  return(clin_proj)
}

prob_link_pair_gen <- function(clin_proj,region){
  candidate_radius_m <- set_units(500, "m")   # candidate generation radius (lenient)
  
  # 2) Generate candidate pairs via spatial join (within candidate_radius_m)
  # Use st_is_within_distance on projected coordinates (units meters)
  # Build sparse candidate index by Subregion to reduce memory: do per-subregion
    subset_sr <- clin_proj %>% filter(Subregion == region)
    if (nrow(subset_sr) < 2) return(NULL)
    # Use st_is_within_distance to get neighbors within candidate radius
    idx_list <- st_is_within_distance(subset_sr, subset_sr, dist = as.numeric(candidate_radius_m))
    # convert idx_list to tidy pairs (i < j)
    tibble(
      i = rep(seq_along(idx_list), lengths(idx_list)),
      j = unlist(idx_list)
    ) %>%
      filter(i < j) %>%
      mutate(
        id_i = subset_sr$clinic_id[i],
        id_j = subset_sr$clinic_id[j]
      ) %>%
      select(id_i, id_j)
}

prob_link_pair_distance <- function(clin_proj,candidate_pairs){
   
  # 3) Attach attributes for both records and compute geodesic distance
  # helper to join attributes: left_join twice
  clin_attrs <- clin_proj %>% 
    st_drop_geometry() %>% 
    select(clinic_id, year, Name1, Name2, Address_Combine, StAddr, Street1, Phone1_clean, URL1_clean, Keys_clean)
  
  pairs_attrs <- candidate_pairs %>%
    left_join(clin_attrs, by = c("id_i" = "clinic_id")) %>%
    left_join(clin_attrs, by = c("id_j" = "clinic_id"), suffix = c("_i","_j"))
  
  # join geometries to compute distance (use s2/geodesic via st_distance on geographic coords)
  geom_tbl <- clin_proj %>% select(clinic_id, geometry)
  
  pairs_geom <- pairs_attrs %>%
    left_join(geom_tbl %>% st_set_geometry(NULL), by = c("id_i" = "clinic_id")) %>%
    left_join(geom_tbl %>% st_set_geometry(NULL), by = c("id_j" = "clinic_id")) 
  
  # But better compute distance using st_distance on rows; build an sf with the two geometries
  geom_join <- clin_proj %>% filter(clinic_id %in% unique(c(pairs_attrs$id_i, pairs_attrs$id_j))) %>%
    select(clinic_id, geometry)
  
  # Convert to a named list of geometries for fast lookup
  geom_lookup <- set_names(st_geometry(geom_join), geom_join$clinic_id)
  
  # compute distances (meters) using geosphere-like approach; we use st_distance on s2 (geographic)
  # Build geometry vectors for all pairs at once
  geom_i <- st_sfc(geom_lookup[pairs_geom$id_i], crs = st_crs(geom_lookup))
  geom_j <- st_sfc(geom_lookup[pairs_geom$id_j], crs = st_crs(geom_lookup))
  
  # Compute all distances in a single call
  pairs_geom <- pairs_geom %>%
    mutate(
      dist_m = as.numeric(st_distance(geom_i, geom_j, by_element = TRUE))
    )

  return(pairs_geom)
  }

prob_link_pairs <- function(pairs_geom,candidate_pairs,clin_proj){
  # 4) Compute text similarity features for the candidate pairs
  # Jaro-Winkler for names, address; token-based measures for keys
  pairs_geom <- pairs_geom %>%
    mutate(
      name1_jw = 1 - stringdist(Name1_i %||% "", Name1_j %||% "", method = "jw"),
      addr_jw  = 1 - stringdist(Address_Combine_i %||% "", Address_Combine_j %||% "", method = "jw"),
      staddr_jw = 1 - stringdist(StAddr_i %||% "", StAddr_j %||% "", method = "jw"),
      phone_exact = (Phone1_clean_i != "" & Phone1_clean_i == Phone1_clean_j),
      url_exact = (!is.na(URL1_clean_i) & URL1_clean_i != "" & URL1_clean_i == URL1_clean_j),
      keys_jw = 1 - stringdist(Keys_clean_i %||% "", Keys_clean_j %||% "", method = "jw"),
      year_gap = abs(year_i - year_j)
    )
  
  # 5) Deterministic "definite matches" rules (lock them as matches)
  # examples:
  #  - exact phone match (non-empty) => same clinic (unless distance > threshold, then treat as different)
  #  - exact URL match similarly
  #  - extremely high name+address jw (>= .98 and dist <= same_threshold) => lock
  pairs_geom <- pairs_geom %>%
    mutate(
      deterministic_match = (phone_exact | url_exact) |
        (name1_jw >= 0.98 & (addr_jw >= 0.95 | staddr_jw >= 0.95))
    )
  
  same_threshold_m <- set_units(60.96, "m")   # 200ft in meters-- clinics within this distance of each other are considered the same
  # But respect the 0.25 mi rule: if deterministic_match but dist_m > same_threshold_m, override to FALSE
  pairs_geom <- pairs_geom %>%
    mutate(
      deterministic_match = ifelse(dist_m > as.numeric(same_threshold_m), FALSE, deterministic_match)
    )
  
  # Step 1: Define rules for a confident match
  pairs_geom <- pairs_geom %>%
    mutate(
      rule_strong = phone_exact |
        (name1_jw > 0.95 & addr_jw > 0.95 & dist_m < 200 & year_gap <= 2),
      
      rule_possible = !rule_strong & (
        (name1_jw > 0.9 & addr_jw > 0.9 & dist_m < 60.96) |
          (keys_jw > 0.9 & year_gap <= 2)
      )
    )
  
  # Step 2: Assign match status
  pairs_geom <- pairs_geom %>%
    mutate(
      match_status = case_when(
        rule_strong ~ "match",
        rule_possible ~ "possible",
        TRUE ~ "non-match"
      )
    )
  
  # Step 3: one-to-one linkage (best link per id_i)
  pairs_best <- pairs_geom %>%
    filter(match_status %in% c("match","possible")) %>%
    group_by(id_i) %>%
    slice_max(order_by = name1_jw + addr_jw + keys_jw - log1p(dist_m) - year_gap, n = 1) %>%
    ungroup()      
  
  # keep only intra-year duplicates
  intra_year_dups <- pairs_geom %>%
    filter(year_i == year_j, match_status == "match") %>%
    select(id_i, id_j, year = year_i)
  
  # build equivalence classes of duplicates (using igraph)
  g_intra <- graph_from_data_frame(intra_year_dups, directed = FALSE)
  comp_intra <- components(g_intra)
  
  # map each clinic ID to a deduplicated ID
  dedup_map <- tibble(
    clinic_id = names(comp_intra$membership),
    dedup_id  = paste0("dedup_", comp_intra$membership)
  )
  
  # keep only cross-year matches
  cross_year_links <- pairs_geom %>%
    filter(year_i != year_j, match_status == "match") %>%
    select(id_i, id_j)
  
  # build equivalence classes across years
  g_cross <- graph_from_data_frame(cross_year_links, directed = FALSE)
  comp_cross <- components(g_cross)
  
  # map dedup_ids to an entity_id
  entity_map <- tibble(
    clinic_id = names(comp_cross$membership),
    entity_id = paste0("entity_", comp_cross$membership)
  )
  
  clinics_clean <- clin_proj %>%
    left_join(dedup_map, by = c("clinic_id" = "clinic_id")) %>%
    left_join(entity_map, by = c("clinic_id" = "clinic_id")) %>%
    group_by(entity_id, year) %>%
    slice(1) %>%    # keep only one row per entity/year
    ungroup()
  
  clinics_clean %>%
    rename(group = entity_id)
}

mode_coords_clinics <- function(clinic_pairs, projcrs){
  mode_coords <- clinic_pairs %>%
    group_by(group) %>% st_drop_geometry() %>%
    count(lat, sort = TRUE) %>%
    slice(1) %>% 
    select(-n) %>%
    left_join(
      clinic_pairs %>%
        group_by(group) %>% st_drop_geometry() %>%
        count(lon, sort = TRUE) %>%
        slice(1)
      , by = "group") %>%
    rename(
      lat_mode = lat,
      lon_mode = lon
    )
  
  coord_match <- clinic_pairs %>%
    st_drop_geometry() %>%
    left_join(mode_coords, by = "group") %>% select(group,lat,lon,lat_mode,lon_mode) %>% 
    rowwise() %>% 
    mutate(d = as.numeric(distm(c(lon, lat), c(lon_mode, lat_mode), fun = distHaversine))) %>%
    distinct() %>%
    group_by(group) %>% 
    mutate(d = max(d))
  
  clinics_linked <- clinic_pairs %>%
    left_join(coord_match, by = c("group","lat","lon")) %>%
    select(-c(lat,lon)) %>%
    st_drop_geometry() %>%
    rename(lat = lat_mode,
           lon = lon_mode) %>%
    st_as_sf(coords = c("lon", "lat"), crs = projcrs) %>%
    st_transform(crs = projcrs) %>%
    mutate(lon = st_coordinates(geometry)[,1],
           lat = st_coordinates(geometry)[,2],
           group = substr(group,8,length(group)))
  
  return(clinics_linked)
}
  
  