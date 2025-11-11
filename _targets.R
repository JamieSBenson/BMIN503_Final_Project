# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # for tar_map / tar_file helpers
tar_option_set(
  packages = c(
    "tidyverse",
    "pdftools",
    "tm",
    "skimr",
    "readr",
    "readxl",
    "data.table",
    "readxl",
    "kableExtra",
    "stringr",
    "foreach",
    "doParallel",
    "ranger",
    "purrr",
    "dplyr",
    "stringr",
    "sf",
    "reclin2",
    "dbscan",
    "stringdist",
    "geosphere",
    "units",
    "igraph",
    "RSocrata",
    "mapview",
    "leaflet",
    "tmap",
    "ggthemes",
    "tidycensus",
    "units",
    "panelView",
    "easystats",
    "ggsci",
    "car",
    "fixest",
    "spdep",
    "spatstat",
    "ggridges",
    "did"
  ),
  format = "qs", # Optionally set the default storage format. qs is fast.
)

# # Set target options:
# tar_option_set(
#   packages = c("tidyverse","pdftools","tm","skimr","readr","readxl","kableExtra","stringr","foreach","doParallel","ranger") # packages that your targets use
#
#  # format = "qs", # Optionally set the default storage format. qs is fast.
#   #
#   # Pipelines that take a long time to run may benefit from
#   # optional distributed computing. To use this capability
#   # in tar_make(), supply a {crew} controller
#   # as discussed at https://books.ropensci.org/targets/crew.html.
#   # Choose a controller that suits your needs. For example, the following
#   # sets a controller that scales up to a maximum of two workers
#   # which run as local R processes. Each worker launches when there is work
#   # to do and exits if 60 seconds pass with no tasks to run.
#   #
#   #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
#   #
#   # Alternatively, if you want workers to run on a high-performance computing
#   # cluster, select a controller from the {crew.cluster} package.
#   # For the cloud, see plugin packages like {crew.aws.batch}.
#   # The following example is a controller for Sun Grid Engine (SGE).
#   #
#   #   controller = crew.cluster::crew_controller_sge(
#   #     # Number of workers that the pipeline can scale up to:
#   #     workers = 10,
#   #     # It is recommended to set an idle time so workers can shut themselves
#   #     # down if they are not running tasks.
#   #     seconds_idle = 120,
#   #     # Many clusters install R as an environment module, and you can load it
#   #     # with the script_lines argument. To select a specific verison of R,
#   #     # you may need to include a version string, e.g. "module load R/4.3.2".
#   #     # Check with your system administrator if you are unsure.
#   #     script_lines = "module load R"
#   #   )
#   #
#   # Set other options as needed.
# )

# Configure parallelization
options(clustermq.scheduler = "multicore")

# Configure backend of tar_make_clustermq()
future::plan(future.callr::callr)

# Load the R scripts in the R/ folder with custom functions:
tar_source(list.files("code", full.names = TRUE))

# Set project coordinate reference system
projcrs <- "EPSG:5070" # Choosing USGS Contiguous US Albers Equal Area, as we are doing density-based calculations and will want to preserve area comparisons
#### Given that we are focusing on Chicago, it may also be appropriate to project to something specific to Cook County, IL, like EPSG:3435 (NAD83 / Illinois East (ftUS))

units::install_unit("overdoses")

# Census API key
#### Located in R environment: readRenviron("~/.Renviron")
# census_api_key("REDACTED")

# Project color palette
# Color Palette
cb_palette <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#999999", # grey

  # extended set (similar luminance/saturation)
  "#117733", # dark green
  "#882255", # wine
  "#44AA99", # teal
  "#332288", # indigo
  "#DDCC77", # sand
  "#AA4499", # magenta
  "#88CCEE", # light blue
  "#661100" # dark brown
)

# Targets function list
list(
  ####################### SECTION 3: CITY / BLOCK GROUP GEOGRAPHY ################

  ### County Geography
  tar_target(
    counties_geo,
    load_sf(
      path = "data/counties/c_10se24.shp",
      crs = projcrs
    )
  ),
  tar_target(
    cook_county,
    filter_county(df = counties_geo, countyname = "Cook", stateabbrev = "IL")
  ),

  ### ACS Variables (block group)
  tar_target(
    acs_mi,
    load_acs(
      vintage = 2023,
      countyname = "Cook",
      stateabbrev = "IL"
    )
  ),
  tar_target(
    acs_mi_nds,
    calc_nds(
      acs_df = acs_mi
    )
  ),

  ### Block Group Geography
  tar_target(
    bg_poly,
    command = get_acs(
      geography = "cbg",
      state = "IL",
      county = "Cook",
      year = 2023,
      geometry = TRUE,
      variables = c(pop = "B01001_001")
    ) %>%
      st_transform(crs = projcrs)
  ),
  tar_target(
    bg_cenpop,
    load_sf(
      path = "data/NHGIS_Block_Group_Centroids/2020/US_blck_grp_cenpop_2020.shp",
      crs = projcrs
    )
  ),
  tar_target(
    mi_cenpop,
    command = st_intersection(bg_cenpop, cook_county) # Filter the block group pop. centroids to only those contained in the block group polygons for Cook County
  ),

  ######################### SECTION 1: CLINIC DATA ############################
  #### City Data
  tar_target(
    city_names,
    load_city_names(file = "data/us_cities_states_counties.csv")
  ),
  #### Facility Keys
  tar_target(
    keys,
    import_keys(directory = "data/dtc_keys")
  ),
  tar_target(
    keys_crosswalk,
    read_csv("data/working_data/keys_crosswalk.csv")
  ),
  #### Ingest Clinic PDF & CSV Data, flatten to single cleaned df
  tar_target(
    years,
    c(2005:2023)
  ),
  tar_target(
    pdf_df,
    extract_pdfs(years),
    pattern = map(years)
  ),
  tar_target(
    pdf_split,
    split_pdf(pdf_df),
    pattern = map(pdf_df)
  ),
  tar_target(
    combined_clinics_tagged,
    tag_clinics(pdf_split, keys, cities = city_names)
  ),
  tar_target(
    clinics_grouped,
    group_clinics(df = combined_clinics_tagged, cities = city_names)
  ),
  tar_target(
    csv_df,
    csv_clinics()
  ),
  tar_target(
    csv_pdf_merged,
    all_clinics_merge(
      pdf_merged = clinics_grouped,
      csv_merged = csv_df
    )
  ),
  tar_target(
    clinics_keyed,
    assign_keys(df = csv_pdf_merged)
  ),
  tar_target(
    clinic_year_summary,
    clinic_years_summary(df = clinics_keyed)
  ),

  #### Save Cleaned, Keyed df to CSV for Geocoding in ArcGIS
  tar_target(
    write_arc_geocode_df,
    write_csv(
      clinics_keyed %>%
        select(year:Zip) %>%
        mutate(Name_Combine = gsub("[^[:alnum:] ]", " ", Name_Combine)), # Clean non-alphanumerics so Arc doesn't complain
      "data/working_data/combined_df_09-03_for_geocode.csv"
    )
  ),

  #### Import Geocoded Clinic Data from ArcGIS
  tar_target(
    clinics_geo,
    read_clinic_geo(df = clinics_keyed)
  ),

  #### Probabalistically Match Clinics Across Years
  tar_target(
    clin_proj,
    prob_link_clean_addr(df = clinics_geo)
  ),
  tar_target(
    clinic_counties,
    unique(clin_proj$Subregion)
  ),
  tar_target(
    candidate_pairs,
    prob_link_pair_gen(clin_proj = clin_proj, region = clinic_counties),
    pattern = map(clinic_counties),
    memory = "transient"
  ),
  tar_target(
    pairs_geom,
    prob_link_pair_distance(
      clin_proj = clin_proj,
      candidate_pairs = candidate_pairs
    )
  ),
  tar_target(
    clinic_pairs_linked,
    prob_link_pairs(
      pairs_geom = pairs_geom,
      clin_proj = clin_proj,
      candidate_pairs = candidate_pairs
    )
  ),
  #### Assign Clinics the most common location across years for their clinic ID
  ##### where there is minor geocoding difference (<80m)
  tar_target(
    clinics_linked,
    mode_coords_clinics(clinic_pairs = clinic_pairs_linked, projcrs)
  ),
  #### Plot Clinic Availability per City
  tar_target(
    plot_clinic_year,
    plot_city_clinic_by_year(
      clinics = clinics_linked,
      selected_city = "Chicago, Illinois"
    )
  ),
  tar_target(
    plot_clinic_year_matrix,
    plot_city_clinic_avail_year(
      clinics = clinics_linked,
      selected_city = "Chicago, Illinois"
    )
  ),

  ##### Clinics in our sample (Just cook county) #####
  tar_target(
    clinics_sample,
    command = st_intersection(clinics_linked, cook_county)
  ),

  ######################### SECTION 2: OVERDOSE DATA ############################
  ### Data Ingest
  # tar_files(
  #   od_files,
  #   command = list.files(path = "data/overdose", pattern = "chicago", full.names = TRUE)
  # ),
  # tar_target(
  #   od_df,
  #   command = read_xlsx(od_files,
  #                       col_types = c("text","text","date","numeric","text","text",
  #                                     "logical",rep("text",6),rep("logical",4),
  #                                     "numeric","text","text","text","numeric",
  #                                     "numeric","text","text","text","text",
  #                                     "numeric","text","logical",
  #                                     rep("numeric",19))),
  #   pattern = map(od_files),
  #   format = "feather"
  # ),
  # tar_target(
  #   ods_geo,
  #   clean_geo_ods(od_df)
  # )
  tar_target(
    ods_pull,
    load_chicago_od_data()
  ),
  tar_target(
    ods_geo,
    filter_ods(
      max_dt = "2025-01-01 00:00:00", # Only pre-2025 deaths
      data = ods_pull
    )
  ),
  tar_target(
    od_years, # Years for which we have OD data
    command = c(min(ods_geo$year, na.rm = T):max(ods_geo$year, na.rm = T))
  ),
  tar_target(
    basemap,
    leaflet_basemap()
  ),
  tar_target(
    ods_interactive_map,
    leaflet_map_ods(data = ods_geo, basemap = basemap)
  ),

  ####################### Data Quality Check: Map All Layers ############
  tar_target(
    all_layer_map,
    tm_shape(cook_county) +
      tm_polygons() +
      tm_shape(
        bg_poly %>%
          left_join(acs_mi_nds, by = "GEOID")
      ) +
      tm_polygons(fill = "ice", midpoint = 0) +
      tm_shape(mi_cenpop) +
      tm_dots(
        fill = "blue",
        fill_alpha = 0.2
      ) +
      tm_shape(ods_geo) +
      tm_dots(
        fill = "red",
        fill_alpha = 0.2
      ) +
      tm_shape(clinics_sample) +
      tm_dots(
        fill = "yellow",
        fill_alpha = 0.2
      )
  ),

  ####################### SECTION 4: CLINIC-OD DISTANCES #########################

  #### Define buffer ring sizes
  tar_target(
    buffers,
    seq(60.96, 804.67, 60.96) # 1,400ft (426.72m) in 200ft (60.96m) increments
  ),
  tar_target(
    clinics_changepoint,
    create_changepoint(
      clinics_df = clinics_sample,
      ods_df = ods_geo
    )
  ),
  tar_target(
    unit_points,
    create_unit_points(
      clinics_df = clinics_changepoint
    )
  ),
  tar_target(
    clinic_chunks,
    create_spatial_chunks(
      points = unit_points,
      chunk_size = 50
    ) %>%
      group_by(chunk_id) %>%
      tar_group(),
    iteration = "group"
  ),
  tar_target(
    chunk_search_poly,
    create_search_poly(
      chunk = clinic_chunks,
      od_data = ods_geo,
      max_distance = max(buffers)
    ),
    pattern = map(clinic_chunks)
  ),
  tar_target(
    nearby_ods, # Pre-filter crimes to search area for entire chunk
    command = st_filter(ods_geo, chunk_search_poly),
    pattern = map(clinic_chunks, chunk_search_poly)
  ),
  tar_target(
    chunk_distances,
    process_clinic_distances(
      clinic_chunk = clinic_chunks,
      nearby_ods = nearby_ods %>% st_as_sf(),
      city_years = od_years,
      buffers = buffers,
      max_distance = max(buffers)
    ),
    pattern = map(clinic_chunks, nearby_ods)
  ),
  tar_target(
    clinics_changepoint_count,
    merge_distance_changepoint(
      changept_df = clinics_changepoint,
      distances_df = chunk_distances,
      clinics_df = clinics_sample
    )
  ),

  ####################### SECTION 5: DIFF-IN-DIFF PREP #########################
  tar_target(
    clinics_open,
    command = clinics_changepoint_count %>%
      filter(count >= 3) %>% # Keep clinics where we have >2 years of data observed (AKA we are reasonably sure it is a legit clinic, and we have enough time to observe effects)
      #filter(
      #((MHSAF | SA | MH.SA | SACA | SAE | SAF | SSA) == 1) & Score >= 95
      #) %>% # Consited keeping only clinics which provide substance abuse treatment, and which were geocoded with >=95% accuracy
      filter(year <= last_open) %>% # Censor clinics after we stop observing them as "open", and they cannot then be re-treated as control units
      mutate(group = as.numeric(group))
  ),

  tar_target(
    selected_buffer, # Given that most ods occurs within 550m buffer, we will use the 548.64m buffer as the "representative" buffer
    command = set_units(548.64, m)
  ),

  ####################### SECTION 6: DIFF-IN-DIFF #########################
  tar_target(
    att_open_buffer, # buffer based treatment effect estimate
    estimate_diff(
      buffer_size = buffers,
      outcome = "od_count",
      cluster = c("group"),
      density = T,
      change_pt = "first_open",
      df = clinics_open
    ),
    pattern = map(buffers)
  ),
  tar_target(
    att_open_dist, # ditance based treatment effect estimate
    estimate_diff(
      buffer_size = selected_buffer,
      outcome = "median_distance",
      cluster = c("group"),
      density = F,
      change_pt = "first_open",
      df = clinics_open
    )
  ),
  ################### SECTION 7: MAPS! #################
  tar_target(
    od_rate_map,
    map_od_rate_bg(blocks = bg_poly, ods = ods_geo)
  )
)
