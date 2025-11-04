# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # for tar_map / tar_file helpers
tar_option_set(
  packages = c(
    "tidyverse","pdftools","tm","skimr","readr","readxl","data.table",
    "kableExtra","stringr","foreach","doParallel","ranger","purrr","dplyr",
    "stringr","sf","reclin2","dbscan","stringdist","geosphere","units","igraph"
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
# tar_source("other_functions.R") # Source other scripts as needed.

# # Replace the target list below with your own:
# list(
#   tar_target(
#     name = pdfs,
#     command = extract_pdfs(directory = "data/directory/", years = c(2005)),
#     #format = "file",
#     description = "Extracts data from SAMSHA clinic directory PDFs by year" # requires development targets >= 1.5.0.9001: remotes::install_github("ropensci/targets")
#   )
# )
# 
# 
# 
# tar_source(list.files("code", full.names = TRUE))

# the years you want to process
#years <- c(2005)   # can be c(2005, 2006, 2007, ...)
# 
# list(
#   tar_files_input(
#     name = pdfs,
#     files = file.path("data/directory", sprintf("directory_%d.pdf", c(2005:2023)))
#   #  , batches = 2
#   ),
#   
# )

projcrs <- "EPSG:4326"

# values = data.frame(
#  year = c(2005:2023)
# )
# 
# # Map over years, creating separate data frames for each clinic-year
# mapped <- tar_map(
#   values = values,
#   tar_target(
#     pdf_df,
#     extract_pdfs(year),
#   ),
#   tar_target(
#     pdf_split,
#     split_pdf(pdf_df)
#   ),
#   tar_target(
#     clinic_tag,
#     tag_clinics(pdf_split,
#                 keys,
#                 cities=city_names)
#   )
# )

list(
  #### City Data
  tar_target(
    city_names,
    load_city_names(file="Data/us_cities_states_counties.csv")
  ),
  #### Facility Keys
  tar_target(
    keys,
    import_keys(directory="data/dtc_keys")
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
    tag_clinics(pdf_split,
                keys,
                cities=city_names)
  ),
  tar_target(
      clinics_grouped,
      group_clinics(df = combined_clinics_tagged, 
                    cities = city_names)
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
    write_csv(clinics_keyed %>% select(year:Zip) %>% 
                mutate(Name_Combine = gsub("[^[:alnum:] ]", " ", Name_Combine)), # Clean non-alphanumerics so Arc doesn't complain
              "data/working_data/combined_df_09-03_for_geocode.csv")
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
    counties,
    unique(clin_proj$Subregion)
  ),
  tar_target(
    candidate_pairs,
    prob_link_pair_gen(clin_proj = clin_proj,
                       region = counties),
    pattern = map(counties),
   # iteration = "list",
    memory = "transient"
  ),
  tar_target(
      pairs_geom,
      prob_link_pair_distance(clin_proj = clin_proj,
                              candidate_pairs = candidate_pairs)
  ),
  tar_target(
    clinic_pairs_linked,
    prob_link_pairs(pairs_geom = pairs_geom,
                    clin_proj = clin_proj,
                    candidate_pairs = candidate_pairs)
  ),
#### Assign Clinics the most common location across years for their clinic ID where there is minor geocoding difference (<80m)
  tar_target(
      clinics_linked,
      mode_coords_clinics(clinic_pairs = clinic_pairs_linked, projcrs)
  ),
#### Plot Clinic Availability per City
  tar_target(
    plot_clinic_year,
    plot_city_clinic_by_year(clinics = clinics_linked,
                             selected_city = "Chicago, Illinois")
  ),
  tar_target(
    plot_clinic_year_matrix,
    plot_city_clinic_avail_year(clinics = clinics_linked,
                             selected_city = "Chicago, Illinois")
)
)
