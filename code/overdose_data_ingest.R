## This selection of functions ingests overdose data from Cook County, IL
## Cleaned Data Source: Dr. Nesoff's lab

load_chicago_od_data <- function(){
  #token <- "ew2rEMuESuzWPqMkyPfOSGJgE"
  df <- read.socrata("https://datacatalog.cookcountyil.gov/resource/cjeq-bs86.json")
}

filter_ods <- function(max_dt,data){
  
  # Lists, obtained from Gabby's code:
  allcauselist = c("HEROIN", "COCAINE", "OPIATE", "CODEINE", "METHADONE", "MORPHINE",
                  "HYDROCODONE", "METHAMPHETAMINE", "CARFENTANIL", "U-47700", "TRAMADOL", "OXYCODONE",
                  "OXYMORPHONE", "AMPHETAMINE", "METHAMPHETAMINE", "BUPRENORPHINE", "MITRAGYNINE",
                  "3-FLUOROPHENMETRAZINE", "MDA", "MDMA", "LYSERGIC ACID DIETHYLAMIDE",
                  "LSD", "OPIOID", "METHYLENEDIOXYMETHAMPHETAMINE", "OPIUM", "HYDROMORPHONE", "MIRTAZAPINE",
                  "OXYMORPHONE", "7-AMINOCLONAZEPAM", "DELORAZEPAM", "OLANZAPINE",
                  "LORAZEPAM", "DIAZEPAM", "CLONAZEPAM", "NORDIAZEPAM", "DEXTROMETHORPHAN",
                  "ETIZOLAM", "MIDAZOLAM", "DICLAZEPAM", "TEMAZEPAM",
                  "4-ANPP", "FENTANYL", "FENTANIL", "COCAINE",
                  "METHAMPHETAMINE",
                  "AMPHETAMINE", "METHAMPHETAMINE", "METHAPHETAMINE",
                  "3-FLUOROPHENMETRAZINE", "MDA", "MDMA","ECSTASY", "LYSERGIC ACID DIETHYLAMIDE", "PHENCYCLIDINE","PCP","NARCOTIC",
                  "LSD", "METHYLENEDIOXYMETHAMPHETAMINE",
                  "7-AMINOCLONAZEPAM", "DELORAZEPAM", "BENZODIAZEPINE",
                  "LORAZEPAM", "DIAZEPAM", "CLONAZEPAM", "NORDIAZEPAM", "ALPRAZOLAM", "CITALOPRAM", "OXCARBAZEPINE",
                  "ETIZOLAM", "MIDAZOLAM", "DICLAZEPAM", "TEMAZEPAM", "XYLAZINE","KETAMINE","NITROUS OXIDE", "GAMMA-HYDROXYBUTYRIC ACID","GHB","TRAMADOL", "METHADONE",
                  "CANNABINOID")
  
  nonopioidlist = c("COCAINE", "METHAMPHETAMINE", "AMPHETAMINE", "METHAMPHETAMINE", "METHAPHETAMINE",
                   "3-FLUOROPHENMETRAZINE", "MDA", "MDMA", "LYSERGIC ACID DIETHYLAMIDE", "PHENCYCLIDINE","PCP", "NARCOTIC",
                   "LSD", "METHYLENEDIOXYMETHAMPHETAMINE",
                   "7-AMINOCLONAZEPAM", "DELORAZEPAM", "BENZODIAZEPINE", "OLANZAPINE", "MIRTAZAPINE",
                   "LORAZEPAM", "DIAZEPAM", "CLONAZEPAM", "NORDIAZEPAM", "ALPRAZOLAM", "CITALOPRAM", "OXCARBAZEPINE",
                   "ETIZOLAM", "MIDAZOLAM", "DICLAZEPAM", "TEMAZEPAM", "CLOMIPRAMINE","ETHANOL")
  
  #other opioid not on previous lists
  other_opioid = c("Vicodin", "Oxycontin", "Dilaudid", "nitazine")
  
  # cause keyword
  keywords = c("DRUG","OPIOD")
  
  # Unified opioid list
  opioidlist <- c(allcauselist[!allcauselist %in% nonopioidlist], other_opioid)
  all_keywords <- c(opioidlist, allcauselist, keywords) # In the python code, this was used initially, then later in the code prior to saving the dataset, opioidlist was used as the final filter. I will skip this and use opioidlist for now.

  data$opioid_related <- grepl(paste0(opioidlist,collapse = "|"), data$primarycause)
    # table(data$opioids,data$opioid_related)
     # FALSE  TRUE
     # FALSE 76359    39
     # TRUE    532 14528
  
  data <- data %>%
    filter(death_date < as.POSIXct(max_dt) & 
             death_date > as.POSIXct("2014-01-01 00:00:00") # Data only available after 2014 reliably
    ) %>%
    mutate(
      drop = case_when(
          incident_date > Sys.time() | death_date > Sys.time() ~ "Incorrect Date", # Some dates were coded incorrectly as occurring in the future
          is.na(latitude) | is.na(longitude) ~ "Missing lat/lon",
          manner != "ACCIDENT" ~ "Non-Accidental Death",
          opioid_related == FALSE & opioids == FALSE ~ "Non-Opioid Related",
          .default = NA
      ))
  
  # table(data$drop)
  # Incorrect Date      Missing lat/lon Non-Accidental Death   Non-Opioid Related 
  # 5                10837                51012                10030 
  
  data %>%
    filter(is.na(drop)) %>%
    reframe(
      objectid = objectid,
      casenumber = casenumber,
      incident_date = incident_date,
      death_date = death_date,
      chi_ward = chi_ward,
      chi_commarea = chi_commarea,
      incident_street = incident_street,
      incident_city = incident_city,
      incident_zip = incident_zip,
      longitude = longitude,
      latitude = latitude,
      residence_city = residence_city,
      residence_zip = residence_zip,
      age = as.numeric(age),
      sex = factor(ifelse(gender %in% c("Male","Female"),gender,NA), 
                   levels = c("Male","Female"), 
                   labels = c("Male","Female")),
      race = factor(ifelse(race %in% c("Other","Unknown"),"Other",race),
                    levels = c("White","Black","Asian","Am. Indian","Other"),
                    labels = c("White","Black","Asian","AIAN","Other")),
      latino = as.logical(latino),
      cold_related = as.logical(cold_related),
      heat_related = as.logical(heat_related),
      gun_related = as.logical(gunrelated),
      covid_related = as.logical(covid_related),
      opioid_related = TRUE,
      primarycause = primarycause,
      primarycause_linea = primarycause_linea,
      primarycause_lineb = primarycause_lineb,
      primarycause_linec = primarycause_linec,
      secondarycause = secondarycause
    ) %>%
      st_as_sf(
      coords = c("longitude","latitude"),
      crs = projcrs
    )
}