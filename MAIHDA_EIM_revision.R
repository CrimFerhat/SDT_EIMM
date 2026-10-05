###############################################################################
# Rethinking Social Disorganisation Theory through an Intersectionality Lens: 
# An Eco-Intersectional Multilevel Analysis of Neighbourhood Crime
# replication script for the BJC revision
#
# HOW TO RUN
#   Open Revision_2026.Rproj in RStudio (this sets the working directory to this
#   folder) and run the whole script. Or, from a terminal in this folder:
#   Rscript MAIHDA_EIM_revision.R
#
#   All data are downloaded automatically the first time (about 30-45 minutes).
#   Downloaded files are kept in data_raw/ and are not downloaded again.
#
# FOLDERS
#   data_raw/      downloaded data exactly as received (+ download_log.csv)
#   data_derived/  crime counts by LSOA, merged datasets, codebook
#   outputs/       tables and figures, named after the paper (see below)
#
# OUTPUTS (outputs/ folder)                        SECTION OF THIS SCRIPT
#   Table 1   Table1_descriptive_statistics         6
#   Table 2   (stratum identifier key, no data)     5 (strata variable)
#   Table 3   Table3_model_results                   7
#   Figure 1  Figure1.png                            9
#   Figure 2  Figure2.png                            9
#   Table S1  TableS1_sample_flow, TableS1_crime_assignment      2, 5
#   Table S2  TableS2_stratum_sizes, TableS2_departures_by_size, TableS2_quartile_strata  8
#   Table S3  TableS3_panelA_..., TableS3_panelB_...             8
#   Table S4  TableS4_all_strata                     8
#   Table S5  TableS5_violence_models                10
#   Table S6  TableS6_correlations                   6
#   Table S7  TableS7_robustness                     11
#   Table S8  TableS8_collinearity                   12
#
# Outcome in the main analysis: all crime (excluding anti-social behaviour).
# Violence is reported in the supplementary material (Table S5).
###############################################################################


##############################
# Packages
##############################

packages <- c("tidyverse", "sf", "httr", "jsonlite", "lme4", "lmerTest", "merTools",
              "sjPlot", "modelsummary", "patchwork", "performance")
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages, repos = "https://cloud.r-project.org")

library(tidyverse)
library(sf)
library(httr)
library(jsonlite)
library(lme4)
library(lmerTest)
library(merTools)
library(sjPlot)
library(modelsummary)
library(patchwork)
library(performance)

select <- dplyr::select   # merTools loads the MASS package, which has its own select()

options(timeout = 3600,                # allow large downloads 
        readr.show_col_types = FALSE)  # keep the console tidy

# Word tables need pandoc, which comes with RStudio. 
if (!rmarkdown::pandoc_available() && dir.exists("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")) {
  Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
}

dir.create("data_raw/census", recursive = TRUE, showWarnings = FALSE)
dir.create("data_derived", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)


###############################################################################
# 1. DOWNLOAD THE DATA (each file is downloaded only if it is not already there)
###############################################################################

ons_api <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/"

##############################
# 1a. Census 2021 tables (counts, LSOA level, from Nomis)
##############################

census_tables <- c("ts001",   # residence type (households / communal establishments)
                   "ts003",   # household composition (lone parents)
                   "ts004",   # country of birth
                   "ts006",   # population density
                   "ts007a",  # age
                   "ts011",   # household deprivation
                   "ts016",   # length of residence in the UK
                   "ts019",   # migrant indicator (address one year ago)
                   "ts021",   # ethnic group
                   "ts066")   # economic activity (students)

for (table in census_tables) {
  census_file <- paste0("data_raw/census/census2021-", table, "-lsoa.csv")
  if (!file.exists(census_file)) {
    download.file(paste0("https://www.nomisweb.co.uk/output/census/2021/census2021-", table, ".zip"),
                  "temp.zip", mode = "wb")
    unzip("temp.zip", files = basename(census_file), exdir = "data_raw/census")
    file.remove("temp.zip")
  }
}

##############################
# 1b. Lookups: LSOA -> local authority -> region, and local authority -> police force area
##############################

if (!file.exists("data_raw/lookup_LSOA21_LAD22_RGN22.csv")) {
  pages <- list()
  for (offset in seq(0, 35000, by = 1000)) {    # the ONS service returns 1,000 rows at a time
    url <- paste0(ons_api, "LSOA21_BUA22_LAD22_RGN22_EW_LU_v2/FeatureServer/0/query?where=1%3D1",
                  "&outFields=LSOA21CD,LSOA21NM,LAD22CD,LAD22NM,RGN22CD,RGN22NM",
                  "&orderByFields=LSOA21CD&resultOffset=", offset, "&resultRecordCount=1000&f=json")
    pages[[length(pages) + 1]] <- fromJSON(url)$features$attributes
  }
  bind_rows(pages) %>% distinct() %>% write_csv("data_raw/lookup_LSOA21_LAD22_RGN22.csv")
}

if (!file.exists("data_raw/lookup_LAD22_CSP22_PFA22.csv")) {
  url <- paste0(ons_api, "LAD22_CSP22_PFA22_EW_LU/FeatureServer/0/query?where=1%3D1&outFields=*&f=json")
  fromJSON(url)$features$attributes %>% rename_with(toupper) %>% write_csv("data_raw/lookup_LAD22_CSP22_PFA22.csv")
}

##############################
# 1b2. Census 2021 ethnic group (8 categories) by country of birth (born in the UK / outside the UK), LSOA level
# (no bulk file exists for this cross-tabulation; it is downloaded from the ONS Census 2021 API, 400 LSOAs at a time)
# The API does not return this table for 143 LSOAs (it returns an error for them; 142 of these are in
# the analysed sample), so they are left out of the robustness check that uses it (Supplementary Table S7).
##############################

if (!file.exists("data_raw/census/ethnicity_by_birthplace_lsoa.csv")) {
  lsoa_codes <- read_csv("data_raw/lookup_LSOA21_LAD22_RGN22.csv")$LSOA21CD
  batches <- split(lsoa_codes, ceiling(seq_along(lsoa_codes) / 400))
  results <- list()
  for (batch in batches) {
    url <- paste0("https://api.beta.ons.gov.uk/v1/population-types/UR/census-observations?area-type=lsoa,",
                  paste(batch, collapse = ","), "&dimensions=ethnic_group_tb_8a,country_of_birth_3a")
    observations <- GET(url) %>% content(as = "text", encoding = "UTF-8") %>% fromJSON() %>% pluck("observations")
    results[[length(results) + 1]] <- tibble(
      LSOA_code  = map_chr(observations$dimensions, ~ .x$option_id[1]),
      ethnicity  = map_chr(observations$dimensions, ~ .x$option[2]),
      birthplace = map_chr(observations$dimensions, ~ .x$option[3]),
      count      = observations$observation)
  }
  bind_rows(results) %>% write_csv("data_raw/census/ethnicity_by_birthplace_lsoa.csv")
}

##############################
# 1c. LSOA 2021 boundaries (full resolution) - used to place crimes and businesses in LSOAs
##############################

if (!file.exists("data_raw/LSOA_Dec2021_BFC_V10.gpkg")) {
  pages <- list()
  for (offset in seq(0, 35000, by = 1000)) {
    url <- paste0(ons_api, "Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BFC_V10/FeatureServer/0/query",
                  "?where=1%3D1&outFields=LSOA21CD&outSR=27700&orderByFields=LSOA21CD",
                  "&resultOffset=", offset, "&resultRecordCount=1000&f=geojson")
    download.file(url, "temp.geojson", mode = "wb", quiet = TRUE)
    page <- st_read("temp.geojson", quiet = TRUE)
    while (nrow(page) == 0) {                       
      download.file(url, "temp.geojson", mode = "wb", quiet = TRUE)
      page <- st_read("temp.geojson", quiet = TRUE)
    }
    pages[[length(pages) + 1]] <- page
  }
  file.remove("temp.geojson")
  do.call(rbind, pages) %>% st_set_crs(27700) %>% st_make_valid() %>% st_write("data_raw/LSOA_Dec2021_BFC_V10.gpkg")
}

##############################
# 1d. Food businesses (Food Standards Agency, Food Hygiene Rating Scheme API)
##############################

if (!file.exists("data_raw/fsa_establishments.csv")) {
  business_types <- c("7843" = "Pub/bar/nightclub",
                      "1"    = "Restaurant/Cafe/Canteen",
                      "7844" = "Takeaway/sandwich shop",
                      "4613" = "Retailers - other",
                      "7840" = "Retailers - supermarkets/hypermarkets",
                      "7842" = "Hotel/bed & breakfast/guest house")
  pages <- list()
  for (type_id in names(business_types)) {
    page_number <- 1
    repeat {
      url <- paste0("https://api.ratings.food.gov.uk/Establishments?businessTypeId=", type_id,
                    "&pageSize=5000&pageNumber=", page_number, "&sortOptionKey=Alpha")
      response <- GET(url, add_headers("x-api-version" = "2")) %>%
        content(as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)
      pages[[length(pages) + 1]] <- response$establishments %>%
        transmute(FHRSID, BusinessName, BusinessType = business_types[type_id], LocalAuthorityName, PostCode,
                  Longitude = as.numeric(geocode.longitude), Latitude = as.numeric(geocode.latitude))
      if (page_number >= response$meta$totalPages) break
      page_number <- page_number + 1
    }
  }
  bind_rows(pages) %>% distinct(FHRSID, .keep_all = TRUE) %>% write_csv("data_raw/fsa_establishments.csv")
}

# Businesses without coordinates: look up their postcode (postcodes.io, based on the ONS Postcode Directory)
if (!file.exists("data_raw/fsa_postcode_centroids.csv")) {
  postcodes <- read_csv("data_raw/fsa_establishments.csv") %>%
    filter(is.na(Longitude) | Longitude == 0) %>%
    mutate(PostCode = str_replace(toupper(str_trim(PostCode)), "^(.*?)\\s*([0-9][A-Z]{2})$", "\\1 \\2")) %>%  # standard "AB1 2CD" format
    filter(str_detect(PostCode, "^[A-Z]{1,2}[0-9][0-9A-Z]? [0-9][A-Z]{2}$")) %>%
    distinct(PostCode) %>%
    pull(PostCode)
  results <- list()
  for (start in seq(1, length(postcodes), by = 100)) {          # the service takes 100 postcodes at a time
    batch <- postcodes[start:min(start + 99, length(postcodes))]
    response <- POST("https://api.postcodes.io/postcodes?filter=postcode,latitude,longitude",
                     body = list(postcodes = batch), encode = "json") %>%
      content(as = "text", encoding = "UTF-8") %>% fromJSON()
    if (is.data.frame(response$result$result)) {
      results[[length(results) + 1]] <- tibble(PostCode = response$result$query,
                                               Latitude = response$result$result$latitude,
                                               Longitude = response$result$result$longitude)
    }
  }
  bind_rows(results) %>% filter(!is.na(Latitude)) %>% distinct(PostCode, .keep_all = TRUE) %>%
    write_csv("data_raw/fsa_postcode_centroids.csv")
}

##############################
# 1e. Rail and metro stations (Department for Transport, NaPTAN)
##############################

if (!file.exists("data_raw/naptan_access_nodes.csv")) {
  download.file("https://naptan.api.dft.gov.uk/v1/access-nodes?dataFormat=csv",
                "data_raw/naptan_access_nodes.csv", mode = "wb")
}

##############################
# 1f. Police-recorded crime, January 2021 - December 2023 (police.uk bulk archive)
# The police.uk API only serves the most recent 36 months, so we use the official archive.
##############################

if (length(list.files("data_raw/police", pattern = "-street.csv$", recursive = TRUE)) != 1564) {
  download.file("https://data.police.uk/data/archive/2023-12.zip", "police_archive.zip", mode = "wb")
  archive_files <- unzip("police_archive.zip", list = TRUE)$Name
  street_files  <- archive_files[str_detect(archive_files, "^202[123]-.*-street.csv$")]   # street-level crime only
  unzip("police_archive.zip", files = street_files, exdir = "data_raw/police")
  file.remove("police_archive.zip")
}

##############################
# 1g. Record what was downloaded (file list with sizes and md5 checksums)
##############################

if (!file.exists("data_raw/police_files_manifest.csv")) {
  police_files <- list.files("data_raw/police", pattern = "-street.csv$", recursive = TRUE, full.names = TRUE)
  tibble(file = police_files, bytes = file.size(police_files), md5 = tools::md5sum(police_files)) %>%
    write_csv("data_raw/police_files_manifest.csv")
}

if (!file.exists("data_raw/download_log.csv")) {
  tibble(file = paste0("census/census2021-", census_tables, "-lsoa.csv"),
         source = paste("ONS Census 2021", toupper(census_tables), "(Nomis bulk file, LSOA level)"),
         url = paste0("https://www.nomisweb.co.uk/output/census/2021/census2021-", census_tables, ".zip")) %>%
    add_row(file = "census/ethnicity_by_birthplace_lsoa.csv",
            source = "ONS Census 2021 ethnic group (8 categories) by country of birth (UK / outside UK), LSOA level (ONS API)",
            url = "https://api.beta.ons.gov.uk/v1/population-types/UR/census-observations") %>%
    add_row(file = "lookup_LSOA21_LAD22_RGN22.csv", source = "ONS LSOA 2021 to LAD 2022 to region lookup",
            url = paste0(ons_api, "LSOA21_BUA22_LAD22_RGN22_EW_LU_v2")) %>%
    add_row(file = "lookup_LAD22_CSP22_PFA22.csv", source = "ONS LAD 2022 to police force area lookup",
            url = paste0(ons_api, "LAD22_CSP22_PFA22_EW_LU")) %>%
    add_row(file = "LSOA_Dec2021_BFC_V10.gpkg", source = "ONS LSOA December 2021 boundaries (BFC V10)",
            url = paste0(ons_api, "Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BFC_V10")) %>%
    add_row(file = "fsa_establishments.csv", source = "Food Standards Agency FHRS API v2",
            url = "https://api.ratings.food.gov.uk/Establishments") %>%
    add_row(file = "fsa_postcode_centroids.csv", source = "postcodes.io bulk postcode lookup",
            url = "https://api.postcodes.io/postcodes") %>%
    add_row(file = "naptan_access_nodes.csv", source = "DfT NaPTAN access nodes",
            url = "https://naptan.api.dft.gov.uk/v1/access-nodes?dataFormat=csv") %>%
    add_row(file = "police_files_manifest.csv", source = "police.uk bulk archive, 1,564 street-level files (md5 of each in this manifest)",
            url = "https://data.police.uk/data/archive/2023-12.zip") %>%
    mutate(downloaded = file.info(file.path("data_raw", file))$mtime,
           bytes = file.size(file.path("data_raw", file)),
           md5 = tools::md5sum(file.path("data_raw", file))) %>%
    select(source, url, file, downloaded, bytes, md5) %>%
    write_csv("data_raw/download_log.csv")
}

# Add the ethnicity-by-birthplace file to an existing download log if it is not yet listed
download_log <- read_csv("data_raw/download_log.csv", col_types = cols(.default = "c"))
if (!"census/ethnicity_by_birthplace_lsoa.csv" %in% download_log$file) {
  new_file <- "data_raw/census/ethnicity_by_birthplace_lsoa.csv"
  download_log %>%
    add_row(source = "ONS Census 2021 ethnic group (8 categories) by country of birth (UK / outside UK), LSOA level (ONS API)",
            url = "https://api.beta.ons.gov.uk/v1/population-types/UR/census-observations",
            file = "census/ethnicity_by_birthplace_lsoa.csv",
            downloaded = format(file.info(new_file)$mtime), bytes = as.character(file.size(new_file)),
            md5 = unname(tools::md5sum(new_file))) %>%
    write_csv("data_raw/download_log.csv")
}


###############################################################################
# 2. MERGE CRIME DATA AND ASSIGN CRIMES TO 2021 LSOAs
###############################################################################

lsoa_boundaries <- st_read("data_raw/LSOA_Dec2021_BFC_V10.gpkg", quiet = TRUE)

# Function: find the 2021 LSOA for each point (longitude/latitude).
# A few points on the coast fall just outside the coastline-clipped boundaries;
# these are given the nearest LSOA if it is within 1 km.
place_in_lsoa <- function(points) {
  points_sf <- points %>%
    mutate(point_id = row_number()) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(27700)
  placed <- points_sf %>%
    st_join(lsoa_boundaries, join = st_intersects) %>%
    distinct(point_id, .keep_all = TRUE)                  # a point on a boundary line: keep the first LSOA
  outside <- is.na(placed$LSOA21CD)
  if (any(outside)) {
    nearest <- st_nearest_feature(placed[outside, ], lsoa_boundaries)
    # distance to the nearest LSOA, calculated 1,000 points at a time to save memory
    batches  <- split(seq_along(nearest), ceiling(seq_along(nearest) / 1000))
    distance <- map(batches, ~ as.numeric(st_distance(placed[outside, ][.x, ], lsoa_boundaries[nearest[.x], ], by_element = TRUE))) %>%
      unlist()
    placed$LSOA21CD[outside] <- if_else(distance <= 1000, lsoa_boundaries$LSOA21CD[nearest], NA_character_)
  }
  placed %>% st_drop_geometry() %>% select(-point_id)
}

crime_assignment <- tibble()   # summary of how records were assigned to LSOAs (Supplementary Table S1)

for (year in 2021:2023) {

  print(paste("Processing crime data:", year))

  street_files <- list.files("data_raw/police", pattern = paste0("^", year, "-.*-street.csv$"),
                             recursive = TRUE, full.names = TRUE)

  crime_data <- street_files %>%
    map(read_csv, show_col_types = FALSE,
        col_select = c("Reported by", "Longitude", "Latitude", "LSOA code", "Crime type")) %>%
    bind_rows()

  # Delete British Transport Police, City of London Police, and Police Service of Northern Ireland
  # (Greater Manchester Police did not publish data for these years)
  crime_data <- crime_data %>%
    filter(!`Reported by` %in% c("British Transport Police", "City of London Police", "Police Service of Northern Ireland"))

  # police.uk uses a mix of 2011 and 2021 LSOA codes.
  # Crimes with a valid 2021 code keep it. Crimes with an old 2011 code (LSOAs whose boundaries
  # changed in 2021) are placed in the 2021 LSOA that contains their map location.
  crime_data <- crime_data %>%
    mutate(LSOA_code = if_else(`LSOA code` %in% lsoa_boundaries$LSOA21CD, `LSOA code`, NA_character_))

  new_locations <- crime_data %>%
    filter(is.na(LSOA_code), !is.na(Longitude)) %>%
    distinct(Longitude, Latitude) %>%
    place_in_lsoa()

  crime_data <- crime_data %>%
    left_join(new_locations, by = c("Longitude", "Latitude")) %>%
    mutate(LSOA_code = coalesce(LSOA_code, LSOA21CD))

  # Check (2021 only): placing crimes that already have a 2021 code by their location gives the same LSOA?
  location_check <- NA
  if (year == 2021) {
    set.seed(2021)
    check <- crime_data %>%
      filter(`LSOA code` %in% lsoa_boundaries$LSOA21CD, !is.na(Longitude)) %>%
      distinct(Longitude, Latitude, `LSOA code`) %>%
      slice_sample(n = 50000) %>%
      place_in_lsoa()
    location_check <- mean(check$LSOA21CD == check$`LSOA code`, na.rm = TRUE) * 100
  }

  crime_assignment <- bind_rows(crime_assignment, tibble(
    year = year,
    records = nrow(crime_data),
    kept_2021_code = sum(crime_data$`LSOA code` %in% lsoa_boundaries$LSOA21CD),
    placed_by_location = sum(!crime_data$`LSOA code` %in% lsoa_boundaries$LSOA21CD & !is.na(crime_data$LSOA_code)),
    no_location = sum(is.na(crime_data$LSOA_code)),
    check_percent_matching = location_check))
  print(tail(crime_assignment, 1))

  # Count records by LSOA and crime type (all 35,672 LSOAs; LSOAs without records get 0)
  crime_counts <- crime_data %>%
    filter(!is.na(LSOA_code)) %>%
    count(LSOA_code, `Crime type`) %>%
    pivot_wider(names_from = `Crime type`, values_from = n, values_fill = 0)
  crime_counts$All_records <- rowSums(crime_counts[, -1])
  names(crime_counts) <- make.names(names(crime_counts))

  tibble(LSOA_code = lsoa_boundaries$LSOA21CD) %>%
    left_join(crime_counts, by = "LSOA_code") %>%
    mutate(across(-LSOA_code, ~ coalesce(.x, 0))) %>%
    write_csv(paste0("data_derived/crime", year, "_by_LSOA21.csv"))
}

write_csv(crime_assignment, "outputs/TableS1_crime_assignment.csv")
rm(crime_data, new_locations, crime_counts)   # free memory
gc()

crime_2021 <- read_csv("data_derived/crime2021_by_LSOA21.csv") %>%
  mutate(Total_crime    = All_records - Anti.social.behaviour,        # all crime, excluding anti-social behaviour
         Total_violence = Violence.and.sexual.offences) %>%
  select(LSOA_code, All_records, Total_crime, Total_violence)


###############################################################################
# 3. CENSUS 2021 DATA
###############################################################################

population_2021 <- read_csv("data_raw/census/census2021-ts001-lsoa.csv") %>%
  rename(LSOA_code = `geography code`,
         Population_number = `Residence type: Total; measures: Value`,
         Communal = `Residence type: Lives in a communal establishment; measures: Value`) %>%
  mutate(Communal_pct = Communal / Population_number * 100) %>%
  select(LSOA_code, Population_number, Communal_pct)

age_2021 <- read_csv("data_raw/census/census2021-ts007a-lsoa.csv") %>%
  rename(LSOA_code = `geography code`) %>%
  mutate(Age1524 = (`Age: Aged 15 to 19 years` + `Age: Aged 20 to 24 years`) / `Age: Total` * 100) %>%
  select(LSOA_code, Age1524)

density_2021 <- read_csv("data_raw/census/census2021-ts006-lsoa.csv") %>%
  rename(LSOA_code = `geography code`,
         Density = `Population Density: Persons per square kilometre; measures: Value`) %>%
  select(LSOA_code, Density)

deprivation_2021 <- read_csv("data_raw/census/census2021-ts011-lsoa.csv") %>%
  rename(LSOA_code = `geography code`,
         Total_households = `Household deprivation: Total: All households; measures: Value`,
         Deprivation2 = `Household deprivation: Household is deprived in two dimensions; measures: Value`,
         Deprivation3 = `Household deprivation: Household is deprived in three dimensions; measures: Value`,
         Deprivation4 = `Household deprivation: Household is deprived in four dimensions; measures: Value`) %>%
  mutate(Deprivation2more = (Deprivation2 + Deprivation3 + Deprivation4) / Total_households * 100) %>%
  select(LSOA_code, Deprivation2more)

loneparent_2021 <- read_csv("data_raw/census/census2021-ts003-lsoa.csv") %>%
  rename(LSOA_code = `geography code`,
         Total_households = `Household composition: Total; measures: Value`,
         Loneparent_households = `Household composition: Single family household: Lone parent family; measures: Value`) %>%
  mutate(Loneparent = Loneparent_households / Total_households * 100) %>%
  select(LSOA_code, Loneparent)

# Residential mobility: % of residents whose address one year ago was elsewhere in the UK or abroad
mobility_2021 <- read_csv("data_raw/census/census2021-ts019-lsoa.csv") %>%
  rename(LSOA_code = `geography code`,
         Total = `Migrant indicator: Total: All usual residents; measures: Value`,
         Moved_within_UK = `Migrant indicator: Migrant from within the UK: Address one year ago was in the UK; measures: Value`,
         Moved_from_abroad = `Migrant indicator: Migrant from outside the UK: Address one year ago was outside the UK; measures: Value`) %>%
  mutate(Movers = (Moved_within_UK + Moved_from_abroad) / Total * 100) %>%
  select(LSOA_code, Movers)

# Recent arrivals to the UK: % of residents born abroad who arrived less than 2 years ago (Supplementary Table S6)
residency_2021 <- read_csv("data_raw/census/census2021-ts016-lsoa.csv") %>%
  rename(LSOA_code = `geography code`,
         Total = `Length of residence in the UK: Total: All usual residents; measures: Value`,
         Less_than_2_years = `Length of residence in the UK: Less than 2 years; measures: Value`) %>%
  mutate(Lessthan2years = Less_than_2_years / Total * 100) %>%
  select(LSOA_code, Lessthan2years)

# Ethnic heterogeneity: ELF = 1 - sum of squared group shares
# (probability that two random residents belong to different ethnic groups)
ethnicity_raw <- read_csv("data_raw/census/census2021-ts021-lsoa.csv") %>%
  rename(LSOA_code = `geography code`, Total = `Ethnic group: Total: All usual residents`)

ethnic_groups_19 <- ethnicity_raw[, c(6:10, 12:14, 16:19, 21:25, 27:28)]   # the 19 detailed groups
ethnic_groups_5  <- ethnicity_raw[, c(5, 11, 15, 20, 26)]                  # the 5 broad groups
stopifnot(all(rowSums(ethnic_groups_19) == ethnicity_raw$Total), all(rowSums(ethnic_groups_5) == ethnicity_raw$Total))

ethnicity_2021 <- ethnicity_raw %>%
  transmute(LSOA_code,
            White = `Ethnic group: White` / Total * 100,
            Asian = `Ethnic group: Asian, Asian British or Asian Welsh` / Total * 100,
            Black = `Ethnic group: Black, Black British, Black Welsh, Caribbean or African` / Total * 100,
            Mixed = `Ethnic group: Mixed or Multiple ethnic groups` / Total * 100,
            Other_ethnicity = `Ethnic group: Other ethnic group` / Total * 100,
            ELF  = 1 - rowSums((ethnic_groups_19 / Total)^2),    # main measure: 19 detailed groups
            ELF5 = 1 - rowSums((ethnic_groups_5 / Total)^2))     # 5 broad groups (Supplementary Table S7)

# Country of birth: % born outside the UK, and birthplace diversity (11 country-of-birth groups)
birth_raw <- read_csv("data_raw/census/census2021-ts004-lsoa.csv") %>%
  rename(LSOA_code = `geography code`,
         Total = `Country of birth: Total; measures: Value`,
         UK = `Country of birth: Europe: United Kingdom; measures: Value`)

birth_groups_11 <- birth_raw[, c(6, 8:11, 13:18)]   # UK, EU14, EU8, EU2, other EU, other Europe, Africa, Middle East & Asia, Americas, Oceania, British Overseas
stopifnot(all(rowSums(birth_groups_11) == birth_raw$Total))

birth_2021 <- birth_raw %>%
  transmute(LSOA_code,
            Non_UK = (1 - UK / Total) * 100,
            BirthDiv = 1 - rowSums((birth_groups_11 / Total)^2))

# Heterogeneity across ethnicity AND birthplace: 7 ethnic groups x born in / outside the UK = 14 groups
# (the most detailed ethnicity-by-birthplace breakdown published for LSOAs; Supplementary Table S7)
ethnicity_birthplace_2021 <- read_csv("data_raw/census/ethnicity_by_birthplace_lsoa.csv") %>%
  filter(ethnicity != "Does not apply", birthplace != "Does not apply") %>%
  group_by(LSOA_code) %>%
  mutate(share = count / sum(count)) %>%
  summarise(ELF_birthplace = 1 - sum(share^2))

students_2021 <- read_csv("data_raw/census/census2021-ts066-lsoa.csv") %>%
  rename(LSOA_code = `geography code`) %>%
  mutate(Students = (`Economic activity status: Economically active and a full-time student` +
                     `Economic activity status: Economically inactive: Student`) /
                     `Economic activity status: Total: All usual residents aged 16 years and over` * 100) %>%
  select(LSOA_code, Students)

area_2021 <- read_csv("data_raw/lookup_LSOA21_LAD22_RGN22.csv") %>%
  left_join(read_csv("data_raw/lookup_LAD22_CSP22_PFA22.csv") %>% distinct(LAD22CD, PFA22NM), by = "LAD22CD") %>%
  transmute(LSOA_code = LSOA21CD, LSOA_name = LSOA21NM, LA = LAD22NM, Region = RGN22NM, PFA = PFA22NM)


###############################################################################
# 4. OPPORTUNITY CONTROLS: ALCOHOL OUTLETS, OTHER FOOD BUSINESSES, STATIONS
###############################################################################

postcode_centroids <- read_csv("data_raw/fsa_postcode_centroids.csv")

businesses <- read_csv("data_raw/fsa_establishments.csv") %>%
  mutate(PostCode = str_replace(toupper(str_trim(PostCode)), "^(.*?)\\s*([0-9][A-Z]{2})$", "\\1 \\2"))

# place businesses with coordinates by their location ...
businesses_located <- businesses %>%
  filter(!is.na(Longitude), Longitude != 0) %>%
  place_in_lsoa()

# ... and businesses without coordinates by their postcode
businesses_postcode <- businesses %>%
  filter(is.na(Longitude) | Longitude == 0) %>%
  select(-Longitude, -Latitude) %>%
  inner_join(postcode_centroids, by = "PostCode") %>%
  place_in_lsoa()

businesses <- bind_rows(businesses_located, businesses_postcode) %>%
  filter(!is.na(LSOA21CD))                          # businesses outside England and Wales drop out here

print(paste("Food businesses placed in an LSOA:", nrow(businesses)))

stations <- read_csv("data_raw/naptan_access_nodes.csv",
                     col_select = c(ATCOCode, CommonName, StopType, Longitude, Latitude, Status)) %>%
  filter(StopType %in% c("RLY", "MET"),                    # rail and metro/underground stations
         is.na(Status) | Status == "active", !is.na(Longitude)) %>%
  place_in_lsoa() %>%
  filter(!is.na(LSOA21CD))

print(paste("Rail and metro stations placed in an LSOA:", nrow(stations)))

opportunity_2021 <- tibble(LSOA_code = lsoa_boundaries$LSOA21CD) %>%
  left_join(businesses %>% filter(BusinessType == "Pub/bar/nightclub") %>% count(LSOA_code = LSOA21CD, name = "Alcohol_outlets"), by = "LSOA_code") %>%
  left_join(businesses %>% filter(BusinessType != "Pub/bar/nightclub") %>% count(LSOA_code = LSOA21CD, name = "Commercial_outlets"), by = "LSOA_code") %>%
  left_join(stations %>% count(LSOA_code = LSOA21CD, name = "Stations"), by = "LSOA_code") %>%
  mutate(across(-LSOA_code, ~ coalesce(.x, 0L)))


###############################################################################
# 5. COMBINE CRIME, CENSUS AND CONTROLS
###############################################################################

combined_data <- area_2021 %>%
  left_join(population_2021, by = "LSOA_code") %>%
  left_join(age_2021, by = "LSOA_code") %>%
  left_join(density_2021, by = "LSOA_code") %>%
  left_join(deprivation_2021, by = "LSOA_code") %>%
  left_join(loneparent_2021, by = "LSOA_code") %>%
  left_join(mobility_2021, by = "LSOA_code") %>%
  left_join(residency_2021, by = "LSOA_code") %>%
  left_join(ethnicity_2021, by = "LSOA_code") %>%
  left_join(birth_2021, by = "LSOA_code") %>%
  left_join(students_2021, by = "LSOA_code") %>%
  left_join(opportunity_2021, by = "LSOA_code") %>%
  left_join(crime_2021, by = "LSOA_code") %>%
  left_join(ethnicity_birthplace_2021, by = "LSOA_code")   # used only in Supplementary Tables S6-S7

print(paste("LSOAs without the ethnicity-by-birthplace measure:", sum(is.na(combined_data$ELF_birthplace))))

# Exclusions
#  - Greater Manchester: Greater Manchester Police did not publish data for 2021
#  - City of London: excluded (City of London Police data excluded; atypical area)
#  - LSOAs with no police.uk record of any kind in 2021: these are new-build 2021 LSOAs where
#    police.uk had no anonymised map points yet, so their crimes were recorded in neighbouring LSOAs
greater_manchester <- c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford",
                        "Stockport", "Tameside", "Trafford", "Wigan")

combined_data <- combined_data %>%
  mutate(Excluded_GM_or_City = LA %in% greater_manchester | LA == "City of London",
         Excluded_no_records = All_records == 0,
         In_sample = !Excluded_GM_or_City & !Excluded_no_records & complete.cases(pick(Population_number:Stations)))

write_csv(combined_data, "data_derived/merged_all_lsoas.csv")        # all 35,672 LSOAs with exclusion flags

# Supplementary Table S1: sample construction
# (the check on new-build LSOAs counts how many of them do have police.uk records in 2023)
records_2023 <- read_csv("data_derived/crime2023_by_LSOA21.csv") %>% select(LSOA_code, All_records_2023 = All_records)
no_record_lsoas <- combined_data %>%
  filter(!Excluded_GM_or_City, Excluded_no_records) %>%
  left_join(records_2023, by = "LSOA_code")

sample_flow <- tibble(
  step = c("All LSOAs in England and Wales",
           "Excluded: Greater Manchester (no police.uk data for 2021)",
           "Excluded: City of London",
           "Excluded: no police.uk record of any type in 2021 (new-build areas)",
           "  of which: with police.uk records in 2023",
           "Excluded: missing data",
           "Final sample"),
  n = c(nrow(combined_data),
        sum(combined_data$LA %in% greater_manchester),
        sum(combined_data$LA == "City of London"),
        nrow(no_record_lsoas),
        sum(no_record_lsoas$All_records_2023 > 0),
        sum(!combined_data$Excluded_GM_or_City & !combined_data$Excluded_no_records & !combined_data$In_sample),
        sum(combined_data$In_sample)))
print(sample_flow)
write_csv(sample_flow, "outputs/TableS1_sample_flow.csv")

combined_data <- combined_data %>% filter(In_sample)

# Dependent variables: rate per 1,000 residents, log transformed (ln(1 + rate)) and standardised (z-score)
combined_data <- combined_data %>%
  mutate(Crime_rate          = Total_crime / Population_number * 1000,
         Crime_rate_log_z    = scale(log1p(Crime_rate))[, 1],
         Violence_rate       = Total_violence / Population_number * 1000,
         Violence_rate_log_z = scale(log1p(Violence_rate))[, 1])

# Other variables
combined_data <- combined_data %>%
  mutate(Density_log     = log1p(Density),
         Alcohol_log     = log1p(Alcohol_outlets),
         Commercial_log  = log1p(Commercial_outlets),
         Station         = as.integer(Stations > 0),
         PFA             = as_factor(PFA),
         Region          = fct_relevel(as_factor(Region), "London"))

# Categorise the five neighbourhood characteristics into tertiles (1 = low, 2 = medium, 3 = high)
combined_data <- combined_data %>%
  mutate(ELF_cat         = as_factor(ntile(ELF, 3)),
         Deprivation_cat = as_factor(ntile(Deprivation2more, 3)),
         Loneparent_cat  = as_factor(ntile(Loneparent, 3)),
         Mobility_cat    = as_factor(ntile(Movers, 3)),
         Density_cat     = as_factor(ntile(Density_log, 3)),
         across(ends_with("_cat"), ~ fct_relevel(.x, "1", "2", "3")))

# Create the strata variable (e.g. "3 1 1 2 3"; the digits are explained in Table 2)
combined_data <- combined_data %>%
  mutate(strata = as_factor(paste(ELF_cat, Deprivation_cat, Loneparent_cat, Mobility_cat, Density_cat)))

write_csv(combined_data, "data_derived/analytic_dataset_final.csv")   # the data analysed

codebook <- tribble(
  ~variable, ~description,
  "LSOA_code, LSOA_name", "2021 Lower Layer Super Output Area",
  "LA, Region, PFA", "Local authority (2022), region (2022), police force area (2022)",
  "Population_number", "Usual residents (Census 2021, TS001)",
  "Total_crime", "Police-recorded street-level crime in 2021, all types excluding anti-social behaviour (police.uk)",
  "Total_violence", "Police-recorded 'Violence and sexual offences' in 2021 (police.uk)",
  "All_records", "All police.uk records in 2021 including anti-social behaviour",
  "Crime_rate, Violence_rate", "Rate per 1,000 usual residents",
  "Crime_rate_log_z, Violence_rate_log_z", "ln(1 + rate), standardised (z-score) - model outcomes",
  "ELF", "Ethnic heterogeneity: 1 - sum of squared shares of 19 detailed ethnic groups (TS021)",
  "ELF5", "Ethnic heterogeneity on 5 broad groups (TS021)",
  "ELF_birthplace", "Heterogeneity across 7 ethnic groups x born in or outside the UK = 14 groups (ONS Census 2021 API)",
  "White, Asian, Black, Mixed, Other_ethnicity", "% of residents in each broad ethnic group (TS021)",
  "Deprivation2more", "% of households deprived in 2 or more of 4 dimensions (TS011)",
  "Loneparent", "% of households that are lone-parent families (TS003)",
  "Movers", "% of residents whose address one year ago was elsewhere in the UK or abroad (TS019)",
  "Lessthan2years", "% of residents born abroad who arrived in the UK less than 2 years ago (TS016)",
  "Density, Density_log", "Residents per square km (TS006), and ln(1 + density)",
  "Age1524", "% of residents aged 15-24 (TS007A)",
  "Non_UK", "% of residents born outside the UK (TS004)",
  "BirthDiv", "Birthplace diversity: 1 - sum of squared shares of 11 country-of-birth groups (TS004)",
  "Students", "% of residents aged 16+ who are full-time students (TS066); used to identify student areas (Supplementary Table S7)",
  "Communal_pct", "% of residents living in communal establishments (TS001)",
  "Alcohol_outlets, Alcohol_log", "Pubs, bars and nightclubs registered with the FSA, and ln(1 + n)",
  "Commercial_outlets, Commercial_log", "Restaurants, cafes, takeaways, shops, supermarkets and hotels registered with the FSA, and ln(1 + n)",
  "Stations, Station", "Rail and metro stations (NaPTAN), and any station (0/1)",
  "..._cat", "Tertiles within the analytic sample (1 low, 2 medium, 3 high)",
  "strata", "Eco-intersectional stratum: ELF, deprivation, lone parents, mobility, density tertiles",
  "Excluded_..., In_sample", "Exclusion flags (merged_all_lsoas.csv)")
write_csv(codebook, "data_derived/codebook.csv")


###############################################################################
# 6. TABLE 1: DESCRIPTIVE STATISTICS, AND SUPPLEMENTARY TABLE S6: CORRELATIONS
###############################################################################

descriptive_statistics <- combined_data %>%
  select(Crime_rate, Violence_rate, Population_number, White, Asian, Black, Mixed, Other_ethnicity, ELF,
         Deprivation2more, Loneparent, Movers, Density, Age1524, Non_UK, Communal_pct,
         Alcohol_outlets, Commercial_outlets, Station)

descriptive_statistics %>%
  pivot_longer(everything(), names_to = "variable") %>%
  mutate(variable = fct_inorder(variable)) %>%
  group_by(variable) %>%
  summarise(mean = mean(value), sd = sd(value), min = min(value), median = median(value), max = max(value)) %>%
  write_csv("outputs/Table1_descriptive_statistics.csv")

datasummary_skim(descriptive_statistics, output = "outputs/Table1_descriptive_statistics.docx")

# Supplementary Table S6: correlations between measures of ethnic heterogeneity, immigration and mobility
combined_data %>%
  select(ELF, ELF5, ELF_birthplace, BirthDiv, Non_UK, Lessthan2years, Movers) %>%
  drop_na() %>%
  cor() %>%
  as_tibble(rownames = "measure") %>%
  write_csv("outputs/TableS6_correlations.csv")


###############################################################################
# 7. TABLE 3: MULTILEVEL MODELS OF CRIME (Models 1-5)
###############################################################################

# Model 1: null model
model1 <- lmer(Crime_rate_log_z ~ 1 + (1 | strata), data = combined_data)

# Model 2: main effects of the five neighbourhood characteristics (additive SDT)
model2 <- lmer(Crime_rate_log_z ~ ELF_cat + Deprivation_cat + Loneparent_cat + Mobility_cat + Density_cat +
                 (1 | strata), data = combined_data)

# Model 3: + population composition (young people, immigrant concentration, communal establishments)
model3 <- lmer(Crime_rate_log_z ~ ELF_cat + Deprivation_cat + Loneparent_cat + Mobility_cat + Density_cat +
                 Age1524 + Non_UK + Communal_pct +
                 (1 | strata), data = combined_data)

# Model 4: + police force area (recording practices, broader geography)
model4 <- lmer(Crime_rate_log_z ~ ELF_cat + Deprivation_cat + Loneparent_cat + Mobility_cat + Density_cat +
                 Age1524 + Non_UK + Communal_pct + PFA +
                 (1 | strata), data = combined_data)

# Model 5: + opportunity structure (alcohol outlets, other food businesses, stations)
model5 <- lmer(Crime_rate_log_z ~ ELF_cat + Deprivation_cat + Loneparent_cat + Mobility_cat + Density_cat +
                 Age1524 + Non_UK + Communal_pct + PFA +
                 Alcohol_log + Commercial_log + Station +
                 (1 | strata), data = combined_data)

summary(model2)
summary(model5)

model_labels <- c("(Intercept)"      = "Intercept",
                  "ELF_cat2"         = "ELF, medium tertile",
                  "ELF_cat3"         = "ELF, high tertile",
                  "Deprivation_cat2" = "Deprivation, medium tertile",
                  "Deprivation_cat3" = "Deprivation, high tertile",
                  "Loneparent_cat2"  = "Lone parenthood, medium tertile",
                  "Loneparent_cat3"  = "Lone parenthood, high tertile",
                  "Mobility_cat2"    = "Mobility, medium tertile",
                  "Mobility_cat3"    = "Mobility, high tertile",
                  "Density_cat2"     = "Density, medium tertile",
                  "Density_cat3"     = "Density, high tertile",
                  "Age1524"          = "% aged 15-24",
                  "Non_UK"           = "% born outside the UK",
                  "Communal_pct"     = "% communal establishments",
                  "Alcohol_log"      = "Alcohol outlets (log)",
                  "Commercial_log"   = "Other food businesses (log)",
                  "Station"          = "Rail or metro station")

# Function: put the results of Models 1-5 into the layout of Table 3 (also used for Table S5).
# Rows: fixed effects (estimate, stars and standard error; police force area dummies not shown),
# then variances, VPC, PCV, AIC and BIC, the number of strata departing from additive expectations
# (95% CI of the stratum random effect excludes zero) and the correlation of the stratum
# random effects with those of Model 2. The variances come from the REML fit; AIC and BIC come from
# maximum-likelihood refits (refitML), because REML-based AIC cannot compare models with different fixed effects.
model_table <- function(models) {
  fixed <- tibble()
  for (m in names(models)) {
    fixed <- bind_rows(fixed,
      as.data.frame(coef(summary(models[[m]]))) %>%
        rownames_to_column("term") %>%
        filter(term %in% names(model_labels)) %>%
        mutate(model = m,
               stars = case_when(`Pr(>|t|)` < 0.001 ~ "***", `Pr(>|t|)` < 0.01 ~ "**", `Pr(>|t|)` < 0.05 ~ "*", TRUE ~ ""),
               value = paste0(sprintf("%.3f", Estimate), stars, " (", sprintf("%.3f", `Std. Error`), ")")))
  }
  fixed <- fixed %>%
    mutate(row = factor(model_labels[term], levels = model_labels)) %>%
    select(row, model, value) %>%
    pivot_wider(names_from = model, values_from = value) %>%
    arrange(row) %>%
    mutate(row = as.character(row))

  random_effects <- map(models, ~ as.data.frame(ranef(.x, condVar = TRUE)))
  strata_variance   <- map_dbl(models, ~ as.data.frame(VarCorr(.x))$vcov[1])
  residual_variance <- map_dbl(models, ~ as.data.frame(VarCorr(.x))$vcov[2])
  departing <- map_dbl(random_effects, ~ sum(.x$condval - 1.96 * .x$condsd > 0 | .x$condval + 1.96 * .x$condsd < 0))
  correlation <- map_dbl(random_effects, ~ cor(.x$condval, random_effects[[2]]$condval))

  bottom <- tibble(
    row = c("Police force area fixed effects", "Strata variance", "Residual variance", "VPC (%)", "PCV from Model 1 (%)", "AIC (ML)", "BIC (ML)",
            "Strata departing from additive expectations", "Correlation of stratum effects with Model 2"),
    !!!set_names(map(seq_along(models), function(i) c(
      if (i >= 4) "Yes" else "No",
      sprintf("%.3f", strata_variance[i]), sprintf("%.3f", residual_variance[i]),
      sprintf("%.1f", strata_variance[i] / (strata_variance[i] + residual_variance[i]) * 100),
      if (i == 1) "" else sprintf("%.1f", (1 - strata_variance[i] / strata_variance[1]) * 100),
      format(round(AIC(refitML(models[[i]]))), big.mark = ","),
      format(round(BIC(refitML(models[[i]]))), big.mark = ","),
      if (i == 1) "" else as.character(departing[i]),
      if (i <= 2) "" else sprintf("%.2f", correlation[i]))), names(models)))
  bind_rows(fixed, bottom) %>% mutate(across(everything(), ~ replace_na(.x, "")))
}

crime_models <- list("Model 1" = model1, "Model 2" = model2, "Model 3" = model3, "Model 4" = model4, "Model 5" = model5)
table3 <- model_table(crime_models)
print(table3, n = Inf)
write_csv(table3, "outputs/Table3_model_results.csv")

tab_model(model1, model2, model3, model4, model5,
          dv.labels = c("Model 1 (Null)", "Model 2 (Main effects)", "Model 3 (Composition)",
                        "Model 4 (Police force area)", "Model 5 (Opportunity)"),
          terms = names(model_labels)[-1], show.intercept = TRUE, pred.labels = model_labels,
          digits = 3, digits.re = 3, show.aic = FALSE, p.style = "stars",
          file = "outputs/Table3_model_results.doc")


###############################################################################
# 8. PREDICTED VALUES AND STRATUM RANDOM EFFECTS (Model 2)
#    Supplementary Tables S2, S3 and S4
###############################################################################

# Predicted mean outcome and 95% intervals for each stratum
set.seed(2021)
m2m <- predictInterval(model2, level = 0.95, include.resid.var = FALSE)

stratum_level <- combined_data %>%
  bind_cols(m2m) %>%
  group_by(strata, ELF_cat, Deprivation_cat, Loneparent_cat, Mobility_cat, Density_cat) %>%
  summarise(Frequency = n(),
            m2mfit = mean(fit), m2mupr = mean(upr), m2mlwr = mean(lwr),
            Crime_rate_log_z = mean(Crime_rate_log_z), .groups = "drop") %>%
  mutate(strata = as.character(strata)) %>%
  mutate(rank = rank(m2mfit))

# Stratum random effects with 95% confidence intervals.
# ranef(condVar = TRUE) gives exact (conditional) standard errors, so results do not
# change from run to run as simulation-based intervals would.
m2u <- as.data.frame(ranef(model2, condVar = TRUE)) %>%
  transmute(strata = as.character(grp), mean = condval, sd = condsd,
            ci_lower = mean - 1.96 * sd, ci_upper = mean + 1.96 * sd,
            Interpretation = case_when(ci_lower > 0 ~ "Higher than expected",
                                       ci_upper < 0 ~ "Lower than expected",
                                       TRUE ~ "As expected"))

# Supplementary Table S4: all strata with labels, predicted values and random effects
merged_strata_data_labeled <- stratum_level %>%
  left_join(m2u, by = "strata") %>%
  mutate(strata_id = str_remove_all(strata, " "),
         strata_label = paste(
    recode(ELF_cat,         "1" = "Low ELF",      "2" = "Med ELF",      "3" = "High ELF"),
    recode(Deprivation_cat, "1" = "Low Depr",     "2" = "Med Depr",     "3" = "High Depr"),
    recode(Loneparent_cat,  "1" = "Low Lone",     "2" = "Med Lone",     "3" = "High Lone"),
    recode(Mobility_cat,    "1" = "Low Mobility", "2" = "Med Mobility", "3" = "High Mobility"),
    recode(Density_cat,     "1" = "Low Density",  "2" = "Med Density",  "3" = "High Density"),
    sep = " | ")) %>%
  relocate(strata_id, strata, strata_label, Frequency) %>%
  arrange(rank)

write_csv(merged_strata_data_labeled, "outputs/TableS4_all_strata.csv")

print(count(merged_strata_data_labeled, Interpretation))
print(paste("Expected number of departing strata by chance (5% of strata):", round(0.05 * nrow(m2u))))

# Supplementary Table S3: Panel A - ten lowest and ten highest predicted strata;
#                         Panel B - ten largest negative and ten largest positive departures
bind_rows(head(merged_strata_data_labeled, 10), tail(merged_strata_data_labeled, 10)) %>%
  write_csv("outputs/TableS3_panelA_lowest_highest_predicted.csv")

bind_rows(merged_strata_data_labeled %>% arrange(mean) %>% head(10),
          merged_strata_data_labeled %>% arrange(desc(mean)) %>% head(10)) %>%
  write_csv("outputs/TableS3_panelB_largest_departures.csv")

# Supplementary Table S2: stratum sizes, and share of strata departing from expectations by size
stratum_sizes <- merged_strata_data_labeled$Frequency
tibble(statistic = c("Strata (of 243 possible)", "Minimum", "Lower quartile", "Median", "Mean", "Upper quartile", "Maximum",
                     "Strata with fewer than 10 LSOAs", "Strata with fewer than 30 LSOAs", "Strata with 100 or more LSOAs"),
       value = c(length(stratum_sizes), min(stratum_sizes), quantile(stratum_sizes, 0.25), median(stratum_sizes),
                 mean(stratum_sizes), quantile(stratum_sizes, 0.75), max(stratum_sizes),
                 sum(stratum_sizes < 10), sum(stratum_sizes < 30), sum(stratum_sizes >= 100))) %>%
  write_csv("outputs/TableS2_stratum_sizes.csv")

merged_strata_data_labeled %>%
  mutate(size_group = ntile(Frequency, 3)) %>%
  group_by(size_group) %>%
  summarise(min_size = min(Frequency), max_size = max(Frequency), strata = n(),
            departing = sum(Interpretation != "As expected"),
            share_departing = mean(Interpretation != "As expected") * 100) %>%
  write_csv("outputs/TableS2_departures_by_size.csv")

# Supplementary Table S2, Panel C: how many strata, and how small, if the five characteristics were cut into
# quartiles instead of tertiles (4^5 = 1,024 possible strata). Descriptive only; no model is fitted.
quartile_sizes <- combined_data %>%
  count(ntile(ELF, 4), ntile(Deprivation2more, 4), ntile(Loneparent, 4), ntile(Movers, 4), ntile(Density_log, 4)) %>%
  pull(n)
tibble(statistic = c("Possible strata", "Strata with at least one LSOA", "Median LSOAs per stratum",
                     "Strata with fewer than 10 LSOAs", "Strata with fewer than 30 LSOAs"),
       tertiles  = c(3^5, length(stratum_sizes), median(stratum_sizes), sum(stratum_sizes < 10), sum(stratum_sizes < 30)),
       quartiles = c(4^5, length(quartile_sizes), median(quartile_sizes), sum(quartile_sizes < 10), sum(quartile_sizes < 30))) %>%
  write_csv("outputs/TableS2_quartile_strata.csv")


###############################################################################
# 9. FIGURES 1 AND 2: predicted values and stratum random effects (Model 2)
# The strip under each plot shows the tertile of each characteristic
###############################################################################

tertile_strip <- function(data) {
  data %>%
    select(position, ELF_cat, Deprivation_cat, Loneparent_cat, Mobility_cat, Density_cat) %>%
    pivot_longer(-position, names_to = "characteristic", values_to = "tertile") %>%
    mutate(characteristic = recode(characteristic, ELF_cat = "Ethnic heterogeneity", Deprivation_cat = "Deprivation",
                                   Loneparent_cat = "Lone parents", Mobility_cat = "Mobility", Density_cat = "Density"),
           characteristic = fct_rev(fct_inorder(characteristic)),
           tertile = recode(tertile, "1" = "Low", "2" = "Medium", "3" = "High")) %>%
    ggplot(aes(x = position, y = characteristic, fill = tertile)) +
    geom_tile() +
    scale_fill_manual(values = c(Low = "#f0f0f0", Medium = "#969696", High = "#252525"), name = "Tertile") +
    scale_x_continuous(expand = expansion(add = 0.5)) +
    labs(x = "Strata ranked from lowest to highest", y = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
}

# Figure 1: predicted crime by stratum
plotA_data <- merged_strata_data_labeled %>% arrange(m2mfit) %>% mutate(position = row_number())

plotA <- ggplot(plotA_data, aes(x = position, y = m2mfit)) +
  geom_pointrange(aes(ymin = m2mlwr, ymax = m2mupr), size = 0.2) +
  scale_x_continuous(expand = expansion(add = 0.5)) +
  labs(x = NULL, y = "Predicted crime (z-log), Model 2") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggsave("outputs/Figure1.png", plotA / tertile_strip(plotA_data) + plot_layout(heights = c(3, 1.4)),
       width = 11, height = 6, dpi = 300, bg = "white")

# Figure 2: stratum random effects (departures from additive expectations)
plotB_data <- merged_strata_data_labeled %>% arrange(mean) %>% mutate(position = row_number())

plotB <- ggplot(plotB_data, aes(x = position, y = mean, colour = Interpretation)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), size = 0.2) +
  scale_colour_manual(values = c("Lower than expected" = "#2166ac", "As expected" = "grey60",
                                 "Higher than expected" = "#b2182b"),
                      breaks = c("Lower than expected", "As expected", "Higher than expected"), name = NULL) +
  scale_x_continuous(expand = expansion(add = 0.5)) +
  labs(x = NULL, y = "Stratum random effect, Model 2") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "top")

ggsave("outputs/Figure2.png", plotB / tertile_strip(plotB_data) + plot_layout(heights = c(3, 1.4)),
       width = 11, height = 6, dpi = 300, bg = "white")


###############################################################################
# 10. SUPPLEMENTARY TABLE S5: VIOLENCE AS THE OUTCOME
###############################################################################

vmodel1 <- lmer(Violence_rate_log_z ~ 1 + (1 | strata), data = combined_data)
vmodel2 <- update(model2, Violence_rate_log_z ~ .)
vmodel3 <- update(model3, Violence_rate_log_z ~ .)
vmodel4 <- update(model4, Violence_rate_log_z ~ .)
vmodel5 <- update(model5, Violence_rate_log_z ~ .)

violence_models <- list("Model 1" = vmodel1, "Model 2" = vmodel2, "Model 3" = vmodel3, "Model 4" = vmodel4, "Model 5" = vmodel5)
tableS5 <- model_table(violence_models)

# Correlation between the Model 2 stratum effects for violence and for all crime (note to Table S5)
violence_strata <- as.data.frame(ranef(vmodel2, condVar = TRUE)) %>%
  transmute(strata = as.character(grp), mean_violence = condval) %>%
  left_join(select(m2u, strata, mean_all_crime = mean), by = "strata")
tableS5 <- tableS5 %>%
  add_row(row = "Correlation of Model 2 stratum effects with all crime",
          `Model 2` = sprintf("%.2f", cor(violence_strata$mean_violence, violence_strata$mean_all_crime)))
tableS5 <- tableS5 %>% mutate(across(everything(), ~ replace_na(.x, "")))
print(tableS5, n = Inf)
write_csv(tableS5, "outputs/TableS5_violence_models.csv")


###############################################################################
# 11. SUPPLEMENTARY TABLE S7: ROBUSTNESS CHECKS
###############################################################################

# Function: re-build the strata and re-fit Models 1 and 2 for one robustness check.
# AIC and BIC (from maximum-likelihood refits of Model 2) are reported only when a check uses the same
# outcome and the same LSOAs as the main analysis (fit_comparable = TRUE); otherwise fit cannot be compared.
robustness_check <- function(check, data, outcome = "Crime_rate_log_z", heterogeneity = "ELF", fit_comparable = TRUE) {
  data <- data %>%
    mutate(c1 = as_factor(ntile(.data[[heterogeneity]], 3)), c2 = as_factor(ntile(Deprivation2more, 3)),
           c3 = as_factor(ntile(Loneparent, 3)), c4 = as_factor(ntile(Movers, 3)),
           c5 = as_factor(ntile(Density_log, 3)), strata_check = paste(c1, c2, c3, c4, c5))
  m1 <- lmer(as.formula(paste(outcome, "~ 1 + (1 | strata_check)")), data = data)
  m2 <- lmer(as.formula(paste(outcome, "~ c1 + c2 + c3 + c4 + c5 + (1 | strata_check)")), data = data)
  v1 <- as.data.frame(VarCorr(m1)); v2 <- as.data.frame(VarCorr(m2))
  re <- as.data.frame(ranef(m2, condVar = TRUE)) %>% filter(grpvar == "strata_check")
  same_strata <- inner_join(tibble(strata = as.character(re$grp), re = re$condval), select(m2u, strata, mean), by = "strata")
  tibble(check = check, LSOAs = nrow(data), strata = n_distinct(data$strata_check),
         VPC_model1 = v1$vcov[v1$grp == "strata_check"] / sum(v1$vcov) * 100,
         VPC_model2 = v2$vcov[v2$grp == "strata_check"] / sum(v2$vcov) * 100,
         PCV = (1 - v2$vcov[v2$grp == "strata_check"] / v1$vcov[v1$grp == "strata_check"]) * 100,
         significant_strata = sum(re$condval - 1.96 * re$condsd > 0 | re$condval + 1.96 * re$condsd < 0),
         correlation_with_main = cor(same_strata$re, same_strata$mean),
         AIC_ML = if (fit_comparable && nrow(data) == nrow(combined_data)) AIC(refitML(m2)) else NA,
         BIC_ML = if (fit_comparable && nrow(data) == nrow(combined_data)) BIC(refitML(m2)) else NA)
}

# Pooled 2022-2023 crime (outside COVID-19 restrictions)
crime_2022_2023 <- read_csv("data_derived/crime2022_by_LSOA21.csv") %>%
  transmute(LSOA_code, Crime_2022 = All_records - Anti.social.behaviour) %>%
  left_join(read_csv("data_derived/crime2023_by_LSOA21.csv") %>%
              transmute(LSOA_code, Crime_2023 = All_records - Anti.social.behaviour), by = "LSOA_code")

data_2022_2023 <- combined_data %>%
  left_join(crime_2022_2023, by = "LSOA_code") %>%
  filter(Crime_2022 + Crime_2023 > 0) %>%
  mutate(Crime_rate_log_z = scale(log1p((Crime_2022 + Crime_2023) / 2 / Population_number * 1000))[, 1])

data_no_students <- combined_data %>%
  filter(Students < 25, Communal_pct < 10) %>%
  mutate(Crime_rate_log_z = scale(log1p(Crime_rate))[, 1])

robustness <- bind_rows(
  robustness_check("Main analysis", combined_data),
  robustness_check("Violence as outcome", combined_data, outcome = "Violence_rate_log_z", fit_comparable = FALSE),
  robustness_check("Strata on 5-group ELF", combined_data, heterogeneity = "ELF5"),
  robustness_check("Strata on ethnicity-by-birthplace heterogeneity (14 groups)", filter(combined_data, !is.na(ELF_birthplace)), heterogeneity = "ELF_birthplace"),
  robustness_check("Strata on birthplace diversity", combined_data, heterogeneity = "BirthDiv"),
  robustness_check("Excluding student (>=25%) and communal (>=10%) LSOAs", data_no_students, fit_comparable = FALSE),
  robustness_check("Outcome: pooled 2022-2023 crime", data_2022_2023, fit_comparable = FALSE))

print(robustness)
write_csv(robustness, "outputs/TableS7_robustness.csv")


##############################
# 12. Collinearity of the model predictors (Table S8)
##############################

# Variance inflation factors for the fixed effects of Models 3 and 5. For variables with more than
# one dummy (the tertiles and police force area) this is the generalised VIF (Fox and Monette, 1992).
collinearity <- bind_rows(
  as_tibble(check_collinearity(model3)) %>% mutate(model = "Model 3"),
  as_tibble(check_collinearity(model5)) %>% mutate(model = "Model 5")) %>%
  select(model, Term, VIF)

print(collinearity, n = Inf)
write_csv(collinearity, "outputs/TableS8_collinearity.csv")

writeLines(capture.output(sessionInfo()), "outputs/session_info.txt")
