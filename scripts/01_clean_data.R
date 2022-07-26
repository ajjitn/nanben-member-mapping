library(tidyverse)
library(urbnmapr)
library(urbnthemes)
library(tigris)
library(zipcodeR)
library(readxl)
library(mapview)
library(sf)
library(tidylog)


mapviewOptions(fgb = FALSE)
options(tigris_use_cache = FALSE)

# --- Read in Data ----
nanben_members = read_excel("data/july_2022_member_mapping.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(uuid = row_number())


cities_for_geocoding = nanben_members %>% 
  mutate(f_address = str_c(city, state, sep = ", ")) %>% 
  select(uuid, f_address) 

# 4 members don't have state listed, 106 members don't have zip code listed
nanben_members %>% 
  count(is.na(zip_code), is.na(state))

# Do some data cleaning
us_nanben_members = nanben_members %>% 
  # There's 1 nanben member in the UK with a UK zip who we filter out for now
  filter(!state == "Brampton") %>% 
  filter(! state %in% c("ON", "UAE", "NSW", "BC")) %>% 
  # 2 members in Chennai we are also filtering out
  filter(!city %in% c("Chennai")) %>% 

  separate(zip_code, 
           into = c("zip_code", "zip_5_9"),
           sep = "-") %>% 
  # Some folks have inputted wrong zip code, lets manually correct
  mutate(zip_code = case_when(
    email == "sethuferrari@gmail.com" ~ "01886",
    TRUE ~ zip_code
  )) %>% 
  # Some states show up as "Ak or "Az"
  mutate(state = str_to_upper(state)) %>% 
  # Someone inputted TX as TC, so we manually correct
  mutate(state =  case_when(
    city == "Plano" & state == "TC" ~ "TX",
    TRUE ~ state
  )) %>% 
  mutate(is_zcta = zipcodeR::is_zcta(zip_code),
         is_in_zipcoder_lib = case_when(
           zip_code %in% zip_code_db$zipcode ~ TRUE,
           TRUE ~ FALSE
         ))


## Generate state specific data
state_data = urbnmapr::get_urbn_map("states", sf = TRUE) %>% 
  tigris::shift_geometry()
  
data_with_states = us_nanben_members %>% 
  # 12 nanben members are in ON, UAE, or NSW (Canada, Dubai, and Australia) and were not joined
  tidylog::left_join(state_data,
                     by = c("state" = "state_abbv")) %>% 
  st_as_sf()

nanben_member_counts_by_state = data_with_states %>% 
  st_drop_geometry() %>% 
  count(state) %>% 
  tidylog::right_join(state_data, by = c("state" = "state_abbv")) %>% 
  st_as_sf() %>% 
  # Replace NAs from join with 0 for count
  replace_na(list(n=0))


data_with_states %>% 
  filter(!is_zcta) 

## Generate zcta specific data
zctas = tigris::zctas(cb = TRUE,
                      refresh = TRUE,
                      class = "sf",
                      # For some reason only setting year = 2019 works
                      year = 2019) 

zctas = zctas %>% 
  janitor::clean_names() %>% 
  select(zcta = zcta5ce10)

data_with_zips = data_with_states %>% 
  st_drop_geometry() %>% 
  left_join(zctas, by = c("zip_code" = "zcta")) %>% 
  st_as_sf()

nanben_member_counts_by_zip = data_with_zips %>% 
  st_drop_geometry() %>% 
  count(zip_code) %>% 
  tidylog::right_join(zctas, by = c("zip_code" = "zcta")) %>% 
  st_as_sf() %>% 
  # Replace NAs from join with 0 for count
  replace_na(list(n=0))


## Generate point data
data_with_points = data_with_zips %>% 
  st_drop_geometry() %>% 
  # Use zip centroids when zip is availabe. The zip centroids are from the
  # zip_code_db in the zipcodeR pkg
  left_join(zip_code_db %>% 
              select(zip_code = zipcode, 
                     zipcode_type, 
                     post_office_city,
                     zipcode_state = state, 
                     lat,
                     lng, 
                     timezone,
                     population, 
                     housing_units),
            by = c("zip_code"))



# ---- Write out data -----

# Create dirs for output
dir.create("data/final-data/", showWarnings  = FALSE)

nanben_member_counts_by_state %>% 
  st_write("data/final-data/nanben_member_counts_by_state.geojson",
           delete_dsn = TRUE)

nanben_member_counts_by_zip %>% 
  st_write("data/final-data/nanben_member_counts_by_zcte.geojson",
           delete_dsn = TRUE)

data_with_points %>% 
  write_csv("data/final-data/us_nanben_members.csv")

cities_for_geocoding %>% 
  write_csv("data/cities_for_geocoding.csv")



