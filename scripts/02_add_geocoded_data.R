library(tidyverse)
library(urbnmapr)
library(urbnthemes)
library(tigris)
library(zipcodeR)
library(readxl)
library(mapview)
library(sf)
library(tidylog)


# ---- Read in point data -----
data_with_points = read_csv(
  "data/final-data/us_nanben_members.csv"
  )

cities_for_geocoding =
  read_csv("data/final-data/cities_for_geocoding-geocoded.csv")

# ---- Append city lat/longs for missing zips -----

data_with_points_appended = data_with_points %>% 
  left_join(cities_for_geocoding %>% 
              # There are 11 rows without Na lat/longs, which show up as 0
              mutate(Latitude = if_else(Latitude == 0, NA_real_, Latitude),
                     Longitude = if_else(Longitude == 0, NA_real_, Longitude)) %>% 
              select(f_address, match_address = Match_addr, 
                     lng_city = Longitude, 
                     lat_city = Latitude,
                     uuid),
            by = "uuid") %>% 
  mutate(lat = case_when(
          is.na(lat) ~ lat_city,
          TRUE ~ lat
          ),
         lng = case_when(
           is.na(lng) ~ lng_city,
           TRUE ~ lng
         ),
  ) %>% 
  # There are still 6 people with just state info. For now we filter them out 
  filter(!is.na(lat)) %>% 
  # Now finally convert to sf now that there are no more NA values for lat/lng
  st_as_sf(
    coords = c("lng", "lat"),
    crs = 4326
  )

  
# ---- Write out ------
data_with_points_appended %>% 
  st_write("data/final-data/us_nanben_members.csv",
           layer_options = "GEOMETRY=AS_XY",
           delete_dsn = TRUE)

data_with_points_appended %>% 
  st_write("data/final-data/us_nanben_members.geojson",
           delete_dsn = TRUE)


# ---- Make maps ------
set_urbn_defaults(style = "map")

sf_use_s2(FALSE)

nanben_member_state_map = nanben_member_counts_by_state %>% 
  ggplot(aes(fill = n)) +
  geom_sf(color = "white") +
  scale_fill_gradient(low = palette_urbn_green[1],
                      high = palette_urbn_green[7])


nanben_member_point_map = data_with_points_appended %>% 
  st_jitter(factor = 0.009) %>% 
  select(geometry, everything()) %>% 
  # sample_n(10) %>% 
  ggplot() +
  geom_sf(data = nanben_member_counts_by_state, fill = palette_urbn_gray[4]) +
  # # Trying a heatmap
  # stat_density2d(aes(x = lng_city, y = lat_city, fill = ..density..), 
  #                geom = 'tile', 
  #                contour = F, 
  #                alpha = .5)
  geom_sf(alpha = 0.15, size = 2, color = "red", stroke = FALSE)
  


