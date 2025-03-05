library(tidyverse)
library(sf)

# Loading the scholl coordinates data
school_cord_data <- read_csv("data/WFP VAM_Verified HGSFP School Location_20241024 1.csv") %>%
  select(School_cod, School_EN, District_E, Commune_E, Lat, Long, dis_geocode, com_geocode) %>%
  filter(
    District_E %in% c("Krakor", "Bakan", "Kandieng", "Phnum Kravanh", "Ta Lou Senchey")
  ) %>% 
  rename(
    SchoolCode = School_cod,
    SchoolName = School_EN,
    District = District_E,
    Commune = Commune_E
  )



schools_sf <- st_as_sf(
  school_cord_data,
  coords = c("Long", "Lat"),  # Use the columns with actual school coordinates
  crs = 4326  # WGS 84 CRS (latitude/longitude)
)

# Counting the number of schools in each district
Schools_n <- schools_sf %>%
  group_by(District) %>%
  summarise(n_nschools = n()) %>% 
  st_drop_geometry()


# read the district level shapefile


districts_sf <- st_read("data/shapefiles/WFP_PST_5Districts.shp") %>%
  rename(District =  Adm2_Name) %>%
  st_transform(crs = 4326) %>%   # Transform the CRS to WGS 84
  select(District, Shape_Area, Adm1_code, geometry) %>% 
  mutate(Shape_Area = Shape_Area / 1000000) %>%   # Convert the area to km²
  rename(CODE = Adm1_code)

# Cutting the mountanious region in the south of Phnum Kravanh

phnum_kravanh <- districts_sf %>% filter(District == "Phnum Kravanh")

schools_in_kravanh <- schools_sf[phnum_kravanh, ]

phnum_hull <- st_buffer(st_union(schools_in_kravanh), dist = 10000)

phnum_clipped <- st_intersection(phnum_kravanh, phnum_hull)

districts_cleaned <- districts_sf %>%
  filter(District != "Phnum Kravanh") %>%
  bind_rows(phnum_clipped)



ggplot() +
  geom_sf(data = districts_cleaned, fill = "lightblue") +
  geom_sf(data = schools_sf, color = "red", size = 1) +
  theme_minimal()

clean_district_area <- districts_cleaned %>% 
  group_by(District) %>%
  mutate(area = as.numeric(st_area(geometry) / 1000000)) %>% 
  left_join(Schools_n, by = "District") %>% 
  st_drop_geometry() %>% 
  select(District, n_nschools, area) %>% 
  mutate(pilot = case_when(
    District == "Ta Lou Senchey" ~ "Commune Centralisation",
    District == "Phnum Kravanh" ~ "District Centralisation",
    TRUE~ "Non-Pilot Districts"
  )) %>% 
  filter(District != "Krakor")
  






