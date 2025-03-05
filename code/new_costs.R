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


  

new_costs <- supplier_costs %>% 
  distinct(procurement, .keep_all = T) %>%
  select(procurement, wetcosts, wetsuppliers, drycosts, drysuppliers) %>% 
  mutate(total_wet_costs = wetcosts * wetsuppliers,
         total_dry_costs = drycosts * drysuppliers)



################################################################################

# Experimenting with the all districts

# Function to clip each district based on its schools
clip_districts <- function(district) {
  # Extract the current district
  district_poly <- districts_sf %>% filter(District == district)
  
  # Extract schools within the district
  schools_in_district <- schools_sf[district_poly, ]
  
  # If no schools in the district, return the original polygon
  if (nrow(schools_in_district) == 0) return(district_poly)
  
  # Create a convex hull around the schools
  district_hull <- st_convex_hull(st_union(schools_in_district))
  
  # Intersect the hull with the original district to remove empty areas
  clipped_district <- st_intersection(district_poly, district_hull)
  
  return(clipped_district)
}

# Apply to all districts
districts_clipped <- map_dfr(unique(districts_sf$District), clip_districts) %>% 
  mutate(Shape_Area = st_area(geometry) / 1e6)

# Calculate area in km²
districts_clipped_clean <- districts_clipped %>%
  mutate(Area_km2 = as.numeric(st_area(geometry)) / 1e6) %>% 
  st_drop_geometry() %>% 
  select(District, Area_km2) %>% 
  left_join(Schools_n, by = "District") %>% 
  mutate(procurement = case_when(
    District == "Ta Lou Senchey" ~ "Commune Centralisation",
    District == "Phnum Kravanh" ~ "District Centralisation",
    TRUE~ "Non-Procurement Pilots"
  )) %>%
  filter(District != "Krakor") %>% 
  group_by(procurement) %>%
  summarise(n_nschools = sum(n_nschools),
            Area_km2 = sum(Area_km2))
  

clean_districts_clipped <- new_costs %>%
  left_join(districts_clipped_clean, by = "procurement")

clean_districts_clipped %>%
  mutate(Wet_cost_per_km2 = total_wet_costs / Area_km2,
         dry_cost_per_km2 = total_dry_costs / Area_km2) %>% 
  select(procurement, Wet_cost_per_km2, n_nschools, Area_km2, dry_cost_per_km2)


