#Loading relevant libraries

library(tidyverse)
library(sf)
library(showtext)
library(lubridate)
library(patchwork)
library(ggspatial)


########################################################################
#Text for the graphs - setting the defalts
font_add_google("Open Sans","opensans")
font_add_google("EB Garamond","garamond")
showtext_auto()
showtext_opts(dpi = 200)
########################################################################

# 1. Continous Supply of Commodities

condinious_supply <- tibble(
  procurement_model = c("Commune Centralisation", "District Centralisation", "Non-Procurement Pilots"),
  `2023` = c(1.88, 2.85, 1.29),
  `2024` = c(0.49, 1.2, 0.55),
  change_perc = c(73.9, 57.9, 57.4),
  schools = c(20, 23, 31)
) %>% 
  pivot_longer(cols = `2023`:`2024`, names_to = "year", values_to = "avg_days") %>%
  mutate(year = as.numeric(year),
         procurement_model = factor(procurement_model, 
                                    levels = c("Non-Procurement Pilots", "District Centralisation", "Commune Centralisation")))

continous_supply_graph <- condinious_supply %>%
  ggplot(aes(x = avg_days, y  = procurement_model)) +
  geom_line(aes(group = procurement_model), color = "#088395", size = 3.5) +
  geom_point(aes(x = avg_days), shape = 21, size = 6, fill = if_else(
    condinious_supply$year == 2024, "#FFB200", "#626F47"
  ), color = "white", stroke = 1.5) +
  geom_text(aes(label = paste0(avg_days, " days")), hjust = -0.1, 
            nudge_x =  -0.1, size = 6,
            nudge_y = -0.15, family = "garamond", color = "#088395") +
  annotate("text", 
           x = 0.49, y = 3.2, label = "2024", 
           family = "garamond", 
           size = 7,
           fontface = "bold",
           color = "#FFB200") +
  annotate("text", 
           x = 1.88, y = 3.2, label = "2023", 
           family = "garamond", 
           size = 7,
           fontface = "bold",
           color = "#626F47") +
  annotate(
    "segment", x = 0.8, xend = 1.1, y = 2.9, yend = 2.7,
    color = "#088395", size = 0.5, arrow = arrow(type = "closed", length = unit(0.1, "inches"))
  ) + 
  annotate("text", x = 1.8, y = 2.45, 
           label = "Clearly, Commune Centralisation model is the\nmost efficient in reducing breakdown days\n(when measured in percentage changes).", 
           family = "garamond", size = 6, color = "#088395", lineheight = 0.7) + 
  annotate(
    "segment", x = 2.45, y = 2.5, xend = 3.2, yend = 2.8,
     arrow = arrow(type = "closed", length = unit(0.1, "inches")),
    color = "#088395"
  )  +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "",
    caption = "",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#ECEFDC", color = "#ECEFDC"),
    plot.title = element_text(family = "garamond", size = 14, face = "bold", colour = "#088395"),
    plot.subtitle = element_text(family = "garamond", size = 10, colour = "#088395"),
    plot.caption = element_text(family = "garamond", size = 8, colour = "#088395"),
    axis.title = element_blank(),
    axis.text.y = element_text(family = "garamond", size = 18, colour = "#088395", face = "bold"),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E6C9", size = 0.6, linetype = "dashed"),
    legend.title = element_text(family = "garamond", size = 8),
    legend.text = element_text(family = "garamond", size = 8)
  )


ggsave("continous_supply_graph.png", continous_supply_graph, width = 8, height = 6, dpi = 200)
# Lets draw the percentage changes graph


changes_graph <- condinious_supply %>%
  ggplot(aes(x = 1, y = procurement_model)) +  # Force all points to be on x = 1
  geom_point(aes(size = change_perc), shape = 21, color = "#626F47", fill = "#FFB200", stroke = 1) +
  geom_text(aes(label = paste0("-", change_perc, "%")), 
            size = 8, fontface = "bold",  
           family = "garamond", color = "#ECEFDC") +
  scale_size_continuous(range = c(20, 30)) +
  scale_x_continuous(breaks = NULL) +  # Remove x-axis labels
  annotate("text", x = 1, y = 3.5, label = "Change", lineheight = 1,
           family = "garamond", size = 9, fontface = "bold", color = "#FFB200") +
  theme_minimal() +
  labs() + 
  theme(
    plot.background = element_rect(fill = "#ECEFDC", color = "#ECEFDC"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  ) 

# Merge the graphs using patchwork

complete_plot <- continous_supply_graph + changes_graph + plot_layout(design=
                               c(
                                 area(l=0,  r=45, t=0, b=1), # defines the main figure area
                                 area(l=46, r=52, t=0, b=1)  # defines the gap figure area
                               )) +
  plot_annotation(title = "Using breakdown days as the measure of continous supply of commodities to\nschools, commune level centralisation was the most efficient model from 2023-24.",
                  subtitle = "Breakdown days are the number of days a school goes without a commodity due to stockouts or other\nreasons. In this study, this was assumed to be as a reason of shortages of commodities", 
                  caption = "Source: Data from SFIS", 
                  theme = theme(plot.background = element_rect(fill = "#ECEFDC", color = "#ECEFDC"),
                                plot.title = element_text(family = "garamond", size = 30, face = "bold", colour = "#088395", lineheight = 0.5), 
                                plot.subtitle = element_text(family = "garamond", size = 23, colour = "#088395", lineheight = 0.5, face = "bold.italic",
                                                             margin = margin(b = 0)), 
                                plot.caption = element_text(family = "garamond", size = 15, colour = "#088395", hjust = 0))) 

ggsave("figures/complete_plot.png", complete_plot, width = 9.72, height = 6.52, dpi = 300, units = "in", device = "png")  
  


######################################################################################################

#2. School Suppliers Costs

supplier_costs <- average_wetcosts %>% 
  left_join(average_drycosts, by = "procurement") %>%
  mutate(schools = c(20, 23, 31),
         wet_cost_per_school = (wetcosts*wetsuppliers)/schools,
         dry_cost_per_school = (drycosts*drysuppliers)/schools) %>% 
  pivot_longer(cols = wet_cost_per_school:dry_cost_per_school, names_to = "cost_type", values_to = "cost_per_school")


supplier_costs_graph <- supplier_costs %>% 
  ggplot(aes(x = cost_per_school, y = procurement)) +
  geom_bar(stat = "identity", aes(fill = cost_type), position = "dodge", width = 0.6) + # Reduce width slightly
  scale_y_discrete(expand = expansion(c(0.2, 0))) +  # Remove space between bars and axis
  scale_x_continuous(position = "top", labels = scales::dollar_format(prefix = "$")) +  # Move x-axis labels closer
  geom_vline(xintercept = 0, color = "#088395", size = 0.5) +  # Add a vertical line at 0
  # geom_text(aes(label = paste0("$", round(cost_per_school, 2))), 
  #           size = 6, family = "garamond", color = "#088395",
  #           nudge_x = 1.5, nudge_y = 0.1, vjust = 0.5) +
  labs(
    title = "Community Centralisation also performed well\nin reducing costs for the suppliers (which can be\ninterpreted as gains from bulk buying/sourcing)",
    subtitle = "",
    x = "",
    y = "",
    caption = "",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#ECEFDC", color = "#ECEFDC"),
    plot.title = element_text(family = "garamond", size = 20, face = "bold", colour = "#088395", lineheight = 0.8),
    plot.title.position = "plot",
    plot.subtitle = element_text(family = "garamond", size = 10, colour = "#088395"),
    plot.caption = element_text(family = "garamond", size = 8, colour = "#088395"),
    axis.title = element_blank(),
    axis.text.y = element_text(family = "garamond", size = 18, colour = "#088395", face = "bold",
                               margin = margin(r = -15)),  # Move y-axis labels closer
    axis.text.x = element_text(family = "garamond", size = 18, colour = "#088395", face = "bold"),  # Move x-axis labels closer
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "#E0E6C9", size = 0.6, linetype = "dashed"),
    legend.position = "none"
  )

ggsave("figures/supplier_costs_graph.png", supplier_costs_graph, width = 6.26, height = 5.02, dpi = 200)


######################################################################################################  

# Maps -  This code is just copied from the other visualisation script i.e., visualisations.R

# Read school level csv data

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
students_per_school <- FullTablesData %>% 
  filter(Year == 2024) %>% 
  group_by(SchoolName) %>% 
  summarise(Students = mean(AvgStudents, na.rm = T))

school_cord_data <- school_cord_data %>%
  left_join(students_per_school, by = "SchoolName") 

schools_sf <- st_as_sf(
  school_cord_data,
  coords = c("Long", "Lat"),  # Use the columns with actual school coordinates
  crs = 4326  # WGS 84 CRS (latitude/longitude)
)


# read the district level shapefile

districts_sf <- st_read("data/shapefiles/WFP_PST_5Districts.shp") %>%
  rename(District =  Adm2_Name) %>%
  st_transform(crs = 4326) %>%   # Transform the CRS to WGS 84
  select(District, Shape_Area, Adm1_code, geometry) %>% 
  mutate(Shape_Area = Shape_Area / 1000000) %>%   # Convert the area to km²
  rename(CODE = Adm1_code)

communes_sf <- st_read("data/shapefiles/WFP_PST_37Communes.shp") %>%
  rename(Commune =  Adm3_Name) %>%
  st_transform(crs = 4326) %>%   # Transform the CRS to WGS 84
  select(Commune, Shape_area, geometry) %>% 
  mutate(Shape_area = Shape_area / 1000000)  # Convert the area to km²

# Merge the school and district data

school_counts <- schools_sf %>% 
  group_by(District) %>%
  summarise(n_schools = n()) %>%
  ungroup() %>% 
  st_drop_geometry()

# Join the school counts to the districts data

districts_sf <- districts_sf %>% 
  left_join(school_counts, by = "District") %>% 
  mutate(school_density = n_schools / Shape_Area) %>% 
  select(District, school_density, geometry)


roads <- st_read("data/roads/khm_trs_roads_gov_wfp_ed2024.shp") %>%
  st_transform(crs = 4326)

water <- st_read("data/water/khm_hyd_rivers_gov.shp") %>%
  st_transform(crs = 4326) 

boundaries <- st_read("data/boundary/BND/khm_bnd_admin2_gov_wfp_ed2022.shp") %>%
  st_transform(crs = 4326) %>% 
  filter(Adm2_NCDD == 1501 | Adm2_NCDD == 1502 | 
           Adm2_NCDD == 1503 | Adm2_NCDD == 1504 | Adm2_NCDD == 1505)


# Join the water and the boundaries data
water_in_boundaries <- st_intersection(water, districts_sf) %>% 
  filter(Size != "Major")

roads_in_boundaries <- st_intersection(roads, districts_sf) %>% 
  filter(Classes == "Provincial and rural road")


school_density_graph_01 <- districts_sf %>% ggplot(aes(fill = school_density)) +
  # District layer with school density
  geom_sf(data = districts_sf, color = "white", size = 1.9) +
  # School points layer
  geom_sf(data = schools_sf, size = 1, fill = "#240A34", alpha = 0.5, shape = 21) +
  #Add commune layer
  #geom_sf(data = communes_sf, fill = "transparent", color = "#FBF4DB", size = 0) +
  # District names layer
  geom_sf_text(data = districts_sf, aes(label = District), size = 5.5, fontface = "bold",
               color = if_else(districts_sf$District == "Ta Lou Senchey", "white", "black"), family = "garamond") +
  coord_sf(expand = FALSE) +
  # Water layer
  # geom_sf(data = water, fill = "#A6D6D6", color = "#A6D6D6") +
  # Custom color scale for school density
  scale_fill_gradient(
    name = "Schools/km²",
    low = "#E0A75E", # Light yellow
    high = "#973131", # Deep red
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 5, # Adjust bar width
      barheight = 0.2 # Adjust bar height
    )
  ) +
  theme_void() +
  annotation_scale(location = "bl", text_family = "serif", height = unit(0.10, "cm")) +
  annotation_north_arrow(which_north = "grid",
                         location    = "tl",
                         style       = north_arrow_orienteering(text_family = "serif"),
                         height      = unit(0.45, "cm"),
                         width       = unit(0.45, "cm")) +
  theme(
    plot.background = element_rect(fill = "#ECEFDC", color = "#ECEFDC"),
    plot.title = element_text(size = 12, hjust = 0.5, family = "garamond", face = "bold", margin = margin(b = 2, t = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 11, family = "garamond", face = "bold", margin = margin(b = 2)),
    legend.text = element_text(size = 13, face = "bold", family = "garamond", margin = margin(t = 2)),
    legend.box.margin = margin(t = 0),
    plot.caption = element_text(size = 10, family = "garamond", hjust = 0),
    plot.caption.position = "plot"
  )


#########################################################################################

districts_sf <- districts_sf %>%
  mutate(
    text_color = if_else(District == "Ta Lou Senchey", "#240750", "#E4E0E1"), # Specify your desired colors
    nudge_y = if_else(District == "Ta Lou Senchey", 0.18, -0.005),
    nudge_x = if_else(District == "Ta Lou Senchey", -0.15, 0)
  )

school_connect_graph_01 <- ggplot() + 
  geom_sf(data = districts_sf, fill  = "#202040", color = "#E8F9FD", size = 1.5) +
  #geom_sf(data = water_in_boundaries, fill = "#478CCF", color = "#478CCF") + 
  geom_sf_text(
    data = districts_sf, 
    aes(label = District), 
    size = 5.5, 
    fontface = "bold",
    color = districts_sf$text_color, 
    family = "garamond", 
    nudge_y = if_else(districts_sf$District == "Ta Lou Senchey", 0.18, -0.01),
    nudge_x = if_else(
      districts_sf$District == "Phnum Kravanh", -0.1, 
      if_else(districts_sf$District == "Ta Lou Senchey", -0.12, 0)
    )
  ) + 
  geom_sf(data = roads_in_boundaries, color =  "#FEFBF6", size = 0.05, alpha = 0.2) + 
  geom_sf(data = schools_sf, fill = "#E6B325", color = "#E6B325", aes(size = Students), alpha = 0.5, shape = 21) + 
  coord_sf(expand = FALSE) +
  annotate(
    "curve",
    x = 103.5,
    xend = 103.6,
    y = 12.62,
    yend = 12.53,
    color = "#240750",
    curvature = -0.2,
    arrow = arrow(type = "closed", length = unit(0.05, "inches"), ends = "last")) +
  theme_void() +
  labs(
    size = "Average Eating Students"
  ) +
  theme(
    plot.background = element_rect(fill = "#ECEFDC", color = "#ECEFDC"),
    plot.title = element_text(size = 12, hjust = 0.5, family = "garamond", face = "bold", margin = margin(b = 2, t = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 11, family = "garamond", face = "bold", margin = margin(b = -0.5)),
    legend.text = element_text(size = 13, face = "bold", family = "garamond", margin = margin(t = 2)),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.caption = element_text(size = 11, family = "garamond", hjust = 0, face = "bold"),
    plot.caption.position = "plot"
  ) + 
  scale_size_continuous(range = c(0.3, 2.5)) +
  guides(size = guide_legend(title.position = "top", title.hjust = 0.5))


 # Merge the two maps

complete_map <- school_density_graph_01 + school_connect_graph_01 + plot_layout(widths = c(1, 1)) +
  plot_annotation(title = "Schools in Ta Lou Senchey district have the highest density of schools and well connected by roads.",
                  subtitle = "The map on the left shows the density of schools in the region, with the map on the right showing the road network and the\naverage number of students in each school. These might be the factors why Ta Lou Senchey perfomed well.", 
                  caption = "Source: Data from SFIS", 
                  theme = theme(plot.background = element_rect(fill = "#ECEFDC", color = "#ECEFDC"),
                                plot.title = element_text(family = "garamond", size = 23, face = "bold", colour = "#088395", lineheight = 0.5), 
                                plot.subtitle = element_text(family = "garamond", size = 19, colour = "#088395", lineheight = 0.5, face = "bold.italic",
                                                             margin = margin(b = 0)), 
                                plot.caption = element_text(family = "garamond", size = 15, colour = "#088395", hjust = 0)))


ggsave("figures/complete_map.png", complete_map, width = 7.15, height = 5.02, dpi = 300, units = "in", device = "png")














