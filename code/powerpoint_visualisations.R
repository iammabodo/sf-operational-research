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
            nudge_y = -0.15, family = "opensans", color = "#088395") +
  annotate("text", 
           x = 0.49, y = 3.2, label = "2024", 
           family = "opensans", 
           size = 7,
           fontface = "bold",
           color = "#FFB200") +
  annotate("text", 
           x = 1.88, y = 3.2, label = "2023", 
           family = "opensans", 
           size = 7,
           fontface = "bold",
           color = "#626F47") +
  annotate(
    "segment", x = 0.8, xend = 1.1, y = 2.9, yend = 2.7,
    color = "#088395", size = 0.5, arrow = arrow(type = "closed", length = unit(0.1, "inches"))
  ) + 
  annotate("text", x = 1.8, y = 2.45, 
           label = "Commune Centralisation model is the\nmost efficient in reducing breakdown days\n(when measured in percentage changes).", 
           family = "opensans", size = 6, color = "#088395", lineheight = 0.7) + 
  annotate(
    "segment", x = 2.55, y = 2.5, xend = 3.2, yend = 2.8,
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
    plot.title = element_text(family = "opensans", size = 14, face = "bold", colour = "#088395"),
    plot.subtitle = element_text(family = "opensans", size = 10, colour = "#088395"),
    plot.caption = element_text(family = "opensans", size = 8, colour = "#088395"),
    axis.title = element_blank(),
    axis.text.y = element_text(family = "opensans", size = 18, colour = "#088395", face = "bold"),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E6C9", size = 0.6, linetype = "dashed"),
    legend.title = element_text(family = "opensans", size = 8),
    legend.text = element_text(family = "opensans", size = 8)
  )


ggsave("continous_supply_graph.png", continous_supply_graph, width = 8, height = 6, dpi = 200)
# Lets draw the percentage changes graph


changes_graph <- condinious_supply %>%
  ggplot(aes(x = 1, y = procurement_model)) +  # Force all points to be on x = 1
  geom_point(aes(size = change_perc), shape = 21, color = "#626F47", fill = "#FFB200", stroke = 1) +
  geom_text(aes(label = paste0("-", change_perc, "%")), 
            size = 7, fontface = "bold",  
           family = "opensans", color = "#ECEFDC") +
  scale_size_continuous(range = c(20, 30)) +
  scale_x_continuous(breaks = NULL) +  # Remove x-axis labels
  annotate("text", x = 1, y = 3.5, label = "Change", lineheight = 1,
           family = "opensans", size = 9, fontface = "bold", color = "#FFB200") +
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
  plot_annotation(title = "Using breakdown days as the measure of continous supply of\ncommodities to schools, commune level centralisation was the most\nefficient model from 2023-24.",
                  caption = "Source: Data from SFIS", 
                  theme = theme(plot.background = element_rect(fill = "#ECEFDC", color = "#ECEFDC"),
                                plot.title = element_text(family = "opensans", size = 30, face = "bold", colour = "#088395", lineheight = 0.5), 
                                plot.subtitle = element_text(family = "opensans", size = 23, colour = "#088395", lineheight = 0.5, face = "bold.italic",
                                                             margin = margin(b = 0)), 
                                plot.caption = element_text(family = "opensans", size = 15, colour = "#088395", hjust = 0))) 

ggsave("figures/complete_plot.png", complete_plot, width = 9.72, height = 6.52, dpi = 300, units = "in", device = "png")  
  


######################################################################################################

#2. School Suppliers Costs

supplier_costs <- average_wetcosts %>% 
  left_join(average_drycosts, by = "procurement") %>%
  mutate(schools = c(20, 23, 31),
         wet_cost_per_school = (wetcosts*wetsuppliers)/schools,
         dry_cost_per_school = (drycosts*drysuppliers)/schools) %>% 
  pivot_longer(cols = wet_cost_per_school:dry_cost_per_school, names_to = "cost_type", values_to = "cost_per_school") %>% 
  mutate(cost_type = case_when(
    cost_type == "wet_cost_per_school" ~ "Wet Commodities",
    cost_type == "dry_cost_per_school" ~ "Dry Commodities"))


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
  plot_annotation(title = "Ta Lou Senchey district have the highest density of schools and well connected\nby major roads.",
                  caption = "Source: Data from SFIS", 
                  theme = theme(plot.background = element_rect(fill = "#ECEFDC", color = "#ECEFDC"),
                                plot.title = element_text(family = "garamond", size = 23, face = "bold", colour = "#088395", lineheight = 0.5), 
                                plot.subtitle = element_text(family = "garamond", size = 19, colour = "#088395", lineheight = 0.5, face = "bold.italic",
                                                             margin = margin(b = 0)), 
                                plot.caption = element_text(family = "garamond", size = 15, colour = "#088395", hjust = 0)))


ggsave("figures/complete_map.png", complete_map, width = 7.15, height = 5.02, dpi = 300, units = "in", device = "png")



######################################################################################

# Stages of the procurement process improved graph


ImprovementGraphPP_01 <- ImprovementTable %>% 
  filter(Pilot == "Non-Procurement Pilot") %>%
  ggplot(aes(x = TenderProcessImp, y = Percentage)) +
  geom_bar(stat = "identity", fill =  "#441752", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 0), "% (", Number, ")")), 
            hjust = 0.5, color = "#441752", size = 6, nudge_y = 2,
            family = "garamond", fontface = "bold") + 
  coord_cartesian(ylim = c(0, 80)) +
  annotate(
    "segment", x = 1.28, xend = 2, y = 68, yend = 46,
    color = "#441752", size = 0.5, arrow = arrow(type = "closed", length = unit(0.1, "inches"))
  )+
  annotate(
    "text", x = 2.5, y = 40, label = "Contract management and implementation\nstage are still the most challenging\nin non-pilot districts compared to other\nstages...", 
    family = "garamond", size = 5.8, color = "#441752", lineheight = 0.7) +
  annotate(
    "text", x = 2.5, y = 75, label = "Non-Procurement\nPilot Districts",
    family = "garamond", size = 7, color = "#441752", lineheight = 0.7, fontface = "bold") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +  # Wrap x-axis labels
  scale_y_continuous(expand = c(0, 2)) + 
  theme_clean() +  # Set the theme
  labs(
    caption = ""
  ) + 
  theme(
    plot.background = element_rect(fill = "#FEF5DA", color = "#FEF5DA"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, lineheight = 0.5, face = "bold", family = "garamond", color = "#441752", size = 13),
    axis.line.x = element_line(color = "#441752", size = 0.5),
    axis.text.y = element_text(angle = 0, hjust = 0.5, lineheight = 0.5, face = "bold", family = "garamond", color = "#441752", size = 13),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 8, family = "garamond", hjust = 0.3, face = "bold", color = "#441752", margin = margin(b = 10)), 
    strip.background = element_rect(fill = "#FEF5DA", color = "#FEF5DA"),
    plot.caption = element_text(hjust = 0, size = 5, family = "garamond", color = "#441752", face = "bold"),
    plot.caption.position = "plot"
    
  )

# Second graph
ImprovementGraphPP_02 <- ImprovementTable %>% 
  filter(Pilot != "Non-Procurement Pilot") %>%
  ggplot(aes(x = TenderProcessImp, y = Percentage)) +
  geom_bar(stat = "identity", fill =  "#F29F58", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 0), "% (", Number, ")")), 
            hjust = 0.5, color = "#441752", size = 6, nudge_y = 2,
            family = "garamond", fontface = "bold") + 
  coord_cartesian(ylim = c(0, 80)) +
  annotate(
    "segment", x = 1, xend = 1.2, y = 18, yend = 48,
    color = "#441752", size = 0.5, arrow = arrow(type = "closed", length = unit(0.1, "inches"))
  )+
  annotate(
    "text", x = 2, y = 53, label = "...while in procurement pilot districts, the\ncontract management and implementation stage\nseems to be the most improved.", 
    family = "garamond", size = 7, color = "#441752", lineheight = 0.5) +
  annotate(
    "text", x = 2.5, y = 75, label = "Procurement Pilot\nDistricts",
    family = "garamond", size = 7, color = "#441752", lineheight = 0.7, fontface = "bold") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +  # Wrap x-axis labels
  scale_y_continuous(expand = c(0, 2)) + 
  theme_clean() +  # Set the theme
  labs(
    caption = ""
  ) + 
  theme(
    plot.background = element_rect(fill = "#FEF5DA", color = "#FEF5DA"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, lineheight = 0.5, family = "garamond", color = "#441752", size = 13, face = "bold"),
    axis.line.x = element_line(color = "#441752", size = 0.5),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 8, family = "garamond", hjust = 0.3, face = "bold", color = "#441752", margin = margin(b = 10)), 
    strip.background = element_rect(fill = "#FEF5DA", color = "#FEF5DA"),
    plot.caption = element_text(hjust = 0, size = 5, family = "garamond", color = "#441752", face = "bold"),
    plot.caption.position = "plot"
    
  )


# Merge the two graphs

complete_survey_graph <- ImprovementGraphPP_01 + ImprovementGraphPP_02 + plot_layout(widths = c(1, 1)) +
  plot_annotation(title = "Procurement Pilot Districts have shown significant improvements in the\ncontract management and implementation stage compared to non-pilot districts.",
                  subtitle = "", 
                  caption = "Source: Data from Survey", 
                  theme = theme(plot.background = element_rect(fill = "#FEF5DA", color = "#FEF5DA"),
                                plot.title = element_text(family = "garamond", size = 23, face = "bold", colour = "#441752", lineheight = 0.5), 
                                plot.subtitle = element_text(family = "garamond", size = 19, colour = "#441752", lineheight = 0.5, face = "bold.italic",
                                                             margin = margin(b = 5)), 
                                plot.caption = element_text(family = "garamond", size = 15, colour = "#441752", hjust = 0)))


ggsave("figures/complete_survey_graph.png", complete_survey_graph, width = 7.46, height = 6.52, dpi = 300, units = "in", device = "png")


#######################################################################################

# Map on the school connectedness


districts_sf <- districts_sf %>%
  mutate(
    text_color = if_else(District == "Ta Lou Senchey", "#240750", "#E4E0E1"), # Specify your desired colors
    nudge_y = if_else(District == "Ta Lou Senchey", 0.18, -0.005),
    nudge_x = if_else(District == "Ta Lou Senchey", -0.15, 0)
  )
remote_schools <- "One of the most\n'inaccessible' schools,\n(Or Soam), is the biggest\nbeneficiary of district\ncentralisation"
school_connect_graph <- ggplot() + 
  geom_sf(data = districts_sf, fill  = "#202040", color = "#E8F9FD", size = 1.5) +
  #geom_sf(data = water_in_boundaries, fill = "#478CCF", color = "#478CCF") + 
  geom_sf_text(
    data = districts_sf, 
    aes(label = if_else(districts_sf$District != "Ta Lou Senchey", District, "")), 
    size = 7, 
    fontface = "bold",
    color = districts_sf$text_color, 
    family = "opensans", 
    nudge_y = if_else(districts_sf$District == "Ta Lou Senchey", 0.18, -0.01),
    nudge_x = if_else(
      districts_sf$District == "Phnum Kravanh", -0.1, 
      if_else(districts_sf$District == "Ta Lou Senchey", -0.10, 0)
    )
  ) + 
  geom_sf(data = roads_in_boundaries, color =  "#FEFBF6", size = 0.05, alpha = 0.2) + 
  geom_sf(data = schools_sf, fill = "#E6B325", color = "#E6B325", aes(size = Students), alpha = 0.5, shape = 21) + 
  coord_sf(expand = FALSE) +
  annotate(
    "text",
    x = 103.4,
    y = 12.64,
    label = "Ta Lou\nSenchey",
    size = 7,
    family = "opensans",
    fontface = "bold",
    color = "#240750",
    lineheight = 0.5,
    hjust = 0
  ) + 
  annotate(
    "segment",
    x = 103.5,
    xend = 103.6,
    y = 12.60,
    yend = 12.53,
    color = "#240750",
    #curvature = -0.2,
    arrow = arrow(type = "closed", length = unit(0.05, "inches"), ends = "last")) +
  annotate(
    "text",
    label = remote_schools,
    x = 103.1,
    y = 11.95,
    size = 8,
    family = "opensans",
    lineheight = 0.5,
    hjust = 0,
    color = "#4D3509"
  ) + 
  annotate(
    "segment",
    x = 103.74,
    xend = 103.58,
    y = 12.23,
    yend = 12.18,
    color = "#E6B325",
    linewidth = 0.7,
    #alpha = 0.5,
    arrow = arrow(type = "closed", length = unit(0.05, "inches"), ends = "first")) +
  annotate(
    "segment",
    x = 103.58,
    xend = 103.47,
    y = 12.18,
    yend = 12.03,
    color = "#E6B325",
    linewidth = 0.7,
    #alpha = 0.5,
    arrow = arrow(type = "closed", length = unit(0.05, "inches"), ends = "last")) +
  geom_point(
    data = data.frame(x = 103.7632, y = 12.23453), 
    aes(x = x, y = y), 
    size = 5,  # Adjust as needed
    color = "#E6B325",
    fill = NA,
    #alpha = 0.5,
    stroke = 1.5,
    shape = 21
  ) +
  theme_void() +
  labs(
    title = "School Accessibility (Road Network)",
    size = "Average Eating Students"
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    plot.title = element_text(size = 20, hjust = 0.5, family = "opensans", face = "bold", margin = margin(b = 2, t = 10)),
    legend.position = "none",
    legend.title = element_text(size = 11, family = "opensans", face = "bold", margin = margin(b = -0.5)),
    legend.text = element_text(size = 10, face = "bold", family = "opensans", margin = margin(t = 2)),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.caption = element_text(size = 11, family = "opensans", hjust = 0, face = "bold"),
    plot.caption.position = "plot"
  ) + 
  scale_size_continuous(range = c(0.3, 2.5)) +
  guides(size = guide_legend(title.position = "top", title.hjust = 0.5))

ggsave(
  "figures/school_connect_graph.png",
  plot = school_connect_graph,
  width = 6.5, height = 6.5, dpi = 300,
  bg = "transparent"
)


#######################################################################################

clean_bdd <- annual_breakdowndays %>% 
  filter(pilot != "PursatNon-Pilot") %>% 
  select(-change) %>% 
  mutate(change_percent = abs(change_percent)) %>% 
  pivot_longer(cols = c(`2023`, `2024`), names_to = "year", values_to = "breakdown_days") %>% 
  mutate(pilot = case_when(
    pilot == "Commune Centralisation" ~  "Commune\nCentralisation\n(Ta Lou Senchey)",
    pilot == "District Centralisation" ~ "District\nCentralisation\n(Phnum Kravanh)",
    pilot == "Non-Pilot" ~ "Non-Pilot\n(All other HGSF\ndistricts)"
  )) %>%
  mutate(year = factor(year, levels = c("2023", "2024")),
         pilot = factor(pilot, levels = c("Non-Pilot\n(All other HGSF\ndistricts)", 
                                          "Commune\nCentralisation\n(Ta Lou Senchey)", 
                                          "District\nCentralisation\n(Phnum Kravanh)")))

kravanh_clean <- connected_kravanh %>% 
  select(-change) %>%
  mutate(change_percent = abs(change_percent)) %>% 
  pivot_longer(cols = c(`2023`, `2024`), names_to = "year", values_to = "breakdown_days") %>%
  mutate(year = factor(year, levels = c("2023", "2024"))) %>% 
  rename("pilot" = roadconected) %>% 
  mutate(pilot = factor(pilot))

complete_bdd <- kravanh_clean %>% 
  rbind(clean_bdd)





bbdays_heatmap <- ggplot(clean_bdd, aes(x = year, y = pilot, fill = breakdown_days)) +
  geom_tile() +
  geom_text(aes(label = round(breakdown_days, 0)), color = if_else(clean_bdd$year == "2024", "#500073", "#D4EBF8"), size = 9,
            family = "opensans", fontface = "bold") +
  scale_fill_gradient(low = "#A1E3F9", high = "#2A004E") +
  labs(title = "Average Breakdown Days by Procurement Model and\nAnnual School Year",
       fill = "Breakdown Days") +
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "#F6F8EE", color = "#F6F8EE"),
    plot.title = element_text(family = "opensans", size = 20, face = "bold", colour = "#2A004E", hjust = 0, lineheight = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family = "opensans", size = 15, colour = "#2A004E", lineheight = 0.5, face = "bold.italic",
                                 margin = margin(b = 0)), 
    plot.caption = element_text(family = "opensans", size = 15, colour = "#2A004E", hjust = 0),
    axis.text = element_text(family = "opensans", size = 18, colour = "#2A004E", hjust = 0.5, lineheight = 0.5),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(family = "opensans", size = 15, colour = "#2A004E", hjust = 0.5,
                                margin = margin(b = 1.5, t = 0)),
    legend.title.position = "top",
    legend.text = element_text(family = "opensans", size = 15, colour = "#2A004E", hjust = 0.5),
    legend.box.margin = margin(b = 0, t= -10),
    legend.key.height = unit(0.2, "cm"),
    legend.key.width = unit(1.2, "cm"),
    plot.caption.position = "plot"
  ) 

ggsave("figures/bbdays_heatmap.png", bbdays_heatmap, width = 5.4, 
       height = 4, dpi = 300, units = "in", device = "png", bg = "white")  


# Percentage change heatmap

bbdays_heatmap <- ggplot(clean_bdd %>% filter(year == 2024), aes(x = year, y = pilot, fill = change_percent)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(change_percent, 0), "%")), color =  "#D4EBF8", size = 9,
            family = "opensans", fontface = "bold") +
  scale_fill_gradient(low = "#EAD196", high = "#7D0A0A") +
  labs(title = "Percentage\nDecrease",
       fill = "Percentage Change", y = "") +
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "#F6F8EE", color = "#F6F8EE"),
    plot.title = element_text(family = "opensans", size = 20, face = "bold", colour = "#7D0A0A", hjust = 0, lineheight = 0.5),
    plot.title.position = "plot",
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "opensans", size = 18, colour = "#7D0A0A", hjust = 0.5, lineheight = 0.5,
                              margin = margin(b = 45)),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(family = "opensans", size = 15, colour = "#7D0A0A", hjust = 0.5,
                                margin = margin(t = 1.5, b = 0)),
    legend.title.position = "left",
    legend.text = element_text(family = "opensans", size = 15, colour = "#7D0A0A", hjust = 0.5),
    legend.box.margin = margin(b = 0, l= -10),
    legend.key.height = unit(0.9, "cm"),
    legend.key.width = unit(0.2, "cm")) + 
  guides(fill = guide_colourbar(title.theme = element_text(angle = 90)))
  

ggsave("figures/bbdays_heatmap_change.png", bbdays_heatmap, width = 1.7, 
       height = 4, dpi = 300, units = "in", device = "png", bg = "white")














