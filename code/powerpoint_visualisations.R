#Loading relevant libraries

library(tidyverse)
library(sf)
library(showtext)
library(lubridate)


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
  geom_line(aes(group = procurement_model), color = "#088395", size = 1.5) +
  geom_point(aes(x = avg_days), shape = 21, size = 3, fill = if_else(
    condinious_supply$year == 2024, "#FFB200", "#626F47"
  ), color = "white") +
  geom_text(aes(label = paste0(avg_days, " days")), hjust = -0.1, 
            nudge_x =  -0.2, size = 3,
            nudge_y = -0.15, family = "garamond", color = "#088395") +
  annotate("text", 
           x = 0.49, y = 3.2, label = "2023", 
           family = "garamond", 
           size = 3,
           fontface = "bold",
           color = "#FFB200") +
  annotate("text", 
           x = 1.88, y = 3.2, label = "2024", 
           family = "garamond", 
           size = 3,
           fontface = "bold",
           color = "#626F47") +
  labs(
    title = "Continous Supply of Commodities",
    subtitle = "Average Days to Deliver Commodities to Schools",
    x = "Year",
    y = "Average Days",
    caption = "Source: Ministry of Education",
    color = "Procurement Model"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#ECEFDC"),
    plot.title = element_text(family = "garamond", size = 14, face = "bold"),
    plot.subtitle = element_text(family = "garamond", size = 10),
    plot.caption = element_text(family = "garamond", size = 8),
    axis.title = element_text(family = "garamond", size = 10),
    axis.text = element_text(family = "garamond", size = 8),
    legend.title = element_text(family = "garamond", size = 8),
    legend.text = element_text(family = "garamond", size = 8)
  )
