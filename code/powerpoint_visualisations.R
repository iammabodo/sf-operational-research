#Loading relevant libraries

library(tidyverse)
library(sf)
library(showtext)
library(lubridate)
library(patchwork)


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
  

























