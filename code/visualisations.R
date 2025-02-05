#Loading libraries

library(tidyverse)
library(ggthemes)
library(showtext)
library(ggforce)
library(marquee)
library(biscale)
library(cowplot)
library(sf)

########################################################################
#Text for the graphs - setting the defalts
font_add_google("Open Sans","opensans")
showtext_auto()
showtext_opts(dpi = 200)
########################################################################

# Visualise the relationship between the cost per child and the number of eating students

WetCostsPerChild <- FullTablesData %>% 
  filter(District != "Krakor") %>% 
  group_by(SchoolId, procurement, Year) %>%
  summarise(AvgStudents = mean(AvgStudents, na.rm = TRUE),
            WetCostsPerChild = mean(WetCostsPerChild, na.rm = TRUE),
            DryCostsPerChild = mean(DryCostsPerChild, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(AvgStudents > 0 & AvgStudents < 480 ) %>%
  filter(Year == 2024) %>% 
  #filter(WetCostsPerChild > quantile(WetCostsPerChild, 0.05) & WetCostsPerChild < quantile(WetCostsPerChild, 0.95)) %>%
  ggplot(aes(x = AvgStudents, y = WetCostsPerChild)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), colour = "black", size = 0.5) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue", linewidth = 0.5) + 
  #scale_y_log10() +
  facet_wrap(~ procurement, scales = "free") + 
  theme_clean() +
  coord_cartesian(ylim = c(1.5, 3)) +
  labs(title = "Relationship between the cost per child and the number of eating students (2024)",
       x = "Number of Eating Students",
       y = "Cost per Child (USD)") +
  theme(
    plot.title = element_text(size = 7, family = "opensans", face = "bold", lineheight = 2),
    axis.title.y = element_text(size = 6, family = "opensans", face = "bold", margin = margin(l = 5, r = 5)),
    axis.title.x = element_text(size = 6, family = "opensans", face = "bold", margin = margin(t = 10, b = 5)),
    axis.text.x = element_text(family = "opensans", size = 5),
    axis.text.y = element_text(family = "opensans", size = 5),
    strip.text = element_text(size = 5, face = "bold", family = "opensans", margin = margin(t = 10)),
    strip.background = element_rect(fill = "white"))

# Save the graph
ggsave("report/WetCostsPerChild.png", 
       plot = WetCostsPerChild, 
       width = 6.29, height = 3, dpi = 200)

# Visualise the relationship between the cost per child and the number of eating students using a line graph



costs_trend_graph <- PanelData %>% 
  #filter(!MonthYear %in% c("Jan 2023", "Apr 2023", "May 2023", "Oct 2023", "Nov 2023", "Apr 2024", "Jul 2024")) %>%
  ggplot(aes(x = MonthYear, y = AvgStudents, group = procurement, color = procurement)) +
  geom_line(linewidth = 0.7,
            linetype = "solid") +
  scale_x_discrete(breaks = c("Feb 2023", "Jun 2023", "Aug 2023", "Dec 2023", "Feb 2024", "May 2024", "Aug 2024")) +
  geom_point(size = 1) +
  geom_vline(xintercept = "Feb 2024", linetype = "dashed",
             linewidth = 0.7,
             colour = "darkblue") +
  # Move annotation text to the left of "Feb 2024"
  annotate("text", x = "Feb 2024", y = 3.5, label = "Procurement Pilot Starts", 
           angle = 90, vjust = -1, size = 1.8, color = "darkblue", fontface = "bold.italic",
           family = "opensans", lineheight = 1.2) +
  theme_clean() +
  labs(title = "Cost per Child over Time",
       x = "Month",
       y = "Cost per Child (USD)") +
  theme(
    plot.title = element_text(size = 7, face = "bold", lineheight = 2),
    axis.title.y = element_text(size = 6, family = "opensans", face = "bold", margin = margin(l = 5, r = 5)),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = "opensans", size = 5, color = "black"),
    axis.text.y = element_text(family = "opensans", size = 5, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 4, family = "opensans", color = "black"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.spacing = unit(0, "cm"),
    legend.position = "bottom",
    legend.background = element_blank())


# Save the graph
ggsave("report/costs_trend_graph.png", 
       plot = costs_trend_graph, 
       width = 6.29, height = 3, dpi = 200)

# Visualise MealsDays 
MealsDaysGraph <- MealsDays %>% 
  ggplot(aes(x = MonthYear, y = MeanCookingDays, group = procurement, color = procurement)) +
  geom_line(linewidth = 0.7,
            linetype = "solid") +
  scale_x_discrete(breaks = c("Feb 2023", "Jun 2023", "Aug 2023", "Dec 2023", "Feb 2024", "May 2024", "Aug 2024"))



ImprovementGraph <- ImprovementProcurement %>% 
  arrange(desc(Percentage)) %>%  # Arrange in descending order
  mutate(TenderProcessImp = factor(TenderProcessImp, levels = unique(TenderProcessImp))) %>%  # Wrap and reorder levels
  ggplot(aes(x = TenderProcessImp, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Set fill color for the bars
  coord_flip() +  # Flip the coordinates
  geom_text(aes(y = 2, label = str_wrap(TenderProcessImp, 10), lineheight = 1.2), 
            hjust = 0, color = "white", size = 2,
            family = "opensans") +  # Position labels at the start of the bar
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            hjust = 0, color = "white", size = 2, nudge_y = -4,
            family = "opensans", fontface = "bold") +  # Align percentages at the end of the bar
  theme_minimal() +  # Set the theme
  labs(title = "Stages of procurement process to be improved (Pilot)",
       x = "Percentage",
       y = "Improvement") +  # Set the labels
  theme(
    # Removing the axis titles and labels
    axis.title.y = element_blank(),  # Remove the y-axis title
    axis.text.y = element_blank(),  # Remove the y-axis labels
    axis.ticks.y = element_blank(),  # Remove the y-axis ticks
    axis.title.x = element_blank(),  # Remove the x-axis title
    axis.text.x = element_blank(),  # Set the size of the x-axis labels
    panel.grid.major.y = element_blank(),  # Remove the y-axis gridlines
    panel.grid.minor.y = element_blank(),  # Remove the y-axis gridlines
    panel.grid.major.x = element_blank(),  # Remove the x-axis gridlines
    panel.grid.minor.x = element_blank(),  # Remove the x-axis gridlines
    # Style the plot title
    plot.title = element_text(size = 8, family = "opensans", hjust = 0.2, face = "bold"))



# Save the graph
ggsave("report/ImprovementGraph2.png", 
       plot = ImprovementGraph, 
       width = 3.53, height = 3, dpi = 200,
       bg = "white")

newImprovementGraph <- ImprovementTable %>% 
  ggplot(aes(x = TenderProcessImp, y = Percentage)) +
  geom_bar(stat = "identity", fill = if_else(ImprovementTable$Pilot == "Non-Procurement Pilot", "steelblue", "grey"), width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 0), "% (", Number, ")")), 
            hjust = 0.5, color = "black", size = 2.5, nudge_y = 2,
            family = "opensans") + 
# Align percentages at the end of the bar
  facet_wrap(~Pilot, strip.position = "top") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +  # Wrap x-axis labels
  scale_y_continuous(expand = c(0, 2)) + 
  theme_clean() +  # Set the theme
  labs(
    caption = "Source: Supplier Survey"
  ) + 
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, lineheight = 0.8, family = "opensans", size = 7),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.text.y = element_text(family = "opensans", size = 7),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 8, family = "opensans", hjust = 0.3, face = "bold", margin = margin(b = 10)), 
    plot.caption = element_text(hjust = 0, size = 5, family = "opensans", color = "black", face = "bold"),
    plot.caption.position = "plot"
  
  )

# save the graph

ggsave("report/newImprovementGraph.png", 
       plot = newImprovementGraph, 
       width = 6.51, height = 3, dpi = 200,
       bg = "white")

incomeeffectgraph <- suppliers_data %>% 
  filter(!is.na(ProductsSupplied_2)) %>% 
  count(DirectSupplier, procurement, SIncStatus) %>% 
  filter(procurement != "Non-Procurement Pilots") %>%
  filter(DirectSupplier == "Supplying to contracted suppliers.") %>% 
  # Calculate the percentage of suppliers by income status
  group_by(procurement) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  #Round the percentage to 1 decimal place
  mutate(Percentage = round(Percentage, 1)) %>%
  ggplot(aes(x = SIncStatus, y = Percentage)) +
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
  facet_wrap(~procurement, strip.position = "bottom") + 
  coord_flip() + 
  geom_text(aes(label = Percentage), size = 2, 
            family = "opensans", fontface = "bold",
            nudge_y = 3) + 
  theme_minimal() + 
  labs(title = "Income status of smallholder farmers (%)",
       x = "Percentage",
       y = "Income Status") +
  theme(
    # Removing the axis titles and labels
    axis.title.y = element_blank(),  # Remove the y-axis title
    axis.text.y = element_text(size = 6, family = "opensans", face = "bold"),  # Remove the y-axis labels
    axis.ticks.y = element_blank(),  # Remove the y-axis ticks
    axis.title.x = element_blank(),  # Remove the x-axis title
    axis.text.x = element_blank(),  # Set the size of the x-axis labels
    #panel.grid.major.y = element_blank(),  # Remove the y-axis gridlines
    panel.grid.minor.y = element_blank(),  # Remove the y-axis gridlines
    panel.grid.major.x = element_blank(),  # Remove the x-axis gridlines
    panel.grid.minor.x = element_blank(),  # Remove the x-axis gridlines
    # Style the plot title
    plot.title = element_text(size = 8, family = "opensans", hjust = 0, face = "bold"),
    strip.text = element_text(size = 6, family = "opensans", margin = margin(t = 10), face = "bold"))


# Save the graph
ggsave(
  "report/incomeeffectgraph.png", 
  plot = incomeeffectgraph, 
  width = 6.29, height = 3, dpi = 200,
  bg = "white"
)


#######################################################################################

# Visualise Challenges faced by suppliers
ChallengesGraph <- Challenges %>% 
  arrange(Percentage) %>%  # Arrange Percentage in ascending order
  ggplot(aes(x = reorder(Challenges, Percentage), y = Percentage)) +  # Reorder Challenges based on Percentage
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~procurement, 
             strip.position = "bottom") +
  coord_flip() + 
  geom_text(aes(label = round(Percentage, 1)), 
            hjust = 0, color = "black", size = 2, nudge_y = 0,
            family = "opensans", fontface = "bold") +   # Align percentages at the end of the bar
  theme_minimal() +  # Set the theme
  labs(title = "Challenges faced by suppliers (%)",
       x = "Percentage",
       y = "Challenges") +  # Set the labels
  theme(
    # Removing the axis titles and labels
    axis.title.y = element_blank(),  # Remove the y-axis title
    axis.ticks.y = element_blank(),  # Remove the y-axis ticks
    axis.title.x = element_blank(),  # Remove the x-axis title
    axis.text.y = element_text(family = "opensans", size = 7, face = "bold", colour = "black"),  # Set the size of the x-axis labels
    axis.text.x = element_blank(),  # Remove the x-axis labels
    #panel.grid.major.y = element_blank(),  # Remove the y-axis gridlines
    panel.grid.minor.y = element_blank(),  # Remove the y-axis gridlines
    panel.grid.major.x = element_blank(),  # Remove the x-axis gridlines
    panel.grid.minor.x = element_blank(),  # Remove the x-axis gridlines
    # Style the plot title
    plot.title = element_text(size = 8, family = "opensans", hjust = 0.2, face = "bold"),
    strip.text = element_text(size = 7, family = "opensans", margin = margin(t = 10), face = "bold"))



# Save the graph
ggsave(
  "report/ChallengesGraph.png",
  plot = ChallengesGraph,
  width = 6.29, height = 3, dpi = 200,
  bg = "white"
)


##########################################################################################

# Visualise wet costs by procurement type

drycosts_data %>%
  ggplot(aes(x = procurement, y = Percentage, fill = `Costs Share`)) +  # Reorder wetsuppliers
  geom_bar(stat = "identity", position = position_stack()) +  # Create stacked bar chart
  geom_text(aes(label = paste0(round(Percentage, 0), "%")), 
           size = 3, color = "black", nudge_y = 15) +  # Add percentage labels
  facet_wrap(~`Costs Share`, nrow = 1) +  # Facet by procurement
  coord_flip() +  # Flip the coordinates
  labs(title = "Cost Share Distribution by Procurement Type",
       x = "Wet Suppliers",
       y = "Percentage",
       fill = "Cost Share") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



BiddingExperience <- suppliers_data %>% 
  filter(!is.na(TenderProcessExp)) %>% 
  filter(procurement != "Non-Procurement Pilots") %>%
  group_by(TenderProcessExp) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  ggplot(aes(x = TenderProcessExp, y = n)) +  # Fill inside aes()
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
  geom_text(aes(label = n), 
            size = 2, 
            family = "opensans", 
            fontface = "bold",
            nudge_y = 0.5) + 
  theme_clean() +
  labs(title = "Number of suppliers by tender process experience",
       x = "Tender Process Experience",
       y = "Number of Suppliers") +
  theme(
    plot.title = element_text(size = 7, family = "opensans", face = "bold", lineheight = 2),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 6, family = "opensans", face = "bold", margin = margin(t = 10, b = 5)),
    axis.text.x = element_text(family = "opensans", size = 5, face = "bold"),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.text.y = element_blank())


ggsave(
  "report/BiddingExperience.png",
  plot = BiddingExperience,
  width = 3.48, height = 3, dpi = 200,
  bg = "white"
)
 
##################################################################################################

# Evidence generation visualisation


costs_trend_graph <- PanelData %>% 
  filter(!MonthYear %in% c("Jan 2023", "Apr 2023", "May 2023", "Oct 2023", "Apr 2024")) %>%
  ggplot(aes(x = MonthYear, y = AvgTotalCost, group = procurement, color = procurement)) +
  geom_line(linewidth = 0.7,
            linetype = "solid") +
  scale_x_discrete(breaks = c("Feb 2023", "Jun 2023", "Sep 2023", "Jan 2024", "Feb 2024", "May 2024", "Aug 2024"), expand = c(0,1)) +
  geom_point(size = 1) +
  annotate(
    geom = "rect",
    xmin = 0, xmax = "Feb 2024",
    ymin = 0, ymax = 9.4,
    fill = "grey",
    alpha = 0.2) +
  annotate(
    "marquee",
    x = "Aug 2023",
    y = 6,
    label = pre_pilot_phase,
    width = 0.3,
    color = "black",
    size = 3,
    family = "opensans") +
  annotate(
    "marquee",
    x = "Jun 2024",
    y = 1.5,
    label = piolt_phase,
    width = 0.3,
    color = "black",
    size = 3,
    family = "opensans") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_clean() +
  labs(title = "",
       x = "Month",
       y = "Cost per Child (USD)") +
  theme(
    plot.title = element_text(size = 7, face = "bold", lineheight = 2),
    axis.title.y = element_text(size = 8, family = "opensans", face = "bold", margin = margin(l = 5, r = 5)),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = "opensans", size = 6, color = "black"),
    axis.text.y = element_text(family = "opensans", size = 7, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 6, family = "opensans", color = "black"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.spacing = unit(0, "cm"),
    legend.position = "bottom",
    legend.background = element_blank())


ggsave(
  "report/costs_trend_graph.png", 
  plot = costs_trend_graph, 
  width = 6.49, height = 3, dpi = 300
)

# Buble chart for the costs

costs <- tibble(
  Pilot = c("Commune", "District", "Non-Pilot"),
  `Dry Costs` = c(120, 244, 233),
  `Wet Costs` = c(36, 55, 121),
  Schools = c(20, 23, 31)
) %>% 
  pivot_longer(cols = c(`Dry Costs`, `Wet Costs`), names_to = "Costs", values_to = "Cost") 



supplier_costsgraph <- costs %>% 
  ggplot(aes(x = Pilot, y = Cost, size = Cost)) +
  # Add background rectangles with different colors for each facet
  geom_rect(data = costs %>% filter(Costs == "Dry Costs"),
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill = "#FEFFD2", color = NA, alpha = 0.3) +
  new_scale_fill() +
  geom_rect(data = costs %>% filter(Costs == "Wet Costs"),
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill = "#F5F5F5", color = NA, alpha = 0.3) +
  
  # Main plot layers
  geom_point(aes(x = Pilot, y = Cost, size = Cost), 
             color = if_else(costs$Costs == "Dry Costs", "red", "black"),
             fill = if_else(costs$Costs == "Dry Costs", "red", "black"),
             shape = 21) +
  geom_text(data = costs %>% filter(Costs == "Dry Costs"), 
            aes(x = Pilot, y = Cost, label = paste0("$", Cost)), 
            size = 3, 
            family = "opensans",
            fontface = "bold",
            color = "white") +
  geom_text(data = costs %>% filter(Costs == "Dry Costs"), 
            aes(x = Pilot, y = Cost, label = Pilot), 
            size = 3, 
            family = "opensans",
            fontface = "bold",
            color = "black",
            nudge_y = -35) +
  geom_text(data = costs %>% filter(Costs == "Wet Costs"), 
            aes(x = Pilot, y = Cost, label = paste0("$", Cost)), 
            size = 2, 
            family = "opensans",
            fontface = "bold",
            color = "white") + 
  geom_text(data = costs %>% filter(Costs == "Wet Costs"), 
            aes(x = Pilot, y = Cost, label = Pilot), 
            size = 3, 
            family = "opensans",
            fontface = "bold",
            color = "black",
            nudge_y = 35) + 
  facet_wrap(~Costs) + 
  labs(
    x = element_blank(),
    y = element_blank(),
    title = "Costs of delivering Dry and Wet commodities to the schools",
    subtitle = "",
    caption = "Source: Supplier Survey"
  ) + 
  theme_minimal(base_size = 12, base_family = "opensans") +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.spacing = unit(0, "lines"),  # Narrow the gap between facets
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey", size = 0.5, linetype = "dashed"),
    plot.caption = element_text(hjust = 0, size = 8,  family = "opensans"),
    plot.caption.position = "plot"
  ) + 
  scale_size_area(max_size = 25) + 
  coord_cartesian(ylim = c(0, 300))


wet_commodities_graph <- costs %>% 
  filter(Costs == "Wet Costs") %>%
  ggplot(aes(x = Pilot, y = Cost, size = Cost)) +
  geom_point(aes(color = "#3A1078"), shape = 21, fill = "#3A1078", stroke = 0) +
  geom_text(aes(label = paste0("$",Cost), size = 6, family = "opensans", fontface = "bold"), color = "#E3F1FD") +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = "Total costs of delivering Wet Commodities to the schools (Daily)",
    subtitle = "",
    caption = "Source: Supplier Survey"
  ) +
  annotate(
    "curve",
    x = 1,
    xend = 1.75,
    y = 48,
    yend = 120,
    color = "#3A1078",
    curvature = -0.3,
    arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "first")) +
  annotate(
    "point",
    x = 1.75,
    y = 120,
    color = "#3A1078",
    fill = "#3A1078",
    size = 2,
    shape = 21) +
  annotate(
    "marquee",
    x = 2.3,
    y = 120,
    label = "Commune-level suppliers also incur less costs per single supply of wet commodities (daily) compared to other supply modalities",
    width = 0.3,
    color = "#3A1078",
    size = 4,
    family = "opensans") +
  theme_minimal(base_size = 12, base_family = "opensans") +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 15, family = "opensans", face = "bold", colour = "#3A1078"),
    panel.spacing = unit(0, "lines"),  # Narrow the gap between facets
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D5C5EB", size = 0.5, linetype = "dashed"),
    plot.caption = element_text(hjust = 0, size = 10,  family = "opensans", face = "bold", color = "#3A1078"),
    plot.caption.position = "plot",
    plot.title = element_text(size = 18, family = "opensans", face = "bold", hjust = 0.5, color = "#3A1078")) +
  scale_size_area(max_size = 25) +
  coord_cartesian(ylim = c(0, 150))

ggsave(
  "report/wet_costsgraph.png",
  plot = wet_commodities_graph,
  width = 6.67, height = 4.63, dpi = 300,
  bg = "#E3F1FD"
)



dry_commodities_graph <- costs %>% 
  filter(Costs == "Dry Costs") %>%
  ggplot(aes(x = Pilot, y = Cost, size = Cost)) +
  geom_point(aes(color = "#740938"), shape = 21, fill = "#740938", stroke =  0) +
  geom_text(aes(label = paste0("$",Cost), size = 9, family = "opensans", fontface = "bold"), color = "#FFFFF2") +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = "Total costs of delivering Dry Commodities to the schools (Monthly)",
    subtitle = "",
    caption = "Source: Supplier Survey"
  ) +
  annotate(
    "segment",
    x = 1.16,
    xend = 1.75,
    y = 120,
    yend = 120,
    color = "#740938",
    linewidth = 0.5
  ) + 
  annotate(
    "point",
    x = 1.75,
    y = 120,
    color = "#740938",
    fill = "#740938",
    size = 2,
    shape = 21) + 
  annotate(
    "point",
    x = 1.16,
    y = 120,
    color = "#740938",
    fill = "#740938",
    size = 2,
    shape = 21) +
  annotate(
    "marquee",
    x = 2.3,
    y = 120,
    label = "Commune-level suppliers incur less costs per single supply of dry commodities (monthly) compared to District-level and non-pilot suppliers",
    width = 0.3,
    color = "#740938",
    size = 4,
    family = "opensans") +
  theme_minimal(base_size = 12, base_family = "opensans") +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 15, family = "opensans", face = "bold"),
    panel.spacing = unit(0, "lines"),  # Narrow the gap between facets
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey", size = 0.5, linetype = "dashed"),
    plot.caption = element_text(hjust = 0, size = 10,  family = "opensans", face = "bold", color = "#740938"),
    plot.caption.position = "plot",
    plot.title = element_text(size = 18, family = "opensans", face = "bold", hjust = 0.5, color = "#740938")) +
  scale_size_area(max_size = 25) +
  coord_cartesian(ylim = c(0, 300))

ggsave(
  "report/dry_costsgraph.png",
  plot = dry_commodities_graph,
  width = 6.67, height = 4.63, dpi = 300,
  bg = "#FFFFF2"
)

#############################################################################################################################
FullTablesData %>% 
  group_by(procurement, Year) %>%  
  summarise(meanrice = mean(RiceCostsPerChild, na.rm = TRUE)) %>% 
  mutate(meanrice = meanrice/4100) %>% 
  pivot_wider(names_from = Year, values_from = meanrice) %>% 
  #calculate percentage change
  mutate(percentage_change = ((`2024` - `2023`)/`2023`) * 100)

# Calculate average eating students per pilot model between 2023 and 2024
FullTablesData %>% 
  group_by(procurement, Year) %>%  
  summarise(meanstudents = mean(AvgStudents, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Year, values_from = meanstudents) %>% 
  #calculate percentage change
  mutate(percentage_change = ((`2024` - `2023`)/`2023`) * 100)

# Calculate average salt costs per child per pilot model between 2023 and 2024
FullTablesData %>% 
  group_by(procurement, Year) %>%  
  summarise(meansalt = mean(SaltCostsPerChild, na.rm = TRUE)) %>% 
  mutate(meansalt = meansalt/4100) %>% 
  pivot_wider(names_from = Year, values_from = meansalt) %>% 
  #calculate percentage change
  mutate(percentage_change = ((`2024` - `2023`)/`2023`) * 100)


# Calculate average oil costs per child per pilot model between 2023 and 2024
FullTablesData %>% 
  group_by(procurement, Year) %>%  
  summarise(meanoil = mean(OilCostsPerChild, na.rm = TRUE)) %>% 
  mutate(meanoil = meanoil/4100) %>% 
  pivot_wider(names_from = Year, values_from = meanoil) %>% 
  #calculate percentage change
  mutate(percentage_change = ((`2024` - `2023`)/`2023`) * 100)

# Calculate dry costs per child per pilot model between 2023 and 2024
FullTablesData %>% 
  group_by(procurement, Year) %>%  
  summarise(meandry = mean(DryCostsPerChild, na.rm = TRUE)) %>% 
  #mutate(meandry = meandry/4100) %>%
  pivot_wider(names_from = Year, values_from = meandry) %>%
  #calculate percentage change
  mutate(percentage_change = ((`2024` - `2023`)/`2023`) * 100)


# Cost Efficiency Graph - Evidence Generation

WetCostsPerChildEG <- FullTablesData %>% 
  filter(District != "Krakor") %>% 
  group_by(SchoolId, procurement, Year) %>%
  summarise(AvgStudents = mean(AvgStudents, na.rm = TRUE),
            WetCostsPerChild = mean(WetCostsPerChild, na.rm = TRUE),
            DryCostsPerChild = mean(DryCostsPerChild, na.rm = TRUE),
            RiceCostsPerChild = mean(RiceCostsPerChild, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(AvgStudents > 0 & AvgStudents < 500 ) %>%
  #filter(Year == 2024) %>% 
  #filter(WetCostsPerChild > quantile(WetCostsPerChild, 0.05) & WetCostsPerChild < quantile(WetCostsPerChild, 0.95)) %>%
  ggplot(aes(x = AvgStudents, y = WetCostsPerChild)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), size = 0.8) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "#1230AE", linewidth = 0.5, fill = "#7F8FE5") + 
  #scale_y_log10() +
  facet_grid(Year~ procurement, scales = "free") + 
  theme_clean() +
  coord_cartesian(ylim = c(1.5, 2.6)) +
  labs(title = "Relationship between the monthly cost per child and the number of eating students (2024)",
       x = "Number of Eating Students",
       y = "Monthly Cost per Child (USD)",
       caption = "Data Source: SFIS") +
  theme(
    plot.title = element_text(size = 16, family = "opensans", face = "bold", hjust = 0.5, lineheight = 2, color = "#1230AE"),
    axis.title.y = element_text(size = 14, family = "opensans", face = "bold", margin = margin(l = 5, r = 5), color = "#1230AE"),
    axis.title.x = element_text(size = 14, family = "opensans", face = "bold", margin = margin(t = 10, b = 5), color = "#1230AE"),
    axis.text.x = element_text(family = "opensans", size = 12, color = "#1230AE", face = "bold"),
    axis.line.x = element_line(color = "#1230AE"),
    axis.line.y = element_line(color = "#1230AE"),
    axis.ticks = element_line(color = "#1230AE"),
    axis.text.y = element_text(family = "opensans", size = 10, color = "#1230AE", face = "bold"),
    panel.background = element_rect(fill = "#FBFBFB", color = "#FBFBFB"),
    plot.background = element_rect(fill = "#FBFBFB", color = "#FBFBFB"),
    plot.title.position = "plot",
    plot.caption = element_text(size = 10, hjust = 0, family = "opensans", face = "bold", color = "#1230AE"),
    plot.caption.position = "plot",
    panel.border = element_blank(),
    strip.text = element_text(size = 12, face = "bold", family = "opensans", margin = margin(t = 10), color = "#1230AE"),
    strip.background = element_rect(fill = "#FBFBFB"))

# Save the graph
ggsave("report/WetCostsPerChildEG.png", 
       plot = WetCostsPerChildEG, 
       width = 9.14, height = 5.36, dpi = 300)

##############################################################################################################

# Cost per child trend graph - Evidence Generation Workshop

pre_pilot_phase <- "Period before the procurement pilot starts (Feb 2023 - Jan 2024)"

piolt_phase <- "Period after the procurement pilot starts (Feb 2024 - Aug 2024)"

peak <- "Three moths payment due to bulk delivery of fortifed rice"

costs_trend_graphEG <- PanelData %>% 
  filter(!MonthYear %in% c("Jan 2023", "Apr 2023", "May 2023", "Oct 2023", "Apr 2024")) %>%
  ggplot(aes(x = MonthYear, y = AvgTotalCost, group = procurement, color = procurement)) +
  geom_line(linewidth = 0.7, linetype = "solid") +
  scale_x_discrete(breaks = c("Feb 2023", "Jun 2023", "Aug 2023", "Dec 2023", "Feb 2024", "May 2024", "Aug 2024"),
                   expand = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_point(size = 1) +
  theme_clean() +
  annotate(
    geom = "rect",
    xmin = 0, xmax = "Feb 2024",
    ymin = 0, ymax = 9.4,
    fill = "grey",
    alpha = 0.2) +
  annotate(
    "marquee",
    x = "Aug 2023",
    y = 5.5,
    label = pre_pilot_phase,
    width = 0.3,
    color = "black",
    size = 3.5,
    family = "opensans") +
  annotate(
    "marquee",
    x = "Jun 2024",
    y = 1.5,
    label = piolt_phase,
    width = 0.3,
    color = "black",
    size = 3.5,
    family = "opensans") +
  annotate(
    "marquee",
    x = "May 2024",
    y = 6,
    label = peak,
    width = 0.2,
    color = "black",
    size = 3.5,
    family = "opensans") +
  annotate(
    "curve",
    x = "May 2024",
    xend = "Jul 2024",
    y = 6.5,
    yend = 8.9,
    color = "black",
    curvature = -0.3,
    arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "first")) +
  labs(title = "Total Monthly Cost per Child over Time",
       x = "Month",
       y = "Total Monthly Cost per Child (US$)",
       caption = "Data Source: SFIS") +
  theme(
    plot.title = element_text(size = 18, face = "bold", lineheight = 2, hjust = 0.5, family = "opensans"),
    axis.title.y = element_text(size = 12, family = "opensans", face = "bold", margin = margin(l = 5, r = 5)),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = "opensans", size = 12, color = "black", face = "bold"),
    axis.text.y = element_text(family = "opensans", size = 12, color = "black", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10, family = "opensans", color = "black", face = "bold"),
    legend.margin = margin(t = 5),
    legend.box.spacing = unit(0, "cm"),
    legend.position = "bottom",
    legend.background = element_blank(),
    plot.caption = element_text(hjust = 0, size = 10, family = "opensans", face = "bold", color = "black"),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = "white"))


ggsave(
  "report/costs_trend_graphEG.png",
  plot = costs_trend_graphEG,
  width = 10.42, height = 5.56, dpi = 300,
  bg = "white"
)

##############################################################################################################

# Lets visualise the school density in each district

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


school_density_graph <- districts_sf %>% ggplot(aes(fill = school_density)) +
  # District layer with school density
  geom_sf(data = districts_sf, color = "white", size = 1.9) +
  # School points layer
  geom_sf(data = schools_sf, size = 1, fill = "#240A34", alpha = 0.5, shape = 21) +
  #Add commune layer
  #geom_sf(data = communes_sf, fill = "transparent", color = "#FBF4DB", size = 0) +
  # District names layer
  geom_sf_text(data = districts_sf, aes(label = District), size = 3.5, fontface = "bold",
               color = if_else(districts_sf$District == "Ta Lou Senchey", "white", "black"), family = "opensans") +
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
  # Add labels and customize the plot
  labs(
    title = "Panel (A): School Density"
  ) +
  theme_void() +
  annotation_scale(location = "bl", text_family = "serif", height = unit(0.10, "cm")) +
  annotation_north_arrow(which_north = "grid",
                         location    = "tl",
                         style       = north_arrow_orienteering(text_family = "serif"),
                         height      = unit(0.45, "cm"),
                         width       = unit(0.45, "cm")) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, family = "opensans", face = "bold", margin = margin(b = 2, t = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 11, family = "opensans", face = "bold", margin = margin(b = 2)),
    legend.text = element_text(size = 10, face = "bold", family = "opensans", margin = margin(t = 2)),
    legend.box.margin = margin(t = 0),
    plot.caption = element_text(size = 9, family = "opensans", hjust = 0),
    plot.caption.position = "plot"
  )


#########################################################################################

districts_sf <- districts_sf %>%
  mutate(
    text_color = if_else(District == "Ta Lou Senchey", "#240750", "#E4E0E1"), # Specify your desired colors
    nudge_y = if_else(District == "Ta Lou Senchey", 0.18, -0.005),
    nudge_x = if_else(District == "Ta Lou Senchey", -0.15, 0)
  )

school_connect_graph <- ggplot() + 
  geom_sf(data = districts_sf, fill  = "#202040", color = "#E8F9FD", size = 1.5) +
  #geom_sf(data = water_in_boundaries, fill = "#478CCF", color = "#478CCF") + 
  geom_sf_text(
    data = districts_sf, 
    aes(label = District), 
    size = 3.5, 
    fontface = "bold",
    color = districts_sf$text_color, 
    family = "opensans", 
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
    title = "Panel (B): School Connectedness (Road Network)",
    size = "Average Eating Students"
  ) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, family = "opensans", face = "bold", margin = margin(b = 2, t = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 11, family = "opensans", face = "bold", margin = margin(b = -0.5)),
    legend.text = element_text(size = 10, face = "bold", family = "opensans", margin = margin(t = 2)),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.caption = element_text(size = 11, family = "opensans", hjust = 0, face = "bold"),
    plot.caption.position = "plot"
  ) + 
  scale_size_continuous(range = c(0.3, 2.5)) +
  guides(size = guide_legend(title.position = "top", title.hjust = 0.5))
  
  
  
complete_graph <- school_density_graph + school_connect_graph +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    #title = "School Density and Connectedness in the 5 Districts",
    caption = "Source: SFIS Data",
    theme = theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5, family = "opensans"),
                  plot.caption = element_text(size = 11, family = "opensans", hjust = 0, face = "bold"),
                  plot.margin = margin(0, 0, 0, 0))
  ) 

# Save the graph
ggsave("report/school_density_graph.png", 
       plot = complete_graph, 
       width = 8.27, height = 4.63, dpi = 300, bg = "white")

##############################################################################################################

# School Maps - Picture

complete_schools <- st_read("data/WFP_VAM_Verified_HGSFP_School_Location/WFP_VAM_Verified_HGSFP_School_Location_20241024/WFP_VAM_Verified_HGSFP_School_Location_20241024.shp") %>%
  select(School_cod, Province_E, School_EN, District_E, Type, ToT_studen, Commune_E, Lat, Programme, Long, geometry) %>%
  rename(
    SchoolCode = School_cod,
    SchoolName = School_EN,
    District = District_E,
    Commune = Commune_E,
    Students = ToT_studen,
    Province = Province_E
  )

# provincial boundaries

provincial_boundaries <- st_read("data/boundary/BND/khm_bnd_admin1_gov_wfp_ed2024.shp") %>%
  st_transform(crs = 4326) %>% 
  rename(Province = Adm1_Name) %>% 
  filter(Province %in% c("Banteay Meanchey", "Battambang", "Kampong Cham", "Kampong Chhnang", "Kampong Thom",
                         "Preah Vihear", "Pursat", "Siemreap", "Stung Treng", "Oddar Meanchey")) %>% 
  select(Province, geometry)

unique_provinces <- unique(complete_schools$Province)
# # Join the water and the boundaries data
# water_in_province_boundaries <- st_intersection(water, provincial_boundaries) %>% 
#   filter(Size == "Major") %>% 
#   filter(Describe != "Non-Perenial/Intermittent/Fluctuating")
# 
# roads_in_province_boundaries <- st_intersection(roads,  provincial_boundaries) %>% 
#   filter(Classes == "Provincial and rural road")

province_filtered <- provincial_boundaries %>% 
  filter(Province %in% unique_provinces) %>% 
  st_transform(crs = 4326)

districts_sf <- st_read("data/boundary/BND/khm_bnd_admin2_gov_wfp_ed2024.shp") %>%
  rename(District =  Adm2_Name) %>%
  st_transform(crs = 4326) 

districts_centroid <- st_centroid(districts_sf) %>% st_transform(crs = 4326)


district_filtered <- st_join(districts_centroid, provincial_boundaries, left = FALSE)

district_filtered <- districts_sf %>%
  left_join(st_drop_geometry(district_filtered), by = c("District")) %>%
  filter(!is.na(Province)) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lat = st_coordinates(centroid)[,2]) %>%
  filter(!(District == "Samraong" & lat < 11.5)) %>%
  select(-centroid, -lat)

school_district_stats <- complete_schools %>% 
  st_join(district_filtered) %>%
  rename(District = District.x) %>%
  select(-District.y) %>%
  group_by(District) %>% 
  summarise(n_schools = n(), avg_students = sum(Students)) %>% 
  mutate(has_data = if_else(coalesce(n_schools, 0L) > 0 | coalesce(avg_students, 0) > 0, "Yes", "No")) %>% 
  st_drop_geometry()

district_bivariate <- district_filtered %>% 
  left_join(school_district_stats, by = c("District")) %>% 
  filter(Province %in% unique_provinces) %>% 
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lat = st_coordinates(centroid)[,2]) %>%
  filter(!(District == "Samraong" & lat < 11.5)) %>%
  select(-centroid, -lat)


# Define categories for density and school size
district_bivariate <- district_bivariate %>% 
  bi_class(
    x= n_schools,
    y = avg_students,
    style = "quantile",
    dim = 4
  ) %>% 
  mutate(bi_class = if_else(has_data == "Yes", bi_class, NA_character_)) 



pallete2 <- "PurpleGrn"

pallet_clrspace <-  matrix(colorspace::sequential_hcl(36, palette = "Emrld"), nrow = 6, ncol = 6, byrow = TRUE)

pal_vector <- as.vector(pallet_clrspace)


school_map <- ggplot() +
  # Overlay: Districts with bivariate classification
  geom_sf(data = district_bivariate, aes(fill = bi_class), color = NA) +
  # Apply the custom bivariate fill scale
  bi_scale_fill(pal = pallete2, dim = 4, guide = FALSE, na.value = "#CC796E") +
  geom_sf(data = provincial_boundaries, fill = NA, color = "#B55245", linewidth = 0.3) + 
  # Add provincial names
  geom_sf_text(data = province_filtered, aes(label = Province), size = 3, color = "white", family = "opensans", fontface = "bold",
               nudge_y = if_else(province_filtered$Province == "Pursat", -0.1, 0)) +
  # # District names for the pilot districts
  # geom_sf_text(data = district_filtered, aes(label = if_else(District == "Phnum Kravanh" | District == "Ta Lou Senchey", District, NA)), 
  #              size = 2.5, color = "white", family = "opensans",
  #              nudge_x = if_else(district_filtered$District == "Ta Lou Senchey", -0.5, 0),
  #              nudge_y = if_else(district_filtered$District == "Ta Lou Senchey", -0.2, 0)) +
  # annotate(
  #   "curve",
  #   x = 103.6,
  #   xend = 103.3,
  #   y = 12.45,
  #   yend = 12.3,
  #   color = "white",
  #   curvature = 0.2,
  #   linewidth = 0.2,
  #   arrow = arrow(type = "closed", length = unit(0.05, "inches"), ends = "last")) +
  # Minimal theme or a dark theme for a better contrast
  theme_void() + 
  coord_sf() + 
  labs(
    title = "School Feeding Programme in Cambodia",
    subtitle = "The number of schools and students per district in provinces where the school feeding programme is implemented",
    caption = "Note: Data used is from SFIS. Districts in brown do not have the school feeding programme. The bivariate classification is based on quantiles."
  ) + 
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, family = "opensans", face = "bold", margin = margin(b = 5, t = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, family = "opensans", margin = margin(b = -5)),
    plot.caption = element_text(size = 10, family = "opensans", hjust = 0.5, face = "italic", margin = margin(b = 5, t = -20))
  )


map_legend <- bi_legend(
  pal = pallete2,
  dim = 4,
  xlab = "Schools per District",
  ylab = "Students per District",
  size = 8,
  arrows = TRUE
) +
  theme(
    axis.title.x = element_text(family = "opensans", face = "bold", hjust = 0.5),
    axis.title.y = element_text(family = "opensans", face = "bold", hjust = 0.5)
    # Do not override legend.key, legend.text, etc.
  )
# Merge the two graphs

school_map_complete <- ggdraw() +
  draw_plot(school_map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(map_legend, x = 0.6, y = 0.23, width = 0.3, height = 0.3)

# Save the graph
ggsave("report/school_map.png", 
       plot = school_map_complete, 
       width = 8.27, height = 4.63, dpi = 300, bg = "white")

  
# School connectedness and desity by province

roads <- st_read("data/roads/khm_trs_roads_gov_wfp_ed2024.shp") %>%
  st_transform(crs = 4326)

#Ithersect the roads with the provincial boundaries
roads_in_province_boundaries <- st_intersection(roads, provincial_boundaries) %>% 
  filter(Classes == "Provincial and rural road")

water <- st_read("data/water/khm_hyd_rivers_gov.shp") %>%
  st_transform(crs = 4326) %>% 
  filter(Describe != "Non-Perenial/Intermittent/Fluctuating")

water_in_province_boundaries <- st_intersection(water, provincial_boundaries) %>% 
  filter(Size == "Major")


# Graph for school connectedness and density by province
school_connection <- ggplot() +
  geom_sf(data = district_filtered, fill = "#071952", color = "white", linewidth = 0.1) +
  geom_sf(data = province_filtered, fill = NA, color = "white", linewidth = 0.5) +  # Province boundaries
    # District boundaries
  geom_sf(data = roads_in_province_boundaries, color = "#B5651D", linewidth = 0.25, alpha = 0.5) +  # Roads
  geom_sf(data = complete_schools, aes(fill = Type, color = Type), shape = 21, 
          size = 0.5, stroke = 0.5, alpha = 0.8) +  
  scale_fill_manual(name = "School Type", values = c("Government school" = "#FFB200", "WFP school" = "#FFB38E")) +  # Custom colors for school types
  scale_color_manual(values = c("Government school" = "#FFB200", "WFP school" = "#FFB38E")) +  # Custom colors for district boundaries and roads
  guides(color = "none") +  # Remove the legend for school type
  geom_sf_text(data = province_filtered, aes(label = Province), size = 3, fontface = "bold", color = "white", family = "opensans") +  # Province names
  #geom_sf(data = water_in_province_boundaries, fill = "#9EDDFF", color = "#9EDDFF") +  # Water bodies
  theme_void() +
  theme(legend.position = c(0.87, 0.3),
        legend.direction = "horizontal",
        legend.title = element_text(size = 8, family = "opensans", face = "bold", hjust = 0.5,
                                    margin = margin(b = 0)),
        legend.text = element_text(size = 8, family = "opensans", margin = margin(t = 0, r =0)),
        legend.title.position = "top")


ggsave("report/school_connection.png",
       plot = school_connection,
       width = 8.27, height = 4.63, dpi = 300, bg = "white")



# Combine the two graphs
