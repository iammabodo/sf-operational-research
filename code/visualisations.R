#Loading libraries

library(tidyverse)
library(ggthemes)
library(showtext)
library(ggforce)
library(marquee)

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
  filter(AvgStudents > 0 & AvgStudents < 500 ) %>%
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
  geom_text(aes(y = 2, label = TenderProcessImp), 
            hjust = 0, color = "white", size = 2,
            family = "opensans", fontface = "bold") +  # Position labels at the start of the bar
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            hjust = 0, color = "white", size = 2, nudge_y = -8,
            family = "opensans", fontface = "bold") +  # Align percentages at the end of the bar
  theme_minimal() +  # Set the theme
  labs(title = "Stages of procurement process to be improved",
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
ggsave("report/ImprovementGraph.png", 
       plot = ImprovementGraph, 
       width = 3.53, height = 3, dpi = 200,
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
  #filter(!MonthYear %in% c("Jan 2023", "Apr 2023", "May 2023", "Oct 2023", "Nov 2023", "Apr 2024", "Jul 2024")) %>%
  ggplot(aes(x = MonthYear, y = AvgWetCostsPerChild, group = procurement, color = procurement)) +
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
  geom_point(aes(color = "red"), shape = 21, fill = "red") +
  geom_text(aes(label = paste0("$", Cost)), size = 2, family = "opensans", fontface = "bold") +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = "Number of schools receiving Wet Commodities",
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
  coord_cartesian(ylim = c(0, 200))



dry_commodities_graph <- costs %>% 
  filter(Costs == "Dry Costs") %>%
  ggplot(aes(x = Pilot, y = Cost, size = Cost)) +
  geom_point(aes(color = "red"), shape = 21, fill = "red") +
  geom_text(aes(label = paste0("$",Cost), size = 2, family = "opensans", fontface = "bold")) +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = "Number of schools receiving Wet Commodities",
    subtitle = "",
    caption = "Source: Supplier Survey"
  ) +
  theme_minimal(base_size = 12, base_family = "opensans") +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    #axis.text.x = element_blank(),
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



