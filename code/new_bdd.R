library(tidyverse)
library(readxl)



SFIS_BDD_Complete2223 <- read_excel("data/SFIS_BDD_Complete.xls", 
                                sheet = "2022-2023") %>% 
  rename(Cookdays = Cookday,
         study_days = study_day
         ) %>% 
  mutate(Year = 2023)

SFIS_BDD_Complete2324 <- read_excel("data/SFIS_BDD_Complete.xls", 
                                    sheet = "2023-2024") %>% 
  mutate(Year = 2024)


SFIS_BDD_Complete <- bind_rows(SFIS_BDD_Complete2223, SFIS_BDD_Complete2324)


SFIS_Sch_Locations <- read_excel("data/SFIS_Sch_Locations.xlsx") %>% 
  rename(School_Code = `...5`) %>% 
  mutate(School_Code = as.character(School_Code))

Complete_data <- SFIS_Sch_Locations %>% 
  left_join(SFIS_BDD_Complete, by = "School_Code") %>% 
  filter(District != "Krakor") %>% 
  mutate(pilot = case_when(
    District == "Phnum Kravanh" ~ "District Centralisation",
    District == "Ta Lou SenChey" ~ "Commune Centralisation",
    District == "Kandieng" | District == "Bakan"~"PursatNon-Pilot",
    TRUE ~ "Non-Pilot"
  ),
  breakdown_days = study_days - Cookdays)



Complete_data %>% 
  filter(Activity  == "hgsf_full") %>% 
  filter(study_days > 0) %>%
  group_by(pilot, Year) %>%
  summarise(
    n = n(),
    mean_breakdown_days = mean(breakdown_days, na.rm = TRUE),
    sd_breakdown_days = sd(breakdown_days, na.rm = TRUE)
  )
  
  




