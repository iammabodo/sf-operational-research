library(tidyverse)
library(readxl)
library(openxlsx)



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



annual_breakdowndays <- Complete_data %>% 
  filter(Activity  == "hgsf_full") %>% 
  filter(study_days > 0) %>%
  group_by(pilot, Year) %>%
  summarise(
    n = n(),
    mean_breakdown_days = mean(breakdown_days, na.rm = TRUE)
  ) %>% 
  select(-n) %>%
  pivot_wider(names_from = Year, values_from = mean_breakdown_days) %>%
  mutate(change = `2024` - `2023`,
         change_percent = ((`2024` - `2023`)/`2023`)*100)

write.xlsx(annual_breakdowndays, "data/annual_breakdowndays.xlsx")
  
  
connected_kravanh <- Complete_data %>% 
  filter(Activity  == "hgsf_full") %>%
  filter(District == "Phnum Kravanh") %>% 
  select(Commune, School_Code, `School name`, Year, breakdown_days) %>% 
  mutate(roadconected = case_when(
    `School name` == "Or Soam" ~ "Less Connected",
    TRUE ~ "Connected"
  )) %>% 
  group_by(Year, roadconected) %>%
  summarise(
    n = n(),
    mean_breakdown_days = mean(breakdown_days, na.rm = TRUE)
  ) %>%
  select(-n) %>%
  pivot_wider(names_from = Year, values_from = mean_breakdown_days) %>% 
  mutate(change = `2024` - `2023`,
         change_percent = ((`2024` - `2023`)/`2023`)*100,
         change_percent = abs(change_percent))


write.xlsx(connected_kravanh, "data/connected_kravanh.xlsx")


Bakan_bddays <- Complete_data %>% 
  filter(District == "Bakan") %>%
  mutate(
    connected = case_when(
      `School name` == "Sras Makak" | `School name` == "Anlung Kray"| `School name` == "O Ta Pong" |
        `School name` == "Wat Chre" | `School name` == "Robos Raing" | `School name` == "Prasat" |
        `School name` == "Ko Khsach" | `School name` == "Kdat" | `School name` == "Angkanh" |
        `School name` == "Tuol Leap" ~ "Not Connected",
      TRUE ~ "Connected"
    )
  ) %>% 
  group_by(Year, connected) %>%
  summarise(
    n = n(),
    mean_breakdown_days = mean(breakdown_days, na.rm = TRUE)
  )


