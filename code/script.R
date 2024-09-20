library(tidyverse)
library(readxl)
library(lubridate)
library(ggthemes)
library(plm)


################################################################################
# Load data

#1. January 2023

January_2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                           sheet = "January 2023") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "January",
         Year = 2023)


# 2. February 2023
February_2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                            sheet = "February 2023") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "February",
         Year = 2023)

# 3. March 2023
March_2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                         sheet = "March 2023") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "March",
         Year = 2023)

# 4. April 2023
April_2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                         sheet = "April 2023") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "April",
         Year = 2023)


# 5. May 2023

May_2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                       sheet = "May 2023") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "May",
         Year = 2023)


# 6. June 2023

June_2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                        sheet = "June 2023") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "June",
         Year = 2023)


# 7. July 2023

July_2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                        sheet = "July 2023") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "July",
         Year = 2023)

# 8. August 2023

August_2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                          sheet = "August 2023") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "August",
         Year = 2023)


# 9. September 2023

September_2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                             sheet = "September 2023") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "September",
         Year = 2023)

# 10. October 2023

October_2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                           sheet = "October 2023") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "October",
         Year = 2023)


# November 2023

November_2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                            sheet = "November 2023") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "November",
         Year = 2023)

# December 2023

December_2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                            sheet = "December 2023") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "December",
         Year = 2023)


# Combine all the data
Jan_Dec_2023_Data <- bind_rows(January_2023, 
                               February_2023, 
                               March_2023, 
                               April_2023, 
                               May_2023, 
                               June_2023, 
                               July_2023, 
                               August_2023, 
                               September_2023, 
                               October_2023, 
                               November_2023, 
                               December_2023) %>% 
  mutate(MonthYear = my(paste(Month, Year)),
         MonthYear = format(MonthYear, "%b %Y"),
         MonthYear = factor(MonthYear,
                            levels = c("Jan 2023",
                                       "Feb 2023",
                                       "Mar 2023",
                                       "Apr 2023",
                                       "May 2023",
                                       "Jun 2023",
                                       "Jul 2023",
                                       "Aug 2023",
                                       "Sep 2023",
                                       "Oct 2023",
                                       "Nov 2023",
                                       "Dec 2023"))) %>% 
  # mutate the pilot schools varaible
  mutate(PilotSchools = case_when(
    District == "Phnum Kravanh" | District == "Ta Lou SenChey" ~ "Pilot School",
    TRUE ~ "Non-Pilot School"))


# load 2024 data

January_2024 <- read_excel("data/SFIS Clean_January_June_2024.xlsx",
                           sheet = "January 2024")%>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "January",
         Year = 2024)


February_2024 <- read_excel("data/SFIS Clean_January_June_2024.xlsx",
                            sheet  = "February 2024") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "February",
         Year = 2024)


March_2024 <- read_excel("data/SFIS Clean_January_June_2024.xlsx",
                         sheet = "March 2024") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "March",
         Year = 2024)


April_2024 <-  read_excel("data/SFIS Clean_January_June_2024.xlsx",
                          sheet = "April 2024") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "April",
         Year = 2024) 

May_2024 <- read_excel("data/SFIS Clean_January_June_2024.xlsx",
                       sheet = "May 2024") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "May",
         Year = 2024) 


June_2024 <- read_excel("data/SFIS Clean_January_June_2024.xlsx",
                        sheet = "June 2024") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "June",
         Year = 2024) 

July_2024 <- read_excel("data/SFIS Clean_January_June_2024.xlsx",
                        sheet = "July 2024") %>% 
  rename(AvgStudents = `​ AvgStudents`) %>% 
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>% 
  mutate(Month = "July",
         Year = 2024)


August_2024 <- read_excel("data/SFIS Clean_January_June_2024.xlsx",
                          sheet = "Aug 2024") %>%
  rename(AvgStudents = `​ AvgStudents`) %>%
  # Change character variables to factor
  mutate(across(where(is.character), as.factor),
         SchoolId = as.character(SchoolId)) %>%
  mutate(Month = "August",
         Year = 2024)

# Compine the 2024 data tables

Jan_Aug_2024 <- rbind(January_2024, February_2024,
                       March_2024, April_2024,
                       May_2024, June_2024,
                      July_2024, August_2024) %>% 
  mutate(MonthYear = my(paste(Month, Year)),
         MonthYear = format(MonthYear, "%b %Y"),
         MonthYear = factor(MonthYear,
                            levels = c("Jan 2024",
                                       "Feb 2024",
                                       "Mar 2024",
                                       "Apr 2024",
                                       "May 2024",
                                       "Jun 2024",
                                       "Jul 2024",
                                       "Aug 2024"))) %>% 
  # mutate the pilot schools varaible
  mutate(PilotSchools = case_when(
    District == "Phnum Kravanh" | District == "Ta Lou SenChey" ~ "Pilot School",
    TRUE ~ "Non-Pilot School"))
  

# Combine the two years

FullTablesData <- rbind(Jan_Dec_2023_Data,
                        Jan_Aug_2024) %>% 
  # Mutate the number when child was not fed the meal
  mutate(BreakDownDays = SchoolDays - CookingDays,
         # Seperate costs per child
         TotalChildExp = SeparateExpenditureTotal/AvgStudents,
         ProteinSCostsPerChild = SeparateExpenditureProtein/AvgStudents,
         RiceCostsPerChild = SeparateExpenditureRice/AvgStudents,
         VegCostsPerChild = SeparateExpenditureVegetable/AvgStudents,
         FoodExpensesPerChild = SeparateExpenditureFood/AvgStudents,
         OilCostsPerChild = SeparateExpenditureOil/AvgStudents,
         SaltCostsPerChild = SeparateExpenditureSalt/AvgStudents,
         #chane PilotsShools variable to factor
         DryCostsPerChild = OilCostsPerChild + SaltCostsPerChild + RiceCostsPerChild,
         WetCostsPerChild = ProteinSCostsPerChild + VegCostsPerChild, 
         DryCost = SeparateExpenditureOil + SeparateExpenditureSalt + SeparateExpenditureRice,
         WetCost= SeparateExpenditureProtein + SeparateExpenditureVegetable,
         FeedingRatio = (AvgStudents/BenefticaryTotal)*100,
         # Feeding effciency
         FeedingEfficiency = (CookingDays/SchoolDays) * 100) %>% 
  # Round numeric variables to 2dp
  mutate(across(where(is.numeric), ~round(., 2)))
  
  
simple_model <- FullTablesData %>% 
  filter(AvgStudents != 0) %>% 
  lm(SeparateExpenditureTotal ~ AvgStudents, data = .)


summary(simple_model)

interaction_model <- FullTablesData %>% 
  filter(AvgStudents != 0) %>% 
  lm(CostsPerChild ~ BenefticaryTotal * PilotSchools, data = .) 

summary(interaction_model)  
  
  
# Quadratic model
quadratic_model <- FullTablesData %>% 
  filter(AvgStudents != 0) %>% 
  lm(SeparateExpenditureTotal ~ AvgStudents + I(AvgStudents^2)* District, data = .)


summary(quadratic_model)  
  

# Quadratic model and interaction model
quadratic_interaction_model <- FullTablesData %>% 
  filter(AvgStudents != 0) %>% 
  lm(WetCostsPerChild ~ AvgStudents  * District, data = .)


summary(quadratic_interaction_model)  

FullTablesData %>% 
  filter(AvgStudents != 0) %>% 
  ggplot(aes(x = TotalChildExp, y = AvgStudents)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  facet_grid(Year~PilotSchools) +
  labs(title = "Cost per Child vs. Total Beneficiaries",
       x = "Total Beneficiaries",
       y = "Cost per Child")


FullTablesData %>%
  filter(FeedingRatio <= 95) %>%
  #filter(WetCostsPerChild >= 7000 & WetCostsPerChild <= 12000 & AvgStudents >60) %>% 
  #group_by(MonthYear, PilotSchools) %>%
  #summarise(AvgCostsPerChild = mean(TotalChildExp, na.rm = TRUE)) %>%
  ggplot(aes(x = FeedingRatio, y =  TotalChildExp)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = T) +
  #scale_y_log10() +
  facet_grid(Year~District) + 
  theme_clean()



FullTablesData %>%
  filter(FeedingRatio>=50 & FeedingRatio<=95) %>% 
  filter(District != "Krakor") %>%
  #group_by(MonthYear, PilotSchools) %>%
  #summarise(AvgCostsPerChild = mean(TotalChildExp, na.rm = TRUE)) %>%
  ggplot(aes(x = FeedingRatio, y = TotalChildExp)) +
  geom_point(position = position_jitter()) +
  scale_x_log10() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = T) +
  facet_grid(Year~District)


FullTablesData %>%
  group_by(MonthYear, District) %>%
  summarise(AvgCostsPerChild = mean(TotalChildExp, na.rm = TRUE),
            AvgProteinSCostsPerChild = mean(ProteinSCostsPerChild, na.rm = TRUE),
            AvgRiceCostsPerChild = mean(RiceCostsPerChild, na.rm = TRUE),
            AvgVegCostsPerChild = mean(VegCostsPerChild, na.rm = TRUE),
            AvgOilCostsPerChild = mean(OilCostsPerChild, na.rm = TRUE),
            AvgSaltCostsPerChild = mean(SaltCostsPerChild, na.rm = TRUE),
            AvgDryCostsPerChild = mean(DryCostsPerChild, na.rm = TRUE),
            AvgWetCostsPerChild = mean(WetCostsPerChild, na.rm = TRUE),
            AvgBreakDownDays = mean(BreakDownDays),
            AvgStudents = mean(AvgStudents, na.rm = TRUE)) %>% 
  ungroup() %>%
  filter(District != "Krakor" & AvgStudents > 100) %>%
  ggplot(aes(x = AvgStudents, y = (AvgDryCostsPerChild/4000), color = District)) +
  geom_point() + 
  geom_smooth(method = "lm",  formula = y ~ poly(x, 2), se = TRUE) +
  facet_wrap(~District, nrow = 1) 

PanelData <- FullTablesData %>% 
  filter(AvgStudents != 0 & District != "Krakor") %>% 
  mutate(Time  = my(paste(Month, Year))) %>% 
  arrange(SchoolId, Time) %>%
  pdata.frame(index = c("SchoolName", "Time"))


# Estimate the fixed effects model

fixed_effects_model <- plm(DryCost ~ AvgStudents + I(AvgStudents^2) * PilotSchools * SchoolName, data = PanelData, model = "within")

summary(fixed_effects_model)









FullTablesData %>% filter(District == "Phnum Kravanh" & is.nan(WetCostsPerChild)) %>%  
  select(SchoolName, Month, Year, District, SchoolDays, CookingDays, AvgStudents, Commune, contains("Separate"), DryCost, WetCost) %>% 
  write.xlsx("data/MissingDetailsSchools.xlsx")

































