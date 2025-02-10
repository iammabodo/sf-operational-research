

library(tidyverse)
library(readxl)
library(lubridate)
library(ggthemes)
library(plm)
library(here)
library(gtsummary)

################################################################################
# Load data

#1. January 2023

January_2023 <- read_excel(here("data/SFIS Clean_January_Dec_2023.xlsx"),
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
         FeedingEfficiency = (CookingDays/SchoolDays) * 100,
         # Mutate procurement and non procurement schools - Lets edit this code onces we have clarity on the pilot schools from SF
         procurement =  case_when(
           District == "Phnum Kravanh" ~ "District Centralisation",
           District == "Ta Lou SenChey" ~ "Commune Centralisation",
           TRUE ~ "Non-Procurement Pilots"),
         # Costs of indivividual comodity delivered
         OilUnitCost = SeparateExpenditureOil/ReceivedOil,
         RiceUnitCost = SeparateExpenditureRice/ReceivedRice,
         VegetableUnitCost = SeparateExpenditureVegetable/ReceivedVegetable,
         ProteinUnitCost = SeparateExpenditureProtein/RecievedProtein,
         SaltUnitCost = SeparateExpenditureSalt/ReceivedSalt) %>%
  # Change costs to USD
  mutate(DryCostsPerChild = DryCostsPerChild/4000,
         WetCostsPerChild = WetCostsPerChild/4000,
         TotalChildExp = TotalChildExp/4000,
         DryCost = DryCost/4000,
         WetCost = WetCost/4000) %>%
  # Round numeric variables to 2dp ReceivedTotal
  mutate(across(where(is.numeric), ~round(., 2)))

# Bidders and suppliers data
Bidders2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                                   sheet = "Bidders") %>% 
  mutate(Year = 2023) 

Bidders2024 <- read_excel("data/SFIS Clean_January_June_2024.xlsx",
                          sheet = "Bidders") %>%
  mutate(Year = 2024)

Suppliers2023 <- read_excel("data/SFIS Clean_January_Dec_2023.xlsx",
                            sheet = "Suppliers") %>%
  mutate(Year = 2023)

Suppliers2024 <- read_excel("data/SFIS Clean_January_June_2024.xlsx",
                            sheet = "Suppliers") %>%
  mutate(Year = 2024) %>% 
  rename(SuppliersTotal = SuppilersTotal)

SuppliersData <- bind_rows(Suppliers2023, Suppliers2024) %>% 
  mutate(Year = as.factor(Year))

BiddersData <- bind_rows(Bidders2023, Bidders2024) %>%
  mutate(Year = as.factor(Year))

# Merge bidders and suppliers data with the main data
BiddersSuppliers <- BiddersData %>% 
  left_join(SuppliersData, by = c("SchoolName", "Year", "Commune"))

FullTablesData <- FullTablesData %>% 
  mutate(Year = as.factor(Year)) %>%
  left_join(BiddersSuppliers, by = c("SchoolName", "Year", "Commune")) %>% 
  mutate(MonthYear = parse_date_time(MonthYear, "b Y")) %>% 
  arrange(MonthYear) %>% 
  mutate(Time = (year(MonthYear) - year(min(MonthYear))) * 12 +
          month(MonthYear) - month(min(MonthYear))) %>% 
  mutate(logChildrenFed = log(AvgStudents),
         logChildrenFed2 = logChildrenFed^2,
         AvgStudents2 = AvgStudents^2,
         PostPilot = if_else(Time >= 14, 1, 0)) %>% 
  filter(
    !is.na(TotalChildExp) &
      !is.na(Time) &
      !is.na(PostPilot) &
      !is.na(procurement) &
      !is.na(logChildrenFed) &
      !is.na(logChildrenFed2)
  ) %>% 
  filter(is.finite(logChildrenFed) & is.finite(logChildrenFed2)) %>% 
  filter(!MonthYear %in% c("Sep 2023", "Oct 2023", "Nov 2023", "Dec 2023", "Jan 2024")) %>% 
  mutate(
    AvgStudents_scaled = scale(AvgStudents),
    AvgStudents2_scaled = scale(AvgStudents^2),
    logWetCostsPerChild = if_else(WetCostsPerChild > 0, log(WetCostsPerChild), NA_real_),
    logDryCostsPerChild = if_else(DryCostsPerChild > 0, log(DryCostsPerChild), NA_real_),
    logTotalChildExp = if_else(TotalChildExp > 0, log(TotalChildExp), NA_real_),
    PostPilot = factor(PostPilot, levels = c(0, 1), labels = c("Pre-Pilot", "Post-Pilot")),
    procurement = factor(procurement))

#############################################################################################

# # Cost perchild per district and year
# 
# model1 <- lmer(
#   WetCostsPerChild ~ PostPilot*procurement*Time + AvgStudents_scaled  + AvgStudents2_scaled*procurement + (1|SchoolId),
#   data = FullTablesData)
# 
# summary(model1)
# 
# model2 <- lm(
#   TotalChildExp ~ PostPilot*procurement  + Time + AvgStudents + AvgStudents2,
#   data = FullTablesData)
# 
# summary(model2)
# 
# anova_model <- aov(
#   TotalChildExp ~ PostPilot * procurement,
#   data = FullTablesData
# )
# 
# summary(anova_model)
# 
# ggplot(FullTablesData, aes(x = procurement, y = TotalChildExp, fill = PostPilot)) +
#   geom_boxplot() +
#   labs(title = "Interaction of Procurement Model and Pilot on Total Child Expenditure",
#        x = "Procurement Model", y = "Total Child Expenditure") +
#   theme_minimal()
#######################################################################################################

# Panel Model on the relationship between Cost per child and other variables


write.xlsx(FullTablesData, "FullTablesData.xlsx")


PanelFullData24 <-FullTablesData %>% 
  filter(AvgStudents > 0 & District != "Krakor") %>%
  filter(Year == 2024) %>% 
  group_by(procurement, SchoolId, MonthYear) %>%
  # mutate averages for every variable
  summarise(AvgDryCostsPerChild = mean(DryCostsPerChild, na.rm = TRUE),
         AvgWetCostsPerChild = mean(WetCostsPerChild, na.rm = TRUE),
         AvgTotalCost = mean(TotalChildExp, na.rm = TRUE),
         AvgEatingStudents = mean(AvgStudents, na.rm = TRUE),
         AvgBreakDownDays = mean(BreakDownDays, na.rm = TRUE),
         AvgStudents = mean(AvgStudents, na.rm =TRUE)) %>%
  mutate(procurement = as.factor(procurement),
         Year = as.factor(MonthYear),
         SchoolId = as.factor(SchoolId)) %>% 
  ungroup() %>%
  filter(MonthYear != "2024-07-01") %>% 
  pdata.frame(., index = c("SchoolId", "MonthYear"))
#######################################################################################################################################

CostsChangesTable <- FullTablesData %>% 
  # filter(Month == "February" | Month == "March" | Month == "April" | 
  #          Month == "May" | Month == "June" | Month == "July" | Month == "August") %>%
  filter(AvgStudents > 0 & District != "Krakor") %>%
  group_by(procurement, Year) %>%
  summarise(
            TotalDryCost = mean(DryCostsPerChild, na.rm = TRUE),
            TotalWetCost = mean(WetCostsPerChild, na.rm = TRUE),
            TotalEatingStudents = mean(AvgStudents, na.rm = TRUE),
            TotalBreakDownDays = mean(BreakDownDays, na.rm = TRUE)) %>% 
  ungroup() 

DryCostsTable <- CostsChangesTable %>%
  select(procurement, Year, TotalDryCost) %>% 
  pivot_wider(names_from = Year, values_from = TotalDryCost) %>% 
  mutate(Change = (`2024` - `2023`)/`2023` * 100) %>% 
  rename(`Dry Costs 2023` = `2023`,
         `Dry Costs 2024` = `2024`,
         `Change in Dry Costs` = Change) %>% 
  # round numeric variables to 2dp
  mutate(across(where(is.numeric), ~round(., 2)))

WetCostsTab <- CostsChangesTable %>%
  select(procurement, Year, TotalWetCost) %>% 
  pivot_wider(names_from = Year, values_from = TotalWetCost) %>% 
  mutate(Change = (`2024` - `2023`)/`2023` * 100) %>% 
  rename(`Wet Costs 2023` = `2023`,
         `Wet Costs 2024` = `2024`,
         `Change in Wet Costs` = Change) %>%
  # round numeric variables to 2dp
  mutate(across(where(is.numeric), ~round(., 2)))


BreakDownDaysTable <- CostsChangesTable %>%
  select(procurement, Year, TotalBreakDownDays) %>% 
  pivot_wider(names_from = Year, values_from = TotalBreakDownDays) %>% 
  mutate(Change = (`2024` - `2023`)/`2023` * 100) %>% 
  rename(`Breakdown Days 2023` = `2023`,
         `Breakdown Days 2024` = `2024`,
         `Change in Breakdown Days` = Change) %>%
  # round numeric variables to 2dp
  mutate(across(where(is.numeric), ~round(., 2)))

# cOMBINE THE TABLES

CostsChangesTable <- DryCostsTable %>% 
  left_join(WetCostsTab, by = "procurement") %>% 
  left_join(BreakDownDaysTable, by = "procurement")

# write the table to excel

write.xlsx(CostsChangesTable, "CostsChangesTable.xlsx")

FullTablesData %>% 
  filter(AvgStudents > 0 & District != "Krakor") %>% 
  distinct(SchoolName, Year, .keep_all = TRUE) %>%
  group_by(procurement, Year) %>%
  summarise(Suppliers = sum(SuppliersTotal),
            Bidders = sum(FemaleBidders),
            FemaleSuppliers = sum(FemaleSuppliers),
            FemaleSuppliersRatio = FemaleSuppliers/Suppliers * 100,
            FemaleBidSuccess = FemaleSuppliers/Bidders * 100,
            n = n()) %>% 
  pivot_longer(cols = c(Suppliers, FemaleSuppliers, FemaleSuppliersRatio),
               names_to = "Suppliers",
               values_to = "Values") %>% 
  filter(Suppliers == "FemaleSuppliersRatio") %>%
  ggplot(aes(x = Year, y = Values)) +
  geom_bar(stat = "identity") +
  facet_wrap(~procurement) + 
  theme_clean()


MealsDays <- FullTablesData %>% 
  group_by(procurement, MonthYear) %>%
  summarise(MeanCookingDays = mean(CookingDays)) %>% 
  ungroup() %>%
  mutate(MeanCookingDays = round(MeanCookingDays, 0))

BreakDownDays <- FullTablesData %>% 
  group_by(procurement, Year) %>%
  summarise(MeanBreakDownDays = mean(BreakDownDays)) %>% 
  ungroup() %>%
  mutate(MeanBreakDownDays  = round(MeanBreakDownDays, 2))



MonthYearCosts <- FullTablesData %>% 
  group_by(procurement, Month, Year) %>% 
  summarise(meanOilUnitCos = mean(OilUnitCost, na.rm = T), 
            meanRiceUnitCost = mean(RiceUnitCost, na.rm = T), 
            meanVegetableUnitCost = mean(VegetableUnitCost, na.rm = T), 
            meanProteinUnitCost = mean(ProteinUnitCost, na.rm = T), 
            meanUnitCostSalt = mean(SaltUnitCost, na.rm = T)) %>% 
  ungroup() %>%
  # Change the costs to USD
  mutate(meanOilUnitCos = meanOilUnitCos/4000,
         meanRiceUnitCost = meanRiceUnitCost/4000,
         meanVegetableUnitCost = meanVegetableUnitCost/4000,
         meanProteinUnitCost = meanProteinUnitCost/4000,
         meanUnitCostSalt = meanUnitCostSalt/4000) 



MonthYearCosts %>%
  filter(!is.infinite(meanOilUnitCos)) %>%
  filter(!is.infinite(meanRiceUnitCost)) %>%
  filter(!is.infinite(meanVegetableUnitCost)) %>%
  filter(!is.infinite(meanProteinUnitCost)) %>%
  filter(!is.infinite(meanUnitCostSalt)) %>%
  group_by(procurement, Year) %>%
  summarise(
    meanOilUnitCos = mean(meanOilUnitCos, na.rm = TRUE), 
    meanRiceUnitCost = mean(meanRiceUnitCost, na.rm = TRUE), 
    meanVegetableUnitCost = mean(meanVegetableUnitCost, na.rm = TRUE), 
    meanProteinUnitCost = mean(meanProteinUnitCost, na.rm = TRUE), 
    meanUnitCostSalt = mean(meanUnitCostSalt, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  View()





