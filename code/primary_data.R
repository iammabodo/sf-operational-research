library(tidyverse)
library(haven)
library(labelled)
library(readxl)
library(shadowtext)
library(ggthemes)
library(showtext)


# Set the fonts for the plots
#Text for the graphs - setting the defalts
font_add_google("Open Sans","opensans")
showtext_auto()
showtext_opts(dpi = 200)

# Load the data
suppliers_data <- read_excel("data/IRL_Evaluation of Procurement Piloting Model of SF program_Identified Resp (WFP) v4.0.xlsx") %>% 
  # Clean the character variables
  mutate(across(where(is.character), ~ ifelse(is.na(.), NA, gsub("^\\d+\\.\\s*", "", .)))) %>% 
  #filter(!is.na(DirectSupplier)) %>% 
  mutate(I2_FuelandGas = ifelse(is.na(I2_FuelandGas), 0, I2_FuelandGas),
         I2_PackageandStorage = ifelse(is.na(I2_PackageandStorage), 0, I2_PackageandStorage),
         I2_LabourCosts = ifelse(is.na(I2_LabourCosts), 0, I2_LabourCosts),
         WetTransport = ifelse(is.na(I2_When), 0, I2_When),
         I1_FuelandGas = ifelse(is.na(I1_FuelandGas), 0, I1_FuelandGas),
         I1_PackageandStorage = ifelse(is.na(I1_PackageandStorage), 0, I1_PackageandStorage),
         I1_LabourCosts = ifelse(is.na(I1_LabourCosts), 0, I1_LabourCosts),
         DryTransport = ifelse(is.na(I1_When), 0, I1_When),
         WetCosts = I2_FuelandGas + I2_PackageandStorage + I2_LabourCosts + WetTransport,
         DryCosts = I1_FuelandGas + I1_PackageandStorage + I1_LabourCosts + DryTransport,
         TotalCosts = WetCosts + DryCosts,
         DCShareFuel = I1_FuelandGas/DryCosts * 100,
         DCSharePackage = I1_PackageandStorage/DryCosts * 100,
         DCShareLabour = I1_LabourCosts/DryCosts * 100,
         DCShareTransport = DryTransport/DryCosts * 100,
         WCShareFuel = I2_FuelandGas/WetCosts * 100,
         WCSharePackage = I2_PackageandStorage/WetCosts * 100,
         WCShareLabour = I2_LabourCosts/WetCosts * 100,
         WCShareTransport = WetTransport/WetCosts * 100,
         procurement = case_when(
           RespDistrict == "Kravanh district" ~ "District Centralisation",
           RespDistrict == "Talo Sen Chey district" ~ "Commune Centralisation",
           TRUE ~ "Non-Procurement Pilots"),
         supplyingboth = case_when(
           ProductsSupplied_1 == "Dry Commodities [Rice, Salt, and Oil]" & 
             ProductsSupplied_2 == "Wet Commodities [Vegetables, animal protein etc.]" ~ "Yes",
           TRUE ~ "No"),
         DirectSupplier = ifelse(DirectSupplier != "Supplying Directly to the School." & 
                                   procurement == "Non-Procurement Pilots", NA,  DirectSupplier),
         education = case_when(
           RespEducation == "No Education" ~ "No Education",
           RespEducation == "Some Primary" ~ "Some Primary",
           TRUE ~ "Primary and Above"))
  

average_drycosts <- suppliers_data %>% 
  filter(ProductsSupplied_1  == "Dry Commodities [Rice, Salt, and Oil]" &
           DirectSupplier == "Supplying Directly to the School.") %>% 
  #filter(supplyingboth == "Yes") %>% 
  group_by(procurement) %>% 
  summarise(drycosts = mean(DryCosts, na.rm = T), drysuppliers = n()) %>% 
  ungroup() %>%
  mutate(drycosts = drycosts/4100)

average_school_supplied <- suppliers_data %>% 
  filter(ProductsSupplied_1  == "Dry Commodities [Rice, Salt, and Oil]" &
           DirectSupplier == "Supplying Directly to the School.") %>%  
  #filter(supplyingboth == "No") %>%
  group_by(procurement) %>% 
  summarise(Schools = mean(HGSFSupplierNumCS, na.rm = T), n = n()) %>% 
  ungroup()

DCFuelShare <- suppliers_data %>%
filter(ProductsSupplied_1  == "Dry Commodities [Rice, Salt, and Oil]" &
         DirectSupplier == "Supplying Directly to the School.") %>%
group_by(procurement) %>%
summarise(dryfulecostsshare = mean(DCShareFuel, na.rm = T))

DCPackageShare <- suppliers_data %>%
filter(ProductsSupplied_1  == "Dry Commodities [Rice, Salt, and Oil]" &
       DirectSupplier == "Supplying Directly to the School.") %>%
group_by(procurement) %>%
summarise(drypackagecostsshare = mean(DCSharePackage, na.rm = T))

DCLabourShare <- suppliers_data %>%
filter(ProductsSupplied_1  == "Dry Commodities [Rice, Salt, and Oil]" &
         DirectSupplier == "Supplying Directly to the School.") %>%
group_by(procurement) %>%
summarise(drylabourcostsshare = mean(DCShareLabour, na.rm = T))

DCTransportShare <- suppliers_data %>%
filter(ProductsSupplied_1  == "Dry Commodities [Rice, Salt, and Oil]" &
         DirectSupplier == "Supplying Directly to the School.") %>%
group_by(procurement) %>%
summarise(drytransportcostsshare = mean(DCShareTransport, na.rm = T))

drycosts_data <- average_drycosts %>% 
  left_join(DCFuelShare, by = "procurement") %>% 
  left_join(DCPackageShare, by = "procurement") %>% 
  left_join(DCLabourShare, by = "procurement") %>% 
  left_join(DCTransportShare, by = "procurement") %>%
  pivot_longer(cols = c(dryfulecostsshare, drypackagecostsshare, drylabourcostsshare, drytransportcostsshare), 
               names_to = "Costs Share", values_to = "Percentage") %>% 
  mutate(`Costs Share` = case_when(
    `Costs Share` == "dryfulecostsshare" ~ "Fuel",
    `Costs Share` == "drypackagecostsshare" ~ "Package",
    `Costs Share` == "drylabourcostsshare" ~ "Labour",
    `Costs Share` == "drytransportcostsshare" ~ "Transport",
    TRUE ~ "Other"))

average_wetcosts <-  suppliers_data %>% 
  filter(ProductsSupplied_2  == "Wet Commodities [Vegetables, animal protein etc.]" & 
           DirectSupplier == "Supplying Directly to the School.") %>%  
  #filter(supplyingboth == "Yes") %>%
  group_by(procurement) %>% 
  summarise(wetcosts = mean(WetCosts, na.rm = T), wetsuppliers = n()) %>% 
  ungroup() %>%
  mutate(wetcosts = wetcosts/4100)

average_schools_supplied_wet <-  suppliers_data %>% 
  filter(ProductsSupplied_2  == "Wet Commodities [Vegetables, animal protein etc.]" & 
           DirectSupplier == "Supplying Directly to the School.") %>%  
  group_by(procurement) %>% 
  summarise(average_schools = mean(HGSFSupplierNumCS, na.rm = T), n = n()) %>% 
  ungroup()


WCFuelShare <- suppliers_data %>% 
  filter(ProductsSupplied_2  == "Wet Commodities [Vegetables, animal protein etc.]" & 
             DirectSupplier == "Supplying Directly to the School.") %>% 
  group_by(procurement) %>% 
  summarise(wetfuelcostsshare = mean(WCShareFuel, na.rm = T))

WCPackageShare <- suppliers_data %>%
  filter(ProductsSupplied_2  == "Wet Commodities [Vegetables, animal protein etc.]" & 
             DirectSupplier == "Supplying Directly to the School.") %>% 
  group_by(procurement) %>%
  summarise(wetpackagecostsshare = mean(WCSharePackage, na.rm = T))

WCLabourShare <- suppliers_data %>%
  filter(ProductsSupplied_2  == "Wet Commodities [Vegetables, animal protein etc.]" & 
             DirectSupplier == "Supplying Directly to the School.") %>% 
  group_by(procurement) %>%
  summarise(wetlabourcostsshare = mean(WCShareLabour, na.rm = T))

WCTransportShare <- suppliers_data %>%
  filter(ProductsSupplied_2  == "Wet Commodities [Vegetables, animal protein etc.]" & 
             DirectSupplier == "Supplying Directly to the School.") %>% 
  group_by(procurement) %>%
  summarise(wettransportcostsshare = mean(WCShareTransport, na.rm = T))

# Join thye wet costs data

wet_costs_data <- average_wetcosts %>% 
  left_join(WCFuelShare, by = "procurement") %>% 
  left_join(WCPackageShare, by = "procurement") %>% 
  left_join(WCLabourShare, by = "procurement") %>% 
  left_join(WCTransportShare, by = "procurement") %>%
  pivot_longer(cols = c(wetfuelcostsshare, wetpackagecostsshare, wetlabourcostsshare, wettransportcostsshare), 
               names_to = "Costs Share", values_to = "Percentage") %>% 
  mutate(`Costs Share` = case_when(
    `Costs Share` == "wetfuelcostsshare" ~ "Fuel",
    `Costs Share` == "wetpackagecostsshare" ~ "Package",
    `Costs Share` == "wetlabourcostsshare" ~ "Labour",
    `Costs Share` == "wettransportcostsshare" ~ "Transport",
    TRUE ~ "Other"))


suppliers_data %>% 
  filter(ProductsSupplied_2  == "Wet Commodities [Vegetables, animal protein etc.]" &
           DirectSupplier == "Supplying to contracted suppliers.") %>% 
  count(procurement, RespSex) %>% 
  pivot_wider(names_from = RespSex, values_from = n) %>%
  mutate(totalsuppliers = Female + Male,
         femaleshare = Female/totalsuppliers * 100)







#########################################################################################

# Total costs by procurement

total_costs <- suppliers_data %>% 
  filter(DirectSupplier == "Supplying Directly to the School.") %>%
  group_by(procurement) %>% 
  summarise(totalcosts = sum(TotalCosts, na.rm = T), totalsuppliers = n()) %>% 
  ungroup() %>%
  mutate(totalcosts = totalcosts/4100)



average_drycosts %>% 
  left_join(average_wetcosts, by = "procurement") %>% 
  left_join(total_costs, by = "procurement") %>% 
  mutate(totaldrycosts = drycosts * drysuppliers,
         totalwetcosts = wetcosts * wetsuppliers * 20,
         totalcosts = totalcosts * totalsuppliers) %>%
  select(procurement, totaldrycosts, totalwetcosts, totalcosts) %>% 
  pivot_longer(cols = c(totaldrycosts, totalwetcosts, totalcosts), names_to = "Costs Category", values_to = "Total Costs") %>% 
  ggplot(aes(x = procurement, y = `Total Costs`)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  facet_wrap(~`Costs Category`) +
  labs(title = "Total Costs by Procurement",
       x = "Procurement",
       y = "Total Costs") +
  theme(legend.position = "none")
  

#########################################################################################

# Improving the procumerent model

#1. Part of the procurement process which need to be improved

ImprovementProcurement <- suppliers_data %>% 
  filter(!is.na(TenderProcessImp)) %>%
  count(TenderProcessImp) %>%
  mutate(Percentage = n/sum(n) * 100) %>% 
  arrange(Percentage)

#2. Bid experience

BidExperience <- suppliers_data %>% 
  filter(!is.na(TenderProcessExp)) %>%
  count(TenderProcessExp) %>%
  # calculate the percentage
  mutate(Percentage = n/sum(n) * 100)


#3. Improved in Income

IncomeStatus <- suppliers_data %>% 
  count(SIncStatus) %>% 
  mutate(Percentage = n/sum(n) * 100)

# 4. Challenges faced

Challenge1 <- suppliers_data %>% 
  count(procurement, Schallenges_1) %>% 
  filter(!is.na(Schallenges_1)) %>% 
  rename(Challenges = Schallenges_1)

Challenge2 <- suppliers_data %>%
  count(procurement, Schallenges_2) %>% 
  filter(!is.na(Schallenges_2)) %>% 
  rename(Challenges = Schallenges_2)

Challenge3 <- suppliers_data %>%
  count(procurement, Schallenges_3) %>% 
  filter(!is.na(Schallenges_3)) %>% 
  rename(Challenges = Schallenges_3)

Challenge4 <- suppliers_data %>%
  count(procurement, Schallenges_4) %>% 
  filter(!is.na(Schallenges_4)) %>% 
  rename(Challenges = Schallenges_4)

Challenge5 <- suppliers_data %>%
  count(procurement, Schallenges_5) %>% 
  filter(!is.na(Schallenges_5)) %>% 
  rename(Challenges = Schallenges_5)

Challenge6 <- suppliers_data %>%
  count(procurement, Schallenges_6) %>% 
  filter(!is.na(Schallenges_6)) %>% 
  rename(Challenges = Schallenges_6)


Challenges <- bind_rows(Challenge1, Challenge2, Challenge3, 
                        Challenge4, Challenge5, Challenge6) %>%
  group_by(procurement) %>%
  # Calculate the percentage
  mutate(Percentage = n/sum(n) * 100) %>% 
  # Round the percentage to 1 decimal place
  mutate(Percentage = round(Percentage, 0)) %>%
  mutate(Challenges = if_else(Challenges == "Different school requirements (esp. vegetables)", "Different Veg Requirements", Challenges),
         Challenges = if_else(Challenges == "Long distance (to schools and market)", "Distance", Challenges),
         Challenges = if_else(Challenges == "High capital requirements", "Capital Requirements", Challenges),
         Challenges = if_else(Challenges == "Extended Payment Period", "Payment Period", Challenges)) %>% 
  filter(Challenges != "Other (specify)") %>% 
  filter(procurement != "Non-Procurement Pilots") %>% 
  mutate(Challenges = as_factor(Challenges),
         procurement = as_factor(procurement))
  
  group_by(Challenges) %>%
  # calculate the percentage
  summarise(Percentage = n/sum(n) * 100) 

##########################################################################################
  
# Intergration of smallholder farmers
  
wet_products_suppliers <- suppliers_data %>% 
    count(DirectSupplier, procurement, ProductsSupplied_2) %>% 
    filter(!is.na(ProductsSupplied_2)) %>% 
    filter(procurement != "Non-Procurement Pilots")





  suppliers_data %>% 
    filter(!is.na(ProductsSupplied_2)) %>% 
    count(DirectSupplier, procurement, SIncStatus) %>% 
    filter(procurement != "Non-Procurement Pilots") %>%
    ggplot(aes(x = SIncStatus, y = n)) +
    geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
    facet_wrap(~procurement) + 
    coord_flip()
    

##############################################################################################################
  
# Household Demographic Characteristics
  
suppliers_data %>% 
  count(DirectSupplier, procurement) %>% 
  filter(!is.na(DirectSupplier))

  
  suppliers_data %>% 
    count(RespSex, procurement) 


suppliers_data %>% 
  group_by(procurement) %>%
  summarise(mean = mean(RespAge, na.rm = T))




suppliers_data %>% count(procurement, education)


