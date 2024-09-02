library(tidyverse)
library(readxl)


SupplierData <- read_excel("data/IRL_Evaluation_of_Procurement_Piloting_Model_of_SF_program_Deidentified_Label.xlsx") %>% 
  filter(!is.na(SbjNum))


WetCommoditiesSup <- SupplierData %>% 
  filter(ProductsSupplied_2 == "2. Wet Commodities [Vegetables, animal protein etc.]") %>% 
  select(-c(I1_SupplyFreq:I1_OtherCostsMN))
