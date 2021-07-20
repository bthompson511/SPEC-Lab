#########################
# Correlation of Growth Rates Measure
# Last Updated by BT 06-30-2021
# Description: A 10-year rolling correlation for each dyadic time series.
##### LOAD PACKAGES AND DATA #####

library(dplyr)
library(tidyr)
library(peacesciencer)
library(roll)

# Load wdi_pwt data
load(paste(bilateral_rawdata, "prepped_WDI_PW.RDATA", sep = ""))

###### PREP: MAKING MONADIC DATA DYADIC #####
# Omit extraneous variables including populations variables and non-Gleditsch-Ward
# ID codes.
wdi_pwt <- wdi_pwt %>% 
  select(gwno, year, growth_WDI_PW) 

# Make a duplicate of the data set to merge later.
wdi_pwt_dup <- wdi_pwt

# Create a dyad-year panel using the Gleditsch-Ward numbering system.
# Only keep the years included in the wdi_pwt data (1950 - 2019).
panel<- create_dyadyears(system = "gw") %>%
  filter(year > 1949, year < 2020) 

# Merge the dyadic data panel with the country-year data (wdi_pwt).
# Matching values from wdi_pwt are merged with panel.
wdi_pwt <- left_join(wdi_pwt, panel, by = c("gwno" = "gwcode1", "year")) 

# Merge the duplicate wdi_pwt data to get the growth rates for parcountry
# then rename variables while reordering.
wdi_pwt <- left_join(wdi_pwt, wdi_pwt_dup, 
                     by = c("gwcode2" = "gwno", "year"), 
                     suffix = c("_rep", "_par")) %>% 
          select(repgwno = gwno,
                 pargwno = gwcode2,
                 everything()) 

##### CORRELATION OF GROWTH RATES MEASURE #####
# Create a new column called cgr_measure. 
# This will be the correlation of growth rates measure: a rolling correlation 
# (window width = 10) of growth rates for each time series.Group by dyad, 
# arrange by year, then ungroup once complete.

growth_cor <- wdi_pwt %>% 
  group_by(repgwno, pargwno) %>%
  arrange(year) %>%
  mutate(cgr_measure = roll_cor(growth_WDI_PW_rep, growth_WDI_PW_par, 10,
                                min_obs = 3, complete_obs = F)) %>% 
  ungroup() 

# Save the wdi_pwt data set
save(wdi_pwt, file=paste(bilateral_prepped, "WDI_PWT_W_CGR_BT_063021.RDATA", sep = ""))