########################################
# Pairwise correlation table
# for bilateral FDI stock indicators
# Last Updated 07-06-2021 BT
# Description: Gather all the measures 
# available for bilateral FDI stocks and 
# produce a simple (not publication-grade) 
# table of the pairwise correlations between them.
######LOAD DATA AND PACKAGES########
# Load packages.
library(dplyr)
library(stargazer)
library(ggcorrplot)

# Load the bilateral IPE data.
load(paste(bilateral_dat, "Bilateral_IPE_v1.RDATA", sep = ""))

###### PREP AND MAKE THE CORRELATION TABLE#####
# Select variables of interest (FDI position) and make the names slightly easier
# to read on the table.
ipe_bilat <- ipe_bilat %>%
  select(BEA_FDI_posIN = posIN_FDI_BEA, 
         BEA_FDI_posOUT = posOUT_FDI_BEA, 
         CDIS_FDI_posIN = positionIN_CDIS, 
         CDIS_FDI_posOUT = positionOUT_CDIS, 
         OECD_FDI_posIN = FDIposIN_FDI_OECD,
         OECD_FDI_posOUT = FDIposOUT_FDI_OECD)

# Make a correlation table of these variables, view the resulting table,
# and print the html for this table.
cortable <- cor(ipe_bilat, use = "complete")
View(cortable)
stargazer(cortable, type = "html", 
          title = "Correlation of Bilateral FDI Stock Indicators",
          out = paste(output, "FDI_STOCK_INDC_CORRS_BT_070621.txt", sep = ""))

# I wasn't sure which format we'd prefer, so I also provided the format below:
ggcorrplot(cortable, title = "Correlation of Bilateral FDI Stock Indicators", 
           lab = T)

# Export the output
ggsave(paste(output, "FDI_STOCK_INDC_CORRS_BT_070621.pdf", sep = "")) 
