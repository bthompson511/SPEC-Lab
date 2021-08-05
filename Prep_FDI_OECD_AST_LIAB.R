# /*************************
# OECD FDI positions and flows by partner country
# according to Benchmark Definition 4th Edition (BMD4)
# Years: 2001 - 2019 (position), 2000 - 2019 (flows)
# URL: https://stats.oecd.org/Index.aspx?#
# Level of Counterparts: Immediate
# Units: USD Millions
# Variables: 
# flow_in_net: Inward FDI Flow - Net
# flow_out_net: Outward FDI Flow - Net
# pos_out_net: Outward FDI Position - Net
# pos_in_net: Inward FDI Position - Net
# pos_in_assets: Inward FDI Position - Assets
# pos_out_assets: Outward FDI Position - Assets
# pos_in_liabilities: Inward FDI Position - Liabilities
# pos_out_liabilities; Outward FDI Position - Liabilities
# Last Updated: BT 07/12/21
# Special thanks to Miriam Barnum (SPEC Lab - USC) whose scripts and 
# training heavily informed my use of a prep function to prep 
# more than one data set at once.
# *************************/
library(tidyr)
library(dplyr)

# Make a list of the file names we want (fnames). Then make an object (ldf) that's the
# result of applying "read.csv" to all items in fnames.
fnames <- list.files(rawdata, pattern=c("FDI", ".csv"))
ldf <- lapply(paste(rawdata, fnames, sep = ""), read.csv, stringsAsFactors = F)

# Make a prep function to be applied to all imported data frames that does the following.
# Only keep rows where the value of Currency is "US Dollar" and Type.of.FDI 
# equals total financial flows or total FDI positions. Select the variables you
# want to keep while renaming them. Make a variable called "Type_Principle" that
# combines the string values from Type.of.FDI, Measurement.principle. and Accounting.entry.
prep <- function(data){
  data <- data %>% 
    filter(Currency == "US Dollar", 
           Type.of.FDI == c("FDI financial flows - Total","FDI positions -Total")) %>% 
    select(repcountry = Reporting.country,
           parcountry = Partner.country.territory,
           year = Year, Type.of.FDI, currency = Currency,
           Measurement.principle, Accounting.entry, Value) %>% 
    unite("Type_Principle", Type.of.FDI, Measurement.principle, Accounting.entry )
    
  return(data)
  
}

# Apply the prep function to all data sets in ldf
ldf <- lapply(ldf, prep)

# Bind the data frames together by rows. Call the resulting data frame fdi_oecd
fdi_oecd <- do.call("rbind", ldf)
rm(ldf) # Done with the ldf object. Remove it.

# Remove duplicates
fdi_oecd <- fdi_oecd[!duplicated(fdi_oecd),]

# Make an object called n_occur to show which rows are duplicated 
n_occur <- fdi_oecd %>% 
  group_by(repcountry, parcountry, year) %>% 
  summarise(count= n()) %>% 
  filter(count > 1)

# Investigate the cause of count being creater than one in some instances
check <- fdi_oecd %>% filter(repcountry == "Australia", parcountry == "Armenia", year == 2011)
View(check)
# Looks like it is due to the different Type_Principle values

# Pivot the data wider so the Type_Principle values become column names
fdi_oecd <- pivot_wider(fdi_oecd, values_from = "Value", names_from ="Type_Principle") 

# Rename the columns
fdi_oecd <- fdi_oecd %>%
  rename(flow_in_net = "FDI financial flows - Total_Directional principle: Inward_Net ",
         flow_out_net = "FDI financial flows - Total_Directional principle: Outward_Net ",
         pos_in_net = "FDI positions -Total_Directional principle: Inward_Net ",
         pos_out_net = "FDI positions -Total_Directional principle: Outward_Net ",
         pos_in_assets = "FDI positions -Total_Directional principle: Inward_Assets ",
         pos_out_assets = "FDI positions -Total_Directional principle: Outward_Assets ",
         pos_in_liabilities = "FDI positions -Total_Directional principle: Inward_Liabilities ",
         pos_out_liabilities = "FDI positions -Total_Directional principle: Outward_Liabilities "
         )

# Label the columns
library(Hmisc)
label(fdi_oecd$flow_in_net) <- "Inward FDI Flow - Net (USD Millions)"
label(fdi_oecd$flow_out_net) <- "Outward FDI Flow - Net (USD Millions)"
label(fdi_oecd$pos_out_net) <- "Outward FDI Position - Net (USD Millions)"
label(fdi_oecd$pos_in_net) <- "Inward FDI Position - Net (USD Millions)"
label(fdi_oecd$pos_in_assets) <- "Inward FDI Position - Assets (USD Millions)"
label(fdi_oecd$pos_out_assets) <- "Outward FDI Position - Assets (USD Millions)"
label(fdi_oecd$pos_in_liabilities) <- "Inward FDI Position - Liabilities (USD Millions)"
label(fdi_oecd$pos_out_liabilities) <- "Outward FDI Position - Liabilities (USD Millions)"

# Remove any duplicates pivoting may have created
fdi_oecd <- fdi_oecd[!duplicated(fdi_oecd),]

# Read in the dyad append ids file
source(paste(ids_path,"append_ids.R",sep=""))

# Append IDs
fdi_oecd <-  append_ids(fdi_oecd, dyad = T, breaks = F)
fdi_oecd <-  append_suffix(fdi_oecd, "FDI_OECD", dyad = T)

# Check to see if they appended
names(fdi_oecd)

# Remove any duplicates created by appending ids
fdi_oecd <- fdi_oecd[!duplicated(fdi_oecd), ]

# Check for duplicates again 
n_occur <- fdi_oecd %>% 
  group_by(repcountry, parcountry, year) %>% 
  summarise(count= n()) %>% 
  filter(count > 1)

# Look at the product
View(fdi_oecd)

# Save the prepped data
save(fdi_oecd,file=paste(preppeddata,"PREPPED_FDI_OECD_AST_LIAB_BT_070921.RDATA",sep=""))
