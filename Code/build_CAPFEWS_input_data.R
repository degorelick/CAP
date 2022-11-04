### -----------------------------------------------------
##  Build CAPFEWS Input Data Files
##    D. Gorelick (Aug 2022)
##  To be stored in degorelick/CAP repository on GitHub
### -----------------------------------------------------

rm(list=ls()) # clear memory
setwd('C:/Users/dgorelic/OneDrive - University of North Carolina at Chapel Hill/UNC/Research/IM3/CAP/Data') # set directory
library(tidyverse); options(dplyr.summarise.inform = FALSE)

## Read in all the data we need, some of which has
##  already been cleaned by explore_CAPdata.R
Historical_CAPDiversion = read.csv("CAP_diversions_summary_2008_to_2021.csv", header = TRUE)

Historical_Deliveries = read.csv("CAP_deliveries_byuser_monthly_2016_to_2021.csv", header = TRUE)
Historical_Deliveries_ForRecharge = read.csv("CAP_recharge_byuser_monthly_2016_to_2021.csv", header = TRUE)
Historical_Annual_Scheduled_Deliveries = read.csv("CAP_deliveries_byuser_summary_2016_to_2021.csv", header = TRUE)
#Historical_Deliveries = read.csv("CAP_deliveries_by_user_2008_to_2021.csv", header = TRUE)

Historical_EnergyPrices = read.csv("Palo Verde Energy Prices.csv", header = TRUE)
Historical_PowerUse = read.csv("CAP_power_data_2008_to_2021.csv", header = TRUE)
Historical_Budget = read.csv("CAP_annual_budgets_2011_to_2020.csv", header = TRUE)
Historical_Rates = read.csv("CAP_annual_rates_2011_to_2020.csv", header = TRUE)
Historical_MeadElevation = read.csv("LakeMead_HistoricalMonthlyElevation_2008_to_2021.csv", header = TRUE) # from USBR website

CRSS_MeadElevation = readxl::read_xlsx("PNNL.xlsx", sheet = "Mead Pool elevation")
CRSS_CAPDiversion = readxl::read_xlsx("PNNL.xlsx", sheet = "CAP")
CRSS_ShortageSummary = readxl::read_xlsx("PNNL.xlsx", sheet = "Shortage")
#CRSS_AZDiversion = readxl::read_xlsx("PNNL.xlsx", sheet = "AZ") # may not need this

### Collect Historic and CRSS Water Availability -----------------------------------------------------
## Collect Lake Mead projections from CRSS
##  and historical water withdrawals into CAP system

##    PNNL.xlsx - CRSS Mead elevation, CAP diversion request traces, 2023-2054 by month
##      can use averages like 24MS for initial tests?
CRSS_CAPDiversion_Organized = NA
CRSS_MeadElevation_Organized = NA

##    forecast Historical 2008 to 2021.xlsx - top table colorado river diversions, by month
##    (im collecting this with some new code, but CO River diversions is the same as
##     "VOLUME PASSING HAV" in the historical data too)
Historical_CAPDiversion_Organized = Historical_CAPDiversion %>% 
  filter(Group == "COLORADO RIVER DIVERSIONS") %>%
  select(Jan:Mar, Apr:Jun, Jul:Sep, Oct:Dec, Year) %>%
  pivot_longer(cols = -starts_with('Year'), names_to = 'Month', values_to = 'CAP_div') %>% 
  mutate(Month = match(Month, month.abb)) %>%
  mutate(datetime = lubridate::make_datetime(year = Year, month = Month)) %>%
  select(datetime, CAP_div)
  
Historical_MeadElevation_Organized = Historical_MeadElevation %>% drop_na() %>% 
  pivot_longer(cols = -starts_with('Year'), names_to = 'Month', values_to = 'MDE_ele') %>% 
  mutate(Month = match(Month, toupper(month.abb))) %>%
  mutate(datetime = lubridate::make_datetime(year = Year, month = Month)) %>%
#  mutate(datetime = format(as.Date(datetime, '%Y-%m-%d'), "%m/%d/%Y")) %>%
  select(datetime, MDE_ele)

Historical_PleasantElevation_Organized = Historical_PowerUse %>% 
  filter(Table == "Lake Pleasant Projected EOM Elevation (ft)") %>%
  select(Year, Month, Value) %>% rename("PLS_ele" = "Value") %>%
  mutate(Month = match(Month, toupper(month.abb))) %>%
  mutate(datetime = lubridate::make_datetime(year = Year, month = Month)) %>%
  select(datetime, PLS_ele)

Historical_PleasantInflow_Organized = Historical_CAPDiversion %>% 
  filter(Group == "WADDELL PUMPING") %>%
  select(Jan:Mar, Apr:Jun, Jul:Sep, Oct:Dec, Year) %>%
  pivot_longer(cols = -starts_with('Year'), names_to = 'Month', values_to = 'PLS_pumping') %>% 
  mutate(Month = match(Month, month.abb)) %>%
  mutate(datetime = lubridate::make_datetime(year = Year, month = Month)) %>%
  select(datetime, PLS_pumping)

Historical_PleasantOutflow_Organized = Historical_CAPDiversion %>% 
  filter(Group == "WADDELL RELEASES") %>%
  select(Jan:Mar, Apr:Jun, Jul:Sep, Oct:Dec, Year) %>%
  pivot_longer(cols = -starts_with('Year'), names_to = 'Month', values_to = 'PLS_hydrorelease') %>% 
  mutate(Month = match(Month, month.abb)) %>%
  mutate(datetime = lubridate::make_datetime(year = Year, month = Month)) %>%
  select(datetime, PLS_hydrorelease) %>% mutate(PLS_hydrorelease = PLS_hydrorelease * -1)

Historical_CAPCanalLosses_Organized = Historical_CAPDiversion %>% 
  filter(Group == "TOTAL CANAL LOSSES") %>%
  select(Jan:Mar, Apr:Jun, Jul:Sep, Oct:Dec, Year) %>%
  pivot_longer(cols = -starts_with('Year'), names_to = 'Month', values_to = 'CAP_losses') %>% 
  mutate(Month = match(Month, month.abb)) %>%
  mutate(datetime = lubridate::make_datetime(year = Year, month = Month)) %>%
  select(datetime, CAP_losses)


# OUTPUT DATA TABLE FOR INITIAL CAPFEWS TESTS
cap_data = Historical_MeadElevation_Organized %>% 
  left_join(Historical_PleasantElevation_Organized, by = 'datetime') %>%
  left_join(Historical_PleasantInflow_Organized, by = 'datetime') %>%
  left_join(Historical_PleasantOutflow_Organized, by = 'datetime') %>%
  left_join(Historical_CAPDiversion_Organized, by = 'datetime') %>%
  left_join(Historical_CAPCanalLosses_Organized, by = 'datetime')
cap_data_short = cap_data %>% filter(lubridate::year(datetime) > 2012)
write.table(cap_data_short, "../CAPFEWS/calfews_src/data/input/cap-data.csv", sep = ",", row.names = FALSE, col.names = TRUE)

### Collect Historic Contractor Delivery Data -----------------------------------------------------
## Collect major contractor historical deliveries
##  and organize by priority, lease, use (banking/storage/recharge)
##  FIRST STEP HERE: reorganize and clean, keep top-20 users
rights = read.csv("CAP_Subcontracts_CAPOnlineMap.csv", header = TRUE)
UsersToKeep = c("AWBA", "FMYN", "WMAT", "SRPMIC",
                "Ak-Chin", "GRIC", "SCAT", "Tohono O'odham", 
                "HIDD", "HVID", "AZWC", "CAGRD", "CAIDD", "MSIDD", "ASARCO",
                "Chandler", "Gilbert", "Glendale", "Mesa", "Peoria", "AZ State Land",
                "Phoenix", "Scottsdale", "Tempe", "Tucson", "Surprise", "Goodyear")
UsersToKeep = union(UsersToKeep, unique(rights$DavidName))

Historical_Deliveries_Organized = Historical_Deliveries %>%
  mutate(Month = match(Month, month.abb)) %>%
  mutate(datetime = lubridate::make_datetime(year = Year, month = Month)) %>%
  filter(Group != "Summary") %>% select(-c(Data,Year,Month)) %>%
  filter(User != "Total") %>%
  mutate(`Agreement` = ifelse(`Agreement` %in% 
                                c("A--",
                                  "A-RWCD",
                                  "A-Wellton-Mohawk",
                                  "A-Yavapai-Prescott",
                                  "A-CDR"), "Assignment", `Agreement`)) %>% # CDR IS ONLY ASSIGNMENT THAT ISNT FEDERAL WATER
  mutate(`Agreement` = ifelse(`Agreement` %in% 
                                c("UO",
                                  "Unscheduled Overrun"), NA, `Agreement`)) %>%
  mutate(`Agreement` = ifelse(`Agreement` %in% 
                                c("Sub",
                                  "Contract"), NA, `Agreement`)) %>%
  mutate(User = ifelse(User %in% 
                         c("Freeport-Miami",
                           "Freeport-Morenci",
                           "Freeport-Safford"), "Freeport", User)) %>%
  mutate(User = ifelse(User %in% 
                         c("AWBA Interstate",
                           "AWBA Phx AMA",
                           "AWBA Pinal AMA",
                           "AWBA Tucson AMA"), "AWBA", User)) %>%
  mutate(User = ifelse(User %in% 
                         c("AZWC, Casa Grande",
                           "AZWC, Coolidge",
                           "AZWC, Superstition",
                           "AZWC, White Tank"), "AZWC", User)) %>%
  mutate(User = ifelse(User %in% 
                         c("EPCOR, AF",
                           "EPCOR, PV",
                           "EPCOR, SC",
                           "EPCOR, SCW"), "EPCOR", User)) %>%
  mutate(User = ifelse(User %in% 
                         c("Tohono O'odham - SX",
                           "Tohono O'odham - ST"), "Tohono O'odham", User)) %>%
  mutate(Partner = ifelse(Partner %in% 
                         c("Tohono O'odham - SX",
                           "Tohono O'odham - ST"), "Tohono O'odham", Partner)) %>%
  mutate(Group = ifelse(Group %in% c("Excess - Other Excess"), "Excess", Group)) %>%
  mutate(Group = ifelse(Group %in% c("Excess - Ag Pool"), "Ag Pool", Group)) %>%
  mutate(Group = ifelse(Group %in% c("Federal On-Res", "Federal Off-Res"), "Federal", Group)) %>%
  mutate(Partner = ifelse(Partner == User, NA, Partner)) %>%
  mutate(User = ifelse(User %in% UsersToKeep, User, "OTHER"))

## STEP 2: AGGREGATE USE AFTER CLEANING
Historical_Deliveries_Organized_Grouped = Historical_Deliveries_Organized %>%
  group_by(datetime, User, Partner, Agreement, Group) %>% summarize(total_deliveries = sum(deliveries))

### STEP 3: IDENTIFY DELIVERIES TO THESE USERS THAT ARE USED FOR GSF/USF RECHARGE ------------
##   AND COMBINE WITH DELIVERIES ABOVE
colnames(Historical_Deliveries_ForRecharge)[3] = "User"
Historical_Deliveries_ForRecharge_Organized = Historical_Deliveries_ForRecharge %>%
  filter(User != "Total") %>% filter(AMA != "Total") %>%
  mutate(User = ifelse(User %in% 
                         c("Freeport-Miami",
                           "Freeport-Morenci",
                           "Freeport-Safford"), "Freeport", User)) %>%
  mutate(User = ifelse(User %in% 
                         c("AWBA Interstate",
                           "AWBA Phx AMA",
                           "AWBA Pinal AMA",
                           "AWBA Tucson AMA"), "AWBA", User)) %>%
  mutate(User = ifelse(User %in% 
                         c("AZWC, Casa Grande",
                           "AZWC, Coolidge",
                           "AZWC, Superstition",
                           "AZWC, White Tank"), "AZWC", User)) %>%
  mutate(User = ifelse(User %in% 
                         c("EPCOR, AF",
                           "EPCOR, PV",
                           "EPCOR, SC",
                           "EPCOR, SCW"), "EPCOR", User)) %>%
  mutate(User = ifelse(User %in% 
                         c("Tohono O'odham - SX",
                           "Tohono O'odham - ST"), "Tohono O'odham", User)) %>%
  mutate(User = ifelse(User %in% UsersToKeep, User, "OTHER")) %>%
  mutate(Month = match(Month, month.abb)) %>%
  mutate(datetime = lubridate::make_datetime(year = Year, month = Month)) 

###  STEP 3B: SORT RECHARGE DELIVERIES BY FACILITY FOR CAPFEWS AND EXPORT  ------------
## NOTE: THIS IS REPLACED BY AGGREGATING BY AMA FOR CAPFEWS, DONE BELOW

##  STEP 3Bb: SORT RECHARGE DELIVERIES BY AMA FOR CAPFEWS AND EXPORT  
RCRG_byAMA = Historical_Deliveries_ForRecharge_Organized %>%
  select(datetime, User, AMA, deliveries) %>%
  group_by(datetime, User, AMA) %>% summarize(total_recharge = sum(deliveries)) %>%
  pivot_wider(names_from = User, values_from = total_recharge)

write.table(RCRG_byAMA, 
            paste("AMA_deliveries_recharge_byAMA.csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)

RCRG_byAMA_long = Historical_Deliveries_ForRecharge_Organized %>%
  select(datetime, User, AMA, deliveries) %>%
  group_by(datetime, User, AMA) %>% summarize(total_recharge = sum(deliveries))

# separate these out into files with delivered recharge water 
for (ama in unique(RCRG_byAMA_long$AMA)) {
  single_ama = RCRG_byAMA_long %>% 
    filter(AMA == ama)  %>% pivot_wider(names_from = User, values_from = total_recharge)
  
  # add missing users in deliveries file with zeroes
  for (missing_user in setdiff(UsersToKeep, colnames(single_ama))) {
    if (missing_user %in% colnames(single_ama)) {} else {
      single_ama[, missing_user] = 0
    }
  }
  single_ama = single_ama[,which(colnames(single_ama) != "AMA")]
  
  single_ama = single_ama %>%
    select(order(colnames(single_ama))) %>%
    select(datetime, everything()) %>%
    replace(is.na(single_ama), 0)
  
  single_ama[is.na(single_ama)] = 0
  
  # export these for madison
  write.table(single_ama, 
              paste(ama, "AMA_deliveries_recharge.csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
}

# also print total recharge deliveries, to later calculate the fraction
# of deliveries made to each AMA by user, and their seasonality
RCRG_total = Historical_Deliveries_ForRecharge_Organized %>%
  select(datetime, User, deliveries) %>%
  group_by(datetime, User) %>% summarize(total_recharge = sum(deliveries)) %>%
  pivot_wider(names_from = User, values_from = total_recharge)
# add missing users in deliveries file with zeroes
for (missing_user in setdiff(UsersToKeep, colnames(RCRG_total))) {
  if (missing_user %in% colnames(RCRG_total)) {} else {
    RCRG_total[, missing_user] = 0
  }
}
RCRG_total = RCRG_total[,which(colnames(RCRG_total) != "AMA")]

RCRG_total = RCRG_total %>%
  select(order(colnames(RCRG_total))) %>%
  select(datetime, everything()) %>%
  replace(is.na(RCRG_total), 0)
RCRG_total[is.na(RCRG_total)] = 0

# export these
write.table(RCRG_total, 
            paste("AMA_total_deliveries_recharge.csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)

# calculate seasonality of recharge deliveries
RCRG_seasonality = Historical_Deliveries_ForRecharge_Organized %>% 
  mutate(Month = lubridate::month(datetime)) %>%
  select(Month, User, deliveries) %>%
  group_by(Month, User) %>% summarize(total_recharge = sum(deliveries)) %>%
  pivot_wider(names_from = User, values_from = total_recharge)
RCRG_seasonality = apply(RCRG_seasonality, 2, function(x) {x/sum(x, na.rm = TRUE)})

# add missing users in deliveries file with zeroes
RCRG_seasonality = as.data.frame(RCRG_seasonality)
for (missing_user in setdiff(UsersToKeep, colnames(RCRG_seasonality))) {
  if (missing_user %in% colnames(RCRG_seasonality)) {} else {
    RCRG_seasonality[, missing_user] = 0
  }
}
RCRG_seasonality = RCRG_seasonality %>%
  select(order(colnames(RCRG_seasonality))) %>%
  select(Month, everything()) %>%
  replace(is.na(RCRG_seasonality), 0)
RCRG_seasonality[is.na(RCRG_seasonality)] = 0

# give month column its names back
RCRG_seasonality$Month = month.abb

# export these
write.table(RCRG_seasonality, 
            paste("user_deliveries_recharge_seasonality.csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)

# calculate fraction of deliveries each user sends to recharge, by calendar month (seasonality average)
DEL_seasonality = Historical_Deliveries_Organized %>% 
  mutate(Month = lubridate::month(datetime)) %>%
  select(Month, User, deliveries) %>%
  group_by(Month, User) %>% summarize(total_recharge = sum(deliveries)) %>%
  pivot_wider(names_from = User, values_from = total_recharge)
RCRG_seasonality = Historical_Deliveries_ForRecharge_Organized %>% 
  mutate(Month = lubridate::month(datetime)) %>%
  select(Month, User, deliveries) %>%
  group_by(Month, User) %>% summarize(total_recharge = sum(deliveries)) %>%
  pivot_wider(names_from = User, values_from = total_recharge)

# make datasets match, then compare
for (missing_user in setdiff(UsersToKeep, colnames(RCRG_seasonality))) {
  if (missing_user %in% colnames(RCRG_seasonality)) {} else {
    RCRG_seasonality[, missing_user] = 0
  }
}
RCRG_seasonality = RCRG_seasonality %>%
  select(order(colnames(RCRG_seasonality))) %>%
  select(Month, everything()) %>%
  replace(is.na(RCRG_seasonality), 0)
RCRG_seasonality[is.na(RCRG_seasonality)] = 0

for (missing_user in setdiff(UsersToKeep, colnames(DEL_seasonality))) {
  if (missing_user %in% colnames(DEL_seasonality)) {} else {
    DEL_seasonality[, missing_user] = 0
  }
}
DEL_seasonality = DEL_seasonality %>%
  select(order(colnames(DEL_seasonality))) %>%
  select(Month, everything()) %>%
  replace(is.na(DEL_seasonality), 0)
DEL_seasonality[is.na(DEL_seasonality)] = 0

# find the fractional recharging
RCRG_seasonality_fraction = RCRG_seasonality / DEL_seasonality
RCRG_seasonality_fraction[is.na(RCRG_seasonality_fraction)] = 0

# give month column its names back
RCRG_seasonality_fraction$Month = month.abb

# export these
write.table(RCRG_seasonality_fraction, 
            paste("user_deliveries_recharge_seasonality_fraction.csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)



### STEP 4: WIDEN THE DATA TO MAKE INPUT FILES FOR CAPFEWS, ADD MISSING USERS -----------------------
##  (for deliveries, not recharge, I want total to each user
##   and the splits by priority type and purpose separately)
Historical_Deliveries_ForRecharge_Organized_Wide = Historical_Deliveries_ForRecharge_Organized %>%
  select(datetime, User, deliveries) %>%
  group_by(datetime, User) %>% summarize(total_recharge = sum(deliveries)) %>%
  pivot_wider(names_from = User, values_from = total_recharge)

Historical_Deliveries_Organized_Wide = Historical_Deliveries_Organized_Grouped %>%
  select(datetime, User, total_deliveries) %>%
  group_by(datetime, User) %>% summarise(total_deliveries = sum(total_deliveries)) %>%
  pivot_wider(names_from = User, values_from = total_deliveries)
Historical_Deliveries_Organized_Wide[is.na(Historical_Deliveries_Organized_Wide)] = 0

# add missing users in deliveries file with zeroes
for (missing_user in setdiff(UsersToKeep, colnames(Historical_Deliveries_Organized_Wide))) {
  if (missing_user %in% colnames(Historical_Deliveries_Organized_Wide)) {} else {
    Historical_Deliveries_Organized_Wide[, missing_user] = 0
  }
}
Historical_Deliveries_Organized_Wide = Historical_Deliveries_Organized_Wide %>%
  select(order(colnames(Historical_Deliveries_Organized_Wide))) %>%
  select(datetime, everything()) %>%
  replace(is.na(Historical_Deliveries_Organized_Wide), 0)

# add missing users in recharge file (not all top-20 contractors do recharge) with zeroes
for (missing_user in colnames(Historical_Deliveries_Organized_Wide)) {
  if (missing_user %in% colnames(Historical_Deliveries_ForRecharge_Organized_Wide)) {} else {
    Historical_Deliveries_ForRecharge_Organized_Wide[, missing_user] = 0
  }
}
Historical_Deliveries_ForRecharge_Organized_Wide = Historical_Deliveries_ForRecharge_Organized_Wide %>%
  select(order(colnames(Historical_Deliveries_ForRecharge_Organized_Wide))) %>%
  select(datetime, everything()) %>%
  replace(is.na(Historical_Deliveries_ForRecharge_Organized_Wide), 0)

# export these for madison
write.table(Historical_Deliveries_Organized_Wide, "user_monthly_deliveries.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(Historical_Deliveries_ForRecharge_Organized_Wide, "user_monthly_deliveries_recharge.csv", sep = ",", row.names = FALSE, col.names = TRUE)

### STEP 5: EXTRACT THE SEASONALITY CURVES OF DEMAND FOR MAJOR USERS ------------------------
for (year in unique(lubridate::year(Historical_Deliveries_Organized_Wide$datetime))) {
  year_deliveries_by_major_user = Historical_Deliveries_Organized_Wide %>% 
    filter(lubridate::year(datetime) == year) 
  year_deliveries_by_major_user[is.na(year_deliveries_by_major_user)] = 0
  if (year == 2016) {
    year_deliveries_by_major_user_totaltable = year_deliveries_by_major_user[,2:ncol(year_deliveries_by_major_user)]
  } else {
    year_deliveries_by_major_user_totaltable = year_deliveries_by_major_user_totaltable +
      year_deliveries_by_major_user[,2:ncol(year_deliveries_by_major_user)]
  }
}
year_deliveries_by_major_user_totaltable_seasonality = 
  apply(year_deliveries_by_major_user_totaltable, MARGIN = 2, 
        function(x) {x/sum(x)})
year_deliveries_by_major_user_totaltable_seasonality = as.data.frame(year_deliveries_by_major_user_totaltable_seasonality)
year_deliveries_by_major_user_totaltable_seasonality$Month = month.abb

# export these for madison
write.table(year_deliveries_by_major_user_totaltable_seasonality, "user_seasonality_deliveries.csv", sep = ",", row.names = FALSE, col.names = TRUE)

  
### STEP 6: EXTRACT THE FRACTION OF DELIVERIES THROUGH LEASE AGREEMENTS ---------------
  # need to grab data from the second section of data analysis to run this section
Deliveries_ByUse = Historical_Deliveries_Organized %>%
  group_by(datetime, User, Partner, Agreement, Group) %>% summarise(total_use = sum(deliveries))

## what fraction of deliveries are from each class/partner?
Deliveries_Leased = Deliveries_ByUse %>%
  filter(Agreement %in% c("Lease")) %>%
  group_by(datetime, User, Partner, Group) %>% summarise(total_lease = sum(total_use)) %>%
#  pivot_wider(names_from = Partner, values_from = total_lease) %>%
  select(-Group)

# separate these out into files with delivered lease water and (negated) "donated" lease water
for (leaser in unique(Deliveries_Leased$Partner)) {
  single_leaser = Deliveries_Leased %>% 
    filter(Partner == leaser) %>% pivot_wider(names_from = User, values_from = total_lease)
  
  # add missing users in deliveries file with zeroes
  for (missing_user in setdiff(UsersToKeep, colnames(single_leaser))) {
    if (missing_user %in% colnames(single_leaser)) {} else {
      single_leaser[, missing_user] = 0
    }
  }
  single_leaser = single_leaser %>%
    select(order(colnames(single_leaser))) %>%
    select(datetime, everything()) %>%
    replace(is.na(single_leaser), 0) %>%
    select(-Partner)
  
  single_leaser[is.na(single_leaser)] = 0
  
  # export these for madison
  write.table(single_leaser %>% select(-Partner), 
              paste(leaser, "_lease_deliveries.csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
}


### TABULATE EXISTING LEASE AGREEMENT CAPACITIES AND THEIR PRIORITY -------------
# some background, with numbers compiled by hand:
#  https://www.peoriaaz.gov/government/departments/water-services/water-supply
#  Jason Hauter (lawer for GRIC) confirms only CAGRD lease is NIA Priority, rest Indian
#  david has other pdfs of lease agreements
Providers = c("GRIC", "SRPMIC", "Ak-Chin", "SCAT", "FMYN", "Tohono O'odham")
Users = c("ASARCO", "CAGRD", "Chandler", "Gilbert", "Glendale", 
          "Goodyear", "Mesa", "Peoria", "Phoenix", "Scottsdale", 
          "Tempe", "OTHER")
Lease_Matrix = matrix(data = NA, nrow = length(Providers), ncol = length(Users), 
                      dimnames = list(Providers, Users))
Lease_Priority_Matrix = matrix(data = NA, nrow = length(Providers), ncol = length(Users), 
                               dimnames = list(Providers, Users))

# manually assign lease capacities
Lease_Matrix["GRIC","CAGRD"] = 18185
Lease_Matrix["GRIC","Chandler"] = 2450
Lease_Matrix["GRIC","OTHER"] = 10000 + 12000 + 1000
Lease_Matrix["GRIC","Goodyear"] = 7000
Lease_Matrix["GRIC","Peoria"] = 7000
Lease_Matrix["GRIC","Phoenix"] = 15000
Lease_Matrix["GRIC","Scottsdale"] = 12000

Lease_Matrix["SRPMIC","Chandler"] = 2586
Lease_Matrix["SRPMIC","Gilbert"] = 4088
Lease_Matrix["SRPMIC","Glendale"] = 1814
Lease_Matrix["SRPMIC","Mesa"] = 1669
Lease_Matrix["SRPMIC","Phoenix"] = 3023
Lease_Matrix["SRPMIC","Scottsdale"] = 60
Lease_Matrix["SRPMIC","Tempe"] = 60

Lease_Matrix["SCAT","Gilbert"] = 17371
Lease_Matrix["SCAT","Scottsdale"] = 12500
Lease_Matrix["SCAT","OTHER"] = 14000

Lease_Matrix["FMYN","Phoenix"] = 4300

Lease_Matrix["Tohono O'odham","ASARCO"] = 10000

Lease_Matrix["Ak-Chin","OTHER"] = 7356

# manually assign lease priorities - everything Indian Priority except 2 cases
Lease_Priority_Matrix[!is.na(Lease_Matrix)] = "FED"
Lease_Priority_Matrix["GRIC","CAGRD"] = "NIA"
Lease_Priority_Matrix["Tohono O'odham","ASARCO"] = "NIA"

# convert to dataframes for ease of indexing
Lease_Matrix = as.data.frame(Lease_Matrix); Lease_Matrix$Partner = Providers
Lease_Priority_Matrix = as.data.frame(Lease_Priority_Matrix); Lease_Priority_Matrix$Partner = Providers
Lease_Matrix = Lease_Matrix %>% arrange(Partner)
Lease_Priority_Matrix = Lease_Priority_Matrix %>% arrange(Partner)

# match these tables against deliveries to see what fraction of lease capacity is
# delivered in 2021 vs. on average since 2016
Deliveries_Leased_2021 = Deliveries_ByUse %>%
  filter(lubridate::year(datetime) > 2020) %>%
  filter(Agreement %in% c("Lease")) %>%
  group_by(User, Partner, Group) %>% summarise(total_lease = sum(total_use)) %>% # 1 year of data annualized
  pivot_wider(names_from = User, values_from = total_lease) %>%
  select(-Group) %>% mutate("ASARCO" = NA) %>% select(colnames(Lease_Matrix)) %>% arrange(Partner)
Deliveries_Leased_2021[6,] = NA; Deliveries_Leased_2021$Partner[6] = "Tohono O'odham"; Deliveries_Leased_2021$ASARCO[6] = 10000
rownames(Deliveries_Leased_2021) = Deliveries_Leased_2021$Partner

Lease_Use_Fraction_Of_Capacity = as.data.frame(Deliveries_Leased_2021)
for (row in 1:nrow(Lease_Use_Fraction_Of_Capacity)) {
  for (col in 1:(ncol(Lease_Use_Fraction_Of_Capacity)-1)) {
    if (is.na(Lease_Use_Fraction_Of_Capacity[row,col])) {} else {
      Lease_Use_Fraction_Of_Capacity[row,col] = signif(as.numeric(Lease_Use_Fraction_Of_Capacity[row,col]) / Lease_Matrix[row,col],
                                                       digits = 2)
    }
  }
}
rownames(Lease_Use_Fraction_Of_Capacity) = Lease_Use_Fraction_Of_Capacity$Partner

# correct fractional use estimates based on wider range of historic values
# ex: 2021 Phoenix did not use very much of their lease with FMYN, but in other years did
Deliveries_Leased_Avg = Deliveries_ByUse %>%
  filter(Agreement %in% c("Lease")) %>%
  group_by(User, Partner, Group) %>% summarise(total_lease = sum(total_use)/6) %>% # 6 years of data annualized
  pivot_wider(names_from = User, values_from = total_lease) %>%
  select(-Group)

Lease_Use_Fraction_Of_Capacity$Phoenix[which(Lease_Use_Fraction_Of_Capacity$Partner == "FMYN")] = 
  signif(Deliveries_Leased_Avg$Phoenix[which(Deliveries_Leased_Avg$Partner == "FMYN")] / 
         Lease_Matrix$Phoenix[which(Lease_Matrix$Partner == "FMYN")], digits = 2)

# export these results to build json files
write.table(Lease_Matrix, "user_lease.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(Lease_Priority_Matrix, "user_lease_priorities.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(Lease_Use_Fraction_Of_Capacity, "user_lease_fractions.csv", sep = ",", row.names = FALSE, col.names = TRUE)


### Collect historic water delivery rate components ---------------------------------
##  and reconciliation rate updates
Historical_Rates_Organized = Historical_Rates[
    as.logical(rowSums(sapply(c("rate", "Rate"), grepl, Historical_Rates$Group))),]
colnames(Historical_Rates_Organized)[2:11] = seq(2011,2020,1) 

# key for rate components:
#  a) Total = Total Energy + Fixed OM&R
#   b) Fixed OM&R = Fixed O&M + CIP (Big R) + Rate Stabilization
#   c) Total Energy = Pumping + Decommissioning
Historical_Rates_Organized$Variable =   c("FixedOM","CIP","RateStabilization","FixedOMR", 
                                          "Pumping","Decommissioning", "TotalEnergy", "Total", 
                                          "FixedOM","CIP","RateStabilization","FixedOMR", 
                                          "Pumping","Decommissioning", "TotalEnergy", "Total")                              
Historical_Rates_Organized$Group = c(rep("Budgeted", 8), rep("Reconciliation", 8))

Historical_Rates_Organized = Historical_Rates_Organized %>% 
  pivot_longer(cols = -c('Variable', 'Group'), names_to = 'Year', values_to = 'rate') %>%
  mutate(datetime = lubridate::make_datetime(year = as.numeric(Year))) %>%
  select(datetime, Group, Variable, rate) %>%
  pivot_wider(names_from = c(Variable, Group), values_from = rate)

### Collect historic power market energy prices -------------------------------------
##  and pumped water quantities 
Historical_PumpingEnergy_Organized = Historical_PowerUse %>%
  filter(Table == "CAP Pumping Plants - Projection of Energy Use - For Waddell Filling Only" |
           Table == "CAP Pumping Plants - Projection of Energy Use - For Deliveries Only" |
           Table == "CAP Pumping Plants - Projection of Energy Use")

 
### Collect and aggregate entitlement info for CAP contractors --------------------------
##  total in each delivery priority class
#     classes: M&I, NIA, Indian, P3
#     numbers from: CAP Subcontracting Status Report - April 2022
#       totals for 2022: TOTAL = 1,294,717 AF
#                          M&I =   620,678 
#                          NIA =    44,530 +  47,303 +  5,000 + 102,000 + 18,100 + 16,000
#                       Indian =   555,806 +     500          - 102,000 - 18,100 - 16,000 - 50,000
#                           P3 =    22,000 (Wellton-Mohawk IDD, 20,900 AF after system loss)
#                                 + 50,000 (Ak-Chin, 47,500 AF after system loss)
#     Several users here (Ag users) have no P3 or P4 rights:
#       AWBA, CAIDD, HIDD, HVID, MSIDD deliveries
#       are ENTIRELY of excess CAP water (Ag/Excess Pool)

# OCT 2022: Current Entitlements are also available by priority class
# here: https://library.cap-az.com/maps/capallocations
# madison collected this data, repeat for more contractors with her numbers
rights = read.csv("CAP_Subcontracts_CAPOnlineMap.csv", header = TRUE)
UsersToKeep = c("AWBA", "FMYN", "WMAT", "SRPMIC",
                "Ak-Chin", "GRIC", "SCAT", "Tohono O'odham", 
                "HIDD", "HVID", "AZWC", "CAGRD", "CAIDD", "MSIDD", "ASARCO",
                "Chandler", "Gilbert", "Glendale", "Mesa", "Peoria", "AZ State Land",
                "Phoenix", "Scottsdale", "Tempe", "Tucson", "Surprise", "Goodyear")
AllUsersToKeep = union(UsersToKeep, unique(rights$DavidName))

# first collect from madison's dataset and fill in the rest manually 
# with data from CAP entitlement doc
entitlement_values = c()
for (name in unique(rights$DavidName)) {
  user = rights %>% filter(DavidName == name)
  user = as.data.frame(t(as.data.frame(colSums(user[,2:5]))))
  
  # update a handful of entitlements
  if (name == "CAGRD") {
    user$M.I.Priority.Volume. = "6426"
    user$NIA.Priority.Volume. = "18185" # THERE IS ACTUALLY A GRIC LEASE OF THE EXACT SAME SIZE AS WELL...
  }
  
  entitlement_values = rbind(entitlement_values,
                             c(name, 
                               user$P3.Priority.Volume., 
                               user$M.I.Priority.Volume., 
                               user$Indian.Priority.Volume.,
                               user$NIA.Priority.Volume.))
}

# fill in remaining names
RemainingUsers = setdiff(AllUsersToKeep, unique(rights$DavidName))
RemainingEntitlements = rbind(c("AWBA", "0", "0", "0", "0"),
                          c("WMAT", "0", "0", "1218", "0"),
                          c("HIDD", "0", "0", "0", "0"),
                          c("HVID", "0", "0", "0", "0"),
                          c("CAIDD", "0", "0", "0", "0"),
                          c("MSIDD", "0", "0", "0", "0"))
entitlement_values = rbind(entitlement_values,
                           RemainingEntitlements)
colnames(entitlement_values) = c("User", "PTR", "MUI", "FED", "NIA")
entitlement_values = as.data.frame(entitlement_values)
entitlement_values = entitlement_values %>% mutate_at(vars(-User), as.numeric)

# square up these values with the total entitlement counts in 2022
# hope that these are right...
PTR_total_entitlement = (22000 + 50000) * 0.95 # after taking 5% system loss out
MUI_total_entitlement = (620678)
FED_total_entitlement = (555806 - 500 - 102000 - 18100 - 16000 - 50000)
NIA_total_entitlement = (44530 + 47303 + 5000 + 102000 + 18100 + 16000)

# print for a sanity check
colSums(entitlement_values[,2:5])
c(PTR_total_entitlement, MUI_total_entitlement, FED_total_entitlement, NIA_total_entitlement)

entitlement_values$MUI[which(entitlement_values$User == "OTHER")] = 
  entitlement_values$MUI[which(entitlement_values$User == "OTHER")] +
  MUI_total_entitlement - sum(entitlement_values$MUI)
entitlement_values$FED[which(entitlement_values$User == "OTHER")] = 
  entitlement_values$FED[which(entitlement_values$User == "OTHER")] +
  FED_total_entitlement - sum(entitlement_values$FED)
entitlement_values$NIA[which(entitlement_values$User == "OTHER")] = 
  entitlement_values$NIA[which(entitlement_values$User == "OTHER")] +
  NIA_total_entitlement - sum(entitlement_values$NIA)

# export the numbers and the fractions of total entitlement classes
# with code names for capfews, ordered alphabetically
entitlement_values = entitlement_values %>% arrange(User)
UsersCodes = c("AKC", "ASR", "AWB", "ASL", "AWC", "CGD", "CAD",
               "CHD", "EPC", "FYN", "GIL", "GLN", "GYR", "GRC", 
               "HID", "HVD", "MSA", "MSD", "MWD", "ORO", "OTH",
               "PEO", "PHX", "SCT", "SDL", "SIC", "SUR", "TPE",
               "TOH", "TUC", "WMT")
UsersTurnouts = c("SGL", "BLK", "SGL", "HSY", "SGL", "SGL", "SGL", 
                  "SGL", "HSY", "HSY", "SGL", "HSY", "HSY", "SGL",
                  "SGL", "LHQ", "SGL", "SGL", "HSY", "PIC", "SGL",
                  "HSY", "HSY", "HSY", "HSY", "HSY", "HSY", "HSY",
                  "BRW", "BRW", "HSY")
entitlement_values$Code = UsersCodes
entitlement_values$Turnout = UsersTurnouts

write.table(entitlement_values, "user_entitlements.csv", sep = ",", row.names = FALSE, col.names = TRUE)

entitlement_values_norm = apply(entitlement_values[,2:5], 2, function(x) {x/sum(x)})
entitlement_values_norm = data.frame("User" = entitlement_values$User, 
                                     entitlement_values_norm, 
                                     "Code" = entitlement_values$Code, 
                                     "Turnout" = entitlement_values$Turnout)
write.table(entitlement_values_norm, "user_entitlements_fractions.csv", sep = ",", row.names = FALSE, col.names = TRUE)
