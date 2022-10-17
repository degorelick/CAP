### -----------------------------------------------------
##  Build CAPFEWS Input Data Files
##    D. Gorelick (Aug 2022)
##  To be stored in degorelick/CAP repository on GitHub
### -----------------------------------------------------

rm(list=ls()) # clear memory
setwd('C:/Users/dgorelic/OneDrive - University of North Carolina at Chapel Hill/UNC/Research/IM3/CAP/Data') # set directory
library(tidyverse); options(dplyr.summarise.inform = FALSE)

### -----------------------------------------------------
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

### -----------------------------------------------------
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
  pivot_longer(cols = -starts_with('Year'), names_to = 'Month', values_to = 'PLS_inf') %>% 
  mutate(Month = match(Month, month.abb)) %>%
  mutate(datetime = lubridate::make_datetime(year = Year, month = Month)) %>%
  select(datetime, PLS_inf)

Historical_PleasantOutflow_Organized = Historical_CAPDiversion %>% 
  filter(Group == "WADDELL RELEASES") %>%
  select(Jan:Mar, Apr:Jun, Jul:Sep, Oct:Dec, Year) %>%
  pivot_longer(cols = -starts_with('Year'), names_to = 'Month', values_to = 'PLS_out') %>% 
  mutate(Month = match(Month, month.abb)) %>%
  mutate(datetime = lubridate::make_datetime(year = Year, month = Month)) %>%
  select(datetime, PLS_out) %>% mutate(PLS_out = PLS_out * -1)

Historical_CAPCanalLosses_Organized = Historical_CAPDiversion %>% 
  filter(Group == "TOTAL CANAL LOSSES") %>%
  select(Jan:Mar, Apr:Jun, Jul:Sep, Oct:Dec, Year) %>%
  pivot_longer(cols = -starts_with('Year'), names_to = 'Month', values_to = 'CAP_los') %>% 
  mutate(Month = match(Month, month.abb)) %>%
  mutate(datetime = lubridate::make_datetime(year = Year, month = Month)) %>%
  select(datetime, CAP_los)


# OUTPUT DATA TABLE FOR INITIAL CAPFEWS TESTS
cap_data = Historical_MeadElevation_Organized %>% 
  left_join(Historical_PleasantElevation_Organized, by = 'datetime') %>%
  left_join(Historical_PleasantInflow_Organized, by = 'datetime') %>%
  left_join(Historical_PleasantOutflow_Organized, by = 'datetime') %>%
  left_join(Historical_CAPDiversion_Organized, by = 'datetime') %>%
  left_join(Historical_CAPCanalLosses_Organized, by = 'datetime')
write.table(cap_data, "../CAPFEWS/calfews_src/data/input/cap-data.csv", sep = ",", row.names = FALSE, col.names = TRUE)

### -----------------------------------------------------
## Collect major contractor historical deliveries
##  and organize by priority, lease, use (banking/storage/recharge)
##  FIRST STEP HERE: reorganize and clean, keep top-20 users
UsersToKeep = c("AWBA", "FMYN", "WMAT", "SRPMIC",
                "Ak-Chin", "GRIC", "SCAT", "Tohono O'odham", 
                "HIDD", "HVID", "AZWC", "CAGRD", "CAIDD", "MSIDD", "ASARCO",
                "Chandler", "Gilbert", "Glendale", "Mesa", "Peoria", "AZ State Land",
                "Phoenix", "Scottsdale", "Tempe", "Tucson", "Surprise", "Goodyear")

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

## STEP 3: IDENTIFY DELIVERIES TO THESE USERS THAT ARE USED FOR GSF/USF RECHARGE
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

##  STEP 3B: SORT RECHARGE DELIVERIES BY FACILITY FOR CAPFEWS AND EXPORT  
RCRG_byFacility = Historical_Deliveries_ForRecharge_Organized %>%
  select(datetime, User, Recharge.Facility, deliveries) %>%
  group_by(User, Recharge.Facility) %>% summarize(total_recharge = sum(deliveries)) %>%
  pivot_wider(names_from = User, values_from = total_recharge)

RCRG_byFacility2021 = Historical_Deliveries_ForRecharge_Organized %>%
  filter(lubridate::year(datetime) == 2021) %>% 
  mutate(Recharge.Facility = ifelse(Recharge.Facility %in% c("MAR 5", "MAR 1B", "MAR 6B"), "MAR", Recharge.Facility),
         Recharge.Facility = ifelse(Recharge.Facility %in% c("AFRP Constructed", "AFRP Managed"), "AFRP", Recharge.Facility),
         Recharge.Facility = ifelse(Recharge.Facility %in% c("BKW-Milewide"), "BKW", Recharge.Facility)) %>%
  select(datetime, User, Recharge.Facility, deliveries) %>%
  group_by(User, Recharge.Facility) %>% summarize(total_recharge = sum(deliveries)) %>%
  pivot_wider(names_from = User, values_from = total_recharge)

##  STEP 3Bb: SORT RECHARGE DELIVERIES BY AMA FOR CAPFEWS AND EXPORT  
RCRG_byAMA = Historical_Deliveries_ForRecharge_Organized %>%
  select(datetime, User, AMA, deliveries) %>%
  group_by(datetime, User, AMA) %>% summarize(total_recharge = sum(deliveries)) %>%
  pivot_wider(names_from = User, values_from = total_recharge)

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

# RCRG_byAMA2021 = Historical_Deliveries_ForRecharge_Organized %>%
#   filter(lubridate::year(datetime) == 2021) %>% 
#   select(datetime, User, AMA, deliveries) %>%
#   group_by(datetime, User, AMA) %>% summarize(total_recharge = sum(deliveries)) %>%
#   pivot_wider(names_from = User, values_from = total_recharge)


## STEP 4: WIDEN THE DATA TO MAKE INPUT FILES FOR CAPFEWS
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

## STEP 5: EXTRACT THE SEASONALITY CURVES OF DEMAND FOR MAJOR USERS
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

  
## STEP 6: EXTRACT THE FRACTION OF DELIVERIES UNDER EACH ENTITLEMENT CLASS AND PURPOSE, BY USER
Historical_Deliveries_Organized_Normalized_ByClass_ByUse = Historical_Deliveries_Organized %>%
  group_by(datetime, User, Partner, Agreement, Group) %>% summarise(total_use = sum(deliveries))

## what fraction of deliveries are from each class?
# Historical_Deliveries_Organized_Normalized_ByClass_Fraction = Historical_Deliveries_Organized_Normalized_ByClass_ByUse %>%
#   group_by(User, Group) %>% summarise(total_use = sum(total_use)) %>%
#   pivot_wider(names_from = Group, values_from = total_use) 
# 
# Historical_Deliveries_Organized_Normalized_ByClass_Fraction[,2:ncol(Historical_Deliveries_Organized_Normalized_ByClass_Fraction)] =
#   t(apply(Historical_Deliveries_Organized_Normalized_ByClass_Fraction[,2:ncol(Historical_Deliveries_Organized_Normalized_ByClass_Fraction)], 
#         MARGIN = 1, 
#         function(x) {x/sum(x, na.rm = TRUE)}))

## what fraction of deliveries are leases? with which partners?
Historical_Deliveries_Organized_Normalized_ByPurpose = Historical_Deliveries_Organized_Normalized_ByClass_ByUse %>%
  filter(Agreement %in% c("Lease")) %>%
  group_by(datetime, User, Partner, Group) %>% summarise(total_lease = sum(total_use)) %>%
#  pivot_wider(names_from = Partner, values_from = total_lease) %>%
  select(-Group)

# separate these out into files with delivered lease water and (negated) "donated" lease water
for (leaser in unique(Historical_Deliveries_Organized_Normalized_ByPurpose$Partner)) {
  single_leaser = Historical_Deliveries_Organized_Normalized_ByPurpose %>% 
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



# what was total use by users who lease water? what was their total federal use?
# Historical_Deliveries_Organized_Totals_ForLeasers = Historical_Deliveries_Organized_Normalized_ByClass_ByUse %>%
#   filter(User %in% unique(Historical_Deliveries_Organized_Normalized_ByPurpose$User)) %>%
#   group_by(User, Group) %>% summarise(total_use = sum(total_use)) %>%
#   pivot_wider(names_from = Group, values_from = total_use) %>%
#   mutate(total_use_all_classes = sum(across(Excess:`Ag Pool`), na.rm = TRUE)) %>%
#   select(User, Federal, total_use_all_classes)




# combine these little files for final numbers
# Historical_Deliveries_Organized_Normalized_ByPurpose_Combined = Historical_Deliveries_Organized_Normalized_ByPurpose
# Historical_Deliveries_Organized_Normalized_ByPurpose_Combined$Federal = 
#   Historical_Deliveries_Organized_Totals_ForLeasers$Federal
# Historical_Deliveries_Organized_Normalized_ByPurpose_Combined$Total = 
#   Historical_Deliveries_Organized_Totals_ForLeasers$total_use_all_classes
# 
# # ALL LEASES ARE FROM FEDERAL CLASS ENTITLEMENTS
# # SO THESE NUMBERS ARE THE FRACTION OF FEDERAL CLASS WATER
# # EACH USER GETS FROM LEASES
# Leases_asFractionofFederalUse = Historical_Deliveries_Organized_Normalized_ByPurpose_Combined[,2:6] /
#   Historical_Deliveries_Organized_Normalized_ByPurpose_Combined$Federal
# Leases_asFractionofFederalUse$User = 
#   Historical_Deliveries_Organized_Normalized_ByPurpose_Combined$User
# 
# ## what fraction of deliveries are exchanges? with which partners?
# Historical_Deliveries_Organized_Normalized_ByPurpose = Historical_Deliveries_Organized_Normalized_ByClass_ByUse %>%
#   filter(Agreement %in% c("Exchange")) %>%
#   group_by(User, Partner, Group) %>% summarise(total_lease = sum(total_use)) %>%
#   pivot_wider(names_from = Partner, values_from = total_lease) %>%
#   select(-Group)
# 
# # what was total use by users who exchange water? what was their total federal use?
# Historical_Deliveries_Organized_Totals_ForExchange = Historical_Deliveries_Organized_Normalized_ByClass_ByUse %>%
#   filter(User %in% unique(Historical_Deliveries_Organized_Normalized_ByPurpose$User)) %>%
#   group_by(User, Group) %>% summarise(total_use = sum(total_use)) %>%
#   pivot_wider(names_from = Group, values_from = total_use) %>%
#   mutate(total_use_all_classes = sum(across(Excess:`Ag Pool`), na.rm = TRUE)) %>%
#   select(User, Federal, total_use_all_classes)
# 
# # combine these little files for final numbers
# Historical_Deliveries_Organized_Normalized_ByPurpose_Combined = Historical_Deliveries_Organized_Normalized_ByPurpose
# Historical_Deliveries_Organized_Normalized_ByPurpose_Combined$Federal = 
#   Historical_Deliveries_Organized_Totals_ForExchange$Federal
# Historical_Deliveries_Organized_Normalized_ByPurpose_Combined$Total = 
#   Historical_Deliveries_Organized_Totals_ForExchange$total_use_all_classes

# ALL EXCHANGES ARE FROM FEDERAL CLASS ENTITLEMENTS
# SO THESE NUMBERS ARE THE FRACTION OF FEDERAL CLASS WATER
# EACH USER GETS FROM EXCHANGE
# Exchanges_asFractionofFederalUse = Historical_Deliveries_Organized_Normalized_ByPurpose_Combined[,2:4] /
#   Historical_Deliveries_Organized_Normalized_ByPurpose_Combined$Federal
# Exchanges_asFractionofFederalUse$User = 
#   Historical_Deliveries_Organized_Normalized_ByPurpose_Combined$User

## STEP 7: IDENTIFY WHAT FRACTION OF MAJOR USER DELIVERIES ARE DIVERTED FOR 
##   RECHARGE IN EACH MONTH
##  EDIT: DID THIS BY GENERATING MATCHING SPREADSHEETS THAT CAN BE DIVIDED BY EACH OTHER WHEN MAKING JSON FILES
# fraction_recharge = Historical_Deliveries_ForRecharge_Organized_Wide[,-c(1)] / Historical_Deliveries_Organized_Wide[,-c(1)]
# fraction_recharge[is.na(fraction_recharge)] = 0
# 
# fraction_recharge$Month = lubridate::month(Historical_Deliveries_Organized_Wide$datetime)
# fraction_recharge = fraction_recharge %>% select(Month, everything())
# fraction_recharge_seasonality = fraction_recharge %>% group_by(Month) %>% 
#   summarise(across(everything(), list(mean)))
# colnames(fraction_recharge_seasonality) = colnames(fraction_recharge)

# annual summary for each user
#apply(fraction_recharge_seasonality, MARGIN = 2, mean)

### -----------------------------------------------------
## Collect major contractor historical deliveries
##  and organize by priority, lease, banking/storage
##  (initially just tracked top-14 historical largest,
##   then added others that show up consistently)

# group into these major contractors
# and the rest are lumped into 'OTHER' 
major_contractors = list(
  c("ACIC", "Ak-Chin Indian Community", "Ak-Chin", "Ak-Chin IC", "Ak Chin Indian Community", "Ak Chin IC", "Ak Chin Farm"),
  c("AWBA", "Arizona Water Banking Authority"),
  c("CAIDD", "Central Arizona IDD", "Central AZ IDD", "CAID", "Central Arizona ID", "Central AZ ID"),
  c("CAGRD", "Central Arizona GRD"),
  c("Gilbert", "Town of Gilbert"),
  c("GRIC", "Gila River Indian Community", "Gila River IC", "Gila River"),
  c("Mesa", "Mesa, City of"),
  c("MSIDD", "Maricopa Stanfield IDD", "Maricopa-Stanfield", "Maricopa Stanfield"),
  c("Peoria", "City of Peoria"),
  c("Phoenix", "Phoenix, City of", "City of Phoenix", "PHX"),
  c("SCAT", "San Carlos Apache Nation", "SCAN", "SCIC", "San Carlos IC", "San Carlos AT", "San Carlos"),
  c("Scottsdale", "City of Scottsdale"),
  c("TOIC", "Tohono Oodham Indian Nation", "Tohono O'odham Indian Nation", "Tohono", "TOIN", "Tohono IC"),
  c("Tucson", "Tucson, City of"),
  
  c("SRPMIC", "Salt River Pima"),
  c("Chandler"),
  c("Glendale", "Glendale, City of"),
  c("Tempe"),
  c("HIDD", "HID", "Hohokam"),
  c("HVIDD", "HVID", "Harquahala Valley IDD", "Harquahala Valley ID"),
  c("SCIDD", "San Carlos IDD"),
  c("Welton", "Welton-Mohawk", "Wellton-Mohawk"),
  c("CMID", "CMIDD", "Cortaro Marana Irrigation District"),
  c("TIDD", "Tonopah ID", "TID"),
  c("MWD", "Maricopa Water District"),
  c("NMIDD", "New Magma IDD", "New Magma", "NMID")
)


check_totals_in_2008 = 0; check_totals_in_2021 = 0
all_contractors = c()
for (user in major_contractors) {
  # collect deliveries for each subcontractor
  contractor_deliveries = Historical_Deliveries[
    as.logical(rowSums(sapply(user, grepl, Historical_Deliveries$Variable))) |
      as.logical(rowSums(sapply(user, grepl, Historical_Deliveries$Subgroup))),]
  
  # to ensure we aren't counting columns we don't want, check for 
  # unexpected values and clean them
  # print(unique(contractor_deliveries$Group))
  # print(unique(contractor_deliveries$Section))
  contractor_deliveries = contractor_deliveries %>%
    filter(Group != "FLOW AT CS19 (CFS)") %>%
    filter(Section != "TOTAL SYSTEM DELIVERIES") %>%
    filter(Total != 0) %>% mutate(Total = as.numeric(as.character(Total)))
  
  # REMOVE LEASE, EXCHANGE, SETTLEMENT, ASSIGNMENT COLUMNS FROM THE DELIVERY DATA
  contractor_deliveries = contractor_deliveries[!as.logical(rowSums(sapply(
    c("lease", "Lease", "settlement", "Settlement", "Exchange", "exchange",
      "Assignment", "assignment"), grepl, contractor_deliveries$Variable))),]
   
  # and aggregate deliveries by priority type and month/year
  # and account for outliers in the data as we go
  contractor_deliveries_aggregated = contractor_deliveries %>% 
    mutate(Group = ifelse(Group == "Arizona Water Co. (Coolidge) W/CAIDD" |
                            Group == "Maricopa Water District Lake Water(**)(@)" |
                            Group == "Maricopa Water District Lake Water (MWD**)" |
                            Group == "Maricopa Water District Lake Water (**)", "MUNICIPAL & INDUSTRIAL:", Group)) %>% 
    mutate(Group = ifelse(Group == "RECHARGE1:", "RECHARGE:", Group)) %>% 
    mutate(Group = ifelse(Group == "Salt River Project XS ($XX/AF) w/CAIDD" |
                            Group == "Salt River Project XS ($XX/AF) w/MSIDD" |
                            Group == "Salt River Project XS ($10/AF) w/MSIDD" |
                            Group == "Salt River Project XS ($10/AF) w/CAIDD" |
                            Group == "Salt River Project XS ($48/AF) w/MSIDD" |
                            Group == "Salt River Project XS w/CAIDD" |
                            Group == "Salt River Project XS w/MSIDD", "EXCESS:", Group)) %>% 
    mutate(Group = ifelse(Group == "Ag Pool Redistribution/Remarket (+/-)", "AGRICULTURAL:", Group)) %>% 
    group_by(Year, Group) %>% 
    summarise(across(c(Jan:Mar, Apr:Jun, Jul:Sep, Oct:Dec), function(x) {sum(as.numeric(as.character(x)), na.rm = TRUE)})) %>%
    mutate(Subcontractor = user[1])
    
  # add to full list holding all the contractors
  all_contractors = rbind(all_contractors, contractor_deliveries_aggregated)
    
  # sanity check - do we need to account for more users specifically?
  check_totals_in_2008 = check_totals_in_2008 + 
    sum(as.numeric(as.character(contractor_deliveries$Total[which(contractor_deliveries$Year == 2008)])), na.rm = TRUE)
  check_totals_in_2021 = check_totals_in_2021 + 
    sum(as.numeric(as.character(contractor_deliveries$Total[which(contractor_deliveries$Year == 2021)])), na.rm = TRUE)
}

# what fraction of total CAP deliveries in 2008 are to these 14 major contractors + others? in 2021?
# because of the double-counting of many lease or exchanges, these values are inflated
print(paste("Fraction of deliveries to top 14+ contractors in 2008: ", 
            as.character(check_totals_in_2008 / 
                           sum(Historical_CAPDiversion_Organized$CAP_div[
                             which(format(Historical_CAPDiversion_Organized$datetime, format = "%Y") == "2008")])), sep = ""))
print(paste("Fraction of deliveries to top 14+ contractors in 2021: ", 
            as.character(check_totals_in_2021 / 
                           sum(Historical_CAPDiversion_Organized$CAP_div[
                             which(format(Historical_CAPDiversion_Organized$datetime, format = "%Y") == "2021")])), sep = ""))

# clean up full sets, make them "long" then organize
ac_long = all_contractors %>% 
  pivot_longer(cols = -c('Year', 'Subcontractor', 'Group'), names_to = 'Month', values_to = 'del') %>%
  mutate(Month = match(Month, month.abb)) %>%
  mutate(datetime = lubridate::make_datetime(year = Year, month = Month)) %>% ungroup() %>%
  select(datetime, Group, Subcontractor, del)

Historical_Deliveries_ByContractor_ByPriorityClass_Organized = ac_long %>% 
  mutate(del = ifelse(del == 0, NA, del)) %>%
  pivot_wider(names_from = c(Subcontractor, Group), values_from = del)

# clean up column headers for readability in the code
colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized) = 
  sub(":", "", colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized))
colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized) = 
  sub(" ", "", colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized))
colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized) = 
  sub(" ", "", colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized))
colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized) = 
  sub(" ", "", colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized))
colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized) = 
  sub(" ", "", colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized))
colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized) = 
  sub(" ", "", colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized))
colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized) = 
  sub(" ", "", colnames(Historical_Deliveries_ByContractor_ByPriorityClass_Organized))


## manage leases - who gives them and who gets them, sorted by subcontractor and priority group
# leases can only be given from Tribal users to others, so will need to identify
#   ORIGINATORS (tribes) vs RECIPIENTS (M&I, usually) within a loop
all_leases = Historical_Deliveries[as.logical(rowSums(sapply(
  c("lease", "Lease", "settlement", "Settlement", "Exchange", "exchange",
    "Assignment", "assignment"), grepl, Historical_Deliveries$Variable))),]
all_leases = all_leases %>% filter(Total != 0)

lease_originators = list(
  c("SRPMIC"), c("SCAT"), c("FMYN", "Fort McDowell"), c("GRIC"), c("Ak-Chin"))
lease_recipients = list(
  c("Tucson", "SAVSARP", "CAVSARP"), c("CAIDD"), c("CAGRD"), c("Gilbert"),
  c("OTHERS", minor_contractors))

GW_AMAs = list(
  c("Phoenix AMA",
      "GRUSP",
      "AFRP", "Agua Fria",
      "HRF", "Hassayampa RF",
      "HMRP", "Hieroglyphic",
      "TDRP", "Tonopah",
      "SMRP", "Superstition",
      "CHCID", "Chandler ID",
      "MWD", "Maricopa Water District",
      "NMIDD", "New Magma",
      "QCIDD", "Queen Creek",
      "SRP", "Salt River Project", # need to include exception to remove SRPMIC
      "RWCD", "Roosevelt",
      "TID", "Tonopah", # is it the RP or ID? will have to screen for this
      "PHX", "GRIIDD", "GRIC GSF", "Gila River Indian IDD"),
  c("Pinal AMA", 
      "CAIDD", 
      "HIDD", "Hohokam", 
      "MSIDD", 
      "GRIIDD", "GRIC GSF", "Gila River Indian IDD"),
  c("Tucson AMA", 
      "ASARCO", 
      "CMID", 
      "Kai Farms", 
      "BKW Farms", 
      "PMRP", "Pima Mine Road RP", 
      "LSCRP", "Lower Santa Cruz RP", 
      "SAVSARP", 
      "CAVSARP",
      "AVRP", "Avra Valley RP"),
)










## check other contractors, do they have many leases, big fraction of deliveries?
minor_contractors = list(
  c("Avondale"),
  c("Buckeye"),
  c("Bureau of Reclamation", "BOR"),
  c("BKW Farms", "BKW"),
  c("Arizona Water Co."),
  c("ADOT", "Arizona Department of Transportation", "Arizona State Land Dept"),
  c("SRP", "Salt River Project"),
  c("Goodyear"),
  c("Metro Water", "Metro"),
  c("Marana"),
  c("Oro Valley"),
  c("RWCD", "Roosevelt"),
  c("Spanish Trail"),
  c("Surprise"),
  c("QCID", "Queen Creek ID", "Queen Creek Irrigation District"),
  c("Tonto Hills"),
  c("FMYN", "Fort McDowell Yavapai Nation", "Yavapai"),
  c("EPCOR"),
  c("Eloy"),
  c("El Mirage"),
  c("Chaparral", "Chaparral City WC"),
  c("Carefree WC", "Carefree"),
  c("Cave Creek WC", "Cave Creek"),
  c("ASARCO")
)




### -----------------------------------------------------
## Collect historic water delivery rate components
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

### -----------------------------------------------------
## Collect historic power market energy prices
##  and pumped water quantities 
Historical_PumpingEnergy_Organized = Historical_PowerUse %>%
  filter(Table == "CAP Pumping Plants - Projection of Energy Use - For Waddell Filling Only" |
           Table == "CAP Pumping Plants - Projection of Energy Use - For Deliveries Only" |
           Table == "CAP Pumping Plants - Projection of Energy Use")

 
### -----------------------------------------------------
## collect entitlement info for CAP contractors
entitlements = list()
entitlements[[1]] = c("Ak-Chin",        "AWBA",           "AZWC",           "CAGRD",          "CAIDD",
                      "Chandler",       "Gilbert",        "Glendale",       "GRIC",           "HIDD",     
                      "HVID",           "Mesa",           "MSIDD",          "OTHER",          "Peoria",         
                      "Phoenix", "SCAT","Scottsdale",     "Tempe",          "Tohono O'odham", "Tucson")

UsersToKeep

##  total in each delivery priority class
#     classes: M&I, NIA, Indian, P3
#     numbers from: CAP Subcontracting Status Report - April 2022
#       totals for 2022: TOTAL = 1,294,717 AF
#                          M&I =   620,678 
#                          NIA =    44,530 +  47,303 +  5,000 + 102,000 + 18,100 + 50,000 + 16,000
#                       Indian =   555,806 +     500          - 102,000 - 18,100 - 50,000 - 16,000
#                           P3 =    22,000 (Wellton-Mohawk IDD, 20,900 AF after system loss)
#                                 + 50,000 (Ak-Chin, 47,500 AF after system loss)
#     Several users here (Ag users) have no P3 or P4 rights:
#       AWBA, CAIDD, HIDD, HVID, MSIDD deliveries
#       are ENTIRELY of excess CAP water (Ag/Excess Pool)
entitlements[[2]] = list(c(0, 0, 27500, 50000),  #Ak-Chin
                         c(0, 0, 0, 0),  #AWBA
                         c(6285 + 8884 + 2000 + 968, 0, 0, 0), #AZWC
                         c(6426, 18185, 0, 0), #CAGRD
                         c(0, 0, 0, 0), #CAIDD
                         c(8654, 2952 + 972, 0, 4278), #Chandler
                         c(7235, 1832 + 1537, 0, 6762), #Gilbert
                         c(17236, 682, 0, 3000), #Glendale
                         c(0, 102000 + 18100, 173100 + 18600, 0), #GRIC
                         c(0, 0, 0, 0), #HIDD
                         c(0, 0, 0, 0), #HVID
                         c(43503, 4924 + 627, 0, 2760), #Mesa
                         c(0, 0, 0, 0), #MSIDD
                         c(NA, NA, NA, NA), #OTHER - total M&I MUST CALCULATE
                         c(27121, 0, 0, 0), #Peoria
                         c(122204, 36144 + 1136, 0, 5000), #Phoenix
                         c(14665 + 3480, 0, 30800 + 12700, 0), #SCAT
                         c(52810, 3283 + 23, 500, 100), #Scottsdale
                         c(4315, 23, 0, 100), #Tempe
                         c(0, 27000 + 23000 + 10800 + 5200, 8000, 0), #Tohono O'odham
                         c(144191, 0, 0, 0)) #Tucson

# OCT 2022: Current Entitlements are also available by priority class
# here: https://library.cap-az.com/maps/capallocations
entitlements[[2]] = list(c(0, 0, 27500, 50000),  #Ak-Chin
                         c(0, 0, 0, 0),  #AWBA
                         c(6285 + 8884 + 2000 + 968, 0, 0, 0), #AZWC
                         c(6426, 18185, 0, 0), #CAGRD
                         c(0, 0, 0, 0), #CAIDD
                         c(8654, 2952 + 972, 0, 4278), #Chandler
                         c(7235, 1832 + 1537, 0, 6762), #Gilbert
                         c(17236, 682, 0, 3000), #Glendale
                         c(0, 102000 + 18100, 173100 + 18600, 0), #GRIC
                         c(0, 0, 0, 0), #HIDD
                         c(0, 0, 0, 0), #HVID
                         c(43503, 4924 + 627, 0, 2622), #Mesa
                         c(0, 0, 0, 0), #MSIDD
                         c(NA, NA, NA, NA), #OTHER - total M&I MUST CALCULATE
                         c(27121, 0, 0, 0), #Peoria
                         c(122204, 36144 + 1136, 0, 4750), #Phoenix
                         c(14665 + 3480, 0, 30800 + 12700, 0), #SCAT
                         c(52810, 3283 + 23, 500, 100), #Scottsdale
                         c(10249, 0, 0, 0), # Surprise City
                         c(4315, 23, 0, 100), #Tempe
                         c(0, 27000 + 23000 + 10800 + 5200, 8000, 0), #Tohono O'odham
                         c(144191, 0, 0, 0)) #Tucson

# calculate OTHER category for each priority class
# by calculating fraction of each priority class use
# entitlements are for named users and assign remaining
# entitlements of the priority class to OTHER
MaI_total_entitlement = 620678
NIA_total_entitlement = (44530 + 47303 + 5000 + 102000 + 18100 + 50000 + 16000)
FED_total_entitlement = (555806 - 500 - 102000 - 18100 - 50000 - 16000 - 50000)
PTR_total_entitlement = 22000 + 50000

MaIpri_frac = sum(unlist(lapply(entitlements[[2]], function(x) {x[1]/MaI_total_entitlement})), na.rm = TRUE)
NIApri_frac = sum(unlist(lapply(entitlements[[2]], function(x) {x[2]/NIA_total_entitlement})), na.rm = TRUE)
FEDpri_frac = sum(unlist(lapply(entitlements[[2]], function(x) {x[3]/FED_total_entitlement})), na.rm = TRUE)
PTRpri_frac = sum(unlist(lapply(entitlements[[2]], function(x) {x[4]/PTR_total_entitlement})), na.rm = TRUE)

entitlements[[2]][[14]][1] = MaI_total_entitlement * (1 - MaIpri_frac)
entitlements[[2]][[14]][2] = NIA_total_entitlement * (1 - NIApri_frac)
entitlements[[2]][[14]][3] = FED_total_entitlement * (1 - FEDpri_frac)
entitlements[[2]][[14]][4] = PTR_total_entitlement * (1 - PTRpri_frac)

write.table(c(PTR_total_entitlement, MaI_total_entitlement, FED_total_entitlement, NIA_total_entitlement),
            "CAP_priorityclass_entitlementtotals2022.csv", sep = ",", row.names = FALSE, col.names = FALSE)

