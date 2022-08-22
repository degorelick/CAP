### -----------------------------------------------------
##  Build CAPFEWS Input Data Files
##    D. Gorelick (Aug 2022)
##  To be stored in degorelick/CAP repository on GitHub
### -----------------------------------------------------

rm(list=ls()) # clear memory
setwd('C:/Users/dgorelic/OneDrive - University of North Carolina at Chapel Hill/UNC/Research/IM3/CAP/Data') # set directory
library(tidyverse)

### -----------------------------------------------------
## Read in all the data we need, some of which has
##  already been cleaned by explore_CAPdata.R
Historical_Deliveries = read.csv("CAP_deliveries_by_user_2008_to_2021.csv", header = TRUE)
Historical_CAPDiversion = read.csv("CAP_diversions_summary_2008_to_2021.csv", header = TRUE)
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
CRSS_CAPDiversion_Organized
CRSS_MeadElevation_Organized

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


### -----------------------------------------------------
## Collect major contractor historical deliveries
##  and organize by priority, lease, banking/storage
##  (initially just tracked top-14 historical largest,
##   then added others that show up consistently)
major_contractors = list(
  c("Ak-Chin Indian Community", "Ak-Chin", "ACIC", "Ak-Chin IC", "Ak Chin Indian Community", "Ak Chin IC", "Ak Chin Farm"),
  c("AWBA", "Arizona Water Banking Authority"),
  c("CAIDD", "Central Arizona IDD", "Central AZ IDD", "CAID", "Central Arizona ID", "Central AZ ID"),
  c("CAGRD", "Central Arizona GRD"),
  c("Gilbert", "Town of Gilbert"),
  c("Gila River Indian Community", "GRIC", "Gila River IC", "Gila River"),
  c("Mesa", "Mesa, City of"),
  c("MSIDD", "Maricopa Stanfield IDD", "Maricopa-Stanfield", "Maricopa Stanfield"),
  c("Peoria", "City of Peoria"),
  c("Phoenix", "Phoenix, City of", "City of Phoenix", "PHX"),
  c("San Carlos Apache Nation", "SCAT", "SCAN", "SCIC", "San Carlos IC", "San Carlos AT", "San Carlos"),
  c("Scottsdale", "City of Scottsdale"),
  c("Tohono Oodham Indian Nation", "Tohono O'odham Indian Nation", "Tohono", "TOIC", "TOIN", "Tohono IC"),
  c("Tucson", "Tucson, City of"),
  
  c("SRPMIC", "Salt River Pima"),
  c("Chandler"),
  c("Glendale", "Glendale, City of"),
  c("Tempe"),
  c("HIDD", "HID", "Hohokam"),
  c("HVIDD", "HVID", "Harquahala Valley IDD", "Harquahala Valley ID"),
  c("San Carlos IDD", "SCIDD"),
  c("Welton-Mohawk", "Wellton-Mohawk"),
  c("CMID", "CMIDD", "Cortaro Marana Irrigation District"),
  c("Tonopah ID", "TID", "TIDD"),
  c("MWD", "Maricopa Water District"),
  c("NMIDD", "New Magma IDD", "New Magma", "NMID")
)

check_totals_in_2008 = 0; check_totals_in_2021 = 0
all_contractors = c(); all_leases = c()
for (user in major_contractors) {
  # collect deliveries for each subcontractor
  contractor_deliveries = Historical_Deliveries[as.logical(rowSums(sapply(user, grepl, Historical_Deliveries$Variable))),]
  
  # also grab leases to check out later
  contractor_leases = contractor_deliveries[as.logical(rowSums(sapply(c("lease", "Lease"), grepl, contractor_deliveries$Variable))),]
  
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
    filter(Group != "FLOW AT CS19 (CFS)") %>%
    group_by(Year, Group) %>% 
    summarise(across(c(Jan:Mar, Apr:Jun, Jul:Sep, Oct:Dec), function(x) {sum(as.numeric(as.character(x)), na.rm = TRUE)})) %>%
    mutate(Subcontractor = user[1])
    
  # add to full list holding all the contractors (and leases)
  all_contractors = rbind(all_contractors, contractor_deliveries_aggregated)
  all_leases = rbind(all_leases, contractor_leases)
    
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
Historical_Rates_Organized


### -----------------------------------------------------
## Collect historic power market energy prices
##  and long-term PPA information
Historical_PumpingEnergy_Organized
