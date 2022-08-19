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

#   a. Ak-Chin IC
#   b. AWBA
#   c. CAGRD
#   d. CAIDD
#   e. Mesa
#   f. Peoria
#   g. Phoenix
#   h. Scottsdale
#   i. Tucson
#   j. Gila River IC
#   k. MSIDD
#   l. San Carlos Apache Nation
#   m. Tohono O'odham Indian Nation
#   n. Gilbert

### -----------------------------------------------------
## Collect major contractor historical deliveries
##  and organize by priority, lease, banking/storage
major_contractors = list(
  c("Ak-Chin", "ACIC", "Ak-Chin IC", "Ak Chin Indian Community", "Ak Chin IC"),
  c("AWBA", "Arizona Water Banking Authority"),
  c("CAIDD", "Central Arizona IDD", "Central AZ IDD", "CAID", "Central Arizona ID", "Central AZ ID"),
  c("CAGRD", "Central Arizona GRD"),
  c("GRIC", "Gila River IC", "Gila River", "Gila River Indian Community"),
  c("Gilbert", "Town of Gilbert"),
  c("Mesa", "Mesa, City of"),
  c("MSIDD", "Maricopa Stanfield IDD", "Maricopa-Stanfield", "Maricopa Stanfield"),
  c("Peoria", "City of Peoria"),
  c("Phoenix", "Phoenix, City of", "City of Phoenix", "PHX"),
  c("Tohono", "Tohono O'odham Indian Nation", "TOIC", "TOIN", "Tohono IC"),
  c("Tucson", "Tucson, City of"),
  c("SCAT", "San Carlos Apache Nation", "SCAN", "SCIC", "San Carlos IC", "San Carlos AT", "San Carlos"),
  c("Scottsdale", "City of Scottsdale"),
  c("OTHER UPPER CANAL DELIVERIES"), # determined by canal segment, remaining deliveries
  c("OTHER LOWER CANAL DELIVERIES") # determined by canal segment, remaining deliveries
)

c("Avondale"),
c("Buckeye"),
c("Bureau of Reclamation", "BOR"),
c("BKW Farms", "BKW"),
c("Chandler"),
c("Glendale", "Glendale, City of"),
c("Arizona Water Co."),
c("ADOT", "Arizona Department of Transportation", "Arizona State Land Dept"),
c("Tempe"),
c("SRPMIC"),
c("SRP", "Salt River Project"),
c("HIDD", "HID", "Hohokam"),
c("HVIDD", "HVID", "Harquahala Valley IDD", "Harquahala Valley ID"),
c("San Carlos IDD", "SCIDD"),
c("Welton-Mohawk", "Wellton-Mohawk"),
c("CMID", "CMIDD", "Cortaro Marana Irrigation District"),
c("Tonopah ID", "TID", "TIDD"),
c("MWD", "Maricopa Water District"),
c("NMIDD", "New Magma IDD", "New Magma", "NMID"),
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
c("ASARCO"),

Historical_Deliveries_ByContractor_ByPriorityClass_Organized


### -----------------------------------------------------
## Collect historic water delivery rate components
##  and reconciliation rate updates
Historical_Rates_Organized


### -----------------------------------------------------
## Collect historic power market energy prices
##  and long-term PPA information
Historical_PumpingEnergy_Organized
