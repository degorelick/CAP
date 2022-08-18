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
HistoricDeliveries = read.csv("CAP_deliveries_by_user_2008_to_2021.csv", header = TRUE)
CRSS_MeadElevation = readxl::read_xlsx("PNNL.xlsx", sheet = "Mead Pool elevation")
CRSS_AZRequestedDiversion = readxl::read_xlsx("PNNL.xlsx", sheet = "AZ")
CRSS_CAPRequestedDiversion = readxl::read_xlsx("PNNL.xlsx", sheet = "CAP")
CRSS_ShortageSummary = readxl::read_xlsx("PNNL.xlsx", sheet = "Shortage")
EnergyPrices = read.csv("Palo Verde Energy Prices.csv", header = TRUE)
HistoricalPowerUse = read.csv("CAP_power_data_2008_to_2021.csv", header = TRUE)
HistoricalBudget = read.csv("CAP_annual_budgets_2011_to_2020.csv", header = TRUE)
HistoricalRates = read.csv("CAP_annual_rates_2011_to_2020.csv", header = TRUE)

### -----------------------------------------------------
## Collect Lake Mead projections from CRSS
##  and historical water withdrawals into CAP system
##    PNNL.xlsx - CRSS Mead elevation, AZ, and CAP diversion request traces, 2023-2054 by month
##      can use averages like 24MS for initial tests?
##    forecast Historical 2008 to 2021.xlsx - top table colorado river diversions, by month



### -----------------------------------------------------
## Collect major contractor historical deliveries
##  and organize by priority, lease, banking/storage


### -----------------------------------------------------
## Collect historic power market energy prices
##  and long-term PPA information


### -----------------------------------------------------
## Collect historic water delivery rate components
##  and reconciliation rate updates