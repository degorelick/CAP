### -----------------------------------------------------
##  Build CAPFEWS JSON Files
##    D. Gorelick (Sep 2022)
##  To be stored in degorelick/CAP repository on GitHub
### -----------------------------------------------------

rm(list=ls()) # clear memory
setwd('C:/Users/dgorelic/OneDrive - University of North Carolina at Chapel Hill/UNC/Research/IM3/CAP/Data') # set directory
library(jsonlite); library(dplyr)

### Write Canal JSON ------------------------------------
## write JSON for CAP canal capacities, etc

# name of canal turnout "Nodes", in order from "headwaters" (CO River) to "outlet"
# turnouts named by pumping plant immediately "upstream"
# only turnouts where top users have action are included
# final capacity node is always zero - a dummy to tell model to recycle
turnout_names = c("LHQ", "WAD", "HSY", "SGL", "BRD", "PIC", "RED", "SAN", "BRW", "SND", "BLK", "none")

# capacity of turnouts to move water. turnouts can have "normal" "reverse" and "closed" settings
# only Waddell (WAD) can go normal and reverse (fill and spill from Lake Pleasant)
# others assume to have infinite normal capacity for now
normal_capacities = rep(999999, length(turnout_names)); normal_capacities[length(normal_capacities)] = 0
reverse_capacities = rep(0, length(turnout_names)); reverse_capacities[2] = 999999
closed_capacities = rep(0, length(turnout_names))

capacities = data.frame(node = turnout_names,
                        normal = normal_capacities,
                        reverse = reverse_capacities,
                        closed = closed_capacities)
turnout = data.frame(normal = normal_capacities[1:(length(normal_capacities)-1)],
                     reverse = reverse_capacities[1:(length(reverse_capacities)-1)],
                     closed = closed_capacities[1:(length(closed_capacities)-1)])

## write the final json
canal_json = toJSON(list("name" = "CAP",
                         "annual_diversion_capacity" = 1600, # in kAF/yr
                         "capacity" = capacities, 
                         "turnout" = turnout), 
                    pretty = TRUE, dataframe = "columns", simplifyDataFrame = TRUE, auto_unbox = TRUE)
write(canal_json, "../CAPFEWS/calfews_src/canals/CAP_properties.json")





### Write Contracts (Subcontractor Entitlements) JSONs -------------------------------
## write JSON for CAP canal contractors rights (contracts)
## write a separate contract for each CAP water priority class (P3, M&I, Indian, NIA)
##  Ag and Excess will have priority zero - not filled until others satisfied
entitlement_totals = read.csv("user_entitlements.csv", header = TRUE)
entitlement_totals_normalized = read.csv("user_entitlements_fractions.csv", header = TRUE)

# estimate how many entitlements remain in each priority class under each DCP shortage tier
NIA_unentitled_reserve = 119065 # from 2022 subcontracting status report, uncontracted
full_entitlement_year = sum(apply(entitlement_totals[,2:5], 2, sum)) + NIA_unentitled_reserve
T0_entitlement_year = full_entitlement_year - 192000
T1_entitlement_year = full_entitlement_year - 512000
T2a_entitlement_year = full_entitlement_year - 592000
T2b_entitlement_year = full_entitlement_year - 640000
T3_entitlement_year = full_entitlement_year - 720000

# determine reductions by shortage tier
P3_entitlement_total = sum(entitlement_totals$PTR)
MUI_entitlement_total = sum(entitlement_totals$MUI)
FED_entitlement_total = sum(entitlement_totals$FED)
NIA_entitlement_total = sum(entitlement_totals$NIA) + NIA_unentitled_reserve

mui_fed_total = MUI_entitlement_total + FED_entitlement_total

p3_mui_fed_total = P3_entitlement_total + MUI_entitlement_total + FED_entitlement_total

T0_NIA_fraction_remaining = 1 - 192000/NIA_entitlement_total

T1_NIA_fraction_remaining = 0
T1_MUI_fraction_remaining = 1 - ((512000 - NIA_entitlement_total) * (MUI_entitlement_total/mui_fed_total))/MUI_entitlement_total
T1_FED_fraction_remaining = 1 - ((512000 - NIA_entitlement_total) * (FED_entitlement_total/mui_fed_total))/FED_entitlement_total

T2a_NIA_fraction_remaining = 0
T2a_MUI_fraction_remaining = 1 - ((592000 - NIA_entitlement_total) * (MUI_entitlement_total/mui_fed_total))/MUI_entitlement_total
T2a_FED_fraction_remaining = 1 - ((592000 - NIA_entitlement_total) * (FED_entitlement_total/mui_fed_total))/FED_entitlement_total

T2b_NIA_fraction_remaining = 0
T2b_MUI_fraction_remaining = 1 - ((640000 - NIA_entitlement_total) * (MUI_entitlement_total/mui_fed_total))/MUI_entitlement_total
T2b_FED_fraction_remaining = 1 - ((640000 - NIA_entitlement_total) * (FED_entitlement_total/mui_fed_total))/FED_entitlement_total

T3_NIA_fraction_remaining = 0
T3_MUI_fraction_remaining = 1 - ((720000 - NIA_entitlement_total) * (MUI_entitlement_total/mui_fed_total))/MUI_entitlement_total
T3_FED_fraction_remaining = 1 - ((720000 - NIA_entitlement_total) * (FED_entitlement_total/mui_fed_total))/FED_entitlement_total

for (contract_class in c("PTR", "MUI", "FED", "NIA")) {
  entitlement_water = sum(entitlement_totals[,contract_class])/1000
  if (contract_class == "NIA") {
    entitlement_water = sum(entitlement_totals[,contract_class])/1000 + NIA_unentitled_reserve/1000
  }
  contract = list("name" = contract_class, 
                  "total" = entitlement_water, # converted to kAF 
                  "maxForecastValue" = entitlement_water,
                  "carryover" = 0,
                  "type" = "contract",
                  "allocation_priority" = ifelse(test = contract_class %in% c("AGR", "EXC"), yes = 0, no = 1),
                  "storage_priority" = 1,
                  "reduction" = list("T0" = ifelse(test = contract_class %in% c("MUI", "FED"), 
                                                   yes = 1, 
                                                   no = ifelse(test = contract_class %in% c("NIA"), 
                                                               yes = T0_NIA_fraction_remaining, # NIA
                                                               no = 1)), #P3,
                                     "T1" = ifelse(test = contract_class %in% c("MUI", "FED"), 
                                                   yes = T1_MUI_fraction_remaining, 
                                                   no = ifelse(test = contract_class %in% c("NIA"), 
                                                               yes = 0, # NIA
                                                               no = 1)), #P3, 
                                     "T2a" = ifelse(test = contract_class %in% c("MUI", "FED"), 
                                                    yes = T2a_MUI_fraction_remaining, 
                                                    no = ifelse(test = contract_class %in% c("NIA"), 
                                                                yes = 0, # NIA
                                                                no = 1)), #P3
                                     "T2b" = ifelse(test = contract_class %in% c("MUI", "FED"), 
                                                    yes = T2b_MUI_fraction_remaining, 
                                                    no = ifelse(test = contract_class %in% c("NIA"), 
                                                                yes = 0, # NIA
                                                                no = 1)), #P3
                                     "T3" = ifelse(test = contract_class %in% c("MUI", "FED"), 
                                                   yes = T3_MUI_fraction_remaining, 
                                                   no = ifelse(test = contract_class %in% c("NIA"), 
                                                               yes = 0, # NIA
                                                               no = 1)), #P3 
                                     "DP" = ifelse(test = contract_class %in% c("MUI", "FED"), 
                                                   yes = 0.5, 
                                                   no = ifelse(test = contract_class %in% c("NIA"), 
                                                               yes = 0, # NIA
                                                               no = 1))) #P3 
                    )
  
  contract_json = toJSON(contract, pretty = TRUE, dataframe = "columns", simplifyDataFrame = TRUE, auto_unbox = TRUE)
  write(contract_json, paste("../CAPFEWS/calfews_src/contracts/", contract_class, "_properties.json", sep = ""))
}




### Write District (Subcontractor) JSONs --------------------------------
## write JSON for CAP canal contractors (districts)
## each user needs its own JSON file 

# read in demand data, extract annual delivery request growth trends and 2021 levels
UserDemandSeasonality = read.csv("user_seasonality_deliveries.csv", header = TRUE)
UserDemandMonthly = read.csv("user_monthly_deliveries.csv", header = TRUE)
colnames(UserDemandSeasonality)[c(1,4,20,29)] = c("Ak-Chin", "AZ State Land", "Oro Valley", "Tohono O'odham")
colnames(UserDemandMonthly)[c(2,5,21,30)] = c("Ak-Chin", "AZ State Land", "Oro Valley", "Tohono O'odham")
UserDemandSeasonality[is.na(UserDemandSeasonality)] = 0

AnnualDemand = UserDemandMonthly %>% 
  mutate(Year = lubridate::year(datetime)) %>%
  group_by(Year) %>% summarise_at(vars(`Ak-Chin`:WMAT), sum)
MaxAnnualDemand = apply(AnnualDemand,2,max)

# also get AMAs that each district can recharge to
UserDemandRecharge = read.csv("AMA_deliveries_recharge_byAMA.csv", header = TRUE)
UserDemandRecharge[is.na(UserDemandRecharge)] = 0

ama_users = UserDemandRecharge %>% group_by(AMA) %>% summarise(across(-datetime, sum))
ama_users_frac = as.data.frame(apply(ama_users[,2:ncol(ama_users)], 2, function(x) {round(x/sum(x),digits = 1)}))
ama_users_frac$AMA = ama_users$AMA
colnames(ama_users_frac)[c(1,11,18)] = c("Ak-Chin", "Oro Valley", "Tohono O'odham")

# read in recharge seasonality and fraction of total deliveries
UserDemandRecharge = read.csv("AMA_total_deliveries_recharge.csv", header = TRUE)
UserDemandRechargeMonthly = read.csv("user_deliveries_recharge_seasonality.csv", header = TRUE)
UserDemandRechargeMonthlyFraction = read.csv("user_deliveries_recharge_seasonality_fraction.csv", header = TRUE)
colnames(UserDemandRechargeMonthlyFraction)[c(2,5,21,30)] = c("Ak-Chin", "AZ State Land", "Oro Valley", "Tohono O'odham")

# also read in entitlements, to assign contract fractions
entitlement_totals = read.csv("user_entitlements.csv", header = TRUE)
entitlement_fractions = read.csv("user_entitlements_fractions.csv", header = TRUE)

# read in lease information
leases_amount = read.csv("user_lease.csv", header = TRUE)
lease_priority = read.csv("user_lease_priorities.csv", header = TRUE)

# set Ag Mitigation Agreement Parameters for select subcontractors
ag_mitigation_trigger_tiers = c("T1", "T2a", "T2b")
ag_mitigation_donor_list = c("PXA", "PNA", "TSA") # water comes first from stored M&I AMA GSF water
ag_mitigation_taker_list = c("CID", "HID", "HVD", "MSD", "OTH")
ag_mitigation_taker_fractions = c(0.34, 0.11, 0.10, 0.33, 0.12)
ag_mitigation_quantity_tiers = c(105000, 70000, 70000)

# set NIA Mitigation Agreement Parameters for select subcontractors
# NIA mitigation water that is unclaimed may be then used for Ag Mitigation...
nia_mitigation_trigger_tiers = c("T1", "T2a", "T2b")
nia_mitigation_donor_list = c("PLS") # extra water released from Lake Pleasant as needed, up to 50k AF
nia_mitigation_taker_list = c(entitlement_totals$Code[which(entitlement_totals$NIA != 0)]) # any NIA user
nia_mitigation_quantity_tiers = c(1, 1, 1) # Mitigation water will be used to fulfill ALL NIA priority deliveries

AMA_turnouts = data.frame("AMA" = c("Phoenix", "Pinal", "Tucson"),
                          "Code" = c("PXA", "PNA", "TSA"),
                          "Turnout" = c("HSY", "SGL", "BRW"))

# build the JSON files
for (d in entitlement_totals$Code) {
  district_full_name = entitlement_totals$User[which(entitlement_totals$Code == d)]
  district_lease_partners = c("none")
  district_lease_quantity = c(0)
  district_lease_priority = c("none")
  if (district_full_name %in% colnames(leases_amount)) {
    district_lease_partners = leases_amount$Partner[which(!is.na(leases_amount[,district_full_name]))]
    district_lease_quantity = leases_amount[,district_full_name][which(!is.na(leases_amount[,district_full_name]))]/1000
    district_lease_priority = lease_priority[,district_full_name][which(!is.na(leases_amount[,district_full_name]))]
  }
  
  # if this subcontractor is providing leases to others, negate lease amounts to remove it from their availability
  if (district_full_name %in% unique(leases_amount$Partner)) {
    district_lease_partners = colnames(leases_amount)[which(!is.na(leases_amount[which(leases_amount$Partner == district_full_name),]))]
    district_lease_quantity = leases_amount[which(leases_amount$Partner == district_full_name),
                                            which(!is.na(leases_amount[which(leases_amount$Partner == district_full_name),]))]
    district_lease_priority = lease_priority[which(leases_amount$Partner == district_full_name),
                                             which(!is.na(leases_amount[which(leases_amount$Partner == district_full_name),]))]
    
    # remove self from list and convert to kAF
    district_lease_partners = district_lease_partners[1:(length(district_lease_partners)-1)]
    district_lease_quantity = as.vector(unlist(district_lease_quantity[1:(length(district_lease_quantity)-1)])/1000 * -1)
    district_lease_priority = as.vector(unlist(district_lease_priority[1:(length(district_lease_priority)-1)]))
  }
  
  # collect the names of AMAs that a district uses
  ama_used = as.list("none")
  ama_share = as.list(0.0)
  if (district_full_name %in% colnames(ama_users_frac)) {
    ama_used = AMA_turnouts$Code[which(AMA_turnouts$AMA %in% ama_users_frac$AMA[which(ama_users_frac[,district_full_name] != 0)])]
    ama_share = ama_users_frac[which(ama_users_frac[,district_full_name] != 0), district_full_name]
  }
  
  # if CAGRD, priority is recharge, otherwise not
  priority_to_recharge = 0 # 0: meet non-recharge demands first - 1: meet recharge goals first
  if (district_full_name == "CAGRD") {priority_to_recharge = 1}
  
  # identify demand growth rates for each user
  # will do this as a 2017-2021 5-year average annual delivery change
  # while removing outliers (or using them to inform uncertainty ranges)
  plot(AnnualDemand$Year,c(AnnualDemand[,district_full_name])[[1]]); title(district_full_name)
  growth_rate = c(
    round((AnnualDemand[6,district_full_name] - AnnualDemand[2,district_full_name])/AnnualDemand[2,district_full_name]/5, 
          digits = 2))[[1]]
  if (is.na(growth_rate)) {growth_rate = 0.0}
  # if growth rate is too extreme, replace with 2020-2021 rate...
  # if (abs(growth_rate) > 0.1) {
  #   growth_rate = round((AnnualDemand[5,district_full_name] - AnnualDemand[6,district_full_name])/AnnualDemand[5,district_full_name], 
  #                       digits = 1)
  # }
  
  district = list("name" = district_full_name, 
                  "MDD" = 0,
                  "AFY" = MaxAnnualDemand[which(names(MaxAnnualDemand) == district_full_name)]/1000, # converted to kAF 
                  "contract_list" = c("PTR", "MUI", "FED", "NIA"),
                  "turnout_list" = as.list("CAP"), # connected to the cap canal, the canal object json holds the spatial relations
                  "urban_profile" = c(UserDemandSeasonality[which(names(UserDemandSeasonality) == district_full_name)])[[1]],
                  "recharge_profile" = c(UserDemandRechargeMonthlyFraction[which(names(UserDemandRechargeMonthlyFraction) == district_full_name)])[[1]],
                  "project_contract" = list("PTR" = entitlement_fractions$PTR[which(entitlement_fractions$User == district_full_name)],
                                            "MUI" = entitlement_fractions$MUI[which(entitlement_fractions$User == district_full_name)],
                                            "FED" = entitlement_fractions$FED[which(entitlement_fractions$User == district_full_name)],
                                            "NIA" = entitlement_fractions$NIA[which(entitlement_fractions$User == district_full_name)]),
                  "lease_partner" = as.list(district_lease_partners),
                  "lease_quantity" = as.list(district_lease_quantity), # in kAF
                  "lease_priority" = as.list(district_lease_priority),
                  "priority_to_recharge" = c(priority_to_recharge),
                  "must_take_mead" = ifelse(test = entitlement_totals$Turnout[which(entitlement_totals$Code == d)] %in%
                                              c("LHQ"), yes = TRUE, no = FALSE),
                  # "ag_mitigation_triggers" = ag_mitigation_trigger_tiers,
                  # "ag_mitigation_donor" = FALSE,
                  # "ag_mitigation_taker" = FALSE,
                  "ama_used" = as.list(ama_used),
                  "ama_share" = as.list(ama_share),
                  "growth_rate" = growth_rate,
                  "zone" = "zone15" # THIS IS FOR CROP/IRRIGATION PURPOSES - CAP MODEL DOESN'T USE THIS INFORMATION SO IT DEFAULTS TO CA VALUES AND IGNORES THEM
                  )
  
  districts_json = toJSON(district, pretty = TRUE, dataframe = "columns", simplifyDataFrame = TRUE, auto_unbox = TRUE)
  write(districts_json, paste("../CAPFEWS/calfews_src/districts/", d, "_properties.json", sep = ""))
}




### Write Waterbank (Recharge Facility) JSONs -----------------------
## write JSON for CAP recharge projects (banks)
## where users can divert and store deliveries to accumulate credits
AMA_turnouts = data.frame("AMA" = c("Phoenix", "Pinal", "Tucson"),
                          "Code" = c("PXA", "PNA", "TSA"),
                          "Turnout" = c("HSY", "SGL", "BRW"))

UserDemandRecharge = read.csv("AMA_deliveries_recharge_byAMA.csv", header = TRUE)
UserDemandRecharge[is.na(UserDemandRecharge)] = 0

entitlement_totals = read.csv("user_entitlements.csv", header = TRUE)
users_codes = entitlement_totals %>% select(User, Code)

# fix some names when reading in files
colnames(UserDemandRecharge)[c(3,13,20)] = c("Ak-Chin", "Oro Valley", "Tohono O'odham")

for (ama in AMA_turnouts$AMA) {
  ama_users = UserDemandRecharge %>% filter(AMA == ama) %>% select(-datetime, -AMA)
  ama_users = apply(ama_users, 2, sum)
  ama_users = names(ama_users)[which(ama_users != 0)]
  print(paste(ama, " AMA gets recharge deliveries from ", ama_users, sep = ""))
  
  # capfews codes of users in a particular AMA
  ama_user_codes = users_codes$Code[which(users_codes$User %in% ama_users)]
  ama_code = AMA_turnouts$Code[which(AMA_turnouts$AMA == ama)]
  
  # assume uncapped ability to recharge by each user, with equal shares
  # no ability to recover GW, and infinite recharging capacity
  tot_storage = 99999; recovery = 0; initial_recharge = 999999
  recharge_decline = rep(1, 12)
  ownership_shares = rep(1/length(ama_user_codes), length(ama_user_codes))
  participant_type = rep("direct", length(ama_user_codes))
  
  ownership = as.data.frame(t(ownership_shares))
  colnames(ownership) = ama_user_codes
  
  # build the JSON for each AMA and export to CAPFEWS
  ama_file = list("name" = paste(ama, "AMA", sep = " "), 
                  "canal_rights" = as.list("CAP"),
                  "participant_list" = c(ama_user_codes),
                  "initial_recharge" = initial_recharge,
                  "tot_storage" = tot_storage,
                  "recovery" = recovery,
                  "recharge_decline" = recharge_decline,
                  "ownership" = as.list(ownership)
                  )
  
  waterbanks_json = toJSON(ama_file, pretty = TRUE, dataframe = "columns", simplifyDataFrame = TRUE, auto_unbox = TRUE)
  write(waterbanks_json, paste("../CAPFEWS/calfews_src/banks/", ama_code, "_properties.json", sep = ""))
}



### Write Reservoir JSONs -----------------------------------

# lake pleasant monthly guide curve is estimated by me based on 2022 water level projections
# from: https://www.cap-az.com/water/cap-system/water-operations/lake-pleasant/
pleasant_elevation_capacity = 1702 # ft
pleasant_storage_capacity = 850244 # in AF
pleasant_unusable_storage = 37841 # in AF
pleasant_MWDaccount_capacity = 157600 # in AF
pleasant_CAPaccount_capacity = 
  pleasant_storage_capacity - pleasant_MWDaccount_capacity - pleasant_unusable_storage

pleasant_monthly_elevation_projections = # in ft
  c(1681.8, 1686.8, 1691.7, 1692.8, 1693.5, 1685.7,
    1674.2, 1658.6, 1654.2, 1655.1, 1659.8, 1662.7)
pleasant_monthly_gaged_inflow_projections = # in AF/month
  c(3500, 3500, 2200, 1500, 0, 0,
    1200, 1200, 1200, 2000, 2000, 2300)
pleasant_monthly_net_evap_projections = # in inches
  c(1.6, 1.7, 4.1, 6.7, 10.0, 11.0,
    10.3, 9.0, 7.3, 3.25, 3.4, 2.0)
pleasant_monthly_seepage_projections = # in inches
  c(62, 56, 62, 60, 62, 60,
    62, 62, 60, 62, 60, 62)
pleasant_monthly_MWDlakewater_projections = # in AF, exchanged deliveries to MWD
  c(470, 635, 2235, 3411, 3529, 2765,
    3117, 1529, 2588, 3764, 941, 0)
pleasant_monthly_CAPDiversionPumping_projections = # in fraction of CAP diversion
  c(0.57, 0.53, 0.25, 0.12, 0.025, 0,
    0, 0, 0.15, 0.13, 0.56, 0.39)

# from table of elevations and storage, create a storage calculation function
# used excel to get a rough polynomial storage function:
#  Volume = 28846633 + -18579.4*Elevation + -12.8222*Elevation^2 + 0.008269*Elevation^3
pleasant_table = read.csv("Pleasant_StorageToElevation_Chart.csv", header = TRUE)
plot(pleasant_table$Elevation_ft, pleasant_table$Volume_AF)
get_pleasant_volume_from_elevation = function(elev, intercept, a1, a2, a3) {
  volume = intercept + a1 * elev + a2 * elev^2 + a3 * elev^3
  return(volume)
}
lines(pleasant_table$Elevation_ft, 
      get_pleasant_volume_from_elevation(pleasant_table$Elevation_ft,
                                         28846633, -18579.4, -12.8222, 0.008269),
      col = "red")

# similarly:
# Elevation = 1542.36 + 0.000413939*Vol + -4.77E-10*Vol^2 + 2.502E-16*Vol^3
# Area = -3580975 + 6574.7*Elev + -4.0501*Elev^2 + 0.0008383*Elev^3

# operating parameters for New Waddell Dam that connects pleasant to the CAP canal
waddell_pumping_capacity = 3000 # cubic ft per sec: https://www.usbr.gov/lc/phoenix/projects/waddelldamproj.html
waddell_generation_capacity = 40 # MW of power: https://knowyourwaternews.com/reliability-in-the-shadows/

# additional lake pleasant details from "cap_water_forecast 2021.xslm" spreadsheet:
# tabs "B" and "Lake Pleasant Forecast"
# to calculate CAP storage:
#  (1) find total storage:
pleasant_endof2020_CAP_storage = 479341 # in AF
pleasant_endof2020_total_storage = 648953 # in AF, includes unusable storage
#     previous-year total storage + Agua Fria inflow + CAP pumping - Evap - CAP releases - flood release/spill +/- seepage
#  (2) subtract dead and inactive storage
#  (3) estimate CAP storage, based on MWD storage?
#


# natual inflows to lake pleasant from Agua Fria river:
# https://waterdata.usgs.gov/monitoring-location/09512500/#parameterCode=00065&period=P7D
# https://waterdata.usgs.gov/nwis/inventory?site_no=09512500&agency_cd=USGS
# to be consistent for future simulation, I will rely on repeated annual projections 
# from the CAP Lake Pleasant forecast
n_months = 12
carryover_frac = 0.65
Pleasant = list("name" = "Lake Pleasant",
                "capacity" = pleasant_storage_capacity/1000, # in kAF
                "has_downstream_target_flow" = FALSE,
                "env_min_flow" = 
                  list("T0" = rep(0, n_months),
                       "T1" = rep(0, n_months), 
                       "T2a" = rep(0, n_months), 
                       "T2b" = rep(0, n_months), 
                       "T3" = rep(0, n_months), 
                       "DP" = rep(0, n_months)),
                "temp_releases" = 
                  list("T0" = rep(0, n_months),
                       "T1" = rep(0, n_months), 
                       "T2a" = rep(0, n_months), 
                       "T2b" = rep(0, n_months), 
                       "T3" = rep(0, n_months), 
                       "DP" = rep(0, n_months)),
                "carryover_target" = list("T0" = pleasant_storage_capacity * carryover_frac/1000,
                                          "T1" = pleasant_storage_capacity * carryover_frac/1000, # a round number, based on last 3 years of EOY storage
                                          "T2a" = pleasant_storage_capacity * carryover_frac/1000, 
                                          "T2b" = pleasant_storage_capacity * carryover_frac/1000, 
                                          "T3" = pleasant_storage_capacity * carryover_frac/1000, 
                                          "DP" = pleasant_storage_capacity * carryover_frac/1000), 
                "max_outflow" = 150000.0/1000, # in kAF
                "dead_pool" = pleasant_unusable_storage/1000, # in kAF
                "seepage" = pleasant_monthly_seepage_projections/1000, # in kAF
                "evap" = pleasant_monthly_net_evap_projections/1000, # in kAF
                "gaged_inflow" = pleasant_monthly_gaged_inflow_projections/1000, # in kAF
                "MWD_inflow" = pleasant_monthly_MWDlakewater_projections/1000, # in kAF
                "cap_diversion_pump_frac" = pleasant_monthly_CAPDiversionPumping_projections, # fraction of diversion
                "cap_allocation_capacity" = pleasant_CAPaccount_capacity/1000, # in kAF
                "pleasant_target_elev" = pleasant_monthly_elevation_projections, # in ft
                "pump_inflow_capacity" = waddell_pumping_capacity, # in cfs
                "hydropower_generation_capacity" = waddell_generation_capacity) # in MW
pleasant_json = toJSON(Pleasant, pretty = TRUE, dataframe = "columns", simplifyDataFrame = TRUE, auto_unbox = TRUE)
write(pleasant_json, paste("../CAPFEWS/calfews_src/reservoir/", "PLS", "_properties.json", sep = ""))

# collect lake mead information - really just the info about translating elevation
# and shortage tier into CAP water availability
Mead = list("name" = "Lake Mead",
            "capacity" = 999999,
            "az_capacity" = 2800, # in kAF/yr
            "cap_allocation_capacity" = 1600, # in kAF/yr
            # "az_availability" = list("T0" = 2800,
            #                          "T1" = 2800 - 512, # in kAF/yr
            #                          "T2a" = 2800 - 592, 
            #                          "T2b" = 2800 - 640, 
            #                          "T3" = 2800 - 720, 
            #                          "DP" = 2800 - 1400),
            "az_on_river_demand" = 1300) # in kAF/yr
mead_json = toJSON(Mead, pretty = TRUE, dataframe = "columns", simplifyDataFrame = TRUE, auto_unbox = TRUE)
write(mead_json, paste("../CAPFEWS/calfews_src/reservoir/", "MDE", "_properties.json", sep = ""))


### Hold other code that may be helpful --------------------------------
# read in entitlements and collect AMA information to associate districts and waterbanks with turnouts
entitlement_totals = read.csv("user_entitlements.csv", header = TRUE)
User_turnouts = entitlement_totals %>% select(User, Code, Turnout)
AMA_turnouts = data.frame("AMA" = c("Phoenix", "Pinal", "Tucson"),
                          "Turnout" = c("HSY", "SGL", "BRW"))
