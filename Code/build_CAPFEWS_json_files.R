### -----------------------------------------------------
##  Build CAPFEWS JSON Files
##    D. Gorelick (Sep 2022)
##  To be stored in degorelick/CAP repository on GitHub
### -----------------------------------------------------

rm(list=ls()) # clear memory
setwd('C:/Users/dgorelic/OneDrive - University of North Carolina at Chapel Hill/UNC/Research/IM3/CAP/Data') # set directory
library(jsonlite)

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
canal_json = toJSON(list("name" = "CAP", "capacity" = capacities, "turnout" = turnout), 
                    pretty = TRUE, dataframe = "columns", simplifyDataFrame = TRUE, auto_unbox = TRUE)
write(canal_json, "../CAPFEWS/calfews_src/canals/CAP_properties.json")



### Write Contracts JSONs -------------------------------
## write JSON for CAP canal contractors rights (contracts)
## write a separate contract for each CAP water priority class (P3, M&I, Indian, NIA)
##  Ag and Excess will have priority zero - not filled until others satisfied
entitlement_totals = read.csv("user_entitlements.csv", header = TRUE)
entitlement_totals_normalized = read.csv("user_entitlements_fractions.csv", header = TRUE)
#entitlement_totals = rbind(entitlement_totals, data.frame(V1 = c(0, 0), Class = c("AGR", "EXC")))

for (contract_class in c("PTR", "MUI", "FED", "NIA")) {
  contract = list("name" = contract_class, 
                  "total" = sum(entitlement_totals[,contract_class])/1000, # converted to kAF 
                  "maxForecastValue" = 999999,
                  "carryover" = 0,
                  "type" = "contract",
                  "allocation_priority" = ifelse(test = contract_class %in% c("AGR", "EXC"), yes = 0, no = 1),
                  "storage_priority" = 1,
                  "reduction" = list("I" = 1, 
                                     "IIa" = ifelse(test = contract_class %in% c("AGR", "EXC"), yes = 0.5, no = 1), 
                                     "IIb" = ifelse(test = contract_class %in% c("AGR", "EXC"), yes = 0, no = 1), 
                                     "III" = ifelse(test = contract_class %in% c("AGR", "EXC"), yes = 0, no = 1), 
                                     "IV" = ifelse(test = contract_class %in% c("AGR", "EXC"), yes = 0, no = 1)))
  contract_json = toJSON(contract, pretty = TRUE, dataframe = "columns", simplifyDataFrame = TRUE, auto_unbox = TRUE)
  write(contract_json, paste("../CAPFEWS/calfews_src/contracts/", contract_class, "_properties.json", sep = ""))
}



### Write District JSONs --------------------------------
## write JSON for CAP canal contractors (districts)
## each user needs its own JSON file 
library(dplyr)

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

# build the JSON files
for (d in entitlement_totals$Code) {
  district_full_name = entitlement_totals$User[which(entitlement_totals$Code == d)]
  district_lease_partners = c("none")
  district_lease_quantity = c(0)
  district_lease_priority = c("none")
  if (district_full_name %in% colnames(leases_amount)) {
    district_lease_partners = leases_amount$Partner[which(!is.na(leases_amount[,district_full_name]))]
    district_lease_quantity = leases_amount[,district_full_name][which(!is.na(leases_amount[,district_full_name]))]
    district_lease_priority = lease_priority[,district_full_name][which(!is.na(leases_amount[,district_full_name]))]
  }
  
  # if CAGRD, priority is recharge, otherwise not
  priority_to_recharge = 0 # 0: meet non-recharge demands first - 1: meet recharge goals first
  if (district_full_name == "CAGRD") {priority_to_recharge = 1}
  
  district = list("name" = district_full_name, 
                  "MDD" = MaxAnnualDemand[which(names(MaxAnnualDemand) == district_full_name)]/1000, # converted to kAF 
                  "contract_list" = c("PTR", "MUI", "FED", "NIA"),
                  "turnout_list" = c("CAP"), # connected to the cap canal, the canal object json holds the spatial relations
                  "urban_profile" = c(UserDemandSeasonality[which(names(UserDemandSeasonality) == district_full_name)])[[1]],
                  "recharge_profile" = c(UserDemandRechargeMonthlyFraction[which(names(UserDemandRechargeMonthlyFraction) == district_full_name)])[[1]],
                  "project_contract" = list("PTR" = entitlement_fractions$PTR[which(entitlement_fractions$User == district_full_name)],
                                            "MUI" = entitlement_fractions$MUI[which(entitlement_fractions$User == district_full_name)],
                                            "FED" = entitlement_fractions$FED[which(entitlement_fractions$User == district_full_name)],
                                            "NIA" = entitlement_fractions$NIA[which(entitlement_fractions$User == district_full_name)]),
                  "lease_partner" = c(district_lease_partners),
                  "lease_quantity" = c(district_lease_quantity),
                  "lease_priority" = c(district_lease_priority),
                  "priority_to_recharge" = c(priority_to_recharge) 
                  )
  
  districts_json = toJSON(district, pretty = TRUE, dataframe = "columns", simplifyDataFrame = TRUE, auto_unbox = TRUE)
  write(districts_json, paste("../CAPFEWS/calfews_src/districts/", d, "_properties.json", sep = ""))
}



### Write Recharge Facility JSONs -----------------------
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
                  "canal_rights" = c("CAP"),
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


### Hold other code that may be helpful --------------------------------
# read in entitlements and collect AMA information to associate districts and waterbanks with turnouts
entitlement_totals = read.csv("user_entitlements.csv", header = TRUE)
User_turnouts = entitlement_totals %>% select(User, Code, Turnout)
AMA_turnouts = data.frame("AMA" = c("Phoenix", "Pinal", "Tucson"),
                          "Turnout" = c("HSY", "SGL", "BRW"))
