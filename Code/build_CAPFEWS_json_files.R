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
turnout_names = c("LHQ", "WAD", "HSY", "SGL", "BRD", "RED", "SAN", "BRW", "SND", "BLK", "none")

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
## write a separate contract for each CAP water priority class (P3, M&I, Indian, NIA, Ag, Excess)
##  Ag and Excess will have priority zero - not filled until others satisfied
entitlement_totals = read.csv("user_entitlements.csv", header = TRUE)
entitlement_totals_normalized = read.csv("user_entitlements_fractions.csv", header = TRUE)
entitlement_totals = rbind(entitlement_totals, data.frame(V1 = c(0, 0), Class = c("AGR", "EXC")))

for (contract_class in c("PTR", "MUI", "FED", "NIA", "AGR", "EXC")) {
  contract = list("name" = contract_class, 
                  "total" = entitlement_totals$V1[which(entitlement_totals$Class == contract_class)]/1000, # converted to kAF 
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
AnnualDemand = UserDemandMonthly %>% 
  mutate(Year = lubridate::year(datetime)) %>%
  group_by(Year) %>% summarise_at(vars(Ak.Chin:WMAT), sum)
MaxAnnualDemand = apply(AnnualDemand,2,max)

# also read in entitlements, to assign contract fractions
entitlement_totals = read.csv("CAP_priorityclass_entitlementtotals2022.csv", header = FALSE)
entitlement_totals$Class = c("PTR", "MUI", "FED", "NIA")
entitlement_totals = rbind(entitlement_totals, data.frame(V1 = c(0, 0), Class = c("AGR", "EXC")))

# prepare matching vectors to link contract holders to their CAPFEWS codes
# and turnouts where they receive water
UsersToKeep = c("AWBA", "FMYN", "WMAT", "SRPMIC",
                "Ak-Chin", "GRIC", "SCAT", "Tohono.O.odham", 
                "HIDD", "HVID", "AZWC", "CAGRD", "CAIDD", "MSIDD", "ASARCO",
                "Chandler", "Gilbert", "Glendale", "Mesa", "Peoria", "AZ.State.Land",
                "Phoenix", "Scottsdale", "Tempe", "Tucson", "Surprise", "Goodyear", "OTHER")
UsersCodes = c("AWB", "FYN", "WMT", "SIC",
               "AKC", "GRC", "SCT", "TOH",
               "HID", "HVD", "AWC", "CGD", "CAD", "MSD", "ASR",
               "CHD", "GIL", "GLN", "MSA", "PEO", "ALD",
               "PHX", "SDL", "TPE", "TUC", "SUR", "GYR", "OTH")
UsersTurnouts = c()

i = 1
for (d in UsersToKeep) {
  print(paste(d, " is matched with ", UsersCodes[i], sep = ""))
  district = list("name" = UsersCodes[i], 
                  "MDD" = MaxAnnualDemand[which(names(MaxAnnualDemand) == d)]/1000, # converted to kAF 
                  "contract_list" = c("PTR", "MUI", "FED", "NIA", "AGR", "EXC"),
                  "turnout_list" = c("CAP"), # connected to the cap canal, the canal object json holds the spatial relations
                  "urban_profile" = c(UserDemandSeasonality[which(names(UserDemandSeasonality) == d)])[[1]],
                  "project_contract" = list("PTR" = 0,
                                            "MUI" = 0,
                                            "FED" = 0,
                                            "NIA" = 0,
                                            "AGR" = 1,
                                            "EXC" = 1))
  
  districts_json = toJSON(district, pretty = TRUE, dataframe = "columns", simplifyDataFrame = TRUE, auto_unbox = TRUE)
  write(districts_json, paste("../CAPFEWS/calfews_src/districts/", contract_class, "_properties.json", sep = ""))
  
  i = i + 1
}





### Write Recharge Facility JSONs -----------------------
## write JSON for CAP recharge projects (banks)
## where users can divert and store deliveries to accumulate credits




