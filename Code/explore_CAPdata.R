### -----------------------------------------------------
##  Exploring CAP Financial and Water/Power Forecast Data
##    D. Gorelick (Mar 2022)
##  Stored in degorelick/CAP repository on GitHub
### -----------------------------------------------------

rm(list=ls()) # clear memory
setwd('C:/Users/dgorelic/OneDrive - University of North Carolina at Chapel Hill/UNC/Research/IM3/CAP/Data') # set directory
library(tidyverse)

### -----------------------------------------------------
##  Read in annual financial data and clean it
CAP_annual_historical_finances_profit_loss = read.xlsx(file = "P&L_RateRecon history_PNNL request.xlsx", sheetName = "GF P&L")

# remove empty rows and columns, rename FY headers
CAP_annual_historical_finances_profit_loss = CAP_annual_historical_finances_profit_loss %>% 
  select(NA..1,X2011.Actual:X2020.Actual) %>% filter(!is.na(NA..1)) %>% 
  rename(Variable = NA..1,
         '2011' = X2011.Actual, '2012' = X2012.Actual, '2013' = X2013.Actual, '2014' = X2014.Actual, '2015' = X2015.Actual,
         '2016' = X2016.Actual, '2017' = X2017..Actual, '2018' = X2018.Actual, '2019' = X2019.Actual, '2020' = X2020.Actual) %>%
  mutate(Group = NA) %>% filter(row_number() != n())

# set the group labels, clean up duplicates
row_val = CAP_annual_historical_finances_profit_loss$Variable[1]
for (r in 1:nrow(CAP_annual_historical_finances_profit_loss)) {
  if (is.na(CAP_annual_historical_finances_profit_loss$`2011`[r])) {
    row_val = CAP_annual_historical_finances_profit_loss$Variable[r]
  } 
  CAP_annual_historical_finances_profit_loss$Group[r] = row_val
  if (CAP_annual_historical_finances_profit_loss$Group[r] == CAP_annual_historical_finances_profit_loss$Variable[1]) {
    CAP_annual_historical_finances_profit_loss$Group[r] = NA
  }
}

# remove original grouping separation rows, format data the same
CAP_annual_historical_finances_profit_loss = CAP_annual_historical_finances_profit_loss %>% drop_na('2011') %>%
  mutate_if(is.numeric, as.character)

# convert to long format
CAP_PL_long = CAP_annual_historical_finances_profit_loss %>%
  pivot_longer(cols = starts_with('20'), names_to = 'FY', values_to = 'Thousand USD')


##  plot for fun
# individual timeseries for each variable in data
temp = ggplot(data = CAP_PL_long) + 
  geom_bar(aes(x = FY, y = as.numeric(`Thousand USD`), fill = Group), stat = "identity", color = NA) + 
  facet_wrap(Group ~ Variable, scales = "free_y", ncol = 5) + ylab('Thousand USD or AF') +
  theme(axis.text.x = element_text(angle = 90)) + guides(fill = FALSE)
ggsave("visualization/CAP_annual_fiscal_trends_2011_to_2020_separatedflows.png", dpi = 400, units = "in", height = 10, width = 18)

for (group_set in unique(CAP_PL_long$Group)) {
  # individual timeseries for each variable in data
  subset_to_plot = CAP_PL_long %>% filter(Group == group_set) %>% 
    filter(!grepl('Position', Variable)) %>% filter(!grepl('otal', Variable))
  if (nrow(subset_to_plot) == 0) {next}
  temp = ggplot(data = subset_to_plot) + 
    geom_bar(aes(x = FY, y = as.numeric(`Thousand USD`)), stat = "identity", color = NA, fill = "black") + 
    facet_grid(Group ~ Variable, scales = "free_y") + ylab('Thousand USD') +
    theme(axis.text.x = element_text(angle = 90)) + guides(fill = FALSE)
  
  # make sure the name can be used properly for a filepath
  group_set = gsub(pattern = "/", replacement = " or ", x = group_set)
  group_set = gsub(pattern = ":", replacement = "", x = group_set)
  ggsave(paste("visualization/CAP_annual_fiscal_trends_2011_to_2020_separatedflows_", group_set,".png"), 
         dpi = 400, units = "in", height = 6, width = length(unique(subset_to_plot$Variable))*3.2)
}

# grouped by type and shown different ways to demonstrate proportions
subset_to_plot = CAP_PL_long %>% filter(Variable != 'Water Deliveries (acre-feet in thousands)') %>%
  filter(!grepl('otal', Variable)) %>% filter(!grepl('Change', Variable)) %>% filter(!grepl('Position', Variable)) %>%
  filter(!grepl('Loss', Variable))
temp = ggplot(data = subset_to_plot) + 
  geom_bar(aes(x = FY, y = as.numeric(`Thousand USD`), fill = Variable), 
           stat = "identity", position = "stack", size = 1, color = NA) +
  facet_grid(. ~ Group, scales = "free_y") + ylab('Thousand USD') +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("CAP Annual Revenues and Costs")
ggsave("visualization/CAP_annual_fiscal_trends_2011_to_2020_stackedsums.png", dpi = 400, units = "in", height = 6, width = 13)

for (group_set in unique(subset_to_plot$Group)) {
  # individual timeseries for each variable in data
  subset_to_plot_again = subset_to_plot %>% filter(Group == group_set)
  temp = ggplot(data = subset_to_plot_again) + 
    geom_bar(aes(x = FY, y = as.numeric(`Thousand USD`)/1000, fill = Variable), 
             stat = "identity", position = "stack", size = 1, color = NA) +
    facet_grid(. ~ Group, scales = "free_y") + ylab('') + xlab('Fiscal Year') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, face = "bold"), axis.ticks.x = element_blank()) +
    scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "M"))
  
  # make sure the name can be used properly for a filepath
  group_set = gsub(pattern = "/", replacement = " or ", x = group_set)
  group_set = gsub(pattern = ":", replacement = "", x = group_set)
  ggsave(paste("visualization/CAP_annual_fiscal_trends_2011_to_2020_stackedflows_", group_set,".png"), 
         dpi = 400, units = "in", height = 6, width = 8)
}


### ---------------------------------------------
##  Repeat plotting process for remaining finanial data

# read data
CAP_annual_historical_finances_reconciliation = read.xlsx(file = "P&L_RateRecon history_PNNL request.xlsx", sheetName = "OMR Rec")

# remove empty rows and columns, rename FY headers
CAP_annual_historical_finances_reconciliation_reduced = CAP_annual_historical_finances_reconciliation %>% 
  select(Rate.Reconciliation:NA..9) %>% 
  filter(!is.na(Rate.Reconciliation)) %>% 
  filter(Rate.Reconciliation != '(Dollars in Thousands)') %>% 
  rename(Variable = Rate.Reconciliation,
         '2011' = NA., '2012' = NA..1, '2013' = NA..2, '2014' = NA..3, '2015' = NA..4,
         '2016' = NA..5, '2017' = NA..6, '2018' = NA..7, '2019' = NA..8, '2020' = NA..9) 

# group variables
CAP_annual_historical_finances_reconciliation_reduced = 
  CAP_annual_historical_finances_reconciliation_reduced %>% mutate(Group = NA)
row_val = CAP_annual_historical_finances_reconciliation_reduced$Variable[1]
for (r in 1:nrow(CAP_annual_historical_finances_reconciliation_reduced)) {
  if (is.na(CAP_annual_historical_finances_reconciliation_reduced$`2011`[r])) {
    row_val = CAP_annual_historical_finances_reconciliation_reduced$Variable[r]
  } 
  CAP_annual_historical_finances_reconciliation_reduced$Group[r] = row_val
}

# remove original grouping separation rows, format data the same
CAP_annual_historical_finances_reconciliation_reduced_final = 
  CAP_annual_historical_finances_reconciliation_reduced %>% drop_na('2011') %>%
  mutate_if(is.numeric, as.character)

# convert to long format
CAP_Rec_long = CAP_annual_historical_finances_reconciliation_reduced_final %>%
  pivot_longer(cols = starts_with('20'), names_to = 'FY', values_to = 'Thousand USD')

##  plot for fun
# individual timeseries for each variable in data
temp = ggplot(data = CAP_Rec_long) + 
  geom_bar(aes(x = FY, y = as.numeric(`Thousand USD`), fill = Group), stat = "identity", color = NA) + 
  facet_wrap(Group ~ Variable, scales = "free_y", ncol = 5) + ylab('Units (see panel subtitles)') +
  theme(axis.text.x = element_text(angle = 90)) + guides(fill = FALSE)
ggsave("visualization/CAP_reconciliation_fiscal_trends_2011_to_2020_separatedflows.png", 
       dpi = 400, units = "in", height = 10, width = 18)

# grouped by type and shown different ways to demonstrate proportions
# separate water rate, water delivery, and financial categories
subset_to_plot_delivery = CAP_Rec_long %>% filter(grepl('Acre-Feet', Group)) 
temp = ggplot(data = subset_to_plot_delivery) + 
  geom_bar(aes(x = FY, y = as.numeric(`Thousand USD`)/1000, fill = Variable), 
           stat = "identity", position = "stack", size = 1, color = NA) +
  facet_grid(Group ~ Variable) + ylab('Deliveries (kAF)') + guides(fill = FALSE) + 
  xlab('Fiscal Year') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3), axis.ticks.x = element_blank()) + 
  ggtitle("CAP Annual Water Deliveries")
ggsave("visualization/CAP_reconciliation_fiscal_trends_2011_to_2020_separateflows_deliveries.png", 
       dpi = 400, units = "in", height = 5, width = length(unique(subset_to_plot_delivery$Variable))*3)

## repeat for rates
subset_to_plot_rates = CAP_Rec_long %>% filter(grepl('AF', Group)) %>%
  mutate(Variable = gsub(pattern = "Published", replacement = "", x = Variable)) %>%
  mutate(Variable = gsub(pattern = " rate", replacement = "", x = Variable)) %>%
  mutate(Variable = gsub(pattern = "Total  ", replacement = "", x = Variable)) %>%
  mutate(Variable = gsub(pattern = "Total ", replacement = "", x = Variable)) %>%
  mutate(Variable = gsub(pattern = "Calculated ", replacement = "", x = Variable)) %>%
  mutate(Variable = gsub(pattern = "Published ", replacement = "", x = Variable)) %>%
  mutate(Variable = trimws(x = Variable, which = 'both', whitespace = "[ ]")) %>% 
  mutate(Variable = gsub(pattern = "Fixed OM&R Rate", replacement = "Fixed OM&R", x = Variable))

temp = ggplot(data = subset_to_plot_rates) + 
  geom_bar(aes(x = FY, y = as.numeric(`Thousand USD`), fill = Variable), 
           stat = "identity", position = "stack", size = 1, color = NA) +
  facet_grid(Variable ~ Group, scales = "free_y") + ylab('Rate ($/AF)') + guides(fill = FALSE) + 
  xlab('Fiscal Year') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3), axis.ticks.x = element_blank()) + 
  ggtitle("CAP Annual Water Rates")
ggsave("visualization/CAP_reconciliation_fiscal_trends_2011_to_2020_separateflows_rates.png", 
       dpi = 400, units = "in", width = 8, height = length(unique(subset_to_plot_rates$Variable))*3)

# widen subset of rate data to determine differences between budgeted and published rates
subset_to_plot_rates_wide = subset_to_plot_rates %>%
  pivot_wider(names_from = 'FY', values_from = 'Thousand USD') 

difference_set = subset_to_plot_rates_wide %>% 
  filter(Group != 'Water Delivery Rate ($/AF)') %>% select(`2011`:`2020`) %>%
  mutate_if(is.character, as.numeric) - 
  subset_to_plot_rates_wide %>% 
  filter(Group == 'Water Delivery Rate ($/AF)') %>% select(`2011`:`2020`) %>%
  mutate_if(is.character, as.numeric)
difference_set = difference_set %>% 
  mutate(Variable = unique(subset_to_plot_rates_wide$Variable), Group = 'Difference')
difference_set_long = difference_set %>% pivot_longer(cols = starts_with('20'), names_to = 'FY', values_to = '$/AF')

temp = ggplot(data = difference_set_long) + 
  geom_bar(aes(x = FY, y = as.numeric(`$/AF`), fill = Variable), 
           stat = "identity", position = "stack", size = 1, color = NA) +
  facet_grid(Group ~ Variable) + ylab('Rate Difference ($/AF)') + guides(fill = FALSE) + 
  xlab('Fiscal Year') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3), axis.ticks.x = element_blank()) + 
  ggtitle(label = "CAP Annual Water Rates - Differences between Budgeted and Published Rates", 
          subtitle = "(Positive: Published rate higher than budgeted rate)")
ggsave("visualization/CAP_reconciliation_fiscal_trends_2011_to_2020_separateflows_rates_differences.png", 
       dpi = 400, units = "in", height = 6, width = length(unique(subset_to_plot_rates$Variable))*2.5)

## repeat for financial values
subset_to_plot_financial = CAP_Rec_long %>% filter(!grepl('Acre-Feet', Group)) %>% filter(!grepl('AF', Group))
temp = ggplot(data = subset_to_plot_financial) + 
  geom_bar(aes(x = FY, y = as.numeric(`Thousand USD`)/1000, fill = Variable), 
           stat = "identity", position = "stack", size = 1, color = NA) +
  facet_wrap(Group ~ Variable, scales = "free_y") + ylab('') + guides(fill = FALSE) + 
  xlab('Fiscal Year') +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "M")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3), axis.ticks.x = element_blank()) + 
  ggtitle("CAP Annual Financial Actuals")
ggsave("visualization/CAP_reconciliation_fiscal_trends_2011_to_2020_separateflows_finances.png", 
       dpi = 400, units = "in", height = 7, width = 18)






### -----------------------------------------------------
##  Read in forecast spreadsheet and collect info annually
##    this is a huge amount of data so it will take some time
##    there are many tabs, so we can do this annually
for (year_tab in 2008:2021) {
  # read in year tab
  extra_tab_value = ""; if (year_tab >= 2020) {extra_tab_value = " T0"}
  CAP_forecast = readxl::read_xlsx(path = "forecast Historical 2008 to 2021.xlsx", 
                                   sheet = paste(as.character(year_tab), extra_tab_value, sep = ""), 
                                   trim_ws = FALSE)
  print(paste(as.character(year_tab), extra_tab_value, sep = ""))
  
  # standardize basic column headers so we can be consistent between spreadsheets
  n_columns_in_sheet = ncol(CAP_forecast)
  sheet_columns_base_names = paste("C", as.character(c(1:n_columns_in_sheet)), sep="")
  colnames(CAP_forecast) = sheet_columns_base_names
  
  # extract commonly-formatted data
  # first ~80% of each sheet has consistent formatting - last chunk of 
  # forecast data is a set titled TOTAL DELIVERIES BY CLASS
  # that I will use as a flag to split up the data and read it
  # consistently between tabs
  final_first_set_row = which(stringr::str_squish(CAP_forecast$C1) == "TOTAL DELIVERIES")[
    which(stringr::str_squish(CAP_forecast$C1) == "TOTAL DELIVERIES") > 
      which(stringr::str_squish(CAP_forecast$C1) == "TOTAL DELIVERIES BY CLASS")]
  
  # there are consistent section headers in each year, with standard formatting
  # so we can try to collect the info based on this?
  # there are occasionally unexpected additions which contain lowercase variables
  SectionHeaders = c(CAP_forecast$C1[grepl("PP", stringr::str_squish(CAP_forecast$C1))], 
                     "WADDELL PGP", "TOTAL SYSTEM DELIVERIES", "TOTAL DELIVERIES BY CLASS")
  SectionHeaders = SectionHeaders[!str_detect(SectionHeaders, '[:lower:]')]
  
  ColumnNames = c("Variable", 
                  "Jan", "Feb", "Mar", "Q1", 
                  "Apr", "May", "Jun", "Q2", 
                  "Jul", "Aug", "Sep", "Q3", 
                  "Oct", "Nov", "Dec", "Q4", "Total",
                  "Empty1", "Physical Turnout", "Empty2")[1:min(n_columns_in_sheet,21)]
  
  # create master table to hold all years
  if (year_tab == 2008) {
    all_sections = CAP_forecast[1,] %>% 
      rename_at(vars(colnames(CAP_forecast)[1:length(ColumnNames)]), function(x) ColumnNames) %>% 
      select(-grep("Empty", ColumnNames))
    all_sections = all_sections %>%
      select(-grep("C", colnames(all_sections))) %>%
      mutate(Group = NA, Subgroup = NA, Name = NA, Section = NA, Year = NA) %>% filter(!is.na(Jan))
  }
  
  ## run code for first half of data
  # for debugging: section_name = SectionHeaders[4] is Hassayampa PP
  for (section_name in SectionHeaders) {
    # set bounds to read in each section of code
    section_start_row = which(CAP_forecast$C1 == section_name)
    section_end_row = min(which(CAP_forecast$C1 %in% SectionHeaders)[
      which(CAP_forecast$C1 %in% SectionHeaders) > section_start_row], final_first_set_row+1, na.rm = TRUE)
    
    # read in and organize
    section = CAP_forecast[section_start_row:(section_end_row-1),] %>% filter(C1 != section_name) %>%
      rename_at(vars(colnames(CAP_forecast)[1:length(ColumnNames)]), function(x) ColumnNames) %>% 
      select(-grep("Empty", ColumnNames))
    section = section %>%
      select(-grep("C", colnames(section))) %>%
      mutate(Group = NA, Subgroup = NA, Name = NA, Section = stringr::str_squish(section_name), Year = year_tab)
    
    # based on indentations, sort into subcategory headers
    temporary_group = NA; temporary_subgroup = NA; temporary_name = NA
    for (row in 1:nrow(section)) {
      if (is.na(section$Variable[row])) {next}
      
      # appears to be a typo in this particular region, causing
      # all the ag use to be transcribed under "DROUGHT" use
      if (section_name == "SALT GILA PP" & section$Variable[row] == "DROUGHT") {
        section$Variable[row] = "    Drought"
      }
      
      # also, there is a case where some temporary rows are not indented as necessary 
      # and this disrupts how the following rows are classified
      if (section$Variable[row] == "Temporary (-) for reduced deliveries (m&i)") {
        section$Variable[row] = "           Temporary (-) for reduced deliveries"
      } 
      if (section$Variable[row] == "Temporary Reduced M&I demand  (-) ") {
        section$Variable[row] = "           Temporary (-) for reduced deliveries"
      }
      if (section$Variable[row] == "Temporary Indirect Reduced demand (-)") {
        section$Variable[row] = "           Temporary (-) for reduced deliveries"
      }
      if (section$Variable[row] == "Temporary (-) for reduced deliveries (ag)") {
        section$Variable[row] = "           Temporary (-) for reduced deliveries"
      }
      
      # longest indentations are names of users? more than 5 spaces of indentation
      if (startsWith(stringr::str_squish(section$Variable[row]), "TOTAL")) {
        temporary_group = section$Variable[row]; temporary_subgroup = NA; temporary_name = NA
      } else if (startsWith(section$Variable[row], "      ")) {
        temporary_name = section$Variable[row]
      } else if (startsWith(section$Variable[row], " ")) {
        temporary_subgroup = section$Variable[row]; temporary_name = NA
      } else {
        temporary_group = section$Variable[row]; temporary_subgroup = NA; temporary_name = NA
      }
      section$Group[row] = stringr::str_squish(temporary_group)
      section$Subgroup[row] = stringr::str_squish(temporary_subgroup)
      section$Name[row] = stringr::str_squish(temporary_name)
    }
    
    # clean up by dropping empty rows (mostly rows that just had headers in original file)
    # and add to larger master set
    section_clean = section %>% filter(!is.na(Jan) | !is.na(Total))
    all_sections = rbind(all_sections, section_clean)
    
  }
  
  ## collect remaining data from bottom of sheets
  SectionHeaders = c("CAP Pumping Plants - Projection of Water Volumes Pumped",
                     "Analysis and breakdown of energy use", 
                     "CAP Pumping Plants - Average Flow Projection",
                     "CAP Pumping Plants - Projection of Energy Use",
                     "CAP Pumping Plants - Projection of Energy Use - For Waddell Filling Only",
                     "CAP Pumping Plants - Projection of Energy Use - For Deliveries Only",
                     "Projection of Navajo Power Purchases",
                     "Projection of CAP Energy Purchases",
                     "HOURLY AVERAGE of CAP Energy Resources (Mega Watts)",
                     "Monthly Projection of CAP Energy Resources (Mega Watt Hours)",
                     "Mark Wilmer PP Detailed Daily \"Average\"  Power Schedule",
                     "CAP Pumping Plants - Daily \"Average\"  Power Schedule",
                     "Non-firm Transmission Service",
                     "CAP Energy Transmission Losses (Financial Reconcilation)",
                     "CAP Energy Transmission Losses",
                     "Lake Pleasant Projected EOM Elevation (ft)")
  # this is not the best option, but for each table (which shifts position between sheets)
  # set the dimensions box to search based on the location of the chart/section title,
  # and the horizontal offset from the upper left corner of the dimension box
  SectionDimensions = list(c(18,14, 1),
                           c(15,13, 2),
                           c(18,13, 1),
                           c(18,15, 1),
                           c(19,16, 2),
                           c(),
                           c(),
                           c(),
                           c(),
                           c(),
                           c(),
                           c(),
                           c(),
                           c(),
                           c(),
                           c())
  for (section_name in SectionHeaders) {
    section_two = CAP_forecast[(final_first_set_row+1):nrow(CAP_forecast),]
  }
  
    
  CAP_forecast[apply(CAP_forecast, MARGIN = 1, function(r) any(grepl("By:", r))),]
  
  
}

# print to cleaned spreadsheet
write.csv(file = paste("cleaned_annual_forecast_first_section_ALLYEARS.csv", sep = ""), x = all_sections)

# do some plotting!
# results by month?
for (s in unique(all_sections$Section)) {
  plotter = all_sections %>% filter(all_sections$Section == s) %>% 
    filter(!grepl("VOLUME PASSING", Variable)) %>%
    filter(!grepl("TOTAL", Variable)) %>%
    filter(!grepl("SEGMENT DEMAND", Variable))
  temp = ggplot(data = plotter) +
    geom_bar(aes(x = Year, y = as.numeric(Total), fill = Group), stat = "identity", color = NA) + 
    facet_wrap(Section ~ ., scales = "free_y") + ylab('AF') +
    theme(axis.text.x = element_text(angle = 90))
  ggsave(paste("visualization/CAP_forecast_actuals_2008_to_2021_section", s, ".png", sep = ""), 
         dpi = 400, units = "in", height = 5, width = 8)
}

# just do the volume passing - proxy for physical distance from Lake Havasu/CO River?
plotter = all_sections %>% filter(grepl("VOLUME PASSING", Variable)) %>% arrange(desc(as.numeric(Total)))
temp = ggplot(data = plotter) +
  geom_line(aes(x = Section, y = as.numeric(Total), color = Year, group = Year)) + 
   ylab('AF') +
  theme(axis.text.x = element_text(angle = 90))
ggsave(paste("visualization/CAP_forecast_actuals_2008_to_2021_section_VOLUMEPASSING", ".png", sep = ""), 
       dpi = 400, units = "in", height = 5, width = 8)

# identify biggest users?
plotter = all_sections %>% filter(!is.na(Name)) %>% arrange(desc(as.numeric(Total))) %>%
  filter(!is.na(Subgroup)) %>% filter(!grepl("Temporary", Group)) %>% 
  mutate(Group = replace(Group, Group == "RECHARGE1:", "RECHARGE:"))
temp = ggplot(data = plotter) +
  geom_bar(aes(y = reorder(Name, -as.numeric(Total)), x = as.numeric(Total), fill = Year), stat = "identity", color = NA) + 
  facet_wrap(Group ~ ., scales = "free_y", nrow = 1) + ylab('') + xlab("Acre-Feet") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(paste("visualization/CAP_forecast_actuals_2008_to_2021_section_NamedUsers", ".png", sep = ""), 
       dpi = 400, units = "in", height = 35, width = 40)

# plot users over time/diversion region
temp = ggplot(data = plotter[which(plotter$Section != "TOTAL SYSTEM DELIVERIES"),]) +
  geom_bar(aes(y = as.numeric(Total), x = Year, fill = Subgroup), stat = "identity", color = NA) + 
  facet_grid(Section ~ Group) + xlab('Year') + ylab("Acre-Feet") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(paste("visualization/CAP_forecast_actuals_2008_to_2021_section_aggregateintimespace", ".png", sep = ""), 
       dpi = 400, units = "in", height = 18, width = 12)

temp = ggplot(data = plotter[which(plotter$Section != "TOTAL SYSTEM DELIVERIES"),]) +
  geom_bar(aes(y = as.numeric(Total), x = Year, fill = Subgroup), stat = "identity", color = NA) + 
  facet_grid(Section ~ Group, scales = "free_y") + xlab('Year') + ylab("Acre-Feet") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(paste("visualization/CAP_forecast_actuals_2008_to_2021_section_aggregateintimespace_freeyaxis", ".png", sep = ""), 
       dpi = 400, units = "in", height = 18, width = 12)

# plot over time 
temp = ggplot(data = plotter) +
  geom_bar(aes(y = as.numeric(Total), x = Year, fill = Subgroup), stat = "identity", color = NA) + 
  facet_wrap(Group ~ ., nrow = 1) + xlab('Year') + ylab("Acre-Feet") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(paste("visualization/CAP_forecast_actuals_2008_to_2021_section_aggregateintime", ".png", sep = ""), 
       dpi = 400, units = "in", height = 7, width = 15)


