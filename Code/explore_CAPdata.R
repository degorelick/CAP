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
CAP_annual_historical_finances_reconciliation = 
  readxl::read_xlsx(path = "P&L_RateRecon history_PNNL request.xlsx", sheet = "OMR Rec",
                    trim_ws = FALSE)

# remove empty rows and columns, rename FY headers
CAP_annual_historical_finances_reconciliation_reduced = CAP_annual_historical_finances_reconciliation %>% 
  select(`Rate Reconciliation`:...11) %>% 
  filter(!is.na(`Rate Reconciliation`)) %>% 
  filter(`Rate Reconciliation` != '(Dollars in Thousands)') %>% 
  rename(Variable = `Rate Reconciliation`,
         '2011' = ...2, '2012' = ...3, '2013' = ...4, '2014' = ...5, '2015' = ...6,
         '2016' = ...7, '2017' = ...8, '2018' = ...9, '2019' = ...10, '2020' = ...11) 

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

# show stacked contributions to total rate
plotter = subset_to_plot_rates %>% 
  filter(Variable != "Water Delivery Rate") %>%
  filter(Variable != "Fixed OM&R") %>%
  filter(Variable != "Pumping Energy Rate")
temp = ggplot(data = plotter) + 
  geom_bar(aes(x = FY, y = as.numeric(`Thousand USD`), fill = Variable), 
           stat = "identity", position = "stack", size = 1, color = NA) +
  facet_grid(. ~ Group, scales = "free_y") + ylab('Rate ($/AF)') +  
  guides(fill = guide_legend(title = "Rate Component")) +
  xlab('Fiscal Year') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3), axis.ticks.x = element_blank()) + 
  ggtitle("Composition of CAP Annual Water Rates")
ggsave("visualization/CAP_reconciliation_fiscal_trends_2011_to_2020_cumulativecontribution_rates.png", 
       dpi = 400, units = "in", width = 8, height = 4)


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

plotter = difference_set_long %>% 
  filter(Variable != "Water Delivery Rate") %>%
  filter(Variable != "Fixed OM&R") %>%
  filter(Variable != "Pumping Energy Rate")
temp = ggplot(data = plotter) + 
  geom_bar(aes(x = FY, y = as.numeric(`$/AF`), fill = Variable), 
           stat = "identity", position = "stack", size = 1, color = NA) +
  facet_grid(Group ~ Variable) + ylab('Rate Difference ($/AF)') + guides(fill = FALSE) + 
  xlab('Fiscal Year') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3), axis.ticks.x = element_blank()) + 
  ggtitle(label = "CAP Annual Water Rates - Differences between Budgeted and Published Rates", 
          subtitle = "(Positive: Published rate higher than budgeted rate)")
ggsave("visualization/CAP_reconciliation_fiscal_trends_2011_to_2020_separateflows_rates_differences.png", 
       dpi = 400, units = "in", height = 6, width = length(unique(plotter$Variable))*2.5)

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
                     "Lake Pleasant Projected EOM Elevation (ft)",
                     "Monthly Projection of CAP Energy Resources (MWh)")
  # this is not the best option, but for each table (which shifts position between sheets)
  # set the dimensions box to search based on the location of the chart/section title,
  # and the horizontal offset from the upper left corner of the dimension box
  SectionDimensions = list(c(18,25, 2),
                           c(15,23, 2),
                           c(18,23, 3),
                           c(18,25, 3),
                           c(9,25, 2),
                           c(8,25, 1),
                           c(16,25, 6),
                           c(19,29, 7),
                           c(21,19, 1),
                           c(20,24, 1),
                           c(16,22, 1),
                           c(25,20, 1),
                           c(1,20, 0),
                           c(18,30, 1),
                           c(18,30, 1),
                           c(1,14, 0),
                           c(20,24, 1))
  # create master list to hold all tables
  if (year_tab == 2008) {
    all_bottom_sections = list()
  }
  for (section_id in c(1:length(SectionHeaders))) {
    # first, locate table in sheet (if present)
    section_name = SectionHeaders[section_id]
    section_header_row = which(apply(CAP_forecast, MARGIN = 1, function(r) 
      any(grepl(paste("\\<", section_name, "\\>", sep = ""), r) | 
            r == section_name | 
            grepl(paste("\\b", section_name, "\\b", sep = ""), r), na.rm = TRUE)))
    section_header_col = 
      which(apply(CAP_forecast[section_header_row,], MARGIN = 2, function(r) 
        any(grepl(paste("\\<", section_name, "\\>", sep = ""), r) | 
              r == section_name | 
              grepl(paste("\\b", section_name, "\\b", sep = ""), r), na.rm = TRUE)))
    if (length(section_header_row) == 0) {next}
    
    # if more than one header row was caught, scan to find the exact match
    if (length(section_header_row) > 1) {
      temp_data_to_check = CAP_forecast[section_header_row, section_header_col] %>% mutate_if(is.character, stringr::str_squish)
      correct_index = which(temp_data_to_check == section_name, arr.ind = TRUE)
      section_header_row = section_header_row[correct_index[1]]
      section_header_col = section_header_col[correct_index[2]]
    }
    
    # extract range of table and check for errors...
    section_end_row = section_header_row + SectionDimensions[[section_id]][2]
    section_start_col = max(section_header_col - SectionDimensions[[section_id]][3],1)
    section_end_col = min(section_start_col + SectionDimensions[[section_id]][1], 
                          ncol(CAP_forecast))
    section_table = CAP_forecast[section_header_row:section_end_row, section_start_col:section_end_col]
    
    # if this is the first year of data, format the master tables
    # else, clean out empty rows add to table
    # once raw data has been roughly aggregated, more cleaning will be done
    # if this is the last table, it is really a version of previous table 10
    #   with new name, so adjust accordingly
    if (section_id == length(SectionHeaders)) {section_id = 10}
    sheet_columns_base_names = paste("C", as.character(c(1:ncol(section_table))), sep="")
    colnames(section_table) = sheet_columns_base_names
    if (year_tab == 2008 | section_id > length(all_bottom_sections)) {
      all_bottom_sections[[section_id]] = section_table %>% filter(!is.na(C1) | !is.na(C2)) %>% 
        mutate(Year = year_tab, Table = section_name)
      if (section_name == "Projection of CAP Energy Purchases") {
        all_bottom_sections[[section_id]] = section_table %>% filter(!is.na(C1) | !is.na(C2) | !is.na(C3)) %>% 
          mutate(Year = year_tab, Table = section_name)
      }
      if (section_name == "Mark Wilmer PP Detailed Daily \"Average\"  Power Schedule") {
        all_bottom_sections[[section_id]] = section_table %>% 
          mutate(Year = year_tab, Table = section_name)
      }
    } else if (section_name == "Mark Wilmer PP Detailed Daily \"Average\"  Power Schedule" & year_tab == 2013) {
      print("something happens here - not sure why but added exception")
      all_bottom_sections[[section_id]] = section_table %>% 
        mutate(Year = year_tab, Table = section_name)
    } else if (section_name == "Mark Wilmer PP Detailed Daily \"Average\"  Power Schedule" & year_tab == 2014) {
      print("something happens here - not sure why but added exception again")
      all_bottom_sections[[section_id]] = rbind(all_bottom_sections[[section_id]], 
                                                section_table %>% 
                                                  mutate(Year = year_tab, Table = section_name))
    } else {
      all_bottom_sections[[section_id]] = rbind(all_bottom_sections[[section_id]], 
                                                section_table %>% filter(!is.na(C1) | !is.na(C2)) %>% 
                                                  mutate(Year = year_tab, Table = section_name))
    }
  }
}

# print to cleaned spreadsheet
write.csv(file = paste("CAP_deliveries_by_user_2008_to_2021.csv", sep = ""), x = all_sections)

## clean the section section semi-manually!
lower_case_months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
upper_case_months = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                      "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
for (section in all_bottom_sections) {
  
  if (section$Table[1] == "Lake Pleasant Projected EOM Elevation (ft)") {
    section_cleaned = section %>% 
      filter(C1 != "Lake Pleasant Projected EOM Elevation (ft)") %>%
      mutate(Value = ifelse(is.na(C2), as.numeric(C1), as.numeric(C2)),
             Month = ifelse(is.na(C2), C2, C1),
             Variable = "Elevation (ft)")
    section_cleaned = section_cleaned %>% 
      mutate(Month = rep(section$C1[106:117], length.out = nrow(section_cleaned))) %>%
      select(Table, Year, Month, Variable, Value)
  }
  
  if (section$Table[1] == "CAP Energy Transmission Losses") {
    additional_row_names_to_keep = as.character(t(section[7:11,1]))
    names_for_new_cols = as.character(section[6,2:17])
    section_cleaned = section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, additional_row_names_to_keep, "Total")) %>%
      mutate(C1 = toupper(C1)) %>% select(-C18,-C19)
    colnames(section_cleaned) = c("Month", names_for_new_cols, "Year", "Table")
    section_cleaned = section_cleaned %>%
      mutate(Month = ifelse(is.na(Total) & Month == "TOTAL", "TOTAL PERCENT LOSS", Month))
    
    # make final set
    section_cleaned = reshape2::melt(section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value)
  }
  
  if (section$Table[1] == "CAP Energy Transmission Losses (Financial Reconcilation)") {
    additional_row_names_to_keep = as.character(t(section[7:11,1]))
    names_for_new_cols = as.character(section[6,2:17])
    section_cleaned = section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, additional_row_names_to_keep, "Total")) %>%
      mutate(C1 = toupper(C1)) %>% select(-C18,-C19)
    colnames(section_cleaned) = c("Month", names_for_new_cols, "Year", "Table")
    section_cleaned = section_cleaned %>%
      mutate(Month = ifelse(is.na(Total) & Month == "TOTAL", "TOTAL PERCENT LOSS", Month))
    
    # make final set
    section_cleaned = reshape2::melt(section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value)
  }
  
  if (section$Table[1] == "Non-firm Transmission Service") {
    # need to add months column manually
    section_cleaned = section %>% 
      filter(!C1 %in% c("  Non-firm Transmission Service", "          Tucson B Plants only", 
                        "Energy", "(MWh)", "--------")) %>%
      filter(!is.na(C1)) %>% 
      mutate(Month = rep(c(upper_case_months, "TOTAL"), 13))
    colnames(section_cleaned)[1:2] = c("Energy (MWH)", "Energy Rate ($/MWH)")
    
    # make final set
    section_cleaned = reshape2::melt(section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value)
  }
  
  if (section$Table[1] == "CAP Pumping Plants - Daily \"Average\"  Power Schedule") {
    # table size shifts after 2013, again after 2016
    first_section = section %>% filter(Year < 2014)
    second_section = section %>% filter(Year >= 2014 & Year < 2017)
    third_section = section %>% filter(Year >= 2017)
    
    # build headers for each section
    first_top_header_name_row = c(rep("HAV*", 2), rep(NA, 17))
    first_new_names_for_new_cols = stringr::str_squish(
      stringr::str_c(stringr::str_replace_na(first_top_header_name_row, ""), 
                     stringr::str_replace_na(first_section[7,2:20], ""), sep = " "))
    
    second_top_header_name_row = c(rep("HAV*", 3), rep(NA, 18))
    second_new_names_for_new_cols = stringr::str_squish(
      stringr::str_c(stringr::str_replace_na(second_top_header_name_row, ""), 
                     stringr::str_replace_na(second_section[7,2:22], ""), sep = " "))
    
    third_top_header_name_row = c(rep("HAV*", 3), rep(NA, 15), 
                                  rep("Total Load", 2), rep("West Total", 2), rep("Waddell", 2), "South Total")
    third_new_names_for_new_cols = stringr::str_squish(
      stringr::str_c(stringr::str_replace_na(third_top_header_name_row, ""), 
                     stringr::str_replace_na(third_section[7,2:26], ""), sep = " "))
    
    # collect data, clean
    first_section_cleaned = first_section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total")) %>%
      mutate(C1 = toupper(C1)) %>%
      select(-C21,-C22,-C23,-C24,-C25,-C26)
    colnames(first_section_cleaned) = c("Month", first_new_names_for_new_cols, "Year", "Table")
    
    second_section_cleaned = second_section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total")) %>%
      mutate(C1 = toupper(C1)) %>%
      select(-C23,-C24,-C25,-C26)
    colnames(second_section_cleaned) = c("Month", second_new_names_for_new_cols, "Year", "Table")
    
    third_section_cleaned = third_section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total")) %>%
      mutate(C1 = toupper(C1))
    colnames(third_section_cleaned) = c("Month", third_new_names_for_new_cols, "Year", "Table")
    
    # combine
    third_section_cleaned = reshape2::melt(third_section_cleaned, id = c("Table", "Year", "Month")) 
    second_section_cleaned = reshape2::melt(second_section_cleaned, id = c("Table", "Year", "Month")) 
    first_section_cleaned = reshape2::melt(first_section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = rbind(first_section_cleaned, second_section_cleaned, third_section_cleaned)
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value) %>%
      mutate(Value = ifelse(Value == "n/a", NA, Value))
  }
  
  if (section$Table[1] == "Mark Wilmer PP Detailed Daily \"Average\"  Power Schedule") {
    # table size shifts after 2013
    first_section = section %>% filter(Year < 2014)
    second_section = section %>% filter(Year >= 2014)
    
    # apply headers to both sets, melt, then merge
    top_header_name_row = c(rep("Weekdays", 8), rep("Sundays", 4), rep("Monthly", 2))
    first_new_names_for_new_cols = stringr::str_squish(
      stringr::str_c(top_header_name_row, 
                     stringr::str_replace_na(first_section[9,2:15], ""), 
                     stringr::str_replace_na(first_section[10,2:15], ""), sep = " "))
    first_section_cleaned = first_section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total")) %>%
      mutate(C1 = toupper(C1)) %>%
      select(-C16, -C17)
    colnames(first_section_cleaned) = c("Month", first_new_names_for_new_cols, "Year", "Table")
    
    top_header_name_row = c(rep("Weekdays", 11), rep("Sundays", 4))
    second_new_names_for_new_cols = stringr::str_squish(
      stringr::str_c(top_header_name_row, 
                     stringr::str_replace_na(second_section[9,2:16], ""), 
                     stringr::str_replace_na(second_section[10,2:16], ""), sep = " "))
    second_section_cleaned = second_section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total")) %>%
      mutate(C1 = toupper(C1)) %>%
      select(-C17)
    colnames(second_section_cleaned) = c("Month", second_new_names_for_new_cols, "Year", "Table")
    
    # stick them together
    second_section_cleaned = reshape2::melt(second_section_cleaned, id = c("Table", "Year", "Month")) 
    first_section_cleaned = reshape2::melt(first_section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = rbind(first_section_cleaned, second_section_cleaned)
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value) %>%
      mutate(Value = ifelse(Value == "n/a", NA, Value))
  }
  
  if (section$Table[1] == "Monthly Projection of CAP Energy Resources (Mega Watt Hours)") {
    # table size shifts after 2015
    # 2019, 2020 have "TRUE" for long energy sales (should equal negated OPS balance energy)
    first_section = section %>% filter(Year < 2016)
    second_section = section %>% filter(Year >= 2016) %>%
      mutate(C21 = ifelse(C21 == "TRUE", as.character(-as.numeric(C19)), C21))
    
    # apply headers to both sets, melt, then merge
    first_section[8,3] = as.character(as.numeric(first_section[8,3]))
    first_section[8,4] = as.character(as.numeric(first_section[8,4]))
    first_section[6,13] = as.character(second_section[6,13])
    first_new_names_for_new_cols = stringr::str_squish(
      stringr::str_c(stringr::str_replace_na(first_section[6,2:18], ""), 
                     stringr::str_replace_na(first_section[7,2:18], ""), 
                     stringr::str_replace_na(first_section[8,2:18], ""), 
                     stringr::str_replace_na(first_section[9,2:18], ""),sep = " "))
    first_section_cleaned = first_section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total")) %>%
      mutate(C1 = toupper(C1)) %>%
      select(-C19, -C20, -C21)
    colnames(first_section_cleaned) = c("Month", first_new_names_for_new_cols, "Year", "Table")
    
    second_section[8,3] = first_section[8,3]
    second_section[8,4] = first_section[8,4]
    second_new_names_for_new_cols = stringr::str_squish(
      stringr::str_c(stringr::str_replace_na(second_section[6,2:21], ""), 
                     stringr::str_replace_na(second_section[7,2:21], ""), 
                     stringr::str_replace_na(second_section[8,2:21], ""), 
                     stringr::str_replace_na(toupper(second_section[9,2:21]), ""),sep = " "))
    second_section_cleaned = second_section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total")) %>%
      mutate(C1 = toupper(C1))
    colnames(second_section_cleaned) = c("Month", second_new_names_for_new_cols, "Year", "Table")
    
    # stick them together
    second_section_cleaned = reshape2::melt(second_section_cleaned, id = c("Table", "Year", "Month")) 
    first_section_cleaned = reshape2::melt(first_section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = rbind(first_section_cleaned, second_section_cleaned)
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value) %>%
      mutate(Table = "Monthly Projection of CAP Energy Resources (MWH)")
  }
  
  if (section$Table[1] == "HOURLY AVERAGE of CAP Energy Resources (Mega Watts)") {
    # table size shifts after 2015
    # 2019, 2020 have "TRUE" for long energy sales (should equal negated OPS balance energy)
    first_section = section %>% filter(Year < 2016)
    second_section = section %>% filter(Year >= 2016) %>%
      mutate(C21 = ifelse(C21 == "TRUE", as.character(-as.numeric(C19)), C21))
    
    # apply headers to both sets, melt, then merge
    first_section[5,3] = as.character(as.numeric(first_section[5,3]))
    first_section[5,4] = as.character(as.numeric(first_section[5,4]))
    first_section[3,13] = as.character(second_section[3,13])
    first_new_names_for_new_cols = stringr::str_squish(
      stringr::str_c(stringr::str_replace_na(first_section[3,2:18], ""), 
                     stringr::str_replace_na(first_section[4,2:18], ""), 
                     stringr::str_replace_na(first_section[5,2:18], ""), 
                     stringr::str_replace_na(first_section[6,2:18], ""),sep = " "))
  
    first_section_cleaned = first_section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total")) %>%
      mutate(C1 = toupper(C1)) %>%
      select(-C19, -C20, -C21, -C22)
    colnames(first_section_cleaned) = c("Month", first_new_names_for_new_cols, "Year", "Table")
    
    second_section[5,3] = as.character(as.numeric(second_section[5,3]))
    second_section[5,4] = as.character(as.numeric(second_section[5,4]))
    second_new_names_for_new_cols = stringr::str_squish(
      stringr::str_c(stringr::str_replace_na(second_section[3,2:21], ""), 
                     stringr::str_replace_na(second_section[4,2:21], ""), 
                     stringr::str_replace_na(second_section[5,2:21], ""), 
                     stringr::str_replace_na(second_section[6,2:21], ""),sep = " "))
    second_section_cleaned = second_section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total")) %>%
      mutate(C1 = toupper(C1)) %>%
      select(-C22)
    colnames(second_section_cleaned) = c("Month", second_new_names_for_new_cols, "Year", "Table")
    
    # stick them together
    second_section_cleaned = reshape2::melt(second_section_cleaned, id = c("Table", "Year", "Month")) 
    first_section_cleaned = reshape2::melt(first_section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = rbind(first_section_cleaned, second_section_cleaned)
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value)
  }
  
  if (section$Table[1] == "Projection of CAP Energy Purchases") {
    section_cleaned = section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total")) %>%
      mutate(C1 = toupper(C1)) %>%
      select(-C2, -C6, -C10, -C14, -C18, -C19, -C20)
    section[1,3:18] = as.list(rep(unique(as.character(section[1,3:18]))[c(1,3,4,5)], each = 4))
    new_names_for_new_cols = stringr::str_squish(stringr::str_c(stringr::str_replace_na(section[1,3:17], ""), 
                                                                stringr::str_replace_na(section[3,3:17], ""), 
                                                                stringr::str_replace_na(section[4,3:17], ""), 
                                                                stringr::str_replace_na(section[5,3:17], ""), 
                                                                stringr::str_replace_na(section[6,3:17], ""),sep = " "))
    colnames(section_cleaned) = c("Month", 
                                  new_names_for_new_cols[-c(4,8,12)],
                                  "Year", "Table")
    section_cleaned = reshape2::melt(section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value)
  }
  
  if (section$Table[1] == "Projection of Navajo Power Purchases") {
    section_cleaned = section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total")) %>%
      mutate(C1 = toupper(C1)) %>%
      select(-C16, -C17)
    section[3,3] = as.character(as.numeric(section[3,3]))
    new_names_for_new_cols = stringr::str_squish(stringr::str_c(stringr::str_replace_na(section[1,2:15], ""), 
                                                                stringr::str_replace_na(section[2,2:15], ""), 
                                                                stringr::str_replace_na(section[3,2:15], ""), 
                                                                stringr::str_replace_na(section[4,2:15], ""), sep = " "))
    colnames(section_cleaned) = c("Month", 
                                  new_names_for_new_cols,
                                  "Year", "Table")
    section_cleaned = reshape2::melt(section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value)
  }
  
  if (section$Table[1] == "CAP Pumping Plants - Projection of Energy Use - For Deliveries Only") {
    section_cleaned = section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total", "(KWH/AF)->")) %>%
      mutate(C1 = toupper(C1)) %>%
      select(-C8, -C9) %>%
      mutate(C7 = ifelse(C1 == "(KWH/AF)->", NA, C7))
    colnames(section_cleaned) = c("Month", 
                                  stringr::str_c(section[2,2:6], stringr::str_replace_na(section[3,2:6], ""), sep = ""),
                                  "Total West Energy",
                                  "Year", "Table")
    section_cleaned = reshape2::melt(section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value)
  }
  
  if (section$Table[1] == "CAP Pumping Plants - Projection of Energy Use - For Waddell Filling Only") {
    section_cleaned = section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total", "(KWH/AF)->")) %>%
      mutate(C1 = toupper(C1)) %>%
      select(-C7, -C9, -C10) %>%
      mutate(C8 = ifelse(C1 == "(KWH/AF)->", NA, C8))
    colnames(section_cleaned) = c("Month", 
                                  stringr::str_c(section[1,2:6], stringr::str_replace_na(section[2,2:6], ""), sep = ""),
                                  "Total West Energy",
                                  "Year", "Table")
    section_cleaned = reshape2::melt(section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value)
  }
  
  if (section$Table[1] == "CAP Pumping Plants - Projection of Water Volumes Pumped") {
    section_cleaned = section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total")) %>%
      select(-C18, -C19) %>%
      mutate(C1 = toupper(C1))
    colnames(section_cleaned) = c("Month", 
                                  stringr::str_c(section[1,2:17], stringr::str_replace_na(section[2,2:17], ""), sep = ""),
                                  "Year", "Table")
    section_cleaned = reshape2::melt(section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value)
    
    # THIS IS THE FIRST TABLE IN THE DATA SET ASSEMBLED, SO INITIALIZE MAIN SET
    main_power_data_set = section_cleaned
  }
  
  if (section$Table[1] == "Analysis and breakdown of energy use") {
    # other leftover headers to include in column names
    # 						                        SGL to							
    # West Plant	West Plant	Total West	BLK	   (MLD)	Waddell	Total		Average	Average	Average
    leftover_header_parts = c(NA, NA, "West Plant", "West Plant", "Total West", 
                              "SGL to BLK", "(MLD)", "Waddell", "Total", NA, "Average", "Average", "Average")
    new_names_for_new_cols = stringr::str_squish(stringr::str_c(stringr::str_replace_na(leftover_header_parts, ""),
                                            stringr::str_replace_na(section[1,2:14], ""), 
                                            stringr::str_replace_na(section[2,2:14], ""), 
                                            stringr::str_replace_na(section[3,2:14], ""), sep = " "))
    new_names_for_new_cols = c("Month", new_names_for_new_cols[c(1:9,11:13)])
    section_cleaned = section %>% 
      filter(!is.na(C1)) %>% select(-C11, -C15, -C16) %>%
      mutate(C1 = toupper(C1))
    colnames(section_cleaned)[1:13] = new_names_for_new_cols
    section_cleaned = reshape2::melt(section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value)                           
  }
  
  if (section$Table[1] == "CAP Pumping Plants - Average Flow Projection") {
    section_cleaned = section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total", "(KWH/AF)->")) %>%
      select(-C18, -C19) %>%
      mutate(C1 = toupper(C1))
    colnames(section_cleaned) = c("Month", 
                                  stringr::str_c(section[1,2:17], stringr::str_replace_na(section[2,2:17], ""), sep = ""),
                                  "Year", "Table")
    section_cleaned = reshape2::melt(section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value)
  }
  
  if (section$Table[1] == "CAP Pumping Plants - Projection of Energy Use") {
    section_cleaned = section %>% 
      filter(C1 %in% c(lower_case_months, upper_case_months, "Total", "(KWH/AF)->")) %>%
      mutate(C1 = toupper(C1)) %>%
      mutate(C18 = ifelse(C1 == "(KWH/AF)->", NA, C18),
             C19 = ifelse(C1 == "(KWH/AF)->", NA, C19))
    colnames(section_cleaned) = c("Month", 
                                  stringr::str_c(section[1,2:17], stringr::str_replace_na(section[2,2:17], ""), sep = ""),
                                  "Total System Energy", "Invoice Amount ($1000)",
                                  "Year", "Table")
    section_cleaned = reshape2::melt(section_cleaned, id = c("Table", "Year", "Month")) 
    section_cleaned = section_cleaned %>% rename(Variable = variable, Value = value)
  }
  
  # add section to the master set for a final database export and to use for plotting
  main_power_data_set = rbind(main_power_data_set, section_cleaned)
}

# output complete dataset
write.table(file = paste("CAP_power_data_2008_to_2021.csv", sep = ""), 
            x = main_power_data_set, sep = ",", row.names = FALSE)

## do some plotting!
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
  geom_line(aes(x = reorder(Section, -as.numeric(Total)), y = as.numeric(Total)/1000, color = Year, group = Year)) + 
   ylab('kAF') + xlab('CAP Section') + ggtitle('Water volume passing each section of the CAP Canal, by year') +
  theme(axis.text.x = element_text(angle = 90))
ggsave(paste("visualization/CAP_forecast_actuals_2008_to_2021_section_VOLUMEPASSING", ".png", sep = ""), 
       dpi = 400, units = "in", height = 5, width = 8)

# plot seasonal patterns of deliveries at each pumping plant region
plotter = all_sections %>% 
  mutate(Section = fct_relevel(Section, 
                               "HAVASU PP", "BOUSE PP", "LITTLE HARQUAHALA PP", 
                               "HASSAYAMPA PP", "WADDELL PGP", "SALT GILA PP", "BRADY PP",  
                               "PICACHO PP", "RED ROCK PP", "TWIN PEAKS PP", 
                               "SANDARIO PP", "BRAWLEY PP", "SNYDER HILL PP",
                               "SAN XAVIER PP", "BLACK MOUNTAIN PP", 
                               "TOTAL SYSTEM DELIVERIES", "TOTAL DELIVERIES BY CLASS")) %>%
  filter(!grepl("VOLUME PASSING", Variable)) %>%
  filter(!grepl("TOTAL", Variable)) %>%
  filter(!grepl("SEGMENT DEMAND", Variable)) %>%
  filter(!grepl("Canal", Variable)) %>%
  filter(!grepl("FLOW", Variable)) %>%
  pivot_longer(cols = c(Jan:Mar,Apr:Jun,Jul:Sep,Oct:Dec), names_to = 'Month', values_to = 'AF') %>%
  filter(!is.na(AF)) %>%
  filter(!is.na(Name)) %>%
  filter(!grepl("FLOW", Group)) %>%
  filter(!grepl("CAIDD", Group)) %>%
  filter(!grepl("Remarket", Group)) %>%
  filter(!grepl("TOTAL", Section)) %>%
  mutate(Group = replace(Group, Group == "RECHARGE1:", "RECHARGE:"))

temp = ggplot(data = plotter) +
  geom_bar(aes(x = fct_relevel(Month, 
                               "Jan", "Feb", "Mar", "Apr", "May", "Jun",  
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
               y = as.numeric(AF)/1000, fill = Year, group = Year), stat = "identity") + 
  ylab('AF') + xlab('Month') + ggtitle('Seasonal water delivery patterns in each section of the CAP Canal, 2008-2021') +
  facet_grid(Group ~ Section, scales = "free_y") + ylab('kAF') +
  theme(axis.text.x = element_text(angle = 90), strip.text.y = element_text(angle = 0, size = 10))
ggsave(paste("visualization/CAP_forecast_actuals_2008_to_2021_section_seasonal_bysectoryear", ".png", sep = ""), 
       dpi = 400, units = "in", height = 10, width = 21)

temp = ggplot(data = plotter) +
  geom_bar(aes(x = fct_relevel(Month, 
                               "Jan", "Feb", "Mar", "Apr", "May", "Jun",  
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
               y = as.numeric(AF)/1000, fill = Year, group = Year), stat = "identity") + 
  ylab('AF') + xlab('Month') + ggtitle('Seasonal water delivery patterns in each section of the CAP Canal, 2008-2021') +
  facet_grid(Group ~ Section) + ylab('kAF') +
  theme(axis.text.x = element_text(angle = 90), strip.text.y = element_text(angle = 0, size = 10))
ggsave(paste("visualization/CAP_forecast_actuals_2008_to_2021_section_seasonal_bysectoryear_normalized", ".png", sep = ""), 
       dpi = 400, units = "in", height = 10, width = 21)


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
plotter = plotter %>% mutate(Section = fct_reorder(Section, as.numeric(Total), .desc = TRUE))
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

## plotting the power data!
# skip the loop above and read it in from output
main_power_data_set = read.csv(file = paste("CAP_power_data_2008_to_2021.csv", sep = ""), header = TRUE)

main_power_data_set$Value = as.numeric(main_power_data_set$Value)
main_power_data_set$Month = fct_relevel(main_power_data_set$Month, 
                            "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", 
                            "TOTAL", "NGS", "PARKER/DAVIS", "INTERTIE", "SRP", "APS", "TOTAL PERCENT LOSS", "(KWH/AF)->")
for (table_set in unique(main_power_data_set$Table)) {
  # table_set = unique(main_power_data_set$Table)[1] for testing
  plotter = main_power_data_set %>% filter(Table == table_set)
  temp = ggplot(data = plotter) +
    geom_bar(aes(x = Year, y = Value, fill = Month), stat = "identity", color = NA) + 
    facet_grid(Month ~ Variable, scales = "free_y") + xlab('Year') + ylab("Value") +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle(table_set)
  if (table_set == "Mark Wilmer PP Detailed Daily \"Average\"  Power Schedule") {
    table_set = "Mark Wilmer PP Daily Avg Power Schedule"
  }
  if (table_set == "CAP Pumping Plants - Daily \"Average\"  Power Schedule" ) {
    table_set = "CAP Pumping Plants - Daily Avg Power Schedule" 
  }
  ggsave(paste("visualization/CAP_powerdata_", table_set, ".png", sep = ""), 
         dpi = 400, units = "in", height = 15, width = 15)
}

# focus on power uses in space/time and available resources
#  Total Navajo Energy NE= ED+TL-NG-HE-CP-PO
plotter = main_power_data_set %>% 
  filter(Table == "Monthly Projection of CAP Energy Resources (MWH)" |
           Table == "Analysis and breakdown of energy use") %>%
  filter(Variable %in% c("West Plant Energy for Waddell (1) (MWH)",
                         "West Plant Energy for Deliveries(2) (MWH)",
                         "Waddell Generation Energy (MWH)",
                         "SGL to BLK Pumping Energy (MWH)",
                         "(NG) Waddell GEN (MWH)",
                         "(HE) Total Hoover Energy (MWH)",
                         "(CP) CAP Purchases (MWH)",
                         "(PO) Energy Provided by Others (MWH)",
                         "Total RSVD (MWH)",
                         "Purchases (Short) Energy (MWH)",
                         "Sales (Long) Energy (MWH)")) %>%
  filter(Month != "TOTAL") %>%
  mutate(Variable = ifelse(Variable == "West Plant Energy for Waddell (1) (MWH)", 
                           "Waddell Filling", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "West Plant Energy for Deliveries(2) (MWH)",
                           "Pre-Pleasant Deliveries", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "Waddell Generation Energy (MWH)", 
                           "Waddell Generation", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "SGL to BLK Pumping Energy (MWH)", 
                           "Post-Pleasant Pumping", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "(NG) Waddell GEN (MWH)", 
                           "Waddell Generation", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "(HE) Total Hoover Energy (MWH)", 
                           "Hoover", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "(CP) CAP Purchases (MWH)", 
                           "Misc. Purchases", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "(PO) Energy Provided by Others (MWH)", 
                           "Misc. Purchases", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "Total RSVD (MWH)", 
                           "Navajo GS", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "Purchases (Short) Energy (MWH)", 
                           "Market Purchases", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "Sales (Long) Energy (MWH)", 
                           "Market Sales", as.character(Variable))) %>%
  mutate(Value = ifelse(Variable == "Market Sales", -Value, Value))

temp = ggplot(data = plotter) +
  geom_bar(aes(x = Year, y = Value, fill = Variable), stat = "identity", color = NA) + 
  facet_grid(. ~ Table, scales = "free_y") + xlab('Year') + ylab("Energy (MWH)") +
  theme(axis.text.x = element_text(angle = 90),
        legend.background = element_rect(fill = "grey95", color = "black"),
        legend.position = c(0.52,0.6),
        legend.justification = c(0,1),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.35, 'cm'),
        legend.direction = "vertical",) +
  ggtitle("CAP pumping power needed for deliveries and Lake Pleasant filling") +
  guides(fill = guide_legend(ncol = 2))
ggsave(paste("visualization/CAP_powerdata_energysourcessinks.png", sep = ""), 
       dpi = 400, units = "in", height = 4, width = 8)

mypal = colorRampPalette(RColorBrewer::brewer.pal(12, "PRGn"))
temp = ggplot(data = plotter) +
  geom_bar(aes(x = Year, y = Value, fill = Month), stat = "identity", color = NA) + 
  facet_grid(. ~ Table, scales = "free_y") + xlab('Year') + ylab("Energy (MWH)") +
  theme(axis.text.x = element_text(angle = 90),
        legend.background = element_rect(fill = "grey95", color = "black"),
        legend.position = c(0.55,0.9),
        legend.justification = c(0,1),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.35, 'cm'),
        legend.direction = "vertical",) +
  ggtitle("CAP pumping power needed for deliveries and Lake Pleasant filling") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_fill_manual(values = mypal(12))
ggsave(paste("visualization/CAP_powerdata_energysourcessinks_bymonth.png", sep = ""), 
       dpi = 400, units = "in", height = 4, width = 8)

# just check out market energy purchases and sales
plotter = plotter %>% filter(Variable %in% c("Market Purchases", "Market Sales")) %>%
  mutate(Value = ifelse(Variable == "Market Sales", -Value, Value))
mypal = colorRampPalette(RColorBrewer::brewer.pal(12, "PRGn"))
temp = ggplot(data = plotter) +
  geom_bar(aes(x = Year, y = Value, fill = Month), stat = "identity", color = NA) + 
  facet_grid(. ~ Variable, scales = "free_y") + xlab('Year') + ylab("Energy (MWH)") +
  theme(axis.text.x = element_text(angle = 90),
        legend.background = element_rect(fill = "grey95", color = "black"),
        legend.position = c(0.98,0.98),
        legend.justification = c(1,1),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.35, 'cm'),
        legend.direction = "vertical",) +
  ggtitle("CAP historic market power purchases and sales") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_fill_manual(values = mypal(12))
ggsave(paste("visualization/CAP_powerdata_purchasessales_bymonth.png", sep = ""), 
       dpi = 400, units = "in", height = 4, width = 8)


# focus on splits for deliveries/filling waddell
plotter = main_power_data_set %>% 
  filter(Table == "CAP Pumping Plants - Projection of Energy Use - For Waddell Filling Only" |
           Table == "CAP Pumping Plants - Projection of Energy Use - For Deliveries Only") %>%
  filter(Variable != "Total West Energy") %>%
  filter(as.character(Month) != "(KWH/AF)->") %>%
  mutate(Variable = ifelse(Variable == "HAV", "Havasu (Mark Wilmer)", as.character(Variable)),
         Table = stringr::str_squish(sub(".*-", "", Table))) %>%
  mutate(Variable = ifelse(Variable == "BSH", "Bouse", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "LHQ", "Little Harquahala", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "HSY", "Hassayampa", as.character(Variable))) %>%
  mutate(Variable = ifelse(Variable == "WADPump", "Waddell", as.character(Variable))) %>%
  filter(Month != "TOTAL")

colnames(plotter)[4] = "Pumping Plant"
temp = ggplot(data = plotter) +
  geom_bar(aes(x = Year, y = Value, fill = `Pumping Plant`), stat = "identity", color = NA) + 
  facet_grid(. ~ Table, scales = "free_y") + xlab('Year') + ylab("Energy Use (MWH)") +
  theme(axis.text.x = element_text(angle = 90),
        legend.background = element_rect(fill = "grey95", color = "black"),
        legend.position = c(0.98,0.98),
        legend.justification = c(1,1),) +
  ggtitle("CAP pumping power needed for deliveries and Lake Pleasant filling")
ggsave(paste("visualization/CAP_powerdata_waddellpowerneeds.png", sep = ""), 
       dpi = 400, units = "in", height = 4, width = 8)

#fill_range = scales::seq_gradient_pal("blue", "grey80", "Lab")(seq(0,1,length.out=12))
mypal = colorRampPalette(RColorBrewer::brewer.pal(12, "PRGn"))
temp = ggplot(data = plotter) +
  geom_bar(aes(x = Year, y = Value, fill = Month), stat = "identity", color = NA) + 
  facet_grid(. ~ Table, scales = "free_y") + xlab('Year') + ylab("Energy Use (MWH)") +
  theme(axis.text.x = element_text(angle = 90),
        legend.background = element_rect(fill = "grey95", color = "black"),
        legend.position = c(0.98,0.98),
        legend.justification = c(1,1),
        legend.direction = "horizontal") +
#  scale_fill_brewer(palette = "PRGn", ) +
  scale_fill_manual(values = mypal(12)) +
  ggtitle("CAP pumping power needed for deliveries and Lake Pleasant filling")
ggsave(paste("visualization/CAP_powerdata_waddellpowerneeds_bymonth.png", sep = ""), 
       dpi = 400, units = "in", height = 4, width = 8)


### ----------------------------------------------------------
##  Read in final spreadsheet on power purchases for 2021/2022

# read data
CAP_power_data = 
  readxl::read_xlsx(path = "2022 Monthly L&R initial Tier 1 Shortage R1 pricing 2022 post Budget 12-2-21 rev 500+ reductions.xlsx", 
                    sheet = "2022 Transaction Totals", range = "B1:U195")

# organize data - get section names and collect data from each
# each section is 13 rows, including header rows, and we can
# reduce this to 11 rows for current data because last row is
# always empty. we can also only take the first 7 columns and
# the last three because the rest are summary columns.
# there also appear to be some typos that say the data is 2021
# when it should be all 2022...
section_names = CAP_power_data$...1[apply(CAP_power_data, MARGIN = 1, function(x) {grepl("ACES", x[1], fixed = TRUE)})]
upper_case_months = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                      "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "TOTAL")
for (section_number in c(1:length(section_names))) {
  section_start_row = which(CAP_power_data$...1 == section_names[section_number])
  section_end_row = section_start_row + 11
  
  # split into two sections of data to collect more easily
  market_colnames = as.character(CAP_power_data[2,1:7])
  section_data_market = CAP_power_data[(section_start_row+2):section_end_row, c(1:7)]
  section_data_resources = CAP_power_data[(section_start_row+1):section_end_row, c(18:20)]
  
  # remove empty rows and expand set with new columns
  section_data_market = section_data_market[which(as.numeric(section_data_market$...1) > 0),]
  colnames(section_data_market) = market_colnames
  section_data_market$Month = upper_case_months[section_number]
  section_data_market$Year = 2022
  
  # similar cleaning process for resources mix data
  # just capture the SRP, Solar, and Hoover options not
  # the overall total and net purchases
  colnames(section_data_resources) = c("MWh", "Unit Price", "Total Dollars")
  section_data_resources = section_data_resources[!is.na(section_data_resources$MWh),]
  for (r in 1:nrow(section_data_resources)) {
    if (section_data_resources$MWh[r] %in% c("SRP FLEET OPTION", "SOLAR", "HOOVER")) {
      resource_data = section_data_resources[r+1,]
      resource_data$Resource = section_data_resources$MWh[r]
      resource_data$Month = upper_case_months[section_number]
      resource_data$Year = 2022
    } else {
      next()
    }
    if (r == 1) {resource_data_section = resource_data} else {
      resource_data_section = rbind(resource_data_section, resource_data)
    }
  }

  
  # add to full set
  if (section_number == 1) {
    section_data_market_long = section_data_market
    resource_data_long = resource_data_section
  } else {
    section_data_market_long = rbind(section_data_market_long, section_data_market)
    resource_data_long = rbind(resource_data_long, resource_data_section)
  }
  
}

# combine data into single set for plotting
section_data_market_long = section_data_market_long %>% 
  select(-c(Commodity, Trader)) %>%
  rename("Use" = `Buy/Sell`, "MWH" = MWh, "Unit Price ($/MWH)" = `Unit Price`)
market_melted = reshape2::melt(section_data_market_long, id = c("Year", "Month", "Product", "Use"))

resource_data_long = resource_data_long %>% 
  rename("MWH" = MWh, "Unit Price ($/MWH)" = `Unit Price`, "Product" = Resource) %>%
  mutate(Use = "Buy")
resource_melted = reshape2::melt(resource_data_long, id = c("Year", "Month", "Product", "Use"))
  
all_data = rbind(market_melted, resource_melted)
write.table(file = paste("CAP_power_purchase_data_2022.csv", sep = ""), 
            x = all_data, sep = ",", row.names = FALSE)

# do some plotting!




