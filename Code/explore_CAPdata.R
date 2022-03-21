### -----------------------------------------------------
##  Exploring CAP Financial and Water/Power Forecast Data
##    D. Gorelick (Mar 2022)
##  Stored in degorelick/CAP repository on GitHub
### -----------------------------------------------------

rm(list=ls()) # clear memory
setwd('C:/Users/dgorelic/OneDrive - University of North Carolina at Chapel Hill/UNC/Research/IM3/CAP/Data') # set directory
library(xlsx) # load package to read Excel files
library(tidyverse) # load packages for cleaning data

### -----------------------------------------------------
##  Read in annual financial data and clean it
CAP_annual_historical_finances_profit_loss = read.xlsx(file = "P&L_RateRecon history_PNNL request.xlsx", sheetName = "GF P&L")
CAP_annual_historical_finances_reconciliation = read.xlsx(file = "P&L_RateRecon history_PNNL request.xlsx", sheetName = "OMR Rec")

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

### -------------------------------------------------
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
# remove empty rows and columns, rename FY headers
CAP_annual_historical_finances_reconciliation = CAP_annual_historical_finances_reconciliation %>% 
  select(NA..1,X2011.Actual:X2020.Actual) %>% filter(!is.na(NA..1)) %>% 
  rename(Variable = NA..1,
         '2011' = X2011.Actual, '2012' = X2012.Actual, '2013' = X2013.Actual, '2014' = X2014.Actual, '2015' = X2015.Actual,
         '2016' = X2016.Actual, '2017' = X2017..Actual, '2018' = X2018.Actual, '2019' = X2019.Actual, '2020' = X2020.Actual) %>%
  mutate(Group = NA) %>% filter(row_number() != n())






