library(tidyverse)
library(readxl)
library(ggrepel)
library(ggthemes)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set current path as WD


### CREATING DATA SETS ###
pd_by_item <- read_csv("policedata.csv") %>%
  select('item' = `Commitment Items`, year, 'actual' = Actuals) %>%
  separate(item, c("code", "title"), sep = "  ") %>%
  filter(code != "REPORT TOTAL" & code != "REPORT") %>%
  inner_join(read_excel("data_mapping.xlsx"), by=c('code')) %>%
  select(item, mapping, year, actual) %>%
  group_by(item, year, mapping) %>%
  summarise(amount = sum(actual))

pd_by_mapping <- pd_by_item %>%
  group_by(mapping, year) %>%
  summarise(amount = sum(amount)) %>% 
  mutate(
    mapping = case_when(
      mapping == 'salaries' ~ 'Salaries',
      mapping == 'benefits' ~ 'Benefits',
      mapping == 'overtime' ~ 'Overtime',
      mapping == 'taxes' ~ 'Payroll taxes',
      mapping == 'career development' ~ 'Career development',
      mapping == 'bonus' ~ 'Bonuses'))

### SHOWING PERCENT GROWTH 2006 - 2020 ###
spread(pd_by_item, year, amount) %>%
  mutate(pct_growth = (`2020` - `2006`) / `2006`) %>%
  select(mapping, pct_growth, `2006`, `2020`) %>%
  arrange(-pct_growth)

### SHOWING GROWTH FOR EACH MAPPING ###
pd_by_mapping %>%
  ggplot(aes(year, amount, color=mapping, label=mapping)) +
  geom_jitter() + geom_smooth(se=F, method='lm') +
  geom_label_repel(data=pd_by_mapping %>% filter(year == 2020), aes(x=year, y=amount), nudge_x=1) +
  scale_y_continuous(labels=scales::dollar_format()) + theme_fivethirtyeight() +
  theme(legend.position = "none") + scale_color_economist() + xlab("Year") + ylab("") + ggtitle("Increases in Charlottesville Police Department\npersonnel expenditures, FY2006-2020") + 
  labs(caption = "Source: Charlottesville Police Department data")
  
ggsave('plots/CPD totals.png', dpi=300)

pd_by_item %>%
  filter(mapping == "benefits" & item %in% c('healthcare', 'life insurance', 'retirement - contribution', 'retirement')) %>%
  mutate(item = case_when(
    item == 'healthcare' ~ "healthcare",
    item == 'life insurance' ~ 'life insurance',
    item == 'retirement - contribution' ~ '401a matched retirement fund',
    item == 'retirement' ~ 'pension'
  )) %>%
  ggplot(aes(year, amount, color=item, label=item)) +
  geom_jitter() + geom_smooth(se=F, method='lm') +
  geom_label_repel(data=pd_by_item %>%
                     filter(mapping == "benefits" & item %in% c('healthcare', 'life insurance', 'retirement - contribution', 'retirement')) %>%
                     mutate(item = case_when(
                       item == 'healthcare' ~ "Healthcare",
                       item == 'life insurance' ~ 'Life insurance',
                       item == 'retirement - contribution' ~ '401a matched retirement fund',
                       item == 'retirement' ~ 'Pensions'
                     )) %>% filter(year == 2020), aes(x=year, y=amount), nudge_x = 3) +
  scale_y_continuous(labels=scales::dollar_format()) + theme_fivethirtyeight() +
  theme(legend.position = "none") + scale_color_economist() + xlab("Year") + ylab("") + ggtitle("Benefit expenditures of the Charlottesville\nPolice Department, FY2006-2020") + 
  labs(caption = "Source: Charlottesville Police Department data")
  
ggsave('plots/CPD benefits.png', dpi=300)

spread(pd_by_mapping, year, amount) %>%
  mutate(pct_growth = (`2020` - `2006`) / `2006`) %>%
  select(mapping, pct_growth, `2006`, `2020`) %>%
  arrange(-pct_growth) %>%
  ggplot(aes(reorder(mapping, -pct_growth), pct_growth)) + geom_bar(stat='identity') + theme_fivethirtyeight() + scale_y_continuous(labels=scales::label_percent(accuracy = 5L), breaks=seq(from=-1, to=1, by=.1)) +
  scale_x_discrete(labels=c("Benefits", "Bonuses", "Overtime", "Salaries", "Payroll taxes", "Career development")) + ylab("Percent Change") + ggtitle("Increase in Charlottesville Police Department\npersonnel spending by category, FY2006-2020") +
  labs(caption="Source: Charlottesville Police Department data")

ggsave('plots/pct growth by category.png', dpi=300)


spread(pd_by_mapping, year, amount) %>%
  mutate(total_growth = (`2020` - `2006`)) %>%
  select(mapping, total_growth, `2006`, `2020`) %>%
  arrange(-total_growth) %>%
  ggplot(aes(reorder(mapping, -total_growth), total_growth)) + geom_bar(stat='identity') + theme_fivethirtyeight() + scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_discrete(labels=c("Benefits", "Salaries", "Overtime", "Payroll taxes", "Bonuses", "Career development")) + ylab("Total Change") + ggtitle("Increase in Charlottesville Police Department\npersonnel spending by category, FY2006-2020") +
  labs(caption="Source: Charlottesville Police Department data")

ggsave('plots/total growth by category.png', dpi=300)

# pd_by_item %>%
#   mutate(pension = case_when(
#     item == 'retirement' ~ 'pension',
#     item != 'retirement' ~ 'everything else'
#   )) %>%
#   group_by(pension, year) %>%
#   summarise(amount = sum(amount)) %>%
#   ggplot(aes(year, amount, color=pension)) + geom_jitter() + geom_smooth(se=F, method="lm")

pd_by_item %>%
  group_by(year) %>%
  summarise(amount= sum(amount)) %>%
  ggplot(aes(year, amount)) + geom_bar(stat='identity') + theme_fivethirtyeight() + scale_y_continuous(labels=scales::dollar_format()) +
  ggtitle("Charlottesville Police Department personnel spending, FY2006-2020") + labs(caption="Source: Charlottesville Police Department data")

ggsave('plots/total personnel growth.png', dpi=300)

### This graph is not super informative. ###
pd_by_mapping %>%
  ggplot(aes(year, amount, fill=mapping)) + geom_bar(stat='identity')

pd_by_mapping %>%
  filter(mapping == "salaries") %>%
  group_by(year) %>%
  summarise(amount= sum(amount))


