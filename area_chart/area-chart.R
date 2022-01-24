library(outbreakinfo)
library(tidyverse)
library(lubridate)
#devtools::install_github("outbreak-info/R-outbreak-info", ref="dev")

#GISAID reg
authenticateUser()
#(s9X4KdY

#area chart
us_prevalence <- getAllLineagesByLocation(location = "United States", other_threshold = 0.01, nday_threshold = 10, ndays = 100)
percentize <- function(x)(x * 100)

data_df <- us_prevalence %>% 
  as.data.frame() %>% 
  filter(ymd(date) >= ymd("2021-01-01")) %>%
  filter(ymd(date) <= ymd(today() - 13)) %>% 
  select(lineage, date, prevalence_rolling) %>%
  pivot_wider(names_from = lineage, values_from = prevalence_rolling) %>%
  mutate_if(is.numeric, percentize) %>%
  rowwise() %>%
  mutate(ay = sum(across(starts_with("ay")), na.rm = TRUE),
         delta = sum(ay, `b.1.617.2`, na.rm = TRUE)) %>%
  select(-starts_with("ay"), -`b.1.617.2`)

write_csv(data_df, "variants_area.csv")
