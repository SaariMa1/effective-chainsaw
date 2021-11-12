require(knitr)
require(tidyverse)
require(odbc)
require(dbplyr)
require(rstudioapi)
require(here)
require(janitor)
require(lubridate)
require(DT)
require(tsibble)
require(fable)


# Import data

eaps <- read_rds("O:/Analytics Unit/Projects/EST/Employment Action Plan/data/eaps.rds") # All EAPs

kpis <- read_rds("O:/Analytics Unit/Projects/EST/Employment Action Plan/data/kpis.rds") # All KPI targets/commitments

commits <- read_rds("O:/Analytics Unit/Projects/EST/Employment Action Plan/data/commits.rds") # Client


fymonth <- case_when(
  year(max(eaps$startdate)) == 2021 ~ month(max(eaps$startdate)) - 4,
  year(max(eaps$startdate)) == 2022 ~ month(max(eaps$startdate)) + 8)



eaps %>%
  count(fiscalyear, est_catchment_area) %>%
  left_join(commits %>%
              filter(fiscalyear %in% c("2020-21", "2021-22")) %>%
              group_by(fiscalyear, est_catchment_area) %>%
              summarise(target = sum(target, na.rm = TRUE))) %>%
  mutate(targ_ach = n / target,
         pro_rated = case_when( # pro-rating only applies to 2021-22 FY
           fiscalyear == "2020-21" ~ targ_ach,
           fiscalyear == "2021-22" ~ n / (target * (fymonth / 12))))


#Average Length of EAP

glimpse(eaps)

eaps %>%
  filter(!is.na(plan_total_duration)) %>%
  summarize(avg_length = mean(plan_total_duration))


# Average EAP Cost

eaps %>%
  filter(!is.na(plan_total_cost)) %>%
  summarize(avg_cost = mean(plan_total_cost))

eaps %>%
  filter(!is.na(plan_total_cost)) %>%
  filter(est_catchment_area=="Peel") %>%
  filter(plan_total_cost<10000 & plan_total_cost>1) %>%
  filter(fiscalyear=="2021-2022") %>%
  
  group_by(sds_reference_number) %>%
  summarize(total_cost = sum(plan_total_cost)) %>%
  ggplot(aes(reorder(sds_reference_number, -total_cost),total_cost))+
  geom_col()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##EAP starts in Peel

eaps %>%
  filter(est_catchment_area=="Peel") %>%
  filter(fiscalyear=="2021-22") %>%
  count(fiscalyear, sds_name, sds_reference_number) %>%
  mutate(pct = n/sum(n)) %>%
  arrange(desc(pct))


  
           



#####can a client have both an end date and an EAP outcome date? 


eaps %>%
  filter(!is.na(enddate)) %>%
  filter(!is.na(eapoutcomedate)) %>%
  group_by(closure_reason,outcome_emp_situation,enddate) %>%
  count(casereference,closure_reason,outcome_emp_situation,enddate,eapoutcomedate)




            