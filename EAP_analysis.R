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
  group_by(ssm_name) %>%
  summarize(avg_length = mean(plan_total_duration)) %>%
  arrange(desc(-avg_length))


# Average EAP Cost

eaps %>%
  filter(!is.na(plan_total_cost)) %>%
  summarize(avg_cost = mean(plan_total_cost))

eaps %>%
  filter(!is.na(plan_total_cost)) %>%
  filter(est_catchment_area=="Peel") %>%
  group_by(ssm_name,sds_name) %>%
  summarize(total_cost = sum(plan_total_cost)) %>%
  arrange(desc(total_cost))


eaps %>%
  filter(est_catchment_area=="Peel") %>%
  count(fiscalyear,sds_name) %>%
  arrange(desc(n))



##EAP starts in Peel

eaps %>%
  filter(est_catchment_area=="Peel") %>%
  filter(fiscalyear=="2021-22") %>%
  count(fiscalyear, sds_name, sds_reference_number) %>%
  mutate(pct = n/sum(n)) %>%
  arrange(desc(pct))


  
           



#number of starts

eaps %>%
  filter(fiscalyear=="2021-22") %>%
  count(fiscalyear, est_catchment_area) %>%
  summarize(total = sum(n))

#End date grouped by closure reason

eaps %>%
  filter(!is.na(enddate)) %>%
  group_by(closure_reason) %>%
  count() %>%
  summarize(total = sum(n))

#EAP outcome date by Employment situation 87% of clients are employed at 20 hrs+ per week

eaps %>%
  filter(!is.na(eapoutcomedate)) %>%
  count(outcome_emp_situation, ) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(pct))


  



#####can a client have both an end date and an EAP outcome date? 

eaps %>%
  filter(!is.na(enddate)) %>%
  filter(!is.na(eapoutcomedate)) %>%
  group_by(enddate,eapoutcomedate) %>%
  count(enddate,eapoutcomedate)


eaps %>%
  filter(!is.na(enddate)) %>%
  filter(!is.na(eapoutcomedate)) %>%
  group_by(closure_reason,outcome_emp_situation,enddate) %>%
  count(enddate,eapoutcomedate, closure_reason,outcome_emp_situation)



###EXPLORE

eaps %>%
  filter(country_of_birth!="Canada") %>%
  filter(exitflag!="Pre-Employment Services") %>%
  filter(!is.na(exitflag)) %>%
  count(country_of_birth, exitflag) %>%
  group_by(country_of_birth) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(fill=exitflag, country_of_birth,pct))+
  geom_bar(position="fill", stat="identity")+
  coord_flip()+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
  





###employment goal outcome by gender  


test <- eaps %>%
  filter(!is.na(employment_goal_noc_1)) %>% 
  count(employment_goal_noc_1, gender, age) %>%
  group_by(employment_goal_noc_1) %>%
  mutate(pct = n / sum(n)) %>%
  left_join(NOC_ref_1, by = c("employment_goal_noc_1" = "noc_2016_1_digit")) 

test %>%
  ggplot(aes(noc_2016_1_digit_desc,pct,fill=gender))+
  geom_col()+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###employment goal outcome by age group


test2 <- eaps %>%
  filter(!is.na(age)) %>%
  filter(!is.na(employment_goal_noc_1)) %>%
  mutate(age_group = case_when(age>=0 & age<=14 ~ "0-14",
                               age>=15 & age<=24 ~ "15-24",
                               age>=25 & age<=34 ~ "25-34",
                               age>=35 & age<=44 ~ "35-44",
                               age>=45 & age<=54 ~ "45-54",
                               age>=55 & age<=64 ~ "55-64",
                               age>=65 ~ "65 and Over")) %>%
  count(employment_goal_noc_1, age_group, gender) %>%
  group_by(employment_goal_noc_1) %>%
  mutate(pct = n / sum(n)) %>%
  left_join(NOC_ref_1, by = c("employment_goal_noc_1" = "noc_2016_1_digit"))


test2 %>%
  ggplot(aes(noc_2016_1_digit_desc,pct,fill=gender))+
  geom_col()+
  facet_wrap(~age_group)+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


eaps %>%
  ggplot(aes(age))+geom_histogram(bins = 30)







            