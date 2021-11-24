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

test2 %>%
  ggplot(aes(noc_2016_1_digit_desc,pct,fill=gender))+
  geom_col()+
  facet_wrap(~age_group)+
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
  count(employment_goal_noc_1, age_group, gender, employment_goal_hours, employment_goal_wage) %>%
  group_by(employment_goal_noc_1) %>%
  mutate(pct = n / sum(n)) %>%
  left_join(NOC_ref_1, by = c("employment_goal_noc_1" = "noc_2016_1_digit"))


test2 %>%
  filter(!is.na(employment_goal_hours)) %>%
  filter(employment_goal_hours<168 & employment_goal_hours>0) %>%
  group_by(age_group) %>%
  summarize(avg_hours = mean(employment_goal_hours)) %>%
  ggplot(aes(age_group,avg_hours))+geom_col()

test2 %>%
  filter(!is.na(employment_goal_wage)) %>%
  group_by(age_group) %>%
  summarize(avg_wage = mean(employment_goal_wage)) %>%
  ggplot(aes(age_group,avg_wage))+geom_col()

eaps %>%
  ggplot(aes(employment_goal_hours))+geom_histogram()


####Differences in outcome working hours and working wage by Gender


test3 <- eaps %>%
  filter(!is.na(employment_goal_hours)) %>%
  filter(employment_goal_hours<168 & employment_goal_hours>0) %>%
  group_by(gender)


test3 %>%
  filter(!is.na(employment_goal_hours)) %>%
  group_by(gender) %>%
  summarize(avg_hours = mean(employment_goal_hours)) 

eaps %>%
  filter(!is.na(employment_goal_wage)) %>%
  filter(employment_goal_wage<100 & employment_goal_wage>0) %>%
  group_by(gender) %>%
  summarize(avg_wage = mean(employment_goal_wage))

## are these significant differences in hours?

test3 %>%
  group_by(gender) %>%
  summarize(avg_hours = mean(employment_goal_hours))

 test4 <- test3 %>%
   group_by(gender) %>%
   pivot_wider(names_from = gender,
               values_from = employment_goal_hours)
 
 var(test4$Male,na.rm = TRUE)
 
 var(test4$Female,na.rm = TRUE)
 
t.test(test4$Male,test4$Female, var.equal = F)
 
 ## are these significant differences in wages?
 
 test5 <- test3 %>%
   group_by(gender) %>%
   pivot_wider(names_from = gender,
               values_from = employment_goal_wage)
 
var(test5$Male,na.rm = TRUE)
 
var(test5$Female,na.rm = TRUE)
  
t.test(test5$Male,test5$Female, var.equal = F)


eaps %>%
  filter(!is.na(employment_goal_hours)) %>%
  group_by(gender) %>%
  summarize(avg_hours = mean(employment_goal_hours)) 



eaps %>%
  filter(!is.na(employment_goal_hours)) %>%
  filter(employment_goal_hours<100 & employment_goal_hours>0) %>%
  group_by(gender) %>%
  summarize(mean_wage = mean(employment_goal_hours))



########## number of plan items and their duration

eaps %>%
  ggplot(aes(plan_num))+geom_histogram()+scale_x_log10()


eaps %>%
  ggplot(aes(plan_total_cost))+geom_histogram()+scale_x_log10()


eaps %>%
  filter(!is.na(plan_total_duration)) %>%
  ggplot(aes(as.integer(plan_total_duration)))+geom_histogram(bins = 30)+scale_x_log10()

eaps %>%
  filter(plan_total_duration==1) %>%
  filter(!is.na(plan_total_duration)) %>%
  group_by(casereference, sub_EAS,sub_JFS,sub_RET,sub_SDM,sub_SDO,sub_SPS,sub_LST,sub_EFS) %>%
  count(plan_total_duration) %>%
  arrange(desc(plan_total_duration))


eaps %>%
  filter(plan_total_duration==1) %>%
  group_by(sub_EAS,sub_JFS,sub_RET,sub_SDM,sub_SDO,sub_SPS,sub_LST,sub_EFS) %>%
  count() %>%
  arrange(desc(n))

eaps %>%
  filter(!is.na(plan_total_duration)) %>%
  group_by(plan_total_duration,sub_EAS,sub_JFS,sub_RET,sub_SDM,sub_SDO,sub_SPS,sub_LST,sub_EFS) %>%
  count(plan_total_duration) %>%
  arrange(desc(n)) %>%
  pivot_longer(2:9,
               names_to = "subgoal",
               values_to = "count",values_drop_na = TRUE) %>%
  group_by(subgoal) %>%
  count() %>%
  adorn_totals()



eaps %>%
  group_by(plan_total_duration,sub_EAS,sub_JFS,sub_RET,sub_SDM,sub_SDO,sub_SPS,sub_LST,sub_EFS) %>%
  count()  %>%
  pivot_longer(2:9,
               names_to = "subgoal",
               values_to = "count",values_drop_na = TRUE) %>%
  group_by(subgoal) %>%
  summarize(cnt = n()) %>%
  mutate(pct = cnt/sum(cnt)) %>%
  ggplot(aes(reorder(subgoal,-pct),pct,fill=subgoal))+geom_col()



####Distribution of Total Plan Item Cost by Stream####

eaps %>%
  count(plan_total_cost)

eaps %>%
  filter(!is.na(plan_total_cost)) %>%
  filter(plan_total_cost>0) %>%
  group_by(stream, plan_total_cost) %>%
  count() %>%
  ggplot(aes(n,plan_total_cost))+geom_boxplot()+facet_wrap(~stream)+scale_y_log10()


eaps %>%
  filter(!is.na(plan_total_cost),
         !is.na(stream),
         plan_total_cost < quantile(plan_total_cost,  0.99, na.rm = TRUE)) %>%
  group_by(stream) %>%
  ggplot(aes(plan_total_cost,fill=stream))+
  geom_histogram(bins = 30)+
  scale_x_log10()+
  theme_minimal() +
  facet_wrap(~stream)+
  labs(title = "Distribution of Total Plan Item Cost by Stream",
       x = "Plan Item Cost",
       y = "Count",
       caption = "Analytics Unit | FASSB")


#percentage of total pan cost by stream

eaps %>%
  filter(!is.na(stream)) %>%
  filter(!is.na(plan_total_cost)) %>%
  group_by(stream) %>%
  summarize(total = sum(plan_total_cost)) %>%
  mutate(pct = total/sum(total))




# Relationship between Score and total cost

eaps %>%
  filter(!is.na(plan_total_cost),
         !is.na(stream),
         plan_total_cost>0,
         plan_total_cost < quantile(plan_total_cost,  0.99, na.rm = TRUE)) %>%
  group_by(score,plan_total_cost,stream) %>%
  summarize(total_cost = sum(plan_total_cost)) %>%
  ggplot(aes(total_cost,score))+geom_point() +
  geom_smooth(method = "lm")


# cost by plan items

eaps %>%
  group_by(plan_total_cost,sub_EAS,sub_JFS,sub_RET,sub_SDM,sub_SDO,sub_SPS,sub_LST,sub_EFS) %>%
  count()  %>%
  pivot_longer(2:9,
               names_to = "subgoal",
               values_to = "count",values_drop_na = TRUE) %>%
  group_by(subgoal) %>%
  summarize(cnt = n()) %>%
  ggplot(aes(reorder(subgoal,-cnt),cnt,fill=subgoal))+geom_col()







  






            