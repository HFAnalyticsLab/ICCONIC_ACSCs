#Calculate rates

rm(list=ls())
#Library 
library(curl)
library(here)
library(readxl)
library(tidyverse)
library(janitor)
library(THFstyle)



# Download data -----------------------------------------------------------

count18plus<-read_csv(here('data', 'age_grp_tab_plus18.csv'))
count65plus<-read_csv(here('data', 'age_grp_tab_plus65.csv'))
count18plus5conds<-read_csv(here('data', 'age_grp_tab_plus18_5cond.csv'))
count65plus5conds<-read_csv(here('data', 'age_grp_tab_plus65_5cond.csv'))
pop<-read_csv(here('outputs', 'pop_calcs.csv'))
plus18<-read_csv(here('data', 'plus18tab.csv'))
plus65<-read_csv(here('data', 'plus65tab.csv'))
plus18_5cond<-read_csv(here('data', 'plus18tab5cond.csv'))
plus65_5cond<-read_csv(here('data', 'plus65tab5cond.csv'))
#plus18_old<-read_csv(here('data', 'plus18tab_old.csv'))

# Data clean and merge ----------------------------------------------------

#in raw data sets, 1 is most deprived and 5 is least deprived

dt <- count18plus %>% 
  select(-1) %>% 
  mutate(dep_type=ifelse(type %in% c("IMD", "income"), type, NA)) %>% 
  fill(dep_type, .direction="down") %>% 
  filter(!is.na(count)) %>% 
  left_join(count18plus5conds %>% 
              select(-1) %>% 
              rename(count5conds=count) %>% 
              mutate(dep_type=ifelse(type %in% c("IMD", "income"), type, NA)) %>% 
              fill(dep_type, .direction="down"),
            by = c("type", "sex", "age_grp", "dep_type")) %>% 
  #changing it so that 5 is the most deprived and 1 is the least deprived 
  #same with income, 5 is the lowest income and 1 is the highest income 
  mutate(type=case_when(type=="1"~ "5", 
                         type=="2"~ "4", 
                         type=="4"~"2", 
                         type=="5"~ "1", 
                         TRUE~ type)) %>%  
  left_join(pop %>% 
              select(quint_imd:plus65), by = c("type" = "quint_imd", "sex", "age_grp")) %>% 
  mutate(rate19=100*(count/pop19)) %>% 
  mutate(rate19_5conds=100*(count5conds/pop19)) %>% 
  filter(!is.na(rate19)) %>% 
  rename(quintile=type) %>% 
  #adding labels
  mutate(label=case_when(dep_type=="IMD"& quintile=="1"~"Q1-least deprived", 
                         dep_type=="IMD"& quintile=="2"~"Q2",
                         dep_type=="IMD"& quintile=="3"~"Q3", 
                         dep_type=="IMD"& quintile=="4"~"Q4", 
                         dep_type=="IMD"& quintile=="5"~"Q5-most deprived", 
                         dep_type!="IMD"& quintile=="1"~"Q1-Highest income", 
                         dep_type!="IMD"& quintile=="2"~"Q2",
                         dep_type!="IMD"& quintile=="3"~"Q3",
                         dep_type!="IMD"& quintile=="4"~"Q4",
                         dep_type!="IMD"& quintile=="5"~"Q5-Lowest income"))


dep_extract<-function(raw=plus18,cond5=TRUE) {
  #select the counts by deprivation (IMD and income)
  if (cond5==TRUE) {
    raw<-raw[10:21,2:5]
  } else {
    raw<-raw[14:25,2:5]
  }

  #reformat the values
  raw<-raw %>%
    mutate(type=case_when(type=="1"~ "5",
                          type=="2"~ "4",
                          type=="4"~ "2",
                          type=="5"~ "1",
                          TRUE~ type)) %>%
  #add a type so you know what the values mean 
  mutate(dep_type=ifelse(type %in% c("IMD", "income"), type, NA)) %>%
  fill(dep_type, .direction="down") %>%
  filter(!is.na(count))

  return(raw)
}

plus18<-dep_extract(plus18, cond5=FALSE)
plus65<-dep_extract(plus65, cond5=FALSE)
plus18_5cond<-dep_extract(plus18_5cond, cond5=TRUE)
plus65_5cond<-dep_extract(plus165_5cond, cond5=TRUE)


dt_desc<-plus18_5cond %>%
  mutate(conds="5conds", age_grp="18plus") %>%
  full_join(plus65_5cond %>%
              mutate(conds="5conds", age_grp="65plus")) %>%
  full_join(plus18 %>%
              mutate(conds="9conds", age_grp="18plus")) %>%
  full_join(plus65 %>%
              mutate(conds="9conds", age_grp="65plus")) %>%
  left_join(pop %>%
              select(quint_imd, age_grp, pop19), by = c("type" = "quint_imd", "age_grp")) %>%
  mutate(rate=100*(count/pop19)) %>% 
  rename(quintile=type) %>% 
  mutate(label=case_when(dep_type=="IMD"& quintile=="1"~"Q1-least deprived", 
                         dep_type=="IMD"& quintile=="2"~"Q2",
                         dep_type=="IMD"& quintile=="3"~"Q3", 
                         dep_type=="IMD"& quintile=="4"~"Q4", 
                         dep_type=="IMD"& quintile=="5"~"Q5-most deprived", 
                         dep_type!="IMD"& quintile=="1"~"Q1-Highest income", 
                         dep_type!="IMD"& quintile=="2"~"Q2",
                         dep_type!="IMD"& quintile=="3"~"Q3",
                         dep_type!="IMD"& quintile=="4"~"Q4",
                         dep_type!="IMD"& quintile=="5"~"Q5-Lowest income"))
  

# data visualization  ------------------------------------------------------

colfun<-colorRampPalette(c('#53a9cd','#dd0031'))
pal<-colfun(5)


dt %>% 
  ggplot(aes(x=age_grp, y=rate19, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(sex), rows=vars(dep_type))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))


#65+ 

dt %>% 
  filter(plus65==1) %>% 
  ggplot(aes(x=age_grp, y=rate19, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(sex), rows=vars(dep_type))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))


#5 conditions 
dt %>% 
  ggplot(aes(x=age_grp, y=rate19_5conds, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(sex), rows=vars(dep_type))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))


#65+ 

dt %>% 
  filter(plus65==1) %>% 
  ggplot(aes(x=age_grp, y=rate19_5conds, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(sex), rows=vars(dep_type))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))


#IMD

dt %>% 
  filter(dep_type=="IMD") %>% 
  pivot_longer(contains("rate19"), names_to="conds_type", values_to="rate") %>% 
  mutate(conds_type=ifelse(conds_type=="rate19", "all ACSCs", "5 ACSCs")) %>% 
  ggplot(aes(x=age_grp, y=rate, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(sex), rows=vars(conds_type))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))


dt %>% 
  filter(plus65==1) %>% 
  filter(dep_type=="IMD") %>% 
  pivot_longer(contains("rate19"), names_to="conds_type", values_to="rate") %>% 
  mutate(conds_type=ifelse(conds_type=="rate19", "all ACSCs", "5 ACSCs")) %>% 
  ggplot(aes(x=age_grp, y=rate, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(sex), rows=vars(conds_type))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))

#Income
dt %>% 
  filter(dep_type=="income") %>% 
  pivot_longer(contains("rate19"), names_to="conds_type", values_to="rate") %>% 
  mutate(conds_type=ifelse(conds_type=="rate19", "all ACSCs", "5 ACSCs")) %>% 
  ggplot(aes(x=age_grp, y=rate, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(sex), rows=vars(conds_type))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))


dt %>% 
  filter(plus65==1) %>% 
  filter(dep_type=="income") %>% 
  pivot_longer(contains("rate19"), names_to="conds_type", values_to="rate") %>% 
  mutate(conds_type=ifelse(conds_type=="rate19", "all ACSCs", "5 ACSCs")) %>% 
  ggplot(aes(x=age_grp, y=rate, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(sex), rows=vars(conds_type))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))



# Table shells ------------------------------------------------------------

dt_tab<-dt %>% 
  mutate(sex=factor(sex, levels=c("Males", "Females"), labels=c("M", "F"))) %>% 
  mutate(age_grp=factor(age_grp, levels=c("18-39","35-39", "40-44","45-49", "50-54", 
                                          "55-59", "60-64","65-69","70-74", "75-79", "80-84", "85-89","90+"))) %>% 
  select(dep_type, quintile, age_grp, sex, pop19, count, count5conds) %>% 
  group_by(dep_type, quintile, age_grp) %>%
  arrange((sex)) %>% 
  mutate(label=case_when(dep_type=="IMD"& quintile=="1"~"Q1-least deprived", 
                         dep_type=="IMD"& quintile=="2"~"Q2",
                         dep_type=="IMD"& quintile=="3"~"Q3", 
                         dep_type=="IMD"& quintile=="4"~"Q4", 
                         dep_type=="IMD"& quintile=="5"~"Q5-most deprived", 
                         dep_type!="IMD"& quintile=="1"~"Q1-Highest income", 
                         dep_type!="IMD"& quintile=="2"~"Q2",
                         dep_type!="IMD"& quintile=="3"~"Q3",
                         dep_type!="IMD"& quintile=="4"~"Q4",
                         dep_type!="IMD"& quintile=="5"~"Q5-Lowest income"))

  
write.csv(dt_tab,here('outputs', "counts.csv"))

write.csv(dt_desc, here('outputs', "rates_by_dep.csv"))




# ACSC england codes--------------------------------------------------

count18plus_eng<-read_csv(here('data', 'age_grp_tab_plus18_england.csv'))
#count65plus_eng<-read_csv(here('data', 'age_grp_tab_plus65_england.csv'))


dt_eng <- count18plus_eng %>% 
  select(-1) %>% 
  mutate(type=case_when(type=="1"~ "5", 
                        type=="2"~ "4", 
                        type=="4"~ "2", 
                        type=="5"~ "1", 
                        TRUE~ type)) %>% 
  left_join(pop %>% 
              select(-1), by = c("type" = "quint_imd", "sex", "age_grp")) %>% 
  mutate(dep_type=ifelse(type %in% c("IMD", "income"), type, NA)) %>% 
  fill(dep_type, .direction="down") %>% 
  rename(quintile=type) %>% 
  mutate(rate19=100*(count/pop19)) %>% 
  filter(!is.na(rate19)) 

dt_eng %>% 
  #mutate(quintile=factor(quintile, levels=c(5:1), labels=c(1:5))) %>% 
  ggplot(aes(x=age_grp, y=rate19, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(sex), rows=vars(dep_type))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))



#LOS -------------------------------------------------------------------

count18plus_zero_los<-read_csv(here('data', 'age_grp_tab_plus18_zero_los.csv'))

dt_zero_los <- count18plus_zero_los %>% 
  select(-1) %>% 
  mutate(type=case_when(type=="1"~ "5", 
                        type=="2"~ "4", 
                        type=="4"~ "2", 
                        type=="5"~ "1", 
                        TRUE~ type)) %>% 
  select(-sdc) %>% 
  left_join(pop %>% 
              select(-1), by = c("type" = "quint_imd", "sex", "age_grp")) %>% 
  mutate(dep_type=ifelse(type %in% c("IMD", "income"), type, NA)) %>% 
  fill(dep_type, .direction="down") %>% 
  rename(quintile=type) %>% 
  mutate(rate19=100*(count/pop19)) %>% 
  filter(!is.na(rate19)) 

#same pattern as without excluding the 0 day LOS 
dt_zero_los %>% 
  filter(zero_los==0) %>% 
  ggplot(aes(x=age_grp, y=rate19, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(sex), rows=vars(dep_type))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))


dt_zero_los %>% 
  filter(zero_los==1) %>% 
  ggplot(aes(x=age_grp, y=rate19, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(sex), rows=vars(dep_type))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))



# Individual conditions ---------------------------------------------------

count18plus_acsc<-read_csv(here('data', 'age_grp_tab_plus18_acsc.csv'))

dt_acsc <- count18plus_acsc %>% 
  select(-1) %>% 
  mutate(type=case_when(type=="1"~ "5", 
                        type=="2"~ "4", 
                        type=="4"~ "2", 
                        type=="5"~ "1", 
                        TRUE~ type)) %>% 
  select(-sdc) %>% 
  left_join(pop %>% 
              select(-1), by = c("type" = "quint_imd", "sex", "age_grp")) %>% 
  mutate(dep_type=ifelse(type %in% c("IMD", "income"), type, NA)) %>% 
  fill(dep_type, .direction="down") %>% 
  rename(quintile=type) %>% 
  mutate(rate19=100*(count/pop19)) %>% 
  filter(!is.na(rate19)) 

dt_acsc %>% 
  filter(dep_type=="IMD") %>% 
  ggplot(aes(x=age_grp, y=rate19, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(acsc_group), rows=vars(sex))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))

