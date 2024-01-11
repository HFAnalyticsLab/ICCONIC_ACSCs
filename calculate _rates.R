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
count18plus4conds<-read_csv(here('data', 'age_grp_tab_plus18_5cond.csv'))
count65plus4conds<-read_csv(here('data', 'age_grp_tab_plus65_5cond.csv'))
pop<-read_csv(here('outputs', 'pop_calcs.csv'))
plus18<-read_csv(here('data', 'plus18tab.csv'))
plus65<-read_csv(here('data', 'plus65tab.csv'))
plus18_4cond<-read_csv(here('data', 'plus18tab5cond.csv'))
plus165_4cond<-read_csv(here('data', 'plus65tab5cond.csv'))

# Data clean and merge ----------------------------------------------------


dt <- count18plus %>% 
  select(-1) %>% 
  left_join(pop %>% 
              select(-1), by = c("type" = "quint_imd", "sex", "age_grp")) %>% 
  mutate(dep_type=ifelse(type %in% c("IMD", "income"), type, NA)) %>% 
  fill(dep_type, .direction="down") %>% 
  rename(quintile=type) %>% 
  left_join(count18plus4conds %>% 
              select(-1) %>% 
              rename(counts4conds=count) %>% 
              mutate(dep_type=ifelse(type %in% c("IMD", "income"), type, NA)) %>% 
              fill(dep_type, .direction="down") %>% 
              filter(!is.na(counts4conds)), by = c("quintile" = "type", "sex", "age_grp", "dep_type")) %>% 
  mutate(rate19=100*(count/pop19)) %>% 
  mutate(rate20=100*(count/pop20)) %>% 
  mutate(rate19_4conds=100*(counts4conds/pop19)) %>% 
  filter(!is.na(rate19)) 


dep_extract<-function(raw=plus18,cond4=TRUE) {
  if (cond4==TRUE) {
    raw<-raw[9:20,2:5]
  } else {
    raw<-raw[14:25,2:5]
  }
  
  raw<-raw %>% 
  mutate(dep_type=ifelse(type %in% c("IMD", "income"), type, NA)) %>% 
  fill(dep_type, .direction="down") %>% 
  filter(!(dep_type%in% c("IMD", "income")))

  return(raw)
}

plus18<-dep_extract(plus18, cond4=FALSE)
plus65<-dep_extract(plus65, cond4=FALSE)
plus18_4cond<-dep_extract(plus18_4cond, cond4=TRUE)
plus65_4cond<-dep_extract(plus165_4cond, cond4=TRUE)

dt_desc<-plus18_4cond %>% 
  mutate(conds="4conds", age_grp="18plus") %>% 
  full_join(plus65_4cond %>% 
              mutate(conds="4conds", age_grp="65plus")) %>% 
  full_join(plus18 %>% 
              mutate(conds="9conds", age_grp="18plus")) %>% 
  full_join(plus65 %>% 
              mutate(conds="9conds", age_grp="65plus")) %>% 
  left_join(pop %>% 
              select(quint_imd, age_grp, pop19), by = c("type" = "quint_imd", "age_grp")) %>% 
  mutate(rate=100*(count/pop19))


# data visualization  ------------------------------------------------------

colfun<-colorRampPalette(c('#53a9cd','#dd0031'))
pal<-colfun(5)

dt %>% 
  mutate(quintile=factor(quintile, levels=c(5:1), labels=c(1:5))) %>% 
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
  mutate(quintile=factor(quintile, levels=c(5:1), labels=c(1:5))) %>% 
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
  mutate(quintile=factor(quintile, levels=c(5:1), labels=c(1:5))) %>% 
  ggplot(aes(x=age_grp, y=rate19_4conds, fill=quintile, colour=quintile))+
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
  mutate(quintile=factor(quintile, levels=c(5:1), labels=c(1:5))) %>% 
  ggplot(aes(x=age_grp, y=rate19_4conds, fill=quintile, colour=quintile))+
  geom_bar(stat="identity", position="dodge") +
  facet_grid(cols=vars(sex), rows=vars(dep_type))+
  scale_fill_manual(values = pal) + 
  scale_colour_manual(values=pal)+
  labs(x="age group", y="rate of ACSCs admissions")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))

#look at the difference between the conditions

#IMD

dt %>% 
  filter(dep_type=="IMD") %>% 
  pivot_longer(contains("rate19"), names_to="conds_type", values_to="rate") %>% 
  mutate(conds_type=ifelse(conds_type=="rate19", "all ACSCs", "5 ACSCs")) %>% 
  mutate(quintile=factor(quintile, levels=c(5:1), labels=c(1:5))) %>% 
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
  mutate(conds_type=ifelse(conds_type=="rate19", "all ACSCs", "4 ACSCs")) %>% 
  mutate(quintile=factor(quintile, levels=c(5:1), labels=c(1:5))) %>% 
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
  mutate(quintile=factor(quintile, levels=c(5:1), labels=c(1:5))) %>% 
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
  mutate(quintile=factor(quintile, levels=c(5:1), labels=c(1:5))) %>% 
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
  mutate(sex=factor(sex, levels=c("Males", "Females"))) %>% 
  mutate(age_grp=factor(age_grp, levels=c("18-39","35-39", "40-44","45-49", "50-54", 
                                          "55-59", "60-64","65-69","70-74", "75-79", "80-84", "85-89","90+"))) %>% 
  select(dep_type, quintile, age_grp, sex, pop19, count, counts4conds) %>% 
  group_by(dep_type, quintile, age_grp) %>% 
  arrange(desc(sex))

  
write.csv(dt_tab,here('outputs', "counts.csv"))

write.csv(dt_desc, here('outputs', "rates_by_dep.csv"))






