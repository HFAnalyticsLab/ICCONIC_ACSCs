#Pop estimates by age, sex and imd 
rm(list=ls())
#Library 
library(curl)
library(janitor)
library(here)
library(readxl)
library(tidyverse)




# Data download -----------------------------------------------------------

#2019 
link<-'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12413deathregistrationsandpopulationsbyindexofmultipledeprivationimddecileenglandandwales2019/deathsandpopsbyimd2019final.xlsx'

destfile <- here::here('data', "popestimates.xlsx")
curl_download(link, destfile = destfile)



#2019 
pop<-readxl::read_excel("data/popestimates.xlsx", sheet='Table 3 - England populations')

#2019
pop_clean<-pop %>% 
  clean_names() %>% 
  slice(which(x2=="Deprivation Decile (IMD 2019)"):n()) %>% 
  row_to_names(., 1) %>% 
  clean_names() %>% 
  fill(na) %>% 
  filter(na %in% c("Males", "Females")) %>% 
  filter(!is.na(deprivation_decile_imd_2019)) %>% 
  mutate(x90= as.numeric(gsub("\"", "", x90))) %>% 
  pivot_longer(cols=contains("x"), names_to="age", values_to="pop") %>% 
  rename(sex=na) %>%
  mutate(age=as.numeric(gsub("x", "",age))) %>% 
  #1=most deprived, 10=least deprived
  mutate(quint_imd=case_when (deprivation_decile_imd_2019 %in% 1:2 ~ "1", 
                              deprivation_decile_imd_2019 %in% 3:4 ~ "2", 
                              deprivation_decile_imd_2019 %in% 5:6 ~ "3", 
                              deprivation_decile_imd_2019 %in% 7:8 ~ "4", 
                              deprivation_decile_imd_2019 %in% 9:10 ~ "5")) %>% 
  mutate(plus18=ifelse(age>=18,1,0), 
         plus65=ifelse(age>=65,1,0))


#work out mean age, sd, and median

calcs<-pop_clean %>% 
  filter(age>=18) %>% 
  group_by(age) %>% 
  summarise(count=sum(pop))

# Calculate weighted mean age
weighted_mean_age<-weighted.mean(calcs$age, calcs$count)

#mean(rep(calcs$age, times=calcs$count))

# Calculate weighted standard deviation (SD) age
weighted_sd_age <- sqrt(weighted.mean(calcs$age ^ 2, w=calcs$count) - weighted.mean(calcs$age, w=calcs$count) ^ 2)

# Find median age
median_age <- median(rep(calcs$age, times=calcs$count))


# Print the results
print(paste("Mean Age:", weighted_mean_age))
print(paste("Standard Deviation Age:", weighted_sd_age))
print(paste("Median Age:", median_age))

#for65s

calcs65<-pop_clean %>% 
  group_by(age) %>% 
  summarise(count=sum(pop)) %>% 
  filter(age>=65)

# Calculate weighted mean age
weighted_mean_age65<-weighted.mean(calcs65$age, calcs65$count)

#mean(rep(calcs65$age, times=calcs65$count))

# Calculate weighted standard deviation (SD) age
weighted_sd_age65 <- sqrt(weighted.mean(calcs65$age ^ 2, w=calcs65$count) - weighted.mean(calcs65$age, w=calcs65$count) ^ 2)

# Find median age
median_age65 <- median(rep(calcs65$age, times=calcs65$count))


# Print the results
print(paste("Mean Age:", weighted_mean_age65))
print(paste("Standard Deviation Age:", weighted_sd_age65))
print(paste("Median Age:", median_age65))

#proportion of females

pop_clean %>% 
  filter(plus18==1) %>% 
  group_by(sex) %>% 
  summarise(count=sum(pop)) %>% 
  mutate(total=sum(count)) %>% 
  mutate(prop=count/total)

pop_clean %>% 
  filter(plus65==1) %>% 
  group_by(sex) %>% 
  summarise(count=sum(pop)) %>% 
  mutate(total=sum(count)) %>% 
  mutate(prop=count/total)

# By age group, sex, deprivation --------------------------------------------------

pop_clean<-pop_clean %>% 
  #filter 18plus??- check this 
  mutate(age_grp=case_when(age %in% 18:39~ "18-39", 
                               age %in% 35:39~ "35-39", 
                               age %in% 40:44~ "40-44",
                               age %in% 45:49~ "45-49", 
                               age %in% 50:54~ "50-54",
                               age %in% 55:59~ "55-59", 
                               age %in% 60:64~ "60-64",
                               age %in% 65:69~ "65-69", 
                               age %in% 70:74~ "70-74",
                               age %in% 75:79~ "75-79", 
                               age %in% 80:84~ "80-84",
                               age %in% 85:89~ "85-89", 
                               age>=90~ "90+"))


age_grp_pop_19<-pop_clean %>% 
group_by(quint_imd, sex, age_grp) %>% 
  summarise(count=sum(pop)) %>% 
  rows_insert(tibble(quint_imd="Overall_IMD_18plus")) %>% 
  full_join(pop_clean %>% 
              filter(plus18==1) %>% 
              group_by(quint_imd) %>% 
              summarise(count=sum(pop)) %>% 
              mutate(sex="Overall", age_grp="18plus")) %>% 
  rows_insert(tibble(quint_imd="Overall_IMD_65plus")) %>% 
  full_join(pop_clean %>% 
              filter(plus65==1) %>% 
              group_by(quint_imd) %>% 
              summarise(count=sum(pop)) %>% 
              mutate(sex="Overall", age_grp="65plus")) %>% 
  rows_insert(tibble(quint_imd="Overall_pop")) %>% 
  full_join(pop_clean %>% 
              summarise(count=sum(pop)) %>% 
              mutate(sex="Overall", age_grp="Overall", quint_imd="Overall")) %>% 
  full_join(pop_clean %>% 
              filter(plus18==1) %>% 
              summarise(count=sum(pop)) %>% 
              mutate(sex="Overall", age_grp="18plus", quint_imd="Overall")) %>% 
  full_join(pop_clean %>% 
              filter(plus65==1) %>% 
              summarise(count=sum(pop)) %>% 
              mutate(sex="Overall", age_grp="65plus", quint_imd="Overall")) %>% 
  mutate(plus65=ifelse(age_grp %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90+", "65plus"),1,0)) %>% 
  rename(pop19=count)

  

age_grp_pop_19





# 2020 --------------------------------------------------------------------

#2020
link<- 'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/13773populationsbyindexofmultipledeprivationimddecileenglandandwales2020/populationbyimdenglandandwales2020.xlsx'

destfile <- here::here('data', "popestimates.xlsx")
curl_download(link, destfile = destfile)


#2020
pop<-readxl::read_excel("data/popestimates.xlsx", sheet='Table 1 - England')

#2020
pop_clean<-pop %>% 
  clean_names() %>% 
  slice(which(x2=="Deprivation Decile (IMD 2020)"):n()) %>% 
  row_to_names(., 1) %>% 
  clean_names() %>% 
  fill(na) %>% 
  filter(na %in% c("Males", "Females")) %>% 
  filter(!is.na(deprivation_decile_imd_2020)) %>% 
  mutate(x90= as.numeric(gsub("\"", "", x90))) %>% 
  pivot_longer(cols=contains("x"), names_to="age", values_to="pop") %>% 
  rename(sex=na) %>%
  mutate(age=as.numeric(gsub("x", "",age))) %>% 
  mutate(quint_imd=case_when (deprivation_decile_imd_2020 %in% 1:2 ~ "5", 
                              deprivation_decile_imd_2020 %in% 3:4 ~ "4", 
                              deprivation_decile_imd_2020 %in% 5:6 ~ "3", 
                              deprivation_decile_imd_2020 %in% 7:8 ~ "2", 
                              deprivation_decile_imd_2020 %in% 9:10 ~ "1")) %>% 
  mutate(plus18=ifelse(age>=18,1,0), 
         plus65=ifelse(age>=65,1,0))



pop_clean<-pop_clean %>% 
  #filter 18plus??- check this 
  mutate(age_grp=case_when(age %in% 18:39~ "18-39", 
                           age %in% 40:44~ "40-44",
                           age %in% 45:49~ "45-49", 
                           age %in% 50:54~ "50-54",
                           age %in% 55:59~ "55-59", 
                           age %in% 60:64~ "60-64",
                           age %in% 65:69~ "65-69", 
                           age %in% 70:74~ "70-74",
                           age %in% 75:79~ "75-79", 
                           age %in% 80:84~ "80-84",
                           age %in% 85:89~ "85-89", 
                           age>=90~ "90+"))


age_grp_pop_20<-pop_clean %>% 
  group_by(quint_imd, sex, age_grp) %>% 
  summarise(count=sum(pop)) %>% 
  rows_insert(tibble(quint_imd="Overall_IMD_18plus")) %>% 
  full_join(pop_clean %>% 
              filter(plus18==1) %>% 
              group_by(quint_imd) %>% 
              summarise(count=sum(pop)) %>% 
              mutate(sex="Overall", age_grp="18plus")) %>% 
  rows_insert(tibble(quint_imd="Overall_IMD_65plus")) %>% 
  full_join(pop_clean %>% 
              filter(plus65==1) %>% 
              group_by(quint_imd) %>% 
              summarise(count=sum(pop)) %>% 
              mutate(sex="Overall", age_grp="65plus")) %>% 
  rows_insert(tibble(quint_imd="Overall_pop")) %>% 
  full_join(pop_clean %>% 
              summarise(count=sum(pop)) %>% 
              mutate(sex="Overall", age_grp="Overall", quint_imd="Overall")) %>% 
  full_join(pop_clean %>% 
              filter(plus18==1) %>% 
              summarise(count=sum(pop)) %>% 
              mutate(sex="Overall", age_grp="18plus", quint_imd="Overall")) %>% 
  full_join(pop_clean %>% 
              filter(plus65==1) %>% 
              summarise(count=sum(pop)) %>% 
              mutate(sex="Overall", age_grp="65plus", quint_imd="Overall")) %>% 
  mutate(plus65=ifelse(age_grp %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90+", "65plus"),1,0)) %>% 
  rename(pop20=count)


age_grp_pop<-age_grp_pop_19 %>% 
  left_join(age_grp_pop_20)

write.csv(age_grp_pop,here('outputs', "pop_calcs.csv"))





