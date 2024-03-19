#ASCS descriptive
#Anne Alarilla 21/11/23 
rm(list=ls())

library(here)
library(dplyr)
library(aws.s3)
library(gtsummary)
library(janitor)

source(here('AWS_S3_functions.R'))

dt<-bread(buck, '/ACSC/acsc_linked_2019.RDS')
#dt_england<-bread(buck, '/ACSC/acsc_linked_2019_england.RDS')

glimpse(dt)
summary(dt)


#excl codes 
asthma_code<-c("E84", "P27", "Q254", "Q31", "Q32", "Q33", "Q34", "Q39", "Q893")
pneumonia_code<-c("D57", "B20", "B59", "C802", "C888", "C944", "D462", "D47", "D610", "D618", 
                  "D70", "D71", "D720", "D728", "D738", "D76", "D80", "D81", "D82", "D83", "D84", 
                  "D893", "D898", "D899","E40", "E41", "E42", "E43", "I120", "I131", "I132", 
                  "K912", "N185", "T86", "Z482", "Z49", "Z94", "Z922")
uti_code<-c("N11", "N130", "N136", "N137", "N139", "Q60", "Q61", "Q63", "Q64")


id_str <- function(data, find, cols = 'DIAG_'){
  data_frame <- data %>% select(starts_with(cols)) 
    apply(data_frame, 1, function(t) {
      substr_t4<-substr(t,1,4)
      substr_t3<-substr(t,1,3)
      ifelse(any(substr_t4 %in% find[nchar(find)==4]), 1, ifelse(any(substr_t3 %in% find[nchar(find)==3]),1,0)) 
    })
    
}


dt$asthma_excl_flag<-id_str(dt, find=asthma_code, cols='DIAG_')
dt$pneumonia_excl_flag<-id_str(dt, find=pneumonia_code, cols='DIAG_')
dt$uti_excl_flag<-id_str(dt, find=uti_code, cols='DIAG_')

dt<-dt %>% 
  mutate(excl_flag=case_when(ACSC_group=="asthma"& asthma_excl_flag==1 ~ 1, 
                             ACSC_group=="pneumonia"& pneumonia_excl_flag==1~1, 
                             ACSC_group=="uti"& uti_excl_flag==1~1, 
                             TRUE~0))

tabyl(as.data.frame(dt)$excl_flag)



dt<-dt %>% 
  filter(excl_flag==0) %>% 
  mutate(missing_imd=is.na(IMDdec)) %>% 
  mutate(missing_income=is.na(Incomedec))

dt %>% 
  select(contains("missing")) %>% 
  tbl_summary()

#no missing imd or income 

#from this 1=most deprived, 10-least deprived
dt<-dt %>% 
  mutate(ACSC_group=ifelse(ACSC_group=="anigna", "angina", ifelse(ACSC_group=="hypetension", "hypertension", ACSC_group))) %>% 
  mutate(imdquint=as.factor(case_when(IMDdec %in% (1:2)~"1", 
                            IMDdec %in% (3:4)~"2", 
                            IMDdec %in% (5:6)~"3", 
                            IMDdec %in% (7:8)~"4", 
                            IMDdec %in% (9:10)~"5"))) %>% 
  mutate(Incomequint=as.factor(case_when(Incomedec %in% (1:2)~"1", 
                              Incomedec %in% (3:4)~"2", 
                              Incomedec %in% (5:6)~"3", 
                              Incomedec %in% (7:8)~"4", 
                              Incomedec %in% (9:10)~"5"))) %>% 
  mutate(plus18=ifelse(age>=18,1,0), #think the sample should only be 18+ so check this 
         plus65=ifelse(age>=65,1,0)) %>% 
  clean_names() %>% 
  mutate(sex=factor(sex, levels=c(1:2), labels=c("Males", "Females"))) %>%  #check that 1 is male and 2 is female
  mutate(dx5cond=ifelse(acsc_group %in% c("asthma","CHF", "COPD", "diabetes", "hypertension"),1,0)) 
  
glimpse(dt)




dt %>% 
  filter(plus18==1) %>% 
  select(acsc_group,imdquint, incomequint, age, sex, dx5cond) %>% 
  tbl_summary()

#Age, sex, and ACSC conditions 

plus18tab<-tibble(type="ACSC group (count, sum, prop)",count=NA) %>% 
  full_join(dt %>% 
   filter(plus18==1) %>%
              group_by(acsc_group) %>%
              summarise(count=n()) %>%
            rename(type=acsc_group) %>% 
             mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>%
  rows_insert(tibble(type="Sex (count, sum, prop)",count=NA)) %>% 
  full_join(dt %>%
              filter(plus18==1) %>%
              mutate(sex=as.character(sex)) %>%
              group_by(sex) %>%
              summarise(count=n()) %>%
              rename(type=sex) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>% 
  rows_insert(tibble(type="IMD",count=NA)) %>% 
  full_join(dt %>%
              filter(plus18==1) %>%
              group_by(imdquint) %>%
              summarise(count=n()) %>%
              rename(type=imdquint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="income",count=NA)) %>% 
  full_join(dt %>%
              filter(plus18==1) %>%
              group_by(incomequint) %>%
              summarise(count=n()) %>%
              rename(type=incomequint) %>% 
               mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="age (mean,sd,median)",count=NA, sum=NA, stat=NA)) %>% 
  full_join(dt %>%
              filter(plus18==1) %>%
              select(type=age) %>% 
              summarise(count=round(mean(type),2), 
                        sum=round(sd(type),2), 
                        stat=median(type)))
  
plus18tab

write.csv(plus18tab, here('results', 'plus18tab.csv'))

#65plus

plus65tab<-tibble(type="ACSC group (count, sum, prop)",count=NA) %>% 
  full_join(dt %>% 
              filter(plus65==1) %>%
              group_by(acsc_group) %>%
              summarise(count=n()) %>%
              rename(type=acsc_group) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>%
  rows_insert(tibble(type="Sex (count, sum, prop)",count=NA)) %>% 
  full_join(dt %>%
              filter(plus65==1) %>%
              mutate(sex=as.character(sex)) %>%
              group_by(sex) %>%
              summarise(count=n()) %>%
              rename(type=sex) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>% 
  rows_insert(tibble(type="IMD",count=NA)) %>% 
  full_join(dt %>%
              filter(plus65==1) %>%
              group_by(imdquint) %>%
              summarise(count=n()) %>%
              rename(type=imdquint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="income",count=NA)) %>% 
  full_join(dt %>%
              filter(plus65==1) %>%
              group_by(incomequint) %>%
              summarise(count=n()) %>%
              rename(type=incomequint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="age (mean,sd,median)",count=NA, sum=NA, stat=NA)) %>% 
  full_join(dt %>%
              filter(plus65==1) %>%
              select(type=age) %>% 
              summarise(count=round(mean(type),2), 
                        sum=round(sd(type),2), 
                        stat=median(type)))



plus65tab

write.csv(plus65tab, here('results', 'plus65tab.csv'))



# Further breakdown by age ------------------------------------------------

dt<-dt %>% 
  filter(age!=0) %>% #filter out those with missing age 
  mutate(age_grp=case_when(age %in% 18:39~"18-39", 
                           age %in% 40:44~"40-44",
                           age %in% 45:49~"45-49", 
                           age %in% 50:54~"50-54",
                           age %in% 55:59~"55-59", 
                           age %in% 60:64~"60-64",
                           age %in% 65:69~"65-69", 
                           age %in% 70:74~"70-74", 
                           age %in% 75:79~"75-79", 
                           age %in% 80:84~"80-84", 
                           age %in% 85:89~"85-89", 
                           age >=90 ~"90+"))


age_grp_tab_plus18<-tibble(type="IMD", sex=NA,age_grp=NA, count=NA) %>% 
  full_join(dt %>% 
     filter(plus18==1) %>%
    group_by(imdquint,sex, age_grp) %>% 
    summarise(count=n()) %>%
    rename(type=imdquint)) %>% 
  rows_insert(tibble(type="income",sex=NA,age_grp=NA, count=NA)) %>% 
  full_join(dt %>%
              filter(plus18==1) %>%
              group_by(incomequint,sex, age_grp) %>%
              summarise(count=n()) %>%
              rename(type=incomequint)) 
   
age_grp_tab_plus18

write.csv(age_grp_tab_plus18, here('results', 'age_grp_tab_plus18.csv'))


age_grp_tab_plus65<-tibble(type="IMD", sex=NA,age_grp=NA, count=NA) %>% 
  full_join(dt %>% 
              filter(plus65==1) %>%
              group_by(imdquint,sex, age_grp) %>% 
              summarise(count=n()) %>%
              rename(type=imdquint)) %>% 
  rows_insert(tibble(type="income",sex=NA,age_grp=NA, count=NA)) %>% 
  full_join(dt %>%
              filter(plus65==1) %>%
              group_by(incomequint,sex, age_grp) %>%
              summarise(count=n()) %>%
              rename(type=incomequint)) 


write.csv(age_grp_tab_plus65, here('results', 'age_grp_tab_plus65.csv'))


# 5 conditions ------------------------------------------------------------

#Age, sex, and ACSC conditions 

plus18tab5cond<-tibble(type="ACSC group (count, sum, prop)",count=NA) %>% 
  full_join(dt %>% 
              filter(plus18==1) %>%
              filter(dx5cond==1) %>% 
              group_by(acsc_group) %>%
              summarise(count=n()) %>%
              rename(type=acsc_group) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>%
  rows_insert(tibble(type="Sex (count, sum, prop)",count=NA)) %>% 
  full_join(dt %>%
              filter(plus18==1) %>%
              filter(dx5cond==1) %>% 
              mutate(sex=as.character(sex)) %>%
              group_by(sex) %>%
              summarise(count=n()) %>%
              rename(type=sex) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>% 
  rows_insert(tibble(type="IMD",count=NA)) %>% 
  full_join(dt %>%
              filter(plus18==1) %>%
              filter(dx5cond==1) %>% 
              group_by(imdquint) %>%
              summarise(count=n()) %>%
              rename(type=imdquint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="income",count=NA)) %>% 
  full_join(dt %>%
              filter(plus18==1) %>%
              filter(dx5cond==1) %>% 
              group_by(incomequint) %>%
              summarise(count=n()) %>%
              rename(type=incomequint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="age (mean,sd,median)",count=NA, sum=NA, stat=NA)) %>% 
  full_join(dt %>%
              filter(plus18==1) %>%
              filter(dx5cond==1) %>% 
              select(type=age) %>% 
              summarise(count=round(mean(type),2), 
                        sum=round(sd(type),2), 
                        stat=median(type)))

plus18tab5cond

write.csv(plus18tab5cond, here('results', 'plus18tab5cond.csv'))

#65plus

plus65tab5cond<-tibble(type="ACSC group (count, sum, prop)",count=NA) %>% 
  full_join(dt %>% 
              filter(plus65==1) %>%
              filter(dx5cond==1) %>% 
              group_by(acsc_group) %>%
              summarise(count=n()) %>%
              rename(type=acsc_group) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>%
  rows_insert(tibble(type="Sex (count, sum, prop)",count=NA)) %>% 
  full_join(dt %>%
              filter(plus65==1) %>%
              filter(dx5cond==1) %>% 
              mutate(sex=as.character(sex)) %>%
              group_by(sex) %>%
              summarise(count=n()) %>%
              rename(type=sex) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>% 
  rows_insert(tibble(type="IMD",count=NA)) %>% 
  full_join(dt %>%
              filter(plus65==1) %>%
              filter(dx5cond==1) %>% 
              group_by(imdquint) %>%
              summarise(count=n()) %>%
              rename(type=imdquint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="income",count=NA)) %>% 
  full_join(dt %>%
              filter(plus65==1) %>%
              filter(dx5cond==1) %>% 
              group_by(incomequint) %>%
              summarise(count=n()) %>%
              rename(type=incomequint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="age (mean,sd,median)",count=NA, sum=NA, stat=NA)) %>% 
  full_join(dt %>%
              filter(plus65==1) %>%
              filter(dx5cond==1) %>% 
              select(type=age) %>% 
              summarise(count=round(mean(type),2), 
                        sum=round(sd(type),2), 
                        stat=median(type)))



plus65tab5cond

write.csv(plus65tab5cond, here('results', 'plus65tab5cond.csv'))



# Further breakdown by age ------------------------------------------------

age_grp_tab_plus18_5cond<-tibble(type="IMD", sex=NA,age_grp=NA, count=NA) %>% 
  bind_rows(dt %>% 
              filter(plus18==1) %>%
              filter(dx5cond==1) %>% 
              group_by(imdquint,sex, age_grp) %>% 
              summarise(count=n()) %>%
              rename(type=imdquint)) %>% 
  rows_insert(tibble(type="income",sex=NA,age_grp=NA, count=NA)) %>% 
  bind_rows(dt %>%
              filter(plus18==1) %>%
              filter(dx5cond==1) %>% 
              group_by(incomequint,sex, age_grp) %>%
              summarise(count=n()) %>%
              rename(type=incomequint)) 
  

  write.csv(age_grp_tab_plus18_5cond, here('results', 'age_grp_tab_plus18_5cond.csv'))


age_grp_tab_plus65_5cond<-tibble(type="IMD", sex=NA,age_grp=NA, count=NA) %>% 
  bind_rows(dt %>% 
              filter(plus65==1) %>%
              filter(dx5cond==1) %>% 
              group_by(imdquint,sex, age_grp) %>% 
              summarise(count=n()) %>%
              rename(type=imdquint)) %>% 
  rows_insert(tibble(type="income",sex=NA,age_grp=NA, count=NA)) %>% 
  bind_rows(dt %>%
              filter(plus65==1) %>%
              filter(dx5cond==1) %>% 
              group_by(incomequint,sex, age_grp) %>%
              summarise(count=n()) %>%
              rename(type=incomequint)) 


write.csv(age_grp_tab_plus65_5cond, here('results', 'age_grp_tab_plus65_5cond.csv'))


# release outputs -----------------------------------------------

#save outputs in results bucket 
results_buck<-bget("hosdis78", type_in="r")

results_folder<-paste0(results_buck, '/24_01_24')

bwrite(plus18tab, results_folder, 'plus18tab.csv')
bwrite(plus65tab, results_folder, 'plus65tab.csv')
bwrite(age_grp_tab_plus18, results_folder, 'age_grp_tab_plus18.csv')
bwrite(age_grp_tab_plus65, results_folder, 'age_grp_tab_plus65.csv')

bwrite(plus18tab5cond, results_folder, 'plus18tab5cond.csv')
bwrite(plus65tab5cond, results_folder, 'plus65tab5cond.csv')
bwrite(age_grp_tab_plus18_5cond, results_folder, 'age_grp_tab_plus18_5cond.csv')
bwrite(age_grp_tab_plus65_5cond, results_folder, 'age_grp_tab_plus65_5cond.csv')




# Get unique individuals --------------------------------------------------

tab<-tibble(type="Individuals with ACSC",count=NA) %>% 
  full_join(dt %>% 
  filter(plus18==1) %>% 
  summarise(count=n_distinct(encrypted_hesid)) %>% 
  mutate(type="18plus")) %>% 
  full_join(dt %>% 
              filter(plus65==1) %>% 
              summarise(count=n_distinct(encrypted_hesid)) %>% 
              mutate(type="65plus")) %>% 
  full_join(dt %>% 
              filter(dx5cond==1) %>% 
              filter(plus18==1) %>% 
              summarise(count=n_distinct(encrypted_hesid)) %>% 
              mutate(type="18plus_5conds")) %>% 
  full_join(dt %>% 
              filter(dx5cond==1) %>% 
              filter(plus65==1) %>% 
              summarise(count=n_distinct(encrypted_hesid)) %>% 
              mutate(type="65plus_5conds"))


results_folder<-paste0(results_buck, '/17_01_24')

bwrite(tab, results_folder, 'unique_pat_count.csv')

# 
# text<-'I have personally checked this output and concluded it is safe to release following the rules-based SDC criteria as described in SDE203_A.
# 
# This is looking at ambulatory care sensitive conditions admission in the HES data from 2019/20'
# 
# 
# 
# 
# bwrite(text, results_folder, 'safetorelease.txt')


text<-'I have personally checked this output and concluded it is safe to release following the rules-based SDC criteria as described in SDE203_A.

This is looking at ambulatory care sensitive conditions admission in the HES data from 2019/20. The whole sample is 748,009.

age_grp_tab_18plus5cond- breakdown of counts of those with admissions for specific ACSC conditions by deprivation (IMD and income), sex, age_group.

age_grp_tab_65plus5cond- breakdown of counts of those with admissions for specific ACSC conditions by deprivation (IMD and income), sex and age group for those 65+ years.'


bwrite(text, results_folder, 'safetorelease.txt')


# seperate out 0 LOS  ---------------------------------------------------


summary(dt)


dt<-dt %>%
  mutate(missing_disdate=ifelse(is.na(disdate),1,0)) %>% 
  mutate(los=difftime(disdate,admidate, units='days')) %>% 
  mutate(zero_los=ifelse(los==0,1,0))

dt_no_disdate<-dt %>% 
  filter(missing_disdate==0)






age_grp_tab_plus18_zero_los<-tibble(zero_los= NA, type="IMD", sex=NA,age_grp=NA, count=NA) %>% 
  full_join(dt_no_disdate %>% 
              filter(plus18==1) %>%
              group_by(zero_los, imdquint,sex, age_grp) %>% 
              summarise(count=n()) %>%
              rename(type=imdquint)) %>% 
  rows_insert(tibble(type="income",sex=NA,age_grp=NA, count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus18==1) %>%
              group_by(zero_los,incomequint,sex, age_grp) %>%
              summarise(count=n()) %>%
              rename(type=incomequint)) %>% 
  mutate(sdc=ifelse(count<10,1,0))

age_grp_tab_plus18_zero_los


age_grp_tab_plus18_zero_los5cond<-tibble(zero_los= NA, type="IMD", sex=NA,age_grp=NA, count=NA) %>% 
  full_join(dt_no_disdate %>% 
              filter(plus18==1& dx5cond==1) %>%
              group_by(zero_los, imdquint,sex, age_grp) %>% 
              summarise(count=n()) %>%
              rename(type=imdquint)) %>% 
  rows_insert(tibble(type="income",sex=NA,age_grp=NA, count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus18==1 & dx5cond==1) %>%
              group_by(zero_los,incomequint,sex, age_grp) %>%
              summarise(count=n()) %>%
              rename(type=incomequint)) %>% 
  mutate(sdc=ifelse(count<10,1,0))

age_grp_tab_plus18_zero_los5cond

age_grp_tab_plus65_zero_los<-tibble(zero_los= NA, type="IMD", sex=NA,age_grp=NA, count=NA) %>% 
  full_join(dt_no_disdate %>% 
              filter(plus65==1) %>%
              group_by(zero_los, imdquint,sex, age_grp) %>% 
              summarise(count=n()) %>%
              rename(type=imdquint)) %>% 
  rows_insert(tibble(type="income",sex=NA,age_grp=NA, count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus65==1) %>%
              group_by(zero_los,incomequint,sex, age_grp) %>%
              summarise(count=n()) %>%
              rename(type=incomequint)) %>% 
  mutate(sdc=ifelse(count<10,1,0))

age_grp_tab_plus65_zero_los 


age_grp_tab_plus65_zero_los5cond<-tibble(zero_los= NA, type="IMD", sex=NA,age_grp=NA, count=NA) %>% 
  full_join(dt_no_disdate %>% 
              filter(plus65==1& dx5cond==1) %>%
              group_by(zero_los, imdquint,sex, age_grp) %>% 
              summarise(count=n()) %>%
              rename(type=imdquint)) %>% 
  rows_insert(tibble(type="income",sex=NA,age_grp=NA, count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus65==1 & dx5cond==1) %>%
              group_by(zero_los,incomequint,sex, age_grp) %>%
              summarise(count=n()) %>%
              rename(type=incomequint)) %>% 
  mutate(sdc=ifelse(count<10,1,0))

age_grp_tab_plus65_zero_los5cond 


plus18tab_zero_los<-tibble(type="ACSC group (count, sum, prop)",count=NA) %>% 
  full_join(dt_no_disdate %>% 
              filter(plus18==1) %>%
              group_by(zero_los, acsc_group) %>%
              summarise(count=n()) %>%
              rename(type=acsc_group) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>%
  rows_insert(tibble(type="Sex (count, sum, prop)",count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus18==1) %>%
              mutate(sex=as.character(sex)) %>%
              group_by(zero_los, sex) %>%
              summarise(count=n()) %>%
              rename(type=sex) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>% 
  rows_insert(tibble(type="IMD",count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus18==1) %>%
              group_by(zero_los, imdquint) %>%
              summarise(count=n()) %>%
              rename(type=imdquint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="income",count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus18==1) %>%
              group_by(incomequint) %>%
              summarise(count=n()) %>%
              rename(type=incomequint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="age (mean,sd,median)",count=NA, sum=NA, stat=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus18==1) %>%
              group_by(zero_los) %>% 
              select(type=age) %>% 
              summarise(count=round(mean(type),2), 
                        sum=round(sd(type),2), 
                        stat=median(type)))

plus18tab_zero_los




plus65tab_zero_los<-tibble(type="ACSC group (count, sum, prop)",count=NA) %>% 
  full_join(dt_no_disdate %>% 
              filter(plus65==1) %>%
              group_by(zero_los, acsc_group) %>%
              summarise(count=n()) %>%
              rename(type=acsc_group) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>%
  rows_insert(tibble(type="Sex (count, sum, prop)",count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus65==1) %>%
              mutate(sex=as.character(sex)) %>%
              group_by(zero_los, sex) %>%
              summarise(count=n()) %>%
              rename(type=sex) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>% 
  rows_insert(tibble(type="IMD",count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus65==1) %>%
              group_by(zero_los, imdquint) %>%
              summarise(count=n()) %>%
              rename(type=imdquint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="income",count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus65==1) %>%
              group_by(incomequint) %>%
              summarise(count=n()) %>%
              rename(type=incomequint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="age (mean,sd,median)",count=NA, sum=NA, stat=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus65==1) %>%
              group_by(zero_los) %>% 
              select(type=age) %>% 
              summarise(count=round(mean(type),2), 
                        sum=round(sd(type),2), 
                        stat=median(type)))

plus65tab_zero_los

plus18tab_zero_los5cond<-tibble(type="ACSC group (count, sum, prop)",count=NA) %>% 
  full_join(dt_no_disdate %>% 
              filter(plus18==1& dx5cond==1) %>%
              group_by(zero_los, acsc_group) %>%
              summarise(count=n()) %>%
              rename(type=acsc_group) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>%
  rows_insert(tibble(type="Sex (count, sum, prop)",count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus18==1 & dx5cond==1) %>%
              mutate(sex=as.character(sex)) %>%
              group_by(zero_los, sex) %>%
              summarise(count=n()) %>%
              rename(type=sex) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>% 
  rows_insert(tibble(type="IMD",count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus18==1 & dx5cond==1) %>%
              group_by(zero_los, imdquint) %>%
              summarise(count=n()) %>%
              rename(type=imdquint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="income",count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus18==1 & dx5cond==1) %>%
              group_by(incomequint) %>%
              summarise(count=n()) %>%
              rename(type=incomequint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="age (mean,sd,median)",count=NA, sum=NA, stat=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus18==1 & dx5cond==1) %>%
              group_by(zero_los) %>% 
              select(type=age) %>% 
              summarise(count=round(mean(type),2), 
                        sum=round(sd(type),2), 
                        stat=median(type)))

plus18tab_zero_los5cond




plus65tab_zero_los5cond<-tibble(type="ACSC group (count, sum, prop)",count=NA) %>% 
  full_join(dt_no_disdate %>% 
              filter(plus65==1 & dx5cond==1) %>%
              group_by(zero_los, acsc_group) %>%
              summarise(count=n()) %>%
              rename(type=acsc_group) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>%
  rows_insert(tibble(type="Sex (count, sum, prop)",count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus65==1 & dx5cond==1) %>%
              mutate(sex=as.character(sex)) %>%
              group_by(zero_los, sex) %>%
              summarise(count=n()) %>%
              rename(type=sex) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>% 
  rows_insert(tibble(type="IMD",count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus65==1 & dx5cond==1) %>%
              group_by(zero_los, imdquint) %>%
              summarise(count=n()) %>%
              rename(type=imdquint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="income",count=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus65==1 & dx5cond==1) %>%
              group_by(incomequint) %>%
              summarise(count=n()) %>%
              rename(type=incomequint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="age (mean,sd,median)",count=NA, sum=NA, stat=NA)) %>% 
  full_join(dt_no_disdate %>%
              filter(plus65==1 & dx5cond==1) %>%
              group_by(zero_los) %>% 
              select(type=age) %>% 
              summarise(count=round(mean(type),2), 
                        sum=round(sd(type),2), 
                        stat=median(type)))

plus65tab_zero_los5cond


#Export data


#save outputs in results bucket 
results_buck<-bget("hosdis78", type_in="r")

results_folder<-paste0(results_buck, '/17_01_24')

bwrite(plus18tab_zero_los, results_folder, 'plus18tab_zero_los.csv')
bwrite(plus65tab_zero_los, results_folder, 'plus65tab_zero_los.csv')
bwrite(age_grp_tab_plus18_zero_los, results_folder, 'age_grp_tab_plus18_zero_los.csv')
bwrite(age_grp_tab_plus65_zero_los, results_folder, 'age_grp_tab_plus65_zero_los.csv')

bwrite(plus18tab_zero_los5cond, results_folder, 'plus18tab5cond_zero_los.csv')
bwrite(plus65tab_zero_los5cond, results_folder, 'plus65tab5cond_zero_los.csv')
bwrite(age_grp_tab_plus18_zero_los5cond, results_folder, 'age_grp_tab_plus18_zero_los5cond.csv')
bwrite(age_grp_tab_plus18_zero_los5cond, results_folder, 'age_grp_tab_plus65_zero_los5cond.csv')


# Individual conditions rates -----------------------------------------------------------
#visualization 

library(ggplot2)

dt %>% 
  mutate(imdquint=factor(imdquint, levels=c(5:1), labels=c(1:5))) %>% 
  filter(plus18==1) %>% 
  select(acsc_group, imdquint, age_grp) %>% 
  group_by(acsc_group, imdquint, age_grp) %>% 
 summarise(count=n()) %>% 
  ggplot(aes(x=age_grp, y=count, fill=imdquint, colour=imdquint))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(rows=vars(acsc_group))+
  theme_bw()

dt %>% 
  mutate(incomequint=factor(incomequint, levels=c(5:1), labels=c(1:5))) %>% 
  filter(plus18==1) %>% 
  select(acsc_group, incomequint, age_grp) %>% 
  group_by(acsc_group, incomequint, age_grp) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=age_grp, y=count, fill=incomequint, colour=incomequint))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(rows=vars(acsc_group))+
  theme_bw()

#no zero 
dt_no_disdate %>% 
  mutate(imdquint=factor(imdquint, levels=c(5:1), labels=c(1:5))) %>% 
  filter(plus18==1& zero_los==0) %>% 
  select(acsc_group, imdquint, age_grp) %>% 
  group_by(acsc_group, imdquint, age_grp) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=age_grp, y=count, fill=imdquint, colour=imdquint))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(rows=vars(acsc_group))+
  theme_bw()

#only zero

dt_no_disdate %>% 
  mutate(imdquint=factor(imdquint, levels=c(5:1), labels=c(1:5))) %>% 
  filter(plus18==1& zero_los==1) %>% 
  select(acsc_group, imdquint, age_grp) %>% 
  group_by(acsc_group, imdquint, age_grp) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=age_grp, y=count, fill=imdquint, colour=imdquint))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(rows=vars(acsc_group))+
  theme_bw()


#data tables 

age_grp_tab_plus18_acsc<-tibble(acsc_group=NA, type="IMD", sex=NA,age_grp=NA, count=NA) %>% 
  full_join(dt %>% 
              filter(plus18==1) %>%
              group_by(acsc_group, imdquint,sex, age_grp) %>% 
              summarise(count=n()) %>%
              rename(type=imdquint)) %>% 
  rows_insert(tibble(type="income",sex=NA,age_grp=NA, count=NA)) %>% 
  full_join(dt %>%
              filter(plus18==1) %>%
              group_by(acsc_group,incomequint,sex, age_grp) %>%
              summarise(count=n()) %>%
              rename(type=incomequint)) %>% 
  mutate(sdc=ifelse(count<10,1,0))

age_grp_tab_plus18_acsc %>% filter(sdc==1)
#2 counts are less than 10 but the it's non identifiable 


#save outputs in results bucket 
results_buck<-bget("hosdis78", type_in="r")

results_folder<-paste0(results_buck, '/17_01_24')

bwrite(age_grp_tab_plus18_acsc, results_folder, 'age_grp_tab_plus18_acsc.csv')

# England only codes ------------------------------------------------------

dt_england<-dt_england %>% 
  mutate(missing_imd=is.na(IMDdec)) %>% 
  mutate(missing_income=is.na(Incomedec))

dt_england %>% 
  select(contains("missing")) %>% 
  tbl_summary()

#no missing imd or income 

#using 2019 population estimates to match the year 

dt_england<-dt_england %>% 
  mutate(ACSC_group=ifelse(ACSC_group=="anigna", "angina", ifelse(ACSC_group=="hypetension", "hypertension", ACSC_group))) %>% 
  mutate(imdquint=as.factor(case_when(IMDdec %in% (1:2)~"1", 
                                      IMDdec %in% (3:4)~"2", 
                                      IMDdec %in% (5:6)~"3", 
                                      IMDdec %in% (7:8)~"4", 
                                      IMDdec %in% (9:10)~"5"))) %>% 
  mutate(Incomequint=as.factor(case_when(Incomedec %in% (1:2)~"1", 
                                         Incomedec %in% (3:4)~"2", 
                                         Incomedec %in% (5:6)~"3", 
                                         Incomedec %in% (7:8)~"4", 
                                         Incomedec %in% (9:10)~"5"))) %>% 
  mutate(plus18=ifelse(age>=18,1,0), #think the sample should only be 18+ so check this 
         plus65=ifelse(age>=65,1,0)) %>% 
  clean_names() %>% 
  mutate(sex=factor(sex, levels=c(1:2), labels=c("Males", "Females"))) %>%  #check that 1 is male and 2 is female
  mutate(dx5cond=ifelse(acsc_group %in% c("asthma","CHF", "COPD", "diabetes", "hypertension"),1,0)) 

dt_england<-dt_england %>% 
  filter(age!=0) %>% #filter out those with missing age 
  mutate(age_grp=case_when(age %in% 18:39~"18-39", 
                           age %in% 40:44~"40-44",
                           age %in% 45:49~"45-49", 
                           age %in% 50:54~"50-54",
                           age %in% 55:59~"55-59", 
                           age %in% 60:64~"60-64",
                           age %in% 65:69~"65-69", 
                           age %in% 70:74~"70-74", 
                           age %in% 75:79~"75-79", 
                           age %in% 80:84~"80-84", 
                           age %in% 85:89~"85-89", 
                           age >=90 ~"90+"))


plus18tab_england<-tibble(type="ACSC group (count, sum, prop)",count=NA) %>% 
  full_join(dt_england %>% 
              filter(plus18==1) %>%
              group_by(acsc_group) %>%
              summarise(count=n()) %>%
              rename(type=acsc_group) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>%
  rows_insert(tibble(type="Sex (count, sum, prop)",count=NA)) %>% 
  full_join(dt_england %>%
              filter(plus18==1) %>%
              mutate(sex=as.character(sex)) %>%
              group_by(sex) %>%
              summarise(count=n()) %>%
              rename(type=sex) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>% 
  rows_insert(tibble(type="IMD",count=NA)) %>% 
  full_join(dt_england %>%
              filter(plus18==1) %>%
              group_by(imdquint) %>%
              summarise(count=n()) %>%
              rename(type=imdquint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="income",count=NA)) %>% 
  full_join(dt_england %>%
              filter(plus18==1) %>%
              group_by(incomequint) %>%
              summarise(count=n()) %>%
              rename(type=incomequint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="age (mean,sd,median)",count=NA, sum=NA, stat=NA)) %>% 
  full_join(dt_england %>%
              filter(plus18==1) %>%
              select(type=age) %>% 
              summarise(count=round(mean(type),2), 
                        sum=round(sd(type),2), 
                        stat=median(type)))

plus18tab_england


#65plus

plus65tab_england<-tibble(type="ACSC group (count, sum, prop)",count=NA) %>% 
  full_join(dt_england %>% 
              filter(plus65==1) %>%
              group_by(acsc_group) %>%
              summarise(count=n()) %>%
              rename(type=acsc_group) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>%
  rows_insert(tibble(type="Sex (count, sum, prop)",count=NA)) %>% 
  full_join(dt_england %>%
              filter(plus65==1) %>%
              mutate(sex=as.character(sex)) %>%
              group_by(sex) %>%
              summarise(count=n()) %>%
              rename(type=sex) %>% 
              mutate(sum=sum(count)) %>% 
              mutate(stat=round(count/sum,2))) %>% 
  rows_insert(tibble(type="IMD",count=NA)) %>% 
  full_join(dt_england %>%
              filter(plus65==1) %>%
              group_by(imdquint) %>%
              summarise(count=n()) %>%
              rename(type=imdquint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="income",count=NA)) %>% 
  full_join(dt_england %>%
              filter(plus65==1) %>%
              group_by(incomequint) %>%
              summarise(count=n()) %>%
              rename(type=incomequint) %>% 
              mutate(sum=sum(count))) %>% 
  rows_insert(tibble(type="age (mean,sd,median)",count=NA, sum=NA, stat=NA)) %>% 
  full_join(dt_england %>%
              filter(plus65==1) %>%
              select(type=age) %>% 
              summarise(count=round(mean(type),2), 
                        sum=round(sd(type),2), 
                        stat=median(type)))



plus65tab_england


age_grp_tab_plus18_england<-tibble(type="IMD", sex=NA,age_grp=NA, count=NA) %>% 
  full_join(dt_england %>% 
              filter(plus18==1) %>%
              group_by(imdquint,sex, age_grp) %>% 
              summarise(count=n()) %>%
              rename(type=imdquint)) %>% 
  rows_insert(tibble(type="income",sex=NA,age_grp=NA, count=NA)) %>% 
  full_join(dt_england %>%
              filter(plus18==1) %>%
              group_by(incomequint,sex, age_grp) %>%
              summarise(count=n()) %>%
              rename(type=incomequint)) 

age_grp_tab_plus18_england

age_grp_tab_plus65_england<-tibble(type="IMD", sex=NA,age_grp=NA, count=NA) %>% 
  full_join(dt_england %>% 
              filter(plus65==1) %>%
              group_by(imdquint,sex, age_grp) %>% 
              summarise(count=n()) %>%
              rename(type=imdquint)) %>% 
  rows_insert(tibble(type="income",sex=NA,age_grp=NA, count=NA)) %>% 
  full_join(dt_england %>%
              filter(plus65==1) %>%
              group_by(incomequint,sex, age_grp) %>%
              summarise(count=n()) %>%
              rename(type=incomequint)) 

age_grp_tab_plus65_england



#save outputs in results bucket 
results_buck<-bget("hosdis78", type_in="r")

results_folder<-paste0(results_buck, '/17_01_24')

bwrite(plus18tab_england, results_folder, 'plus18tab_england.csv')
bwrite(plus65tab_england, results_folder, 'plus65tab_england.csv')
bwrite(age_grp_tab_plus18_england, results_folder, 'age_grp_tab_plus18_england.csv')
bwrite(age_grp_tab_plus65_england, results_folder, 'age_grp_tab_plus65_england.csv')




# Safe to release text ----------------------------------------------------


text<-'I have personally checked this output and concluded it is safe to release following the rules-based SDC criteria as described in SDE203_A.

This is looking at ambulatory care sensitive conditions admission in the HES data from 2019/20. The whole sample is 748,009.

plus18tab-Table with counts and proportions for ambulatory care sensitive conditions, 
counts and proportions by sex, counts by IMD, counts by income and  mean, SD and median age in the whole sample. This is for patients 18+. 

plus65tab-The same table as above but for patients aged 65+. N=507,430.

age_grp_tab_18plus- breakdown of counts of those with admissions by deprivation (IMD and income), sex, age_group.

age_grp_tab_65plus- breakdown of counts of those with admissions by deprivation (IMD and income), sex and age group for those 65+ years.

The rest of the files with the same file name but have 5cond suffix are the same as above but are for 5 specific ambulatory care sensitive conditions only (N=303,559 and for 65+ N=199,042).

unique_pat_count= looking at unique patients id with an admission by age and type of conditions. The whole sample is 587,742.


The rest of the files with the same file name but have zero_los in the file name is the same as above but is split by 0 day admissions and 1+ day admission. 

age_grp_tab_plus18_acsc.csv- breakdown of counts by acsc group, deprivation (IMD and income), sex and age group. 
For this I think 2 counts are less than 10 but as it is population level data not sure if it is disclosive.
Please advise. 

plus18tab_england, plus65tab_england, age_grp_tab_plus18_england and age_grp_tab_plus65_england- are the same as the above, but it includes different codes that are used in England only' 



bwrite(text, results_folder, 'safetorelease.txt')


# Total number of admissions ----------------------------------------------

#For 18+ 

dt_emergency<-bread(buck, '/ACSC/emeregency_admission__2019.RDS')

dt_emergency<-dt_emergency %>% 
  filter(age>=18)

tab<-tibble(numACSCadmission=nrow(dt),numemergencyadmission=nrow(dt_emergency), propACSCadmission=nrow(dt)/nrow(dt_emergency))

#save outputs in results bucket 
results_buck<-bget("hosdis78", type_in="r")

results_folder<-paste0(results_buck, '/23_02_24')

bwrite(tab, results_folder, 'propACSCadmission.csv')

text<- 'I have personally checked this output and concluded it is safe to release following the rules-based SDC criteria as described in SDE203_A.
This looks at the total of ACSC admissions for 18+ the total number of emergency admissions in HES and the propotion of emergency admissions 
accounter for by ACSC admissions.'

bwrite(text, results_folder, 'safetorelease.txt')
