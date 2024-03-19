## ACSC_admissions_file_prep.R
## Mai Stafford
## Created 02/11/2023

rm(list=ls())

library(tidyverse)
library(aws.s3)
library(lubridate)
library(arrow)
library(janitor)

## codelists for ACSCs

# file_path5 <- "acsc_length5_lookup_comma.csv" #No level 5 so have searched at level 4
# acsclookup5 <- read.csv(file_path5)
file_path4 <- "acsc_length4_lookup_comma.csv"
acsclookup4 <- read.csv(file_path4)
file_path3 <- "acsc_length3_lookup_comma.csv"
acsclookup3 <- read.csv(file_path3)


# acsc5codes<-acsclookup5$icd10_code
acsc4codes<-acsclookup4$icd10_code
acsc3codes<-acsclookup3$icd10_code

print(acsc3codes)
print(acsc4codes)


## Year-January2019-December 2019 
## Note that some admissions in 2020/2021 and 2021/2022 datasets will have started in 2019 so we should add these in too- have done this for 20/21 not 21/22 
#Time period

startdate <- as.Date("2019-01-01")
enddate <- as.Date("2019-12-31")


# 2018 --------------------------------------------------------------------

pats18<-open_dataset(APC_data_2018) %>% 
  select(starts_with("DIAG_"), "ADMIDATE", "ADMIMETH", "DISMETH", "DISDATE", "ENCRYPTED_HESID", "PROCODE", "FYEAR",
         "SPELEND", "LSOA11", "SEX", "STARTAGE", "PROVSPNOPS", "SPELBGIN", "SPELEND", "ADMISORC") %>%
  filter(ADMIDATE>= startdate & ADMIDATE <= enddate) %>% 
  collect()


# Limit the dataset to end of spell and admissions for the ICD codes of interest
pats18acsc <- pats18 %>%
  filter(SPELEND == "Y") %>% # SU noted some incorrect spelend data
  filter(substr(LSOA11, 1, 1) =="E") %>%
  filter((substr(DIAG_01, 1, 4) %in% acsc4codes | substr(DIAG_01, 1, 3) %in% acsc3codes )) 

rm(pats18)

pats18acsc <- pats18acsc %>%
  mutate(age=ifelse(STARTAGE>=7001 & STARTAGE<=7007, 0,
                    ifelse(STARTAGE>=18 & STARTAGE<=105, STARTAGE, NA))) %>%
  mutate(emerghosp=ifelse(ADMIMETH==2 | ADMIMETH==21 | ADMIMETH==22 | ADMIMETH==23 | ADMIMETH==24 | ADMIMETH==25, 1, 0)) %>%
  mutate(lsoa=LSOA11)

pats18acsc <- filter(pats18acsc, emerghosp==1)

#Flag duplicates by patient id, admission date, admission method, discharge method, discharge date, primary diagnosis and spellid 
pats18acsc <- pats18acsc %>% 
  group_by_at(vars(ENCRYPTED_HESID, ADMIDATE,ADMIMETH, DISMETH, DISDATE, DIAG_01, PROVSPNOPS)) %>%
  mutate(Flag = cumsum(n()))

tabyl(as.data.frame(pats18acsc)$Flag)

saveRDS(pats18acsc, file="pats18acsc.RDS")
rm(pats18acsc)



# 2019 --------------------------------------------------------------------

pats19<-open_dataset(APC_data_2019) %>% 
  select(starts_with("DIAG_"), "ADMIDATE", "ADMIMETH", "DISMETH", "DISDATE", "ENCRYPTED_HESID", "PROCODE", "FYEAR",
         "SPELEND", "LSOA11", "SEX", "STARTAGE", "PROVSPNOPS", "SPELBGIN", "SPELEND", "ADMISORC") %>%
  filter(ADMIDATE>= startdate & ADMIDATE <= enddate) %>% 
  collect()

# Limit the dataset to end of spell and admissions for the ICD codes of interest
pats19acsc <- pats19 %>%
  filter(SPELEND == "Y") %>% # SU noted some incorrect spelend data
  filter(substr(LSOA11, 1, 1) =="E") %>%
  filter((substr(DIAG_01, 1, 4) %in% acsc4codes | substr(DIAG_01, 1, 3) %in% acsc3codes ))

rm(pats19)

pats19acsc <- pats19acsc %>%
  mutate(age=ifelse(STARTAGE>=7001 & STARTAGE<=7007, 0,
                    ifelse(STARTAGE>=18 & STARTAGE<=105, STARTAGE, NA))) %>%
  mutate(emerghosp=ifelse(ADMIMETH==2 | ADMIMETH==21 | ADMIMETH==22 | ADMIMETH==23 | ADMIMETH==24 | ADMIMETH==25, 1, 0)) %>%
  mutate(lsoa=LSOA11)

pats19acsc <- filter(pats19acsc, emerghosp==1)

pats19acsc <- pats19acsc %>% 
  group_by_at(vars(ENCRYPTED_HESID, ADMIDATE,ADMIMETH, DISMETH, DISDATE, DIAG_01, PROVSPNOPS)) %>%
  mutate(Flag = cumsum(n()))

tabyl(as.data.frame(pats19acsc)$Flag)

saveRDS(pats19acsc, file="pats19acsc.RDS")
rm(pats19acsc)

###########################################
## Financial year 2020/2021 to catch long-stays

# 2020 --------------------------------------------------------------------

pats20 <- open_dataset(APC_data_2020) %>%
  select(starts_with("DIAG_"), "ADMIDATE", "ADMIMETH", "DISMETH", "DISDATE", "ENCRYPTED_HESID", "PROCODE", "FYEAR",
         "SPELEND", "LSOA11", "SEX", "STARTAGE", "PROVSPNOPS", "SPELBGIN", "SPELEND", "ADMISORC") %>%
  filter(ADMIDATE>= startdate & ADMIDATE <= enddate) %>% 
  collect()

pats20acsc <- pats20 %>%
  filter(SPELEND == "Y") %>% # SU noted some incorrect spelend data
  filter(substr(LSOA11, 1, 1) =="E") %>%
  filter((substr(DIAG_01, 1, 4) %in% acsc4codes | substr(DIAG_01, 1, 3) %in% acsc3codes ))

rm(pats20)

#########################################################
# data derivations on raw data (based on vwDespair_Cube.sql)
pats20acsc <- pats20acsc %>%
  mutate(age=ifelse(STARTAGE>=7001 & STARTAGE<=7007, 0,
                    ifelse(STARTAGE>=18 & STARTAGE<=105, STARTAGE, NA))) %>%
  mutate(emerghosp=ifelse(ADMIMETH==2 | ADMIMETH==21 | ADMIMETH==22 | ADMIMETH==23 | ADMIMETH==24 | ADMIMETH==25, 1, 0)) %>%
  mutate(lsoa=LSOA11)

pats20acsc <- filter(pats20acsc, emerghosp==1)

pats20acsc <- pats20acsc %>% 
  group_by_at(vars(ENCRYPTED_HESID, ADMIDATE,ADMIMETH, DISMETH, DISDATE, DIAG_01, PROVSPNOPS)) %>% 
  mutate(Flag = cumsum(n()))

tabyl(as.data.frame(pats20acsc)$Flag)


saveRDS(pats20acsc, file="pats20acsc.RDS")
rm(pats20acsc)

#######################################################################

# join 2018/2019, 2019/2020 and 2020/2021 datasets

pats18acsc <- readRDS("~/icconic_acsc_admissions/pats18acsc.RDS")
pats19acsc <- readRDS("~/icconic_acsc_admissions/pats19acsc.RDS")
pats20acsc <- readRDS("~/icconic_acsc_admissions/pats20acsc.RDS")
all181920_acsc <- rbind(pats18acsc,pats19acsc, pats20acsc)

rm(pats18acsc, pats19acsc, pats20acsc)

all181920_acsc <- all181920_acsc %>% 
  filter(Flag<2) %>% 
  group_by_at(vars(ENCRYPTED_HESID, ADMIDATE,ADMIMETH, DISMETH, DISDATE, DIAG_01, PROVSPNOPS)) %>%
  mutate(dup_count = cumsum(n()))

tabyl(as.data.frame(all181920_acsc)$dup_count)
 

## merge in ACSC descriptions
all181920_acsc <- all181920_acsc %>%
  filter(dup_count<2) %>% #get rid of the duplicates and only retain the latest entry
  mutate(DIAG_013=substr(DIAG_01, 1, 3)) %>%
  mutate(DIAG_014=substr(DIAG_01, 1, 4)) 

ds3<-all181920_acsc %>% 
  left_join(acsclookup4 %>% 
              select(-icd10_description) %>% 
              mutate(ACSC_group..=str_replace_all(ACSC_group..,"\t", "")), by=c("DIAG_014" = "icd10_code")) %>% 
  left_join(acsclookup3 %>% 
              select(-icd10_description) %>% 
              mutate(ACSC_group..=str_replace_all(ACSC_group..,"\t", "")), by=c("DIAG_013" = "icd10_code"))


ds3 <- ds3 %>%
  ungroup() %>% 
  mutate(ACSC_group=ifelse(!is.na(ACSC_group...x), ACSC_group...x, ACSC_group...y )) %>% 
  select(-contains("..."))
# %>%
#   mutate(ACSC_desc=ifelse(!is.na(icd10_description.x), icd10_description.x, icd10_description.y ))

# ds3 <- subset(ds3, select=-c(ACSC_group.x, ACSC_group.y, icd10_description.x, icd10_description.y))


# ds4  = row level dataset  
# Remove unknown people, years, ages, los
ds4 <- ds3 %>%
  filter(!is.na(age)) %>%
  filter(SEX==1 | SEX==2) %>% 
  filter(!is.na(ACSC_group))


# merge IMD scores
IMDsubdomains_file_path <- "File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv"
imdsubdomainslsoa2019 <- read.csv(IMDsubdomains_file_path)

imdsubdomainslsoa2019 <- imdsubdomainslsoa2019 %>%
  mutate(IMDdec=Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.) %>%
  mutate(Incomedec=Income.Decile..where.1.is.most.deprived.10..of.LSOAs.) %>%
  mutate(lsoa=LSOA.code..2011.) %>%
  mutate(lsoaname=LSOA.name..2011.) %>%
  select("IMDdec", "Incomedec", "lsoa",  "lsoaname")


ds4 <- ds4 %>%
  left_join(imdsubdomainslsoa2019, by = "lsoa")

saveRDS(ds4, file="acsc_linked_2019.RDS")

s3saveRDS(x = ds4
          ,object ='/ACSC/acsc_linked_2019.RDS'
          ,bucket = 'thf-dap-tier3-projects-ihthosdis78-projectbucket-by69gg87foq0'
          ,multipart=TRUE)








