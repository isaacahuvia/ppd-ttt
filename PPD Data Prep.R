#####################
##  PPD Data Prep  ##
#####################

####  Startup  ####
library(tidyverse)

rm(list = ls())



####  Load Data  ####
raw <- read.csv("S:\\TRACK to TREAT\\Data\\Processed Data\\cleaned_qualtrics_ttt_phase_1.csv", stringsAsFactors = F)



####  Clean Data  ####
## Rename columns
#Redo naming convention for youth/parent baseline/follow-up variables
names(raw) <- gsub("^pb_", "b.p.", names(raw))
names(raw) <- gsub("^p3m_", "f.p.", names(raw))
names(raw) <- gsub("^yb_", "b.y.", names(raw))
names(raw) <- gsub("^y3m_", "f.y.", names(raw))

#Redo PPD variable names
names(raw) <- gsub("ppd_1_feeling_down.*$", "def_down", names(raw))
names(raw) <- gsub("ppd_1_.*interest.*$", "def_interest", names(raw))
names(raw) <- gsub("ppd_1_.*asleep.*$", "def_sleep", names(raw))
names(raw) <- gsub("ppd_1_.*appetite.*$", "def_appetite", names(raw))
names(raw) <- gsub("ppd_1_.*energy.*$", "def_energy", names(raw))
names(raw) <- gsub("ppd_1_.*bad_about_yourself.*$", "def_self", names(raw))
names(raw) <- gsub("ppd_1_.*trouble_concentrating.*$", "def_concentrate", names(raw))
names(raw) <- gsub("ppd_1_.*fidgety.*$", "def_pace", names(raw))
names(raw) <- gsub("ppd_1_.*better_off_dead.*$", "def_suicide", names(raw))
names(raw) <- gsub("ppd_1_something_else", "def_other", names(raw))
names(raw) <- gsub("ppd_1_na", "def_none", names(raw))

names(raw) <- gsub("ppd_2", "permanence", names(raw))

names(raw) <- gsub("ppd_3_1", "cause_brain", names(raw))
names(raw) <- gsub("ppd_3a_1", "change_brain", names(raw))
names(raw) <- gsub("ppd_4_1", "cause_env", names(raw))
names(raw) <- gsub("ppd_4a_1", "change_env", names(raw))

names(raw) <- gsub("ppd_5_1", "lessPerm_therapy", names(raw))
names(raw) <- gsub("ppd_5_2", "lessPerm_med", names(raw))
names(raw) <- gsub("ppd_5_3", "lessPerm_other", names(raw))

df <- raw %>%
  
  ## Properly score reverse-coded CDI items
  mutate_at(paste0("b.y.cdi_", c(2, 6, 7, 9, 10, 12, 14, 15, 17, 20, 23, 24, 26, 27)),
            ~ {3 - .}) %>%

  ## Create new variables
  mutate(
    
    ## ID
    LSMHID = b.y.lsmh_id,
    
    ## Demographics
    race = case_when(b.p.childethnicity == "White, non-Hispanic (includes Middle Eastern)" ~ "White",
                     b.p.childethnicity == "Black or African American" ~ "Black",
                     b.p.childethnicity == "Hispanic or Latino/a" ~ "Hispanic",
                     b.p.childethnicity == "Asian (including Asian Desi and Pacific Islander)" ~ "Asian",
                     !is.na(b.p.childethnicity) ~ "Other"),
    gender = case_when(b.p.childgender == "Man" ~ "Male",
                       b.p.childgender == "Woman" ~ "Female",
                       !is.na(b.p.childgender) ~ "NB"),
    age = b.p.childage,
    famIncome = case_when(b.p.income == "$0-$19,000" ~ 1,
                          b.p.income == "$20,000-$39,000" ~ 2,
                          b.p.income == "$40,000-$59,000" ~ 3,
                          b.p.income == "$60,000-$79,000"~ 4,
                          b.p.income == "$80,000 - $99,000" ~ 5,
                          b.p.income == "$100,000 - $119,000" ~ 6,
                          b.p.income == "$120,000-$140,000" ~ 7,
                          b.p.income == "$140,000+" ~ 8),
    
    ## Psychological measures
    # Baseline, youth
    b.y.IDAS = select(., matches("b.y.idas_[a-z]_[0-9]*$")) %>% rowMeans(),
    b.y.CDI2 = select(., matches("b.y.cdi_[0-9]*$")) %>% rowMeans(),
    b.y.malleability = select(., matches("b.y.iptq_[0-9]$")) %>% rowMeans(),
    b.y.hopelessness = select(., matches("b.y.bhs_[0-9]$")) %>% rowMeans(),
    b.y.agency = select(., matches("b.y.pcsc_[0-9]*$")) %>% rowSums(),
    b.y.selfHate = select(., matches("b.y.shs_[0-9]$")) %>% rowSums(),
    
    # Baseline, parent
    b.p.IDAS = select(., matches("b.p.idas_[a-z]_[0-9]*$")) %>% rowMeans(),
    b.p.CDI2 = select(., matches("b.p.cdi_[0-9]*$")) %>% rowMeans(),
    b.p.malleability = select(., matches("b.p.iptq_[0-9]$")) %>% rowMeans(),
    b.p.hopelessness = select(., matches("b.p.bhs_[0-9]$")) %>% rowMeans(),
    b.p.agency = select(., matches("b.p.pcsc_[0-9]*$")) %>% rowSums(),
    b.p.selfHate = select(., matches("b.p.shs_[0-9]$")) %>% rowSums(),
    
    # Follow-up, youth
    f.y.IDAS = select(., matches("f.y.idas_[a-z]_[0-9]*$")) %>% rowMeans(),
    f.y.CDI2 = select(., matches("f.y.cdi_[0-9]*$")) %>% rowMeans(),
    f.y.malleability = select(., matches("f.y.iptq_[0-9]$")) %>% rowMeans(),
    f.y.hopelessness = select(., matches("f.y.bhs_[0-9]$")) %>% rowMeans(),
    f.y.agency = select(., matches("f.y.pcsc_[0-9]*$")) %>% rowSums(),
    f.y.selfHate = select(., matches("f.y.shs_[0-9]$")) %>% rowSums(),
    
    # Follow-up, parent
    f.y.IDAS = select(., matches("f.y.idas_[a-z]_[0-9]*$")) %>% rowMeans(),
    f.y.CDI2 = select(., matches("f.y.cdi_[0-9]*$")) %>% rowMeans(),
    f.y.malleability = select(., matches("f.y.iptq_[0-9]$")) %>% rowMeans(),
    f.y.hopelessness = select(., matches("f.y.bhs_[0-9]$")) %>% rowMeans(),
    f.y.agency = select(., matches("f.y.pcsc_[0-9]*$")) %>% rowSums(),
    f.y.selfHate = select(., matches("f.y.shs_[0-9]$")) %>% rowSums()
    
  ) %>%
  
  select(
    
    LSMHID, race, gender, age, famIncome,
    matches("def"),
    matches("permanence"),
    matches("cause"),
    matches("change"),
    matches("lessPerm"),
    matches("[a-z].[a-z].IDAS"),
    matches("[a-z].[a-z].CDI2"),
    matches("[a-z].[a-z].malleability"),
    matches("[a-z].[a-z].hopelessness"),
    matches("[a-z].[a-z].agency"),
    matches("[a-z].[a-z].selfHate")
    
  )




####  Save Data  ####
saveRDS(df, file = "S:\\TRACK to TREAT\\Data\\Processed Data\\prognostic_pessimism_working_data.rds")
