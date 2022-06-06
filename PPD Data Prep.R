#####################
##  PPD Data Prep  ##
#####################

####  Startup  ####
library(tidyverse)

rm(list = ls())



####  Load Data  ####
# raw <- read.csv("S:\\TRACK to TREAT\\Data\\Processed Data\\cleaned_qualtrics_ttt_phase_1.csv", stringsAsFactors = F)
raw <- read.csv("S:\\TRACK to TREAT\\Data\\Processed Data\\cleaned_qualtrics_ttt_phase_1_fixed_220606.csv", stringsAsFactors = F)



####  Clean Data  ####
## Rename columns
#Redo PPD variable names
names(raw) <- gsub("ppd_1_feeling_down.*$", "defDown", names(raw))
names(raw) <- gsub("ppd_1_.*interest.*$", "defInterest", names(raw))
names(raw) <- gsub("ppd_1_.*asleep.*$", "defSleep", names(raw))
names(raw) <- gsub("ppd_1_.*appetite.*$", "defAppetite", names(raw))
names(raw) <- gsub("ppd_1_.*energy.*$", "defEnergy", names(raw))
names(raw) <- gsub("ppd_1_.*bad_about_yourself.*$", "defSelf", names(raw))
names(raw) <- gsub("ppd_1_.*trouble_concentrating.*$", "defConcentrate", names(raw))
names(raw) <- gsub("ppd_1_.*fidgety.*$", "defPace", names(raw))
names(raw) <- gsub("ppd_1_.*better_off_dead.*$", "defSuicide", names(raw))
names(raw) <- gsub("ppd_1_something_else", "defOther", names(raw))

names(raw) <- gsub("ppd_2", "depPermanence", names(raw))

names(raw) <- gsub("ppd_3_1", "causeBrain", names(raw))
names(raw) <- gsub("ppd_3a_1", "changeBrain", names(raw))
names(raw) <- gsub("ppd_4_1", "causeEnv", names(raw))
names(raw) <- gsub("ppd_4a_1", "changeEnv", names(raw))

names(raw) <- gsub("ppd_5_1", "lessPermTherapy", names(raw))
names(raw) <- gsub("ppd_5_2", "lessPermMed", names(raw))
names(raw) <- gsub("ppd_5_3", "lessPermOther", names(raw))

df <- raw %>%
  
  ## Properly score reverse-coded CDI items
  mutate(across(matches(paste0("cdi_", c(2, 6, 7, 9, 10, 12, 14, 15, 17, 20, 23, 24, 26, 27), "$")),
            ~ {3 - .})) %>%

  ## Properly score reverse-coded PCSC items
  mutate(across(matches(paste0("pcsc_", c(3, 5, 6, 8, 10, 12, 13, 16, 17, 19, 21, 23), "$")),
                ~ {5 - .})) %>%
  
  ## Create new variables
  mutate(
    
    ## Demographics
    # Race is good as-is for the general belief paper, but for the symptom belief
    # paper we want to match it to COPE-FUS categories
    pb_childethnicity_alt = case_when(
      pb_childethnicity == "White, non-Hispanic (includes Middle Eastern)" ~ "White non-Hispanic",
      pb_childethnicity == "Black or African American" ~ "Black non-Hispanic",
      pb_childethnicity == "Hispanic or Latino/a" ~ "Hispanic",
      pb_childethnicity == "Asian (including Asian Desi and Pacific Islander)" ~ "Asian non-Hispanic",
      !is.na(pb_childethnicity) ~ "Other non-Hispanic"
    ),
    
    # Gender, which we can calculate from gender + sex variables
    pb_childgender = case_when(
      pb_childgender == "Man" & pb_childsex == "male" ~ "boy/man",
      pb_childgender == "Woman"& pb_childsex == "female" ~ "girl/woman",
      !is.na(pb_childgender) ~ "gender minority"
    ),
    
    # Age is good as-is
    
    # Family income is good as-is
    
    ## Psychological measures
    # Baseline, youth
    yb_cdi = select(., matches("yb_cdi_[0-9]*$")) %>% rowMeans(),
    yb_bhs = select(., matches("yb_bhs_[0-9]*$")) %>% rowMeans(),
    yb_pcsc = select(., matches("yb_pcsc_[0-9]*$")) %>% rowMeans(),
    yb_iptq = select(., matches("yb_iptq_[0-9]*$")) %>% rowMeans(),
    yb_shs = select(., matches("yb_shs_[0-9]*$")) %>% rowMeans(),

    # Follow-up, youth
    y3m_cdi = select(., matches("y3m_cdi_[0-9]*$")) %>% rowMeans(),
    y3m_bhs = select(., matches("y3m_bhs_[0-9]*$")) %>% rowMeans(),
    y3m_pcsc = select(., matches("y3m_pcsc_[0-9]*$")) %>% rowMeans(),
    y3m_iptq = select(., matches("y3m_iptq_[0-9]*$")) %>% rowMeans(),
    y3m_shs = select(., matches("y3m_shs_[0-9]*$")) %>% rowMeans(),
    
    
  ) %>%
  
  select(
    
    yb_lsmh_id,
    pb_childethnicity, pb_childethnicity_alt, pb_childgender, pb_childage, pb_income,
    matches("def"),
    matches("depPermanence"),
    matches("cause"),
    matches("change"),
    matches("lessPerm"),
    matches("cdi_[0-9]*$"),
    matches("bhs_[0-9]*$"),
    matches("pcsc_[0-9]*$"),
    matches("iptq_[0-9]*$"),
    matches("shs_[0-9]*$"),
    yb_cdi:y3m_shs

  )



####  Save Data  ####
saveRDS(df, file = "S:\\TRACK to TREAT\\Data\\Processed Data\\depression_belief_data.rds")
