#####################
##  PPD Data Prep  ##
#####################

####  Startup  ####
library(easypackages)
libraries("tidyverse", "qualtRics", "conflicted")
walk(c("filter", "select", "count", "group_by"), ~ conflict_prefer(., "dplyr"))


rm(list = ls())



####  Load Data  ####
## Load baseline surveys
# There are both child and parent surveys, and also different surveys for in-person data collection (pre-COVID) and online data collection (post-COVID)
child.inPerson <- qualtRics::fetch_survey(surveyID = "SV_6o0P6PEabnuP7eJ") #Survey name: TRACK to TREAT Initial Youth Survey - Phase 1 - February 10 2020
parent.inPerson <- qualtRics::fetch_survey(surveyID = "SV_batbiAxm2srqISN") #Survey name: Track to Treat PARENT MEASURES - Phase 1 - February 8 2020

child.remote <- qualtRics::fetch_survey(surveyID = "SV_bxb6DO9knhwvVhX") #Survey name: DP-5 - Youth Survey REMOTE - Phase 1
parent.remote <- qualtRics::fetch_survey(surveyID = "SV_6F2DazcRJQQfCWV") #Survey name: DP-5 - PARENT MEASURES REMOTE - Phase 1




####  Clean Data  ####
parent <- map(lst(parent.inPerson, parent.remote),
              ~ transmute(.,
                          LSMHID = pb_lsmh_id,
                          race = case_when(pb_childethnicity == "White, non-Hispanic (includes Middle Eastern)" ~ "White",
                                           pb_childethnicity == "Black or African American" ~ "Black",
                                           pb_childethnicity == "Hispanic or Latino/a" ~ "Hispanic",
                                           pb_childethnicity == "Asian (including Asian Desi and Pacific Islander)" ~ "Asian",
                                           !is.na(pb_childethnicity) ~ "Other"),
                          gender = case_when(pb_childgender == "Man" ~ "Male",
                                             pb_childgender == "Woman" ~ "Female",
                                             !is.na(pb_childgender) ~ "NB"),
                          age = pb_childage,
                          famIncome_temp = pb_income %>%
                            as.numeric(.) %>%
                            if_else(. == 9, NA_real_, .),
                          famIncome = if_else(famIncome_temp < median(famIncome_temp, na.rm = T), "Below Median Income", "At or Above Median Income"),
                          
                          def.down_parent = !is.na(pb_ppd_1_1),
                          def.interest_parent = !is.na(pb_ppd_1_2),
                          def.sleep_parent = !is.na(pb_ppd_1_3),
                          def.appetite_parent = !is.na(pb_ppd_1_4),
                          def.energy_parent = !is.na(pb_ppd_1_5),
                          def.self_parent = !is.na(pb_ppd_1_6),
                          def.concentrate_parent = !is.na(pb_ppd_1_7),
                          def.pace_parent = !is.na(pb_ppd_1_8),
                          def.suicide_parent = !is.na(pb_ppd_1_9),
                          def.other_parent = !is.na(pb_ppd_1_10),
                          def.none_parent = !is.na(pb_ppd_1_11),
                          
                          permanence_parent = case_when(pb_ppd_2 == "Not permanent at all" ~ 1,
                                                 pb_ppd_2 == "Only a little permanent" ~ 2,
                                                 pb_ppd_2 == "Somewhat permanent" ~ 3,
                                                 pb_ppd_2 == "Very permanent" ~ 4,
                                                 pb_ppd_2 == "Completely permanent" ~ 5),
                          
                          cause.brain_parent = pb_ppd_3_1,
                          change.brain_parent = pb_ppd_3a_1,
                          cause.env_parent = pb_ppd_4_1,
                          change.env_parent = pb_ppd_4a_1,
                          
                          lessPerm.therapy_parent = pb_ppd_5_1,
                          lessPerm.med_parent = pb_ppd_5_2,
                          lessPerm.other_parent = pb_ppd_5_3) %>%
                
                select(-contains("temp")) %>%
              
                rowwise() %>%
                
                mutate(lessPerm.any_parent = max(lessPerm.therapy_parent, lessPerm.med_parent, lessPerm.other_parent)) %>%
                
                ungroup()) %>%
  bind_rows(.id = "surveyMode") %>%
  mutate(surveyMode = gsub("parent.", "", surveyMode)) %>%
  filter(!is.na(LSMHID),
         LSMHID != "LSMH00000",
         !duplicated(LSMHID)) #What do we do with NA values here? When does it happen, and is there a way to use & link the data? For now, do this

child <- map(lst(child.inPerson, child.remote),
             
                ## Code IDAS, which is not a factor variable in the raw data
             ~  mutate_at(.,
                          vars(matches("yb_idas_[a-z]")),
                          ~ case_when(. == "Not at all" ~ 1,
                                      . == "A little bit" ~ 2,
                                      . == "Moderately" ~ 3,
                                      . == "Quite a bit" ~ 4,
                                      . == "Extremely" ~ 5,
                                      T ~ NA_real_)) %>%
               
               ## Recode CDI-2; some variables are reversed
               mutate_at(paste0("yb_cdi_", c(1, 3, 4, 5, 8, 11, 13, 16, 18, 19, 21, 22, 25, 28)),
                         ~ as.numeric(.) %>%
                           if_else(. == 4, NA_real_, .)) %>%
               mutate_at(paste0("yb_cdi_", c(2, 6, 7, 9, 10, 12, 14, 15, 17, 20, 23, 24, 26, 27)),
                         ~ as.numeric(.) %>%
                           {4 - .} %>%
                           if_else(. == 0, NA_real_, .)) %>%

               ## Code IPTQ
               # Each item is a statement to the effect of "your personality can't change."
               # Stronger disagree = more endorsement of malleability
               mutate_at(c("yb_iptq_1", "yb_iptq_2", "yb_iptq_3"),
                         ~ case_when(. == "Really Agree" ~ 1,
                                     . == "Agree" ~ 2,
                                     . == "Somewhat Agree" ~ 3,
                                     . == "Somewhat Disagree" ~ 4,
                                     . == "Disagree" ~ 5,
                                     . == "Really Disagree" ~ 6,
                                     T ~ NA_real_)) %>%
               
               ## Code BHS-4
               # Each item is a statement to the effect of "my future is hopeless."
               # Stronger agree = more endorsement of hopelessness
               mutate_at(c("yb_bhs_1", "yb_bhs_2", "yb_bhs_3", "yb_bhs_4"),
                         ~ case_when(. == "VERY FALSE" ~ 1,
                                     . == "SORT OF FALSE" ~ 2,
                                     . == "SORT OF TRUE" ~ 3,
                                     . == "VERY TRUE" ~ 4,
                                     T ~ NA_real_)) %>%
               
               ## Code PCSC; some variables are reversed
               # Normally coded items: stronger agree = more perceived agency
               mutate_at(paste0("yb_pcsc_", c(1, 2, 4, 7, 9, 11, 14, 15, 18, 20, 22, 24)),
                         ~ case_when(. == "VERY FALSE" ~ 1,
                                     . == "SORT OF FALSE" ~ 2,
                                     . == "SORT OF TRUE" ~ 3,
                                     . == "VERY TRUE" ~ 4,
                                     T ~ NA_real_)) %>%
               # Reverse coded items: stronger agree = less perceived agency
               mutate_at(paste0("yb_pcsc_", c(3, 5, 6, 8, 10, 12, 13, 16, 17, 19, 21, 23)),
                         ~ case_when(. == "VERY FALSE" ~ 4,
                                     . == "SORT OF FALSE" ~ 3,
                                     . == "SORT OF TRUE" ~ 2,
                                     . == "VERY TRUE" ~ 1,
                                     T ~ NA_real_)) %>%
             
               ## Code SHS
               # Each item is a statement to the effect of "I hate myself."
               # Stronger agree = more endorsement of self-hate
               # This is actually already a numeric variable, 7 = true for me, 1 = not at all true for me
             
               mutate(LSMHID = yb_lsmh_id,
                         
                         #These variables come from a checklist; when a box isn't checked it's NA in the raw data
                         def.down_child = !is.na(yb_ppd_1_1),
                         def.interest_child = !is.na(yb_ppd_1_2),
                         def.sleep_child = !is.na(yb_ppd_1_3),
                         def.appetite_child = !is.na(yb_ppd_1_4),
                         def.energy_child = !is.na(yb_ppd_1_5),
                         def.self_child = !is.na(yb_ppd_1_6),
                         def.concentrate_child = !is.na(yb_ppd_1_7),
                         def.pace_child = !is.na(yb_ppd_1_8),
                         def.suicide_child = !is.na(yb_ppd_1_9),
                         def.other_child = !is.na(yb_ppd_1_10),
                         def.none_child = !is.na(yb_ppd_1_11),
                         
                         permanence_child = case_when(yb_ppd_2 == "Not permanent at all" ~ 1,
                                                yb_ppd_2 == "Only a little permanent" ~ 2,
                                                yb_ppd_2 == "Somewhat permanent" ~ 3,
                                                yb_ppd_2 == "Very permanent" ~ 4,
                                                yb_ppd_2 == "Completely permanent" ~ 5),
                         
                         cause.brain_child = yb_ppd_3_1,
                         change.brain_child = yb_ppd_3a_1,
                         cause.env_child = yb_ppd_4_1,
                         change.env_child = yb_ppd_4a_1,
                         
                         lessPerm.therapy_child = yb_ppd_5_1,
                         lessPerm.med_child = yb_ppd_5_2,
                         lessPerm.other_child = yb_ppd_5_3,
                         
                         #Add IDAS< CDI2, etc
                         IDAS = select(., yb_idas_a_1:yb_idas_d_9) %>% rowSums(),
                      
                         CDI2 = select(., matches("yb_cdi_[0-9]*$")) %>% rowSums(),
                      
                         personalityMalleability = yb_iptq_1 + yb_iptq_2 + yb_iptq_3,
                      
                         hopelessness = yb_bhs_1 + yb_bhs_2 + yb_bhs_3 + yb_bhs_4,
                      
                         agency = select(., matches("yb_pcsc_[0-9]*$")) %>% rowSums(),
                      
                         selfHate = select(., matches("yb_shs_[0-9]$")) %>% rowSums()
                         
                         ) %>%
               
               rowwise() %>%
               
               mutate(lessPerm.any_child = max(lessPerm.therapy_child, lessPerm.med_child, lessPerm.other_child)) %>%
               
               ungroup() %>%
               
               select(LSMHID:lessPerm.any_child)) %>%
  
  bind_rows(.id = "surveyMode") %>%
  mutate(surveyMode = gsub("child.", "", surveyMode)) %>%
  filter(!is.na(LSMHID),
         LSMHID != "LSMH00000",
         !duplicated(LSMHID))

df <- inner_join(parent, child, by = c("LSMHID", "surveyMode"))



####  Save Data  ####
saveRDS(df, file = "G:\\Shared drives\\Psychology_JSLab\\Projects\\In_Progress\\TRACK_to_TREAT_P1\\Data\\Processed_Data\\PPD Data\\Clean.rds")
