## Need to make a new script so have something to commit to
## git local repository
library(foreign)
library(dplyr)
setwd("/Users/Chloe/Project_1/data-raw/yearly_person_data")

clean_yearly_person_file <- function(study_year) {
  filename <- paste0("Person_", study_year, ".dbf")
  myfile <- read.dbf(filename, as.is = FALSE)
  colnames(myfile) <- tolower(colnames(myfile))
  myfile <- myfile %>%
    select(death_yr, drinking, state, alc_res, drugs, drugres1, drugres2,
           drugres3, age, sex, per_typ, inj_sev, lag_hrs, st_case, veh_no,
           per_no) %>%
    filter(per_typ %in% 01 & inj_sev == 4 & lag_hrs == 00) %>%
    filter(state %in% c(06, 15, 17, 33, 44, 54)) %>%
    mutate(year = death_yr) %>%
    unite(col = unique_id, st_case, veh_no, per_no, sep = "_") %>%
    mutate(unique_id = paste0(unique_id, sep = "_", year)) %>%
    mutate(sex = factor(sex, levels = c(1, 2),
                        labels = c("male", "female")))%>%
    filter(drinking %in% c(0,1) & alc_res %in% c(00:93) & drugs %in%
             c(0,1) & !(age %in% c(998,999)))
    if(study_year < 2009){myfile <- myfile %>% filter(age != 99)} 
    myfile <- myfile %>% mutate(agecat = cut(age, breaks = c(0, 24, 44, 64, 120), 
                                      labels = c("<25", "25-44", "45-64", ">65"))) %>%
    select(alc_res, drugres1, drugres2, drugres3, 
           agecat, unique_id, year, sex) %>%
      mutate(Alcohol = ifelse(alc_res >= 8, "Alcohol", "NA")) %>%
      gather(key = drug_numbers, value = drugs, drugres1, 
             drugres2, drugres3, Alcohol) %>% 
      mutate(drug_name = ifelse(drugs %in% c(500:599, 700:795, 800:895, 900:995, 996), 
                                "Other", NA), 
              drug_name = ifelse(drugs %in% 1, "none", drug_name),
             drug_name = ifelse(drugs %in% 600:695, "Cannabinoid", drug_name),
             drug_name = ifelse(drugs %in% 300:395, "Depressant", drug_name),
             drug_name = ifelse(drugs %in% 100:295, "Narcotic", drug_name),
             drug_name = ifelse(drugs %in% 400:495, "Stimulant", drug_name),
             drug_name = ifelse(drug_numbers %in% "Alcohol", "Alcohol", drug_name))%>% 
   select(-drug_numbers, -drugs, -alc_res)%>%
      filter(!(is.na(drug_name)))
    positive_drugs <- myfile %>% group_by(unique_id, 
                                              drug_name) %>% 
      summarize(has_drug = TRUE) %>% ungroup() %>% 
      mutate(row_num = 1:n()) %>%
      spread(drug_name, has_drug, fill = FALSE) %>%
      select(-row_num)%>%
      group_by(unique_id) %>%
      summarize(Cannabinoid = any(Cannabinoid), Depressant = any(Depressant), 
                Narcotic = any(Narcotic),Other = any(Other), Stimulant = any(Stimulant), 
                Alcohol = any(Alcohol)) %>% 
      ungroup()
    myfile <- myfile %>%
    full_join(positive_drugs, by = "unique_id") %>%
      select(-drug_name) %>%
      gather(drug_name, positive_drugs, Alcohol,
                          Cannabinoid, Depressant, Narcotic, Other, Stimulant) %>%
    mutate(drug_type = factor(drug_name)) %>% select(-drug_name)
}
## try data function
data_1999 <- clean_yearly_person_file(1999)
data_1999 %>% group_by(drug_type) %>% slice(1:3)

## now loop through the data
for(study_year in 1999:2010){
  df <- clean_yearly_person_file(study_year)
  3
  if(study_year == 1999){
    clean_fars <- df
  } else {
    clean_fars <- rbind(clean_fars, df)
  }
}
setwd("/Users/Chloe/Project_1/")
save(clean_fars, file = "data/clean_fars.RData")

