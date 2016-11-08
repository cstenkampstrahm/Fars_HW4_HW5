# Clean the yearly person data
library(foreign)
library(dplyr)
library(tidyverse)
setwd("/Users/Chloe/Project_1/data-raw/yearly_person_data")
clean_yearly_person_file <- function(year){
  
  # Read data in and convert all column names to lowercase (easier to type)
  person_file <- paste0("Person_", year, ".dbf")
  df <- read.dbf(person_file, as.is = FALSE)
  colnames(df) <- tolower(colnames(df))
  
  df <- df %>%
    # Limit to variables we need so it's cleaner to work with 
    dplyr::select(st_case, veh_no, per_no, state, per_typ, lag_hrs, lag_mins,
                  inj_sev, age, alc_res, dplyr::contains("drugres"), sex) %>%
    
    # Limit to only drivers and fatal injuries, then remove those variables
    dplyr::filter(per_typ == 1 & inj_sev == 4) %>%
    dplyr::select(-per_typ, -inj_sev) %>%
    
    # Create a `unique_id`. To be unique, `year` needs to be pasted on.
    tidyr::unite(unique_id, st_case, veh_no, per_no) %>%
    dplyr::mutate(year = year,
                  unique_id = paste(unique_id, year, sep = "_")) %>%
    
    # Limit to study states and then remove the `state` variable.
    dplyr::filter(state %in% c(6,        ## California
                               15,       ## Hawaii
                               17,       ## Illinois
                               33,       ## New Hampshire
                               44,       ## Rhode Island
                               54)) %>%  ## West Virginia
    dplyr::select(-state) %>%
    
    # Convert `sex` to a factor with levels "Male" and "Female"
    dplyr::mutate(sex = ifelse(sex == 9, NA, sex), 
                  sex = factor(sex, levels = c(1, 2),
                               labels = c("Male", "Female"))) %>%
    
    # Use measured alcohol blood level to create `Alcohol` (logical for whether
    # alcohol was present). Then remove the `alc_res` variable. 
    dplyr::mutate(alc_res = ifelse(alc_res > 94, NA, alc_res / 10),
                  Alcohol = alc_res >= 0.01) %>%
    dplyr::select(-alc_res) %>%
    
    # Specify missing values for the lag minutes
    dplyr::mutate(lag_mins = ifelse(lag_mins == 99, NA, lag_mins))
  
  # Coding for missing lag hours changed in 2009
  if(year <= 2008){
    df <- df %>%
      dplyr::mutate(lag_hrs = ifelse(lag_hrs %in% c(99, 999), NA, lag_hrs))
  } else {
    df <- df %>%
      dplyr::mutate(lag_hrs = ifelse(lag_hrs == 999, NA, lag_hrs))
  }
  
  # Limit to deaths within an hour of the accident then remove those variables
  df <- df %>%
    dplyr::filter((lag_hrs < 1) | (lag_hrs == 1 & lag_mins == 0)) %>%
    dplyr::select(-lag_hrs, -lag_mins)
  
  # Coding for missing age changed during study
  if(year <= 2008){
    df <- df %>%
      dplyr::mutate(age = ifelse(age == 99, NA, age))
  } else {
    df <- df %>%
      dplyr::mutate(age = ifelse(age %in% c(998, 999), NA, age))
  }
  
  # Use age to create age categories and then remove `age` variable
  df <- df %>%
    dplyr::mutate(agecat = cut(age, breaks = c(0, 25, 45, 65, 1000),
                             labels = c("< 25 years", 
                                        "25--44 years",
                                        "45--64 years", 
                                        "65 years +"), 
                             include.lowest = TRUE, right = FALSE)) %>%
    dplyr::select(-age)
  
  # Gather all the columns with different drug listings (i.e., `drugres1`,
  # `drugres2`, `drugres3`). Convert from the numeric code listings to 
  # drug categories. 
  gathered_df <- df %>%
    tidyr::gather(drug_number, drug_type_raw, dplyr::contains("drugres")) %>% 
    dplyr::mutate(drug_type = ifelse(drug_type_raw %in% 100:295,
                                     "Narcotic", NA),
                  drug_type = ifelse(drug_type_raw %in% 300:395,
                                     "Depressant", drug_type),
                  drug_type = ifelse(drug_type_raw %in% 400:495,
                                     "Stimulant", drug_type),
                  drug_type = ifelse(drug_type_raw %in% 600:695,
                                     "Cannabinoid", drug_type),
                  drug_type = ifelse(drug_type_raw %in% c(500:595, 700:996),
                                     "Other", drug_type),
                  drug_type = ifelse(drug_type_raw == 1,
                                     "None", drug_type),
                  drug_type = factor(drug_type)) %>%
    dplyr::select(-drug_type_raw, -drug_number) %>%
    
    # Filter out any observations where both alcohol and drug data is missing
    dplyr::filter(!(is.na(Alcohol) & is.na(drug_type)))
  
  # Create a subset with only individuals with at least one non-missing 
  # listing for drugs
  non_missing_drugs <- gathered_df %>% 
    filter(!is.na(drug_type)) %>%
    group_by(unique_id, drug_type) %>%
    summarize(has_drug = TRUE) %>%
    ungroup() %>%
    mutate(row_num = 1:n()) %>%
    spread(drug_type, has_drug, fill = FALSE) %>%
    select(-row_num) %>%
    group_by(unique_id) %>%
    summarize(Cannabinoid = any(Cannabinoid),
              Depressant = any(Depressant),
              Narcotic = any(Narcotic),
              Other = any(Other),
              Stimulant = any(Stimulant)) %>%
    ungroup()
  
  # Join this back into the full dataset
  df <- df %>%
   select(-dplyr::contains("drugres")) %>%
    full_join(non_missing_drugs, by = "unique_id") %>%
   gather(drug_type, positive_for_drug, Alcohol, Cannabinoid,
                  Depressant, Narcotic, Other, Stimulant) %>%
    mutate(drug_type = factor(drug_type))
  
  return(df)
}

# Apply the function across the study years
for(year in 1999:2010){
  df <- clean_yearly_person_file(year)
  if(year == 1999){
    clean_fars <- df
  } else {
    clean_fars <- rbind(clean_fars, df)
  }
}
save(clean_fars, file = "data/clean_fars.RData")
