# write functions to analyze FARS data

library(ggplot2)
library(dplyr)
library(tidyverse)
library(knitr)
library(ggthemes)

# will begin by testing the code for the table shown in the fars_analysis 
# document, and also Dr. Anderson's example pdf. 
table_1 <- clean_fars %>%
  mutate(year_cat = cut(year, breaks = c(1999, 2002, 2006, 2010),
                        labels = c("1999-2002", "2003-2006",
                                   "2007-2010"),
                        include.lowest = TRUE, right = TRUE)) %>%
  filter(!is.na(sex)) %>%
  group_by(drug_type, sex, year_cat) %>%
  summarize(n_non_missing = sum(!is.na(positive_for_drug)),
            positive_test = sum(positive_for_drug, na.rm = TRUE),
            perc_positive = round(100 * positive_test / n_non_missing, 1)) %>%
  select(drug_type, sex, year_cat, perc_positive) %>%
  unite(sex_year_cat, sex, year_cat) %>%
  spread(sex_year_cat, perc_positive)


# Calculate confidence intervals for proportions
perc_cis <- function(x, n) {
  prop <- (x / n) 
    se <- sqrt(prop*(1-prop) / n)
    UCI <- (prop + 1.96 * se*prop)
    LCI <- (prop - 1.96 * se*prop)
out <- paste(round(100*prop, 1),"% (",round(100*LCI, 1), 
             "%, ",round(100*UCI, 1),"%)")
}

# Getting N and X for 1999 and 2010
n <- clean_fars %>% 
  group_by(year) %>%
  filter(year %in% c(1999, 2010)) %>%
  summarize(n = n())

x_1999 <- clean_fars %>% 
  mutate(drug_type = as.numeric(drug_type)) %>%
    group_by(drug_type) %>%
    filter(year %in% 1999) %>%
    summarize(x = sum(drug_type)) %>%
  mutate(drug_name = c("Alcohol", "Cannabinoid", "Depressant", 
                       "Narcotic", "Other", "Stimulant")) %>%
  mutate(year_1999 = 11850)

x_2010 <- clean_fars %>% 
  mutate(drug_type = as.numeric(drug_type)) %>%
  group_by(drug_type) %>%
  filter(year %in% 2010) %>%
  summarize(x = sum(drug_type)) %>%
  mutate(drug_name = c("Alcohol", "Cannabinoid", "Depressant", 
                       "Narcotic", "Other", "Stimulant")) %>%
  mutate(year_2010 = 9582)

# join these data sets together
per_cis_data <- full_join(x_2010, x_1999, by = "drug_name")

# make table with the percentages, using the n values found for the two years
table_data <- data.frame(Drug_Type = per_cis_data$drug_name, 
                         Value_1999 = per_cis_data$x.y, 
                         Value_2010 = per_cis_data$x.x)
table_data <- table_data %>% 
  mutate(Value_1999 = perc_cis(x = table_data$Value_1999, 
                                          n = 11850))
table2 <- table_data %>% 
   mutate(Value_2010 = perc_cis(x = table_data$Value_2010, 
                               n = 9582))
# for some reason had to call the two mutate commands above separately?
# Now will test the trends of different drugs using Cochran-Armitage test
# Need to also create an output for all drugs lumped together, without alcohol, 
# and call it 'nonalcohol'
coch_arm <- function(drug) {
   if(drug == "Nonalcohol"){
    out <- clean_fars %>%
      filter(drug_type %in% c("Cannabinoid", "Depressant", 
                              "Narcotic", "Other", "Stimulant")) %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    ca_alcohol <- prop.trend.test(x = out$positive,
                                  n = out$trials)
    new <- data.frame(Z = sqrt(ca_alcohol$statistic), 
                      p.value = ca_alcohol$p.value)
    as.data.frame(new)
  } else {
    out <- clean_fars %>%
    filter(drug_type == drug) %>%
    group_by(year) %>%
    summarize(positive = sum(positive_for_drug, na.rm = TRUE),
              trials = sum(!is.na(positive_for_drug)))
  ca_alcohol <- prop.trend.test(x = out$positive,
                                n = out$trials)
  new <- data.frame(Z = sqrt(ca_alcohol$statistic), 
                    p.value = ca_alcohol$p.value)
  as.data.frame(new)
  }
}

# now will try to loop through the function and bind the output to make a table
drug_list <- c("Alcohol", "Cannabinoid", "Depressant", 
                "Narcotic", "Other", "Stimulant", "Nonalcohol")
drug_trend_tests_ca <- lapply(drug_list, coch_arm)
drug_trend_tests_ca <- dplyr::bind_rows(drug_trend_tests_ca) %>%
  dplyr::mutate(drug = drug_list) %>%
  dplyr::select(drug, Z, p.value)
drug_trend_tests_ca <- mutate(drug_trend_tests_ca, 
                                   Z = round(Z, 1),
                                   p.value = round(p.value, 3))

# now going to test for trend using logistic regression in a function, similar
# to how coch_arm was done above
log_reg <- function(drug) {
  if(drug == "Nonalcohol"){
    newout <- clean_fars %>%
      filter(drug_type %in% c("Cannabinoid", "Depressant", 
                              "Narcotic", "Other", "Stimulant"))
    log_reg <- glm(positive_for_drug ~ year, data = newout,
                   family = binomial(link = "logit"))
    listdata <- as.list(summary(log_reg)$coefficients)
    intodata <- data.frame(Z = listdata[6],
                           p.value = listdata[8])
    colnames(intodata) <- c("Z", "p.value")
    as.data.frame(intodata)
  } else {
    newout <- clean_fars %>%
      filter(drug_type == drug)
      log_reg <- glm(positive_for_drug ~ year, data = newout,
                     family = binomial(link = "logit"))
      listdata <- as.list(summary(log_reg)$coefficients)
      intodata <- data.frame(Z = listdata[6],
                             p.value = listdata[8])
      colnames(intodata) <- c("Z", "p.value")
      as.data.frame(intodata)
  }
}
# now will loop through the function and bind the output to eventually make a table

drug_list <- c("Alcohol", "Nonalcohol", "Narcotic", "Depressant",
               "Stimulant", "Cannabinoid", "Other")
drug_trend_tests_log_reg <- lapply(drug_list, log_reg)
drug_trend_tests_log_reg <- dplyr::bind_rows(drug_trend_tests_log_reg) %>%
  dplyr::mutate(drug = drug_list) %>%
  dplyr::select(drug, Z, p.value)
drug_trend_tests_log_reg <- mutate(drug_trend_tests_log_reg, 
                                   Z = round(Z, 1),
                                   p.value = round(p.value, 3))

# now want to try to recreate the graphs in the fars_analysis document
# first graph need year on the x axis and positive for nonalcohol drug on Y;
# the data set needs grouped by year and summed by positive_for_drug
# importantly, need the first value that is true for any unique_id (individual)
group_data <- clean_fars %>%
  filter(!is.na(positive_for_drug), !is.na(agecat),
         drug_type %in% c("Cannabinoid", "Depressant", "Narcotic", "Other", "Stimulant")) %>%
  group_by(unique_id) %>%
  summarize(totals = any(positive_for_drug),
            year = first(year),
            agecat = first(agecat)) %>%
  ungroup() %>%
  group_by(year, agecat) %>%
  summarize(age_total_year = sum(totals), 
            perc_total = 100*mean(totals)) %>% ungroup()
  graph_1_fars <- group_data %>%
  ggplot(aes(x = year, y = perc_total, 
             group = agecat)) +
  geom_line() +
  geom_point(aes(shape=agecat), size = 2) +
  xlab("Year") +
  ylab("Positive for Nonalcoholic Drugs, %") +
  theme_few() +
  scale_shape_discrete(name = "Age")

# first plot looks good, now need to make the second plot. need year
# on the y axis and positive for drug % on the y axis, needs to be 
# grouped by drug_type
group1_data <- clean_fars %>%
  filter(!is.na(positive_for_drug), !is.na(agecat),
         drug_type %in% c("Cannabinoid", "Depressant", "Narcotic", "Other", "Stimulant")) %>%
  group_by(drug_type, year) %>%
  summarize(totals = sum(positive_for_drug)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(year_totals = sum(totals)) %>% ungroup() %>%
  mutate(perc_total = totals / year_totals * 100)
graph_2_fars <- group1_data %>%
  ggplot(aes(x = year, y = perc_total, 
             group = drug_type)) +
  geom_line() +
  geom_point(aes(shape=drug_type), size = 2) +
  xlab("Year") +
  ylab("Positive for Drugs, %") +
  theme_few() +
  scale_shape_discrete(name = "Drug type")
            
# second plot looks good, now will make the third. need 
# year on the x axis and positive for cannabinoid on the y,
# categorized by agecat

group2_data <- clean_fars %>%
  filter(!is.na(positive_for_drug), !is.na(agecat),
         drug_type %in% c("Cannabinoid")) %>%
  group_by(unique_id) %>%
  summarize(totals = any(positive_for_drug),
            year = first(year),
            agecat = first(agecat)) %>%
  ungroup() %>%
  group_by(year, agecat) %>%
  summarize(age_total_year = sum(totals), 
            perc_total = 100*mean(totals)) %>% ungroup()
  graph_3_fars <- group2_data %>%
  ggplot(aes(x = year, y = perc_total, 
             group = agecat)) +
  geom_line() +
  geom_point(aes(shape=agecat), size = 2) +
  xlab("Year") +
  ylab("Positive for Cannabinoid, %") +
  theme_few() +
  scale_shape_discrete(name = "Age") 






