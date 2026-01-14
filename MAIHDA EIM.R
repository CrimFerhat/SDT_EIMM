
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(readODS)
library(readxl)
library(tidyr)
library(descr)
library(modelsummary)
library(tidyverse)


library(lme4)
library(Matrix)
library(lmerTest)
library(sjPlot)
library(ggeffects)
library(merTools)
library(labelled)
library(Metrics)



##############################
# Merge crime 2021 CSV files
##############################

# Base path to the crime data folder for the year 2021
base_folder_path_2021 <- "YOUR PATHWAY/Data/2021 crime data"

# List of all month folder names for 2021
month_folders_2021 <- paste0("2021-", sprintf("%02d", 1:12))

# Function to process each folder for 2021
process_folder_2021 <- function(month_folder) {
  folder_path <- file.path(base_folder_path_2021, month_folder)
  file_list <- list.files(path = folder_path, pattern = "-street\\.csv$", full.names = TRUE)
  
  process_file_2021 <- function(file_path) {
    read_csv(file_path, show_col_types = FALSE) %>%
      select(`Month`,`Reported by`, `Falls within`, `LSOA code`, `Crime type`) %>%
      filter(`Crime type` == "Violence and sexual offences") %>%
      count(`Month`,`Reported by`, `Falls within`, `LSOA code`)
  }
  
  combined_data_2021 <- lapply(file_list, process_file_2021) %>%
    bind_rows()
  
  return(combined_data_2021)
}


# Process all folders for 2021 and combine the data
all_combined_crime_data_2021 <- lapply(month_folders_2021, function(month_folder) {
  print(paste("Processing:", month_folder))  
  process_folder_2021(month_folder)
}) %>%
  bind_rows()

View(all_combined_crime_data_2021)


# Greater Manchester has no data
# Delete City of London as it is usually an outlier
# Delete British Transport Police
# Delete Police Service of Northern Ireland

all_combined_crime_data_2021 <- all_combined_crime_data_2021 %>%
  filter(`Reported by` != "British Transport Police",
         `Reported by` != "City of London Police",
         `Reported by` != "Police Service of Northern Ireland") %>%
  rename (`LSOA_code` = `LSOA code`)


View(all_combined_crime_data_2021)

##########
#Checking if PFAs submitted their data fully to police.co.uk in 2021

monthly_data_count_2021 <- all_combined_crime_data_2021 %>%
  group_by(`Falls within`, `Month`) %>%
  summarize(Total_violence = sum(n), .groups = 'drop')

print(monthly_data_count_2021, n = Inf)


# Check if each PFA has data for 12 months
PFA_with_full_data_2021 <- monthly_data_count_2021 %>%
  group_by(`Falls within`) %>%
  summarize(months_count = n(), .groups = 'drop') %>%
  filter(months_count != 12)


# PFAs that do not have full data
print(PFA_with_full_data_2021, n = Inf)


##########

# Aggregate the data by LSOA code over all months and delete missing data
crime_aggregated_by_lsoa_2021 <- all_combined_crime_data_2021 %>%
  group_by(`LSOA_code`) %>%
  summarise(Total_violence = sum(n), .groups = 'drop') %>%
  filter(!is.na(`LSOA_code`)) # LSOAs where crime is recorded are not known

View(crime_aggregated_by_lsoa_2021)

# Check if NAs exist
LSOAcode_NAs_2021 <- any(is.na(crime_aggregated_by_lsoa_2021$`LSOA_code`))
LSOAcode_NAs_2021
Totalviolence_NAs_2021 <- any(is.na(crime_aggregated_by_lsoa_2021$Total_violence))
Totalviolence_NAs_2021

# Check police force names
unique_falls_within_2021 <- unique(all_combined_crime_data_2021$`Falls within`)

# Print the unique values
print(unique_falls_within_2021)


##########################################
# Merge Census 2021 CSV files
##########################################

# Function to read a specific file with given parameters
read_file <- function(file_path, header_row, last_data_row) {
  read_csv(file_path, skip = header_row - 1, n_max = last_data_row - header_row)
}


# Read each file individually with specific parameters
# Adjust 'header_row' for where the header is, 'last_data_row' for the last row of data
# Note that 'header_row - 1' skips rows before the header, and 'last_data_row - header_row'
# accounts for the blank row after the header

population_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/population.csv", 
                             header_row = 8, last_data_row = 35681) %>%
  na.omit() %>%
  select (-`2021 super output area - lower layer`) %>%
  rename (`LSOA_code` = `mnemonic`,
          `Population_number` = `2021`)


View(population_2021)



age_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/age.csv",
                      header_row = 6, last_data_row = 35679) %>%
  na.omit() %>%
  select (-`2021 super output area - lower layer`, -`Total`) %>%
  rename (`LSOA_code` = `mnemonic`) %>%
  mutate(`Age1524` = `Aged 15 to 19 years` + `Aged 20 to 24 years`)  %>%
  select (-`Aged 15 to 19 years`, -`Aged 20 to 24 years`)


View(age_2021)


birth_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/birth.csv",
                        header_row = 8, last_data_row = 35681) %>%
  na.omit() %>%
  select (-`2021 super output area - lower layer`) %>%
  rename (`LSOA_code` = `mnemonic`,
          `Total`=`Total: All usual residents`,
          `UK`=`Europe: United Kingdom`) %>%
  mutate(`Non_UK` = `Total` - `UK`)  %>%
  select (-`Total`, -`UK`)


View(birth_2021)



density_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/density.csv", 
                          header_row = 8, last_data_row = 35681) %>%
  na.omit() %>%
  select (-`2021 super output area - lower layer`) %>%
  rename (`LSOA_code` = `mnemonic`,
          `Density` = `2021`)


View(density_2021)



deprivation_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/deprivation.csv",
                              header_row = 8, last_data_row = 35681) %>%
  na.omit() %>%
  select (-`2021 super output area - lower layer`) %>%
  rename (`LSOA_code` = `mnemonic`,
          `Total`=`Total: All households`,
          `Deprivation0`= `Household is not deprived in any dimension`,
          `Deprivation1`= `Household is deprived in one dimension`,
          `Deprivation2`= `Household is deprived in two dimensions`,
          `Deprivation3`= `Household is deprived in three dimensions`,
          `Deprivation4`= `Household is deprived in four dimensions`) %>%
  mutate(`Deprivation2more` = `Deprivation2` + `Deprivation3` + `Deprivation4`)  %>%
  select (`LSOA_code`, `Deprivation2more`)


View(deprivation_2021)



disability_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/disability.csv", 
                             header_row = 8, last_data_row = 35681) %>%
  na.omit() %>%
  select (-`2021 super output area - lower layer`) %>%
  rename (`LSOA_code` = `mnemonic`,
          `Disability` = `2021`)


View(disability_2021)


ethnicity_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/ethnicity.csv",
                            header_row = 8, last_data_row = 35681) %>%
  na.omit() %>%
  select (-`2021 super output area - lower layer`) %>%
  rename (`LSOA_code` = `mnemonic`,
          `Total_ethnicity`=`Total: All usual residents`,
          `Asian`= `Asian, Asian British or Asian Welsh`,
          `Black`= `Black, Black British, Black Welsh, Caribbean or African`,
          `Mixed`= `Mixed or Multiple ethnic groups`,
          `White`= `White`,
          `Other_ethnicity`= `Other ethnic group`) %>%
  mutate(`Non_White` = `Total_ethnicity` - `White`)  


View(ethnicity_2021)


loneparent_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/loneparent.csv", 
                             header_row = 7, last_data_row = 35680) %>%
  na.omit() %>%
  select (-`2021 super output area - lower layer`, - `Total: All households`) %>%
  rename (`LSOA_code` = `mnemonic`,
          `Loneparent` = `Single family household: Lone parent family`)


View(loneparent_2021)


residency_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/residency.csv", 
                            header_row = 7, last_data_row = 35680) %>%
  na.omit() %>%
  select (-`2021 super output area - lower layer`, - `Total: All usual residents`) %>%
  rename (`LSOA_code` = `mnemonic`,
          `Lessthan2years` = `Less than 2 years`)


View(residency_2021)



sex_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/sex.csv", 
                      header_row = 8, last_data_row = 35681) %>%
  na.omit() %>%
  select (-`2021 super output area - lower layer`) %>%
  rename (`LSOA_code` = `mnemonic`,
          `Male` = `2021`)


View(sex_2021)



language_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/Language.csv", 
                           header_row = 8, last_data_row = 35681) %>%
  na.omit() %>%
  select (-`2021 super output area - lower layer`, -`Total: All households`) %>%
  rename (`LSOA_code` = `mnemonic`,
          `No_English` = `No people in household have English in England, or English or Welsh in Wales as a main language`)


View(language_2021)


tenure_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/Tenure.csv", 
                         header_row = 8, last_data_row = 35681) %>%
  na.omit() %>%
  select (-`2021 super output area - lower layer`) %>%
  rename (`LSOA_code` = `mnemonic`,
          `Social_rented` = `Social rented`,
          `Private_rented` = `Private rented`)


View(tenure_2021)

area_2021 <- read_file("YOUR PATHWAY/Data/2021 uk census/LSOA_LA_Region.csv", 
                       header_row = 1, last_data_row = 35673) %>%
  select (`LSOA21CD`, `LAD22NM`, `RGN22NM`) %>%
  rename (`LSOA_code` = `LSOA21CD`,
          `LA`        = `LAD22NM`,
          `Region`    = `RGN22NM`)


View(area_2021)


##############################
# Combine crime and census data
##############################


combined_data <- crime_aggregated_by_lsoa_2021 %>%
  left_join(population_2021, by = "LSOA_code") %>%
  left_join(age_2021, by = "LSOA_code") %>%
  left_join(birth_2021, by = "LSOA_code") %>%
  left_join(density_2021, by = "LSOA_code") %>%
  left_join(deprivation_2021, by = "LSOA_code") %>%
  left_join(disability_2021, by = "LSOA_code") %>%
  left_join(ethnicity_2021, by = "LSOA_code") %>%
  left_join(loneparent_2021, by = "LSOA_code") %>%
  left_join(residency_2021, by = "LSOA_code") %>%
  left_join(sex_2021, by = "LSOA_code") %>%
  left_join(language_2021, by = "LSOA_code") %>%
  left_join(tenure_2021, by = "LSOA_code") %>%
  left_join(area_2021, by = "LSOA_code") %>%
  na.omit()

View(combined_data)


#quick examination of the data

glimpse(combined_data)
summary(combined_data)
any(is.na(combined_data))

# Create the dependent variable rate per 1,000 people
combined_data <- combined_data %>%
  mutate(Violence_rate = (Total_violence/Population_number)*1000)


View(combined_data)

hist(combined_data$Violence_rate)
min(combined_data$Violence_rate)
max(combined_data$Violence_rate)

# log transform and standardise the dependent variable


combined_data <- combined_data %>%
  mutate(
    Violence_rate_log = log1p(Violence_rate), # ln(1 + rate)
    Violence_rate_log_z = scale(Violence_rate_log)[, 1]  # z-score (mean 0, sd 1)
  )



hist(combined_data$Violence_rate_log)
hist(combined_data$Violence_rate_log_z)
min(combined_data$Violence_rate_log)
max(combined_data$Violence_rate_log)

# compute ethnic heterogeneity variable

combined_data$HHI <- (combined_data$White/100)^2 + 
  (combined_data$Asian/100)^2 + 
  (combined_data$Black/100)^2 + 
  (combined_data$Mixed/100)^2 + 
  (combined_data$Other_ethnicity/100)^2

combined_data$ELF <- 1- combined_data$HHI


hist(combined_data$ELF)


#################################
# Categorise the census variables
#################################


hist(combined_data$ELF)
hist(combined_data$Deprivation2more)
hist(combined_data$Loneparent)
hist(combined_data$Lessthan2years)
hist(combined_data$Density)

# log transform density
combined_data <- combined_data %>%
  mutate(
    Density_log = log1p(Density)
  )

hist(combined_data$Density_log)

combined_data <- combined_data %>%
  mutate(
    ELF_cat         = ntile(ELF, 3),
    Deprivation_cat = ntile(Deprivation2more, 3),
    Loneparent_cat  = ntile(Loneparent, 3),
    Residency_cat   = ntile(Lessthan2years, 3),
    Density_cat     = ntile(Density_log,3)
  )


combined_data$ELF_cat <- as_factor(combined_data$ELF_cat)
combined_data$Deprivation_cat <- as_factor(combined_data$Deprivation_cat)
combined_data$Loneparent_cat <- as_factor(combined_data$Loneparent_cat)
combined_data$Residency_cat <- as_factor(combined_data$Residency_cat)
combined_data$Density_cat <- as_factor(combined_data$Density_cat)

combined_data$LA <- as_factor(combined_data$LA)
combined_data$Region <- as_factor(combined_data$Region)

#########################
#Descriptive statistics
#########################


glimpse(combined_data)

descriptive_statistics <- combined_data %>%
  select(
    Violence_rate,
    White,
    Asian,
    Black,
    Mixed,
    Other_ethnicity,
    ELF,
    Deprivation2more,
    Loneparent,
    Lessthan2years,
    Density,
    Age1524)


datasummary_skim(descriptive_statistics, output = "descriptive_statistics.docx")



vars_cor <- combined_data %>%
  select(
    Violence_rate_log_z,
    ELF,
    Deprivation2more,
    Loneparent,
    Lessthan2years,
    Density_log,
    No_English,
    Age1524
  )

cor_matrix <- cor(
  vars_cor,
  use = "complete.obs",
  method = "pearson"
)

round(cor_matrix, 3)


library(corrplot)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.cex = 0.8,
  number.cex = 0.7
)


##########################
#create strata variables
##########################

combined_data$strata <- paste(combined_data$ELF_cat, 
                              combined_data$Deprivation_cat, 
                              combined_data$Loneparent_cat, 
                              combined_data$Residency_cat,
                              combined_data$Density_cat)




#Get frequencies and percentages for stratum label
result_table_stratum <- combined_data %>%
  group_by(strata) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100)


print(result_table_stratum, n = Inf)

utils::write.csv(result_table_stratum, "result_table_stratum.csv", row.names = FALSE)


#################
# Fit models
################


#Run initial models, building up the models gradually.

combined_data$strata <- as_factor(combined_data$strata)


#Model1 : 2 level null model
model1 <- lmer(Violence_rate_log_z ~ 1 + 
                 (1|strata), 
               data=combined_data)

summary(model1)

# predict the mean outcome
combined_data$m1m <- predict(model1)




#Model2: 2 level full model



model2 <- lmer(Violence_rate_log_z ~ 1 + ELF_cat +
                 Deprivation_cat +
                 Loneparent_cat +
                 Residency_cat +
                 Density_cat +
                 (1 | strata),                
               data = combined_data)



summary(model2)

AIC(model1, model2)

performance::check_collinearity(model2)

###########

model2_lm <- lm(
  Violence_rate_log_z ~ ELF_cat +
    Deprivation_cat +
    Loneparent_cat +
    Residency_cat +
    Density_cat,
  data = combined_data
)

summary(model2_lm)

performance::check_collinearity(model2_lm)

###########



# add age control
model3 <- lmer(Violence_rate_log_z ~ 1 + ELF_cat +
                 Deprivation_cat +
                 Loneparent_cat +
                 Residency_cat +
                 Density_cat +
                 Age1524 +
                 (1 | strata),                
               data = combined_data)



summary(model3)

AIC(model1, model2, model3)

performance::check_collinearity(model3)


###########

model3_lm <- lm(
  Violence_rate_log_z ~ ELF_cat +
    Deprivation_cat +
    Loneparent_cat +
    Residency_cat +
    Density_cat +
    Age1524,
  data = combined_data
)

summary(model3_lm)

performance::check_collinearity(model3_lm)

###########



# add region 
model4 <- lmer(Violence_rate_log_z ~ 1 + ELF_cat +
                 Deprivation_cat +
                 Loneparent_cat +
                 Residency_cat +
                 Density_cat +
                 Age1524 +
                 Region +
                 (1 | strata),                
               data = combined_data)



summary(model4)

AIC(model1, model2, model3, model4)


performance::check_collinearity(model4)



# predict the mean outcome and confidence intervals of prediction (model 2)
m2m <- predictInterval(model2, level = 0.95, include.resid.var = FALSE)

# Create an identifier variable in the new dataframe, called "id"
m2m <- dplyr::mutate(m2m, id = dplyr::row_number())

# predict the stratum random effects and associated SEs
m2u <- REsim(model2)  



# Create id for merge
combined_data$id <- seq.int(nrow(combined_data))


# Merge `combined_data` with m2m predictions
combined_data_v2 <- merge(combined_data, m2m, by = "id") %>%
  dplyr::rename(
    m2mfit = fit,
    m2mupr = upr,
    m2mlwr = lwr
  )

View(combined_data_v2)
glimpse(combined_data_v2)

# Aggregate to stratum level
stratum_level <- stats::aggregate(
  x  = combined_data_v2[c("Violence_rate_log_z")],
  by = combined_data_v2[c("strata", "m2mfit", "m2mupr", "m2mlwr")],
  FUN = mean
)

View(stratum_level)


sjPlot::tab_model(
  model1, model2, model3,
  dv.labels   = c("Model 1 (Null)", "Model 2 (Main Effects)", "Model 3 (Age control"),
  pred.labels = c("Intercept",
                  "ELF (Ethnic heterogeneity) 2nd tertile",
                  "ELF (Ethnic heterogeneity) 3rd tertile",
                  "Deprivation (2+ dimensions) 2nd tertile",
                  "Deprivation (2+ dimensions) 3rd tertile",
                  "Lone parenthood 2nd tertile",
                  "Lone parenthood 3rd tertile",
                  "Residency (less than 2 years) 2nd tertile",
                  "Residency (less than 2 years) 3rd tertile",
                  "Density 2nd tertile",
                  "Density 3rd tertile",
                  "% People aged 15-24"),
  digits   = 3,
  digits.re = 3,
  show.aic = TRUE,
  p.style  = "stars",
  file     = "model_results.doc"
)


sjPlot::tab_model(
  model4,
  dv.labels   = c("Model 4 (Regions)"),
  pred.labels = c("Intercept",
                  "ELF (Ethnic heterogeneity) 2nd tertile",
                  "ELF (Ethnic heterogeneity) 3rd tertile",
                  "Deprivation (2+ dimensions) 2nd tertile",
                  "Deprivation (2+ dimensions) 3rd tertile",
                  "Lone parenthood 2nd tertile",
                  "Lone parenthood 3rd tertile",
                  "Residency (less than 2 years) 2nd tertile",
                  "Residency (less than 2 years) 3rd tertile",
                  "Density 2nd tertile",
                  "Density 3rd tertile",
                  "% People aged 15-24",
                  "North West",
                  "Yorkshire and The Humber",
                  "North East",
                  "West Midlands",
                  "East Midlands",
                  "South West",
                  "East of England",
                  "South East",
                  "Wales"),
  digits   = 3,
  digits.re = 3,
  show.aic = TRUE,
  p.style  = "stars",
  file     = "model_results2.doc"
)




# Rank predicted stratum means
stratum_level <- stratum_level %>%
  dplyr::mutate(rank = rank(m2mfit))

# Order stratum factor by rank
stratum_level$strata <- factor(
  stratum_level$strata,
  levels = unique(stratum_level$strata[order(stratum_level$rank)])
)

# Plot predicted means with 95% CI
plotA <- ggplot2::ggplot(stratum_level, ggplot2::aes(y = m2mfit, x = strata)) +
  ggplot2::geom_point() +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = m2mlwr, ymax = m2mupr)) +
  ggplot2::ylab("Predicted Violence (Z-log), Model 2") +
  ggplot2::xlab("Stratum") + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                 axis.title.x = ggplot2::element_text(size = 16),  
                 axis.title.y = ggplot2::element_text(size = 16)) 

print(plotA)

ggplot2::ggsave("plotA.png", plot = plotA, width = 30, height = 8, dpi = 300, bg = "white")



stratum_level <- stratum_level[order(stratum_level$rank), ]
lowest10  <- head(stratum_level,  n = 10)
highest10 <- tail(stratum_level, n = 10)

utils::write.csv(lowest10,  "lowest10.csv",  row.names = FALSE)
utils::write.csv(highest10, "highest10.csv", row.names = FALSE)

View(stratum_level)
utils::write.csv(stratum_level, "stratum_level_predicted.csv", row.names = FALSE)



View(m2u)
utils::write.csv(m2u, "stratum_level_random_effects.csv", row.names = FALSE)

# Rank strata by RE mean and order factor
m2u <- m2u %>%
  dplyr::arrange(mean) %>%
  dplyr::mutate(groupID = factor(groupID, levels = groupID[order(mean)]))

# Plot random effects with 95% CI
plotB <- ggplot2::ggplot(m2u, ggplot2::aes(x = groupID, y = mean)) + 
  ggplot2::geom_pointrange(ggplot2::aes(ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd)) +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::ylab("Stratum random effect, Model 2") +
  ggplot2::xlab("Stratum") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                 axis.title.x = ggplot2::element_text(size = 16),  
                 axis.title.y = ggplot2::element_text(size = 16))

print(plotB)

ggplot2::ggsave("plotB.png", plot = plotB, width = 30, height = 8, dpi = 300, bg = "white")


# identify significant interaction effects

m2u_flagged <- m2u %>%
  mutate(
    ci_lower = mean - 1.96 * sd,
    ci_upper = mean + 1.96 * sd,
    # CI excludes 0 if entirely above 0 or entirely below 0
    ci_excludes_zero = (ci_lower > 0) | (ci_upper < 0),
    direction = case_when(
      ci_excludes_zero & mean > 0 ~ "positive",
      ci_excludes_zero & mean < 0 ~ "negative",
      TRUE ~ "includes_zero"
    )
  )

# Inspect counts
table(m2u_flagged$ci_excludes_zero)

View(m2u_flagged)

# save these

significant_strata <- m2u_flagged %>%
  filter(ci_excludes_zero) %>%
  arrange(desc(mean))    

# Save to CSV
write.csv(significant_strata, "significant_strata_m2u.csv", row.names = FALSE)


# Calculate the Proportional Change in Variance (PCV) (as a percentage) for model 1 and 2

# first extract variance matrices from the model objects
vc1 <- as.data.frame(VarCorr(model1))
vc1
vc2 <- as.data.frame(VarCorr(model2))
vc2

# calculate PCVs using components of these variance matrices (as percentages)
PCV1 <- ((vc1[1, 4] - vc2[1, 4]) / vc1[1, 4]) * 100
PCV1

# 93.24596


# merge predictions and random effects

# Rename groupID to strata for merge
m2u <- m2u %>%
  dplyr::rename(strata = groupID)

# Calculate 95% CIs
m2u <- m2u %>%
  mutate(
    ci_lower = mean - 1.96 * sd,
    ci_upper = mean + 1.96 * sd
  )


# merge predictions and random effects
merged_strata_data <- merge(stratum_level, m2u, by = "strata")
View(merged_strata_data)


utils::write.csv(merged_strata_data, "merged_strata_data.csv", row.names = FALSE)

#########


library(dplyr)
library(forcats)

# add strata labels
strata_lookup <- combined_data %>%
  distinct(strata, ELF_cat, Deprivation_cat, Loneparent_cat, Residency_cat, Density_cat) %>%
  mutate(
    ELF_lab   = fct_recode(as.factor(ELF_cat),        "Low ELF"="1","Med ELF"="2","High ELF"="3"),
    DEP_lab   = fct_recode(as.factor(Deprivation_cat),"Low Depr"="1","Med Depr"="2","High Depr"="3"),
    LONE_lab  = fct_recode(as.factor(Loneparent_cat), "Low Lone"="1","Med Lone"="2","High Lone"="3"),
    RES_lab   = fct_recode(as.factor(Residency_cat),  "Low Mobility"="1","Med Mobility"="2","High Mobility"="3"),
    DENS_lab  = fct_recode(as.factor(Density_cat),    "Low Density"="1","Med Density"="2","High Density"="3"),
    strata_label = paste(ELF_lab, DEP_lab, LONE_lab, RES_lab, DENS_lab, sep = " | ")
  ) %>%
  dplyr::select(strata, strata_label, ELF_cat, Deprivation_cat, Loneparent_cat, Residency_cat, Density_cat)



# Merge labels, and frequencies into merged data

merged_strata_data_labeled <- merged_strata_data %>%
  left_join(
    result_table_stratum %>% 
      dplyr::select(strata, Frequency),
    by = "strata"
  ) %>%
  left_join(strata_lookup, by = "strata") %>%
  relocate(strata_label, Frequency, .after = strata)

View(merged_strata_data_labeled)

# Save
utils::write.csv(merged_strata_data_labeled, "merged_strata_data_labeled.csv", row.names = FALSE)

# Add strata labels to significant interaction effects


# Rename groupID 
significant_strata_renamed <- significant_strata %>%
  rename(strata = groupID)

View(significant_strata_renamed)

# Bring in strata_label from merged_strata_data_labeled via left_join
significant_strata_labeled <- significant_strata_renamed %>%
  left_join(
    merged_strata_data_labeled %>% select(strata, strata_label, Frequency),
    by = "strata"
  )

View(significant_strata_labeled)
``
# Save
utils::write.csv(significant_strata_labeled, "significant_strata_m2u_labeled.csv", row.names = FALSE)

