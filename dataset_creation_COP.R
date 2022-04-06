
library(readxl)
library(dplyr)
library(writexl)

data <- read_excel(here::here("../BALLST_healthy_cohort_dataset.xlsx"))


###########################################################################################
# Dropping tests.
###########################################################################################

# Include only those with an RER>=1.0.
data <- data %>% filter(max_rer >= 1.0)

# Keep only treadmill tests.
data <- filter(data, test_mode=="TM")

# Drop individuals aged over 85 or under 18.
data <- data[!(data$age>85 | data$age<18),]

# Drop those taking beta blocker medications (keeps them if value is missing).
data <- data[!(data$med_beta %in% 1),]

# Include only tests performed before the year 2019 
# (NDI searched through 2019 and want >=1 year follow-up).
data <- data %>% filter(record_year<2019)

# Drops those if they passed away within 1 year of exercise test.
# (again, want >=1 year follow-up to account for possible underlying disease).
data <- mutate(data, date_diff = difftime(data$death_date, data$record_date, units = "days"))
data$date_diff[is.na(data$date_diff)] <- 1000
data <- filter(data, data$date_diff>365)
data$date_diff <- NULL

# Drops individuals who don't have these key variables 
# (want everyone to have all covariates for cox regressions).
var_int <- c("VO2_rel", "sex", "age", "mortality_status", 
             "obesity", "hypertension", "dyslipidemia", "diabetes", "inactivity", 
             "smoker", "record_year")
data <- data[complete.cases(data[,var_int]),]

#####################################################################
# Find the COP from the "minute" dataset.
#####################################################################

data_min_all <- read_excel(here::here("../FileMaker Minutes Download_10_13_2021.xlsx"),
                           na = "?")

# Rename the two key column labels used to sort.
data_min_all <- data_min_all %>% 
  rename("ID"="Person ID", 
         "testNumber"="Test Number")

# Create vectors of column labels.
col_vo2 <- character(length = 30)
col_ve <- character(length = 30)
col_ve_calc <- character(length = 30)
for(i in 1:30){
  col_vo2[i] <- paste("VO2", i, sep="")
  col_ve[i] <- paste("VE BTPS", i, sep="")
  col_ve_calc[i] <- paste("VE BTPS_calc", i, sep="")
}
col_int <- c(col_vo2, col_ve, col_ve_calc)

data_min <- select(data_min_all, ID, testNumber, all_of(col_int))

# Drop missing ID or test number (needed for matching).
data_min <- filter(data_min, !(is.na(ID)), !(is.na(testNumber)))

# Convert columns to numeric.
data_min <- data_min %>% 
  mutate_at(.vars = col_int,
            .funs = list(~replace(.,grepl("n/a", ., ignore.case = T),NA)))
data_min <- data_min %>% 
  mutate_at(.vars = col_int,
            .funs = list(~replace(.,grepl("n", ., ignore.case = T),NA)))
# Before converting to numeric, fix some data entry issues 
# (should all be fixed now in database for future downloads).
data_min$`VE BTPS1`[data_min$`VE BTPS1` == "22..17"] <- "22.17"
data_min$`VE BTPS3`[data_min$`VE BTPS3` == "\\"] <- NA
data_min$`VE BTPS4`[data_min$`VE BTPS4` == "b/a"] <- NA
data_min$`VE BTPS5`[data_min$`VE BTPS5` == "44q"] <- "44"
data_min$`VE BTPS6`[data_min$`VE BTPS6` == "39..52"] <- "39.52"
data_min$`VE BTPS7`[data_min$`VE BTPS7` == "58..88"] <- "58.88"
data_min$`VE BTPS11`[data_min$`VE BTPS11` == "93..5"] <- "93.5"

data_min[] <- lapply(data_min, as.numeric) 

#####################################################################
# Add in the calculated COP (minimum value of VE/VO2 in a given minute).
#####################################################################

# Add COP column to the MAIN dataset.
data <- mutate(data, COP = NA)
data <- mutate(data, COP_minute = NA)

# Now add in COP (find lowest VE/VO2 from minute data and add that to data.
for(i in 1:nrow(data_min)){
  temp_id <- paste(data_min[i, "ID"])
  temp_test_num <- paste(data_min[i, "testNumber"])
  
  # Determine if the ID and test are in the main dataset.
  # And if the test is in main dataset, filter to just that.
  if ((as.numeric(temp_id) %in% data$ID)){
    temp_df <- filter(data, ID == as.numeric(temp_id))
    if (as.numeric(temp_test_num) %in% temp_df$test_number){
      temp_df <- filter(temp_df, test_number == as.numeric(temp_test_num))
      # Get total test time from main dataset.
      test_time_temp <- floor(temp_df$test_time)
      
      # Need a temp df from minute dataset.
      temp_min_df <- filter(data_min, ID == as.numeric(temp_id),
                            testNumber == as.numeric(temp_test_num))
      
      # Will have to convert some values (like VO2rel to VO2abs).
      wt <- temp_df$weight_SI
      
      # Determine the number of data_min columns to use (can't be more than 30).
      num_col_to_use <- min(c(test_time_temp, 30), na.rm = T)
      
      # Create vector for finding COP
      cop_vec <- as.numeric(character(num_col_to_use))
      for(j in 1:num_col_to_use){
        # Use VE BTPS unless it's missing, then use the calculated version.
        if(is.na(temp_min_df[[col_ve[j]]])){
          temp_ve <- temp_min_df[[col_ve_calc[j]]]
        } else {
          temp_ve <- temp_min_df[[col_ve[j]]]
        }
        
        temp_vo2 <- temp_min_df[[col_vo2[j]]] * wt/1000
        
        # Add the VE/VO2 for that minute to the empty vector.
        cop_vec[j] <- round(temp_ve/temp_vo2, 1)
        
      }
      
      # Add the COP to the MAIN dataset (if the vector is not all NA values).
      if(!all(is.na(cop_vec))){
        data$COP[(data$ID == as.numeric(temp_id)) &
                   (data$test_number == as.numeric(temp_test_num))] <- min(cop_vec, na.rm = T)
        
        # Add the test minute in which the COP occurred.
        data$COP_minute[(data$ID == as.numeric(temp_id)) &
                          (data$test_number == as.numeric(temp_test_num))] <- 
          which(cop_vec == min(cop_vec, na.rm = T))[1]
      }
    }
  }
}
rm(cop_vec, temp_id, test_time_temp, num_col_to_use, temp_test_num, temp_ve, temp_vo2, wt)


#####################################################################
# Finalize dataset for mortality analysis.
#####################################################################

# Drop tests when COP negative, >100, or missing.
data <- filter(data, COP>0, COP<100)

# Select only the earliest test from the remaining complete dataset.
data <- data %>%
  group_by(ID) %>% 
  arrange(record_date) %>% 
  slice(1L) %>%
  ungroup(ID)

# Add in the FRIEND VO2max percentiles (using 2021 ref standards).
data <- mutate(data, FRIEND_pct = 
                 FRIENDanalysis::FRIENDpercentile(VO2 = VO2_rel, age = age, sex = sex, 
                                                  ex_mode = test_mode, ref_edition = 2))

# Add in what percentage of total test time the COP occurred.
data <- mutate(data, COP_perc_tot_time = 
                 round((COP_minute / test_time)*100, 1))

#####################################################################
# Reviewer asked for metabolic syndrome status so adding that determination in here.
#####################################################################

# Create metabolic syndrome coding based on NCEP from GETP11 p292 Table 9.2. 
# NCEP differs as it says > not ≥ 102/88 for waist.

# If a value is missing, person is coded as having MS factor if meds is yes.
# If value missing and meds is no, then MS factor coded as missing.

# Body Weight - uses waist circumference.
data <- mutate(data, MS_waist = ifelse(data$sex=="Male" & data$waist >102, 1,
                                       ifelse(data$sex=="Male" & data$waist <=102,0,
                                              ifelse(data$sex=="Female" & data$waist >88,1,
                                                     ifelse(data$sex=="Female" & data$waist <=88,0,NA)))))

# Insulin resistance/glucose - uses fasting glucose and medication
data <- mutate(data, diabetes_meds = as.numeric(data$med_diabetes))
data <- mutate(data, diabetes_vals = ifelse(data$glucose>=100, 1, 
                                            ifelse(data$glucose<100, 0, NA)))
data <- mutate(data, MS_glucose = rowSums(data[,c("diabetes_meds", "diabetes_vals")], na.rm = T))
data$MS_glucose[is.na(data$diabetes_vals) & data$diabetes_meds == 0 ] <- NA
data$MS_glucose[data$MS_glucose>1] <- 1
data <- subset(data, select = -c(diabetes_meds, diabetes_vals))

# HDL - only uses values (database doesn't have status on HDL medication).
data <- mutate(data, MS_hdl = ifelse(data$sex=="Male" & data$hdl <40, 1,
                                     ifelse(data$sex=="Male" & data$hdl >= 40,0,
                                            ifelse(data$sex=="Female" & data$hdl <50,1,
                                                   ifelse(data$sex=="Female" & data$hdl >= 50, 0, NA)))))

# Triglycerides - uses values and medications.
# Find meds related to triglycerides for coding.
dataMeds <- data %>%
  select("ID", "test_number", "record_date", "medBrand", "medReason")
# Fill in the ID, test number, and date.
dataMeds <- dataMeds %>% tidyr::fill("ID", "test_number", "record_date")
# Coding for trig meds is either "Yes" or NA.
dataMeds <- mutate(dataMeds, trigMeds = ifelse(medBrand == "Niaspan" | medReason == "Triglycerides", 1, NA))
# Select only those that were taking triglyceride meds (and only 1 row from that test).
dataMeds <- filter(dataMeds, trigMeds == 1)
dataMeds <- dataMeds %>%
  group_by(ID, record_date) %>% 
  slice(1L) %>%
  ungroup(ID)
# Combine the datasets.
dataMeds <- dataMeds %>% select(-c(medBrand, medReason))
data <- left_join(data, dataMeds, by = c("ID", "test_number", "record_date"))
# Now can code for triglyceride status.
data <- mutate(data, trig_vals = ifelse(data$trig >= 150, 1, 
                                        ifelse(data$trig < 150, 0, NA)))
data <- mutate(data, MS_trig = rowSums(data[,c("trigMeds", "trig_vals")], na.rm = T))
data$MS_trig[is.na(data$trig_vals) & is.na(data$trigMeds)] <- NA
data$MS_trig[data$MS_trig>1] <- 1
data <- subset(data, select = -c(trig_vals))

# Elevated BP - uses sBP, dBP, and/or meds.
#### This one allows for missing values and will use just medication status.
data <- mutate(data, hypertension_meds = as.numeric(data$med_hypertensives))
data <- mutate(data, hypertension_vals = ifelse(data$resting_sbp>=130 | data$resting_dbp>=85, 1,
                                                ifelse(data$resting_sbp<130 | data$resting_dbp<85, 0, NA)))
data <- mutate(data, MS_bp = rowSums(data[,c("hypertension_meds","hypertension_vals")],na.rm = T))
data$MS_bp[is.na(data$hypertension_vals) & data$hypertension_meds==0] <- NA
data$MS_bp[data$MS_bp>1] <- 1
data <- subset(data, select = -c(hypertension_meds, hypertension_vals))

# Determine if someone has metabolic syndrome (need to have 3 or more).
data <- mutate(data, MS_riskFactors = rowSums(data[,c("MS_waist", "MS_trig", "MS_hdl", "MS_bp", "MS_glucose")]))
data <- mutate(data, MS_diagnosis = ifelse(data$MS_riskFactors>2, 1, 0))


###########################################################################################
# Save files.
###########################################################################################

write_xlsx(data, here::here("../CLEANED_COP_dataset_2_7_2022.xlsx"))
