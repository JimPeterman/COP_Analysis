
library(readxl)
library(dplyr)
library(writexl)
library(FRIENDanalysis) 

data <- read_excel(here::here("BALLST_cohort_dataset.xlsx"))


###########################################################################################
# Dropping tests.
###########################################################################################

# Includes only those with an RER>=1.0 (and drops those with missing RER).
data <- data %>% filter(max_rer >= 1.0)

# Keeps only treadmill tests.
data <- filter(data, test_mode=="TM")

# Drops individuals aged over 85 or under 18.
data <- data[!(data$age>85 | data$age<18),]

# Drops those taking beta blocker medications (keeps them if value is missing).
data <- data[!(data$med_beta %in% 1),]

# Includes only TESTS performed before the year 2019 
# (NDI searched through 2019 and want >=1 year follow-up).
data <- data %>% filter(record_year<2019)

# Drops those if they passed away within 1 year of exercise test.
data <- mutate(data, date_diff = difftime(data$death_date, data$record_date, units = "days"))
data$date_diff[is.na(data$date_diff)] <- 1000
data <- filter(data, data$date_diff>365)
data$date_diff <- NULL

# Drops individuals who don't have these key variables (want everyone to have all covariates).
var_int <- c("VO2_rel", "sex", "age", "mortality_status", 
             "obesity", "hypertension", "dyslipidemia", "diabetes", "inactivity", 
             "smoker", "record_year")
data <- data[complete.cases(data[,var_int]),]

#####################################################################
# Find the COP from the "minute" dataset.
#####################################################################

data_min_all <- read_excel(here::here("FileMaker Minutes Download_10_13_2021.xlsx"),
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
# Before converting to numeric, fix some data entry issues (should all be fixed now in FileMaker).
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

# Add columns to the MAIN dataset.
data <- mutate(data, COP = NA)

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
      }
      
      
    }
  }
}
rm(cop_vec, temp_id, test_time_temp, num_col_to_use, temp_test_num, temp_ve, temp_vo2, wt)

# jp <- select(data, ID, test_number, COP)

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

###########################################################################################
# Save files.
###########################################################################################

write_xlsx(data, here::here("CLEANED COP dataset.xlsx"))