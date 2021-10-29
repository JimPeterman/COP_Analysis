
library(readxl)
library(dplyr)
library(writexl)
library(survival)
library(survminer)
library(ggplot2)
library(tibble)
library(stringr)

data <- read_xlsx(here::here("../CLEANED COP dataset_10_29_2021.xlsx"))

#######################################################################
# Cox proportional hazards and Concordance tests.
#######################################################################

group_type <- c("all", "Male", "Female")
# death_var <- c("all", "Cancer", "CVD")
death_var <- c("all")
var_int <- c("COP")

# Start with a quick check for colinearity of features using VIF (<5 is ok).
for(i in 1:(length(var_int)+1)){
  if(i <= length(var_int)){
    temp_model <- coxph(Surv(follow_up_yrs, mortality_status) ~ data[[var_int]] + age + sex + record_year +
                          obesity + hypertension + dyslipidemia + diabetes + inactivity + smoker, data = data)
    print(paste(var_int[i], "model:"))
    print(rms::vif(temp_model))
  } else{
    temp_model <- coxph(Surv(follow_up_yrs, mortality_status) ~ VO2_rel + age + sex + record_year +
                          obesity + hypertension + dyslipidemia + diabetes + inactivity + smoker, data = data)
    print("VO2 model:")
    print(rms::vif(temp_model))
  }
  rm(temp_model)
}


# Included output for sex comparisons:
# If sex term is sig, then there is a difference in the strength of the relationship
# between men and women (which is stronger can be seen with HR for sex-specific analysis). 
for(p in 1:length(group_type)){
  if(group_type[p] == "all"){
    temp_df <- data
  } else {
    temp_df <- filter(data, sex == group_type[p])
  }
  
  for(i in 1:length(var_int)){
    for(k in 1:length(death_var)){
      
      # Get the correct mortality grouping.
      if(death_var[k] == "all"){
        temp_df <- temp_df
      } else {
        to_keep <- c(NA, death_var[k])
        temp_df <- temp_df[(temp_df$mortality_grouping %in% to_keep),]
        rm(to_keep)    
      }
      

      if(group_type[p] == "all"){
        ##########################################################################
        # Whole cohort Cox models with sex as a covariate 
        # (different loop since you can't have this in sex-specific analyses).
        ##########################################################################
        
        # Model 1 - Univariate Model.
        res_cox_uni <- coxph(Surv(follow_up_yrs, mortality_status) ~ temp_df[[var_int[i]]], data = temp_df)
        
        # Model 2 - Control for: age, sex, test year.
        res_cox_multi_2 <- coxph(Surv(follow_up_yrs, mortality_status) ~ temp_df[[var_int[i]]] + age + sex + record_year, data = temp_df)
        
        # Model 3 - Control for: age, sex, test year, obesity, hypertension, dyslipidemia, diabetes, PA, smoking, record year.
        res_cox_multi_3 <- coxph(Surv(follow_up_yrs, mortality_status) ~ temp_df[[var_int[i]]] + age + sex + record_year +
                                   obesity + hypertension + dyslipidemia + diabetes + inactivity + smoker, data = temp_df)
        
        # Model 4 - Control for: age, sex, test year, obesity, hypertension, dyslipidemia, diabetes, PA, smoking, record year, VO2max.
        res_cox_multi_4 <- coxph(Surv(follow_up_yrs, mortality_status) ~ temp_df[[var_int[i]]] + age + sex + record_year +
                                   obesity + hypertension + dyslipidemia + diabetes + inactivity + smoker +
                                   VO2_rel, data = temp_df)
        
        #######################################################################
        # Do all of the VO2 models.
        # VO2 Univariate model.
        res_cox_uni_VO2 <- coxph(Surv(follow_up_yrs, mortality_status) ~ VO2_rel, data = temp_df)
        
        # VO2 Model 2 - Control for: age, sex, test year.
        res_cox_multi_2_VO2 <- coxph(Surv(follow_up_yrs, mortality_status) ~ VO2_rel + age + sex + record_year, data = temp_df)
        
        # Model 3 - Control for: age, sex, test year, obesity, hypertension, dyslipidemia, diabetes, PA, smoking, record year.
        res_cox_multi_3_VO2 <- coxph(Surv(follow_up_yrs, mortality_status) ~ VO2_rel + age + sex + record_year +
                                       obesity + hypertension + dyslipidemia + diabetes + inactivity + smoker, data = temp_df)
        
        # Create a summary of the Cox analysis.
        models <- c("res_cox_uni", "res_cox_multi_2", "res_cox_multi_3", "res_cox_multi_4",
                    "res_cox_uni_VO2", "res_cox_multi_2_VO2", "res_cox_multi_3_VO2")
        temp_summary <- data.frame(c("Univariate", "Age, sex, test year", 
                                     "Age, sex, test year, obesity, hypertension, dyslipidemia, diabetes, PA, smoking",
                                     "Age, sex, test year, obesity, hypertension, dyslipidemia, diabetes, PA, smoking, VO2max"))
        colnames(temp_summary)[1] <- paste(var_int[i], death_var[k], sep = "_")
        
        # The below counter is to get the VO2 models on the correct rows.
        counter <- 1
        for(j in 1:length(models)){
          if(substr(models[j], (nchar(models[j])-2), (nchar(models[j]))) != "VO2"){
            # Add in the variable of interest.
            
            # Determine whether there needs to be a significance value added.
            sig_sym <- ifelse((summary(get(paste(models)[j]))$coefficients["temp_df[[var_int[i]]]","Pr(>|z|)"] < 0.05), "*", "")
            
            # Create a vector of the Hazard Ratios from the model.
            temp_vec <- sprintf("%.3f", round(unname(summary(get(paste(models)[j]))$conf.int["temp_df[[var_int[i]]]",]),3))
            
            temp_summary[j,paste(var_int[i], "HR (95% CI)")] <- str_trim(paste(temp_vec[1], " (", temp_vec[3], "-",
                                                                      temp_vec[4], ") ", sig_sym, sep = ""))
            temp_summary[j,paste(var_int[i], "p-value")] <- round(summary(get(paste(models)[j]))$coefficients["temp_df[[var_int[i]]]","Pr(>|z|)"],4)
            temp_summary[j,"n"] <- summary(get(paste(models)[j]))$n
            temp_summary[j,"Number of Events"] <- summary(get(paste(models)[j]))$nevent
            
            temp_summary[j,"Concordance"] <- sprintf("%.3f", round(summary(get(paste(models)[j]))$concordance["C"], 3))
            temp_summary[j,"AIC"] <- sprintf("%.1f", round(AIC(get(paste(models)[j])),1))
            if(models[j] == "res_cox_uni"){
              temp_summary[j, paste(var_int[i], "model sex term p-value")] <- NA
            } else {
              temp_summary[j, paste(var_int[i], "model sex term p-value")] <- round(summary(get(paste(models)[j]))$coefficients["sexMale","Pr(>|z|)"],4)
            }
            
            rm(temp_vec)
            
          } else {
            # Add in the VO2 data. 
            
            # Determine whether there needs to be a significance value added.
            sig_sym <- ifelse((summary(get(paste(models)[j]))$coefficients["VO2_rel","Pr(>|z|)"] < 0.05), "*", "")
            
            temp_vec <- sprintf("%.3f", round(unname(summary(get(paste(models)[j]))$conf.int["VO2_rel",]),3))
            temp_summary[counter,"VO2 HR (95% CI)"] <- str_trim(paste(temp_vec[1], " (", temp_vec[3], "-",
                                                             temp_vec[4], ") ", sig_sym, sep = ""))
            temp_summary[counter,"VO2 p-value"] <- round(summary(get(paste(models)[j]))$coefficients["VO2_rel","Pr(>|z|)"],4)
            counter <- counter + 1          
          }
        }
        assign(paste("cox", group_type[p], var_int[i], death_var[k], sep = "_"), temp_summary)
        
        #######################################################################
        # Concordance Tests to compare models.
        
        int_model <- c("res_cox_uni", "res_cox_multi_2", "res_cox_multi_3")
        VO2_models <- c("res_cox_uni_VO2", "res_cox_multi_2_VO2", "res_cox_multi_3_VO2")
        
        concordance_summary <- data.frame("Model_Comparison" = c("Univariate", "Age, Sex, Test Year", 
                                                                 "Age, Sex, Testing Year, and Risk Factors","","",
                                                                 paste("Adding", var_int[i], "to Model"),
                                                                 "","","Correlations"))
        colnames(concordance_summary)[1] <- paste(var_int[i], death_var[k], sep = "_")
        
        
        # Comparing the Variable of interest and VO2 models
        for(m in 1:length(int_model)){
          ctest <- concordance(get(paste(VO2_models[m])), get(paste(int_model[m])))
          
          contr <- c(-1, 1)
          dtest <- contr %*% coef(ctest)
          dvar <- contr %*% vcov(ctest) %*% contr
          # Stat results (z-score > |1.96| is significant).
          concordance_summary[m,"Contrast"] <- round(dtest, 4)
          concordance_summary[m,"SD"] <- round(sqrt(dvar), 4)
          concordance_summary[m,"Z Score"] <- round((dtest/sqrt(dvar)), 2)
          # to get (2 sided) p value it's: 2*pnorm(-abs(z score))
          concordance_summary[m,"p value"] <- round(2*pnorm(-abs((dtest/sqrt(dvar)))), 5)
          # Concordance values - what's getting compared in the models.
          # Determine whether significance needs to be added (different symbol for different sig note).
          sig_sym <- ifelse((round(2*pnorm(-abs((dtest/sqrt(dvar)))), 5) < 0.05), "†", "")
          concordance_summary[m,"VO2 Concordance"] <- 
            str_trim(paste(sprintf("%.3f", round(summary(get(paste(VO2_models)[m]))$concordance["C"], 3)), sig_sym))
          concordance_summary[m, paste(var_int[i], "Concordance")] <- 
            sprintf("%.3f", round(summary(get(paste(int_model)[m]))$concordance["C"], 3))
          # AIC - model fits to describe findings more.
          concordance_summary[m,"VO2 AIC"] <- sprintf("%.1f", round(AIC(get(paste(VO2_models)[m])),1))
          concordance_summary[m, paste(var_int[i], "AIC")] <- sprintf("%.1f", round(AIC(get(paste(int_model)[m])),1))
          
          rm(ctest, dtest, dvar, contr)
        }
        
        # To fill in new labels for the VO2+Variable comparisons.
        concordance_summary[5,2:9] <- c("Contrast", "SD", "Z Score", "p value", "VO2 Concordance",
                                        paste("VO2+", var_int[i], "Concordance"), "VO2 AIC",
                                        paste("VO2+", var_int[i], "AIC"))
        
        # Comparing the addition of Variable of interest to the VO2 model.
        ctest <- concordance(res_cox_multi_3_VO2, res_cox_multi_4)
        contr <- c(-1, 1)
        dtest <- contr %*% coef(ctest)
        dvar <- contr %*% vcov(ctest) %*% contr
        
        concordance_summary[6,"Contrast"] <- round(dtest, 4)
        concordance_summary[6,"SD"] <- round(sqrt(dvar), 4)
        concordance_summary[6,"Z Score"] <- round((dtest/sqrt(dvar)), 2)
        concordance_summary[6,"p value"] <- round(2*pnorm(-abs((dtest/sqrt(dvar)))), 5)
        concordance_summary[6,"VO2 Concordance"] <- 
          sprintf("%.3f", round(res_cox_multi_3_VO2$concordance["concordance"], 3))
        # Add in significance value if VO2+variable of interest model is stronger than just VO2.
        sig_sym <- ifelse((round(2*pnorm(-abs((dtest/sqrt(dvar)))), 5) < 0.05), "‡", "")
        concordance_summary[6,paste(var_int[i], "Concordance")] <- 
          str_trim(paste(sprintf("%.3f", round(res_cox_multi_4$concordance["concordance"], 3)), sig_sym))
        concordance_summary[6,"VO2 AIC"] <- sprintf("%.1f", round(AIC(res_cox_multi_3_VO2),1))
        concordance_summary[6,paste(var_int[i], "AIC")] <- sprintf("%.1f", round(AIC(res_cox_multi_4),1))
        
        rm(ctest, dtest, dvar, contr)
        
        # Correlation between variables.
        concordance_summary[8,2:3] <- c("r", "p-value")
        concordance_summary[9,2] <- sprintf("%.2f", round(cor(temp_df[[var_int[i]]], temp_df$VO2_rel, method = "pearson"), 2)) 
        concordance_summary[9,3] <- round(cor.test(temp_df[[var_int[i]]], temp_df$VO2_rel, method = "pearson")$p.value, 5)
        
        # Add in the description of the significant symbols.
        concordance_summary[8, 6] <- paste("†: significantly different from", var_int[i])
        concordance_summary[8, 7] <- paste("‡: significantly different from full VO2 model")
        
        assign(paste("concordance", group_type[p], var_int[i], death_var[k], sep = "_"), concordance_summary)
        
        
        rm(temp_df, models, temp_summary, res_cox_uni, res_cox_multi_2, res_cox_multi_3, res_cox_multi_4,
           res_cox_uni_VO2, res_cox_multi_2_VO2, res_cox_multi_3_VO2, concordance_summary) 
        rm(counter, temp_vec, int_model, VO2_models)
        
      } else {
        
        ##########################################################################
        # Models when doing male/female analyses.
        # (sex not included as covariate).
        ##########################################################################
        
        # Model 1 - Univariate Model.
        res_cox_uni <- coxph(Surv(follow_up_yrs, mortality_status) ~ temp_df[[var_int[i]]], data = temp_df)
        
        # Model 2 - Control for: age, test year.
        res_cox_multi_2 <- coxph(Surv(follow_up_yrs, mortality_status) ~ temp_df[[var_int[i]]] + age + record_year, data = temp_df)
        
        # Model 3 - Control for: age, test year, obesity, hypertension, dyslipidemia, diabetes, PA, smoking, record year.
        res_cox_multi_3 <- coxph(Surv(follow_up_yrs, mortality_status) ~ temp_df[[var_int[i]]] + age + record_year +
                                   obesity + hypertension + dyslipidemia + diabetes + inactivity + smoker, data = temp_df)
        
        # Model 4 - Control for: age, test year, obesity, hypertension, dyslipidemia, diabetes, PA, smoking, record year, VO2max.
        res_cox_multi_4 <- coxph(Surv(follow_up_yrs, mortality_status) ~ temp_df[[var_int[i]]] + age + record_year +
                                   obesity + hypertension + dyslipidemia + diabetes + inactivity + smoker +
                                   VO2_rel, data = temp_df)
        
        #######################################################################
        # Do all of the VO2 models.
        # VO2 Univariate model.
        res_cox_uni_VO2 <- coxph(Surv(follow_up_yrs, mortality_status) ~ VO2_rel, data = temp_df)
        
        # VO2 Model 2 - Control for: age, test year.
        res_cox_multi_2_VO2 <- coxph(Surv(follow_up_yrs, mortality_status) ~ VO2_rel + age + record_year, data = temp_df)
        
        # Model 3 - Control for: age, test year, obesity, hypertension, dyslipidemia, diabetes, PA, smoking, record year.
        res_cox_multi_3_VO2 <- coxph(Surv(follow_up_yrs, mortality_status) ~ VO2_rel + age + record_year +
                                       obesity + hypertension + dyslipidemia + diabetes + inactivity + smoker, data = temp_df)
        
        # Create a summary of the Cox analysis.
        models <- c("res_cox_uni", "res_cox_multi_2", "res_cox_multi_3", "res_cox_multi_4",
                    "res_cox_uni_VO2", "res_cox_multi_2_VO2", "res_cox_multi_3_VO2")
        temp_summary <- data.frame(c("Univariate", "Age, sex, test year", 
                                     "Age, test year, obesity, hypertension, dyslipidemia, diabetes, PA, smoking",
                                     "Age, test year, obesity, hypertension, dyslipidemia, diabetes, PA, smoking, VO2max"))
        colnames(temp_summary)[1] <- paste(var_int[i], death_var[k], sep = "_")
        
        # The below counter is to get the VO2 models on the correct rows.
        counter <- 1
        for(j in 1:length(models)){
          if(substr(models[j], (nchar(models[j])-2), (nchar(models[j]))) != "VO2"){
            # Add in the variable of interest.
            
            # Determine whether there needs to be a significance value added.
            sig_sym <- ifelse((summary(get(paste(models)[j]))$coefficients["temp_df[[var_int[i]]]","Pr(>|z|)"] < 0.05), "*", "")
            
            # Create a vector of the Hazard Ratios from the model.
            temp_vec <- sprintf("%.3f", round(unname(summary(get(paste(models)[j]))$conf.int["temp_df[[var_int[i]]]",]),3))
            
            temp_summary[j,paste(var_int[i], "HR (95% CI)")] <- str_trim(paste(temp_vec[1], " (", temp_vec[3], "-",
                                                                      temp_vec[4], ") ", sig_sym, sep = ""))
            temp_summary[j,paste(var_int[i], "p-value")] <- round(summary(get(paste(models)[j]))$coefficients["temp_df[[var_int[i]]]","Pr(>|z|)"],4)
            temp_summary[j,"n"] <- summary(get(paste(models)[j]))$n
            temp_summary[j,"Number of Events"] <- summary(get(paste(models)[j]))$nevent
            
            temp_summary[j,"Concordance"] <- sprintf("%.3f", round(summary(get(paste(models)[j]))$concordance["C"], 3))
            temp_summary[j,"AIC"] <- sprintf("%.1f", round(AIC(get(paste(models)[j])),1))
            
            rm(temp_vec)
            
          } else {
            # Add in the VO2 summary.
            
            # Determine whether there needs to be a significance value added.
            sig_sym <- ifelse((summary(get(paste(models)[j]))$coefficients["VO2_rel","Pr(>|z|)"] < 0.05), "*", "")
            
            temp_vec <- sprintf("%.3f", round(unname(summary(get(paste(models)[j]))$conf.int["VO2_rel",]),3))
            temp_summary[counter,"VO2 HR (95% CI)"] <- str_trim(paste(temp_vec[1], " (", temp_vec[3], "-",
                                                             temp_vec[4], ") ", sig_sym, sep = ""))
            temp_summary[counter,"VO2 p-value"] <- round(summary(get(paste(models)[j]))$coefficients["VO2_rel","Pr(>|z|)"],4)
            counter <- counter + 1          
          }
        }
        assign(paste("cox", group_type[p], var_int[i], death_var[k], sep = "_"), temp_summary)
        
        #######################################################################
        # Concordance Tests to compare models.
        
        int_model <- c("res_cox_uni", "res_cox_multi_2", "res_cox_multi_3")
        VO2_models <- c("res_cox_uni_VO2", "res_cox_multi_2_VO2", "res_cox_multi_3_VO2")
        
        concordance_summary <- data.frame("Model_Comparison" = c("Univariate", "Age, Test Year", 
                                                                 "Age, Testing Year, and Risk Factors","","",
                                                                 paste("Adding", var_int[i], "to Model"),
                                                                 "","","Correlations"))
        colnames(concordance_summary)[1] <- paste(var_int[i], death_var[k], sep = "_")
        
        
        # Comparing the Variable of interest and VO2 models
        for(m in 1:length(int_model)){
          ctest <- concordance(get(paste(VO2_models[m])), get(paste(int_model[m])))
          
          contr <- c(-1, 1)
          dtest <- contr %*% coef(ctest)
          dvar <- contr %*% vcov(ctest) %*% contr
          # Stat results (z-score > |1.96| is significant).
          concordance_summary[m,"Contrast"] <- round(dtest, 4)
          concordance_summary[m,"SD"] <- round(sqrt(dvar), 4)
          concordance_summary[m,"Z Score"] <- round((dtest/sqrt(dvar)), 2)
          # to get (2 sided) p value it's: 2*pnorm(-abs(z score))
          concordance_summary[m,"p value"] <- round(2*pnorm(-abs((dtest/sqrt(dvar)))), 5)
          # Concordance values - what's getting compared in the models.
          # Determine whether significance needs to be added (different symbol for different sig note).
          sig_sym <- ifelse((round(2*pnorm(-abs((dtest/sqrt(dvar)))), 5) < 0.05), "†", "")
          concordance_summary[m,"VO2 Concordance"] <- 
            str_trim(paste(sprintf("%.3f", round(summary(get(paste(VO2_models)[m]))$concordance["C"], 3)), sig_sym))
          concordance_summary[m,paste(var_int[i], "Concordance")] <- 
            sprintf("%.3f", round(summary(get(paste(int_model)[m]))$concordance["C"], 3))
          # AIC - model fits to describe findings more.
          concordance_summary[m,"VO2 AIC"] <- sprintf("%.1f", round(AIC(get(paste(VO2_models)[m])),1))
          concordance_summary[m,paste(var_int[i], "AIC")] <- sprintf("%.1f", round(AIC(get(paste(int_model)[m])),1))
          
          rm(ctest, dtest, dvar, contr)
        }
        
        # To fill in new labels for the VO2+Variable comparisons.
        concordance_summary[5,2:9] <- c("Contrast", "SD", "Z Score", "p value", "VO2 Concordance",
                                        paste("VO2+", var_int[i], "Concordance"), "VO2 AIC",
                                        paste("VO2+", var_int[i], "AIC"))
        
        # Comparing the addition of Variable of interest to the VO2 model.
        ctest <- concordance(res_cox_multi_3_VO2, res_cox_multi_4)
        contr <- c(-1, 1)
        dtest <- contr %*% coef(ctest)
        dvar <- contr %*% vcov(ctest) %*% contr
        
        concordance_summary[6,"Contrast"] <- round(dtest, 4)
        concordance_summary[6,"SD"] <- round(sqrt(dvar), 4)
        concordance_summary[6,"Z Score"] <- round((dtest/sqrt(dvar)), 2)
        concordance_summary[6,"p value"] <- round(2*pnorm(-abs((dtest/sqrt(dvar)))), 5)
        concordance_summary[6,"VO2 Concordance"] <- sprintf("%.3f", round(res_cox_multi_3_VO2$concordance["concordance"], 3))
        # Add in significance value if VO2+variable of interest model is stronger than just VO2.
        sig_sym <- ifelse((round(2*pnorm(-abs((dtest/sqrt(dvar)))), 5) < 0.05), "‡", "")
        concordance_summary[6,paste(var_int[i], "Concordance")] <- 
          str_trim(paste(sprintf("%.3f", round(res_cox_multi_4$concordance["concordance"], 3)), sig_sym))
        concordance_summary[6,"VO2 AIC"] <- sprintf("%.1f", round(AIC(res_cox_multi_3_VO2),1))
        concordance_summary[6,9] <- sprintf("%.1f", round(AIC(res_cox_multi_4),1))
        
        rm(ctest, dtest, dvar, contr)
        
        # Correlation between variables.
        concordance_summary[8,2:3] <- c("r", "p-value")
        concordance_summary[9,2] <- sprintf("%.2f", round(cor(temp_df[[var_int[i]]], temp_df$VO2_rel, method = "pearson"), 2)) 
        concordance_summary[9,3] <- round(cor.test(temp_df[[var_int[i]]], temp_df$VO2_rel, method = "pearson")$p.value, 5)
        
        # Add in the description of the significant symbols.
        concordance_summary[8, 6] <- paste("†: significantly different from", var_int[i])
        concordance_summary[8, 7] <- paste("‡: significantly different from full VO2 model")
        
        assign(paste("concordance", group_type[p], var_int[i], death_var[k], sep = "_"), concordance_summary)
        
        
        rm(temp_df, models, temp_summary, res_cox_uni, res_cox_multi_2, res_cox_multi_3, res_cox_multi_4,
           res_cox_uni_VO2, res_cox_multi_2_VO2, res_cox_multi_3_VO2, concordance_summary) 
        rm(counter, temp_vec, int_model, VO2_models)
        
      }
      
    }
    
  }
  
}


###############################################################################
# Mean ± SD Statistics.
###############################################################################

# Vector which lists all of the variables you want summarized.
variables <- c("age", "COP", "VO2_rel", "FRIEND_pct", "max_hr", "max_rer",
               "bmi", "waist", "glucose", "total_cholesterol", "follow_up_yrs")

group_type <- c("all", "Male", "Female")
death_var <- c("all")

# Create mean ± SD, sample size, and t-test summary for publication tables.
for(k in 1:length(death_var)){
  
  # Get the correct mortality grouping.
  if(death_var[k] == "all"){
    temp_mor_df <- data
  } else {
    to_keep <- c(NA, death_var[k])
    temp_mor_df <- data[(data$mortality_grouping %in% to_keep),]
    rm(to_keep)    
  }
  df_summary <- data.frame(variables)
  
  
  # T-tests to compare variables between groups (based on sex).
  # Then used below to add significance symbols.
  for(j in 1:length(variables)){
    temp_df_M <- filter(temp_mor_df, sex == "Male")
    temp_df_F <- filter(temp_mor_df, sex == "Female")
    df_summary[j, "sex_tTest"] <- round(t.test(temp_df_F[[variables[j]]],
                                               temp_df_M[[variables[j]]],
                                               paired = F, var.equal = F)$p.value, 4)
    rm(temp_df_M, temp_df_F)
  }
  
  # Means and SD (whole cohort and by sex).
  for (j in 1:length(variables)){
    for(s in 1:length(group_type)){
      
      if(group_type[s] == "all"){
        temp_df <- temp_mor_df
      } else {
        temp_df <- filter(temp_mor_df, sex ==  group_type[s])
      }
      
      # Add in significance symbol if t-test p-value <0.05.
      sig_sym <- ifelse((group_type[s] == "Female" & df_summary[j, "sex_tTest"] < 0.05), "*", "")
      
      if(variables[j] == "VO2_abs" | variables[j] == "max_rer"){
        df_summary[j, paste(group_type[s], "mean_sd", sep="_")] <- 
          str_trim(paste(sprintf("%.2f", round(mean(temp_df[[variables[j]]], na.rm=T),2)), "±", 
                sprintf("%.2f", round(sd(temp_df[[variables[j]]], na.rm=T), 2)), sig_sym))
      } else {
        df_summary[j, paste(group_type[s], "mean_sd", sep="_")] <- 
          str_trim(paste(sprintf("%.1f", round(mean(temp_df[[variables[j]]], na.rm=T),1)), "±", 
                sprintf("%.1f", round(sd(temp_df[[variables[j]]], na.rm=T), 1)), sig_sym))
      }
    }
    rm(sig_sym)
  }
      
  
  # Sample Size (whole cohort and by sex).
  for (j in 1:length(variables)){
    for(s in 1:length(group_type)){
      
      if(group_type[s] == "all"){
        temp_df <- temp_mor_df
      } else {
        temp_df <- filter(temp_mor_df, sex ==  group_type[s])
      }
      
      df_summary[j,paste(group_type[s], "n", sep="_")] <- length(which(!is.na(temp_df[variables[j]])))
    }
  }
  
  # T-tests to compare variables between mortality status groups.
  # Then used below to add significance symbols.
  for(j in 1:length(variables)){
    for(s in 1:length(group_type)){
      if(group_type[s] != "all"){
        df_mor <- filter(temp_mor_df, sex == group_type[s])
      } else{
        df_mor <- temp_mor_df
      }
      
      temp_df_D <- filter(df_mor, mortality_status == 1)
      temp_df_A <- filter(df_mor, mortality_status == 0)
      df_summary[j, paste(group_type[s], "mort_tTest", sep="_")] <- 
        round(t.test(temp_df_A[[variables[j]]], temp_df_D[[variables[j]]], paired = F, 
                     var.equal = F)$p.value, 4)
      rm(temp_df_A, temp_df_D)
    }
  }
  
  # Mean and SD BY MORTALITY STATUS.
  for (j in 1:length(variables)){
    
    for(s in 1:length(group_type)){
      for (m in 1:2){
        if(group_type[s] == "all"){
          df_mor <- temp_mor_df
          # Determine whether t-test was significant for variable.
          sig_t_test <- df_summary[j, "all_mort_tTest"] < 0.05
        } else {
          df_mor <- filter(temp_mor_df, sex == group_type[s])
          # Determine whether t-test was significant for variable.
          sig_t_test <- df_summary[j, paste(group_type[s], "mort_tTest", sep="_")] < 0.05
        }
        
        if (m == 1){
          df_mor <- filter(df_mor, mortality_status == 0)
          
          if(variables[j] == "VO2_abs" | variables[j] == "max_rer"){
            df_summary[j,paste(group_type[s], "mean_sd_Alive", sep="_")] <-
              paste(sprintf("%.2f", round(mean(df_mor[[variables[j]]], na.rm=T),2)), "±",
                    sprintf("%.2f", round(sd(df_mor[[variables[j]]], na.rm=T), 2)))
          } else {
            df_summary[j,paste(group_type[s], "mean_sd_Alive", sep="_")] <-
              paste(sprintf("%.1f", round(mean(df_mor[[variables[j]]], na.rm=T),1)), "±",
                    sprintf("%.1f", round(sd(df_mor[[variables[j]]], na.rm=T), 1)))
          }
        } else {
          df_mor <- filter(df_mor, mortality_status == 1)
          
          # Decide whether a significance symbol needs to be added.
          sig_sym <- ifelse(sig_t_test, "*", "")
          
          if(variables[j] == "VO2_abs" | variables[j] == "max_rer"){
            df_summary[j,paste(group_type[s], "mean_sd_Deceased", sep="_")] <-
              str_trim(paste(sprintf("%.2f", round(mean(df_mor[[variables[j]]], na.rm=T),2)), "±",
                    sprintf("%.2f", round(sd(df_mor[[variables[j]]], na.rm=T), 2)), sig_sym))
          } else {
            df_summary[j,paste(group_type[s], "mean_sd_Deceased", sep="_")] <-
              str_trim(paste(sprintf("%.1f", round(mean(df_mor[[variables[j]]], na.rm=T),1)), "±",
                    sprintf("%.1f", round(sd(df_mor[[variables[j]]], na.rm=T), 1)), sig_sym))
          }
        }
      }
      rm(sig_t_test, sig_sym)
    }
  }

  # Sample size BY MORTALITY STATUS.
  for (j in 1:length(variables)){
    
    for(s in 1:length(group_type)){
      for (m in 1:2){
        if(group_type[s] == "all"){
          df_mor <- temp_mor_df
        } else {
          df_mor <- filter(temp_mor_df, sex == group_type[s])
        }
        
        if (m == 1){
          df_mor <- filter(df_mor, mortality_status == 0)
          df_summary[j,paste(group_type[s], "n_Alive", sep="_")] <- 
            length(which(!is.na(df_mor[variables[j]])))
        } else {
          df_mor <- filter(df_mor, mortality_status == 1)
          df_summary[j,paste(group_type[s], "n_Deceased", sep="_")] <- 
            length(which(!is.na(df_mor[variables[j]])))
        }
      }
    }
  }
  
  # Add in the test date ranges.
  df_summary[nrow(df_summary)+2, 2] <- paste("Earliest Test Date: ", min(temp_df$record_date))
  df_summary[nrow(df_summary)+1, 2] <- paste("Latest Test Date: ", max(temp_df$record_date))
  
  # Add in the explanation of significance symbols.
  df_summary[nrow(df_summary)-1, "Female_mean_sd"] <- paste("* sig diff from males")
  df_summary[nrow(df_summary)-1, "all_mean_sd_Deceased"] <- paste("* sig diff from those alive (all cohort)")
  df_summary[nrow(df_summary)-1, "Male_mean_sd_Deceased"] <- paste("* sig diff from living males")
  df_summary[nrow(df_summary)-1, "Female_mean_sd_Deceased"] <- paste("* sig diff from living females")
  
  
  # Rename summaries.
  assign(paste(death_var[k], "summary_data", sep="_"), df_summary)
  rm(temp_df, df_summary, df_mor)
}


###############################################################################
# Risk Factor Summary.
###############################################################################

# Create columns with yes/no for coding of ethnicity.
table(data$ethnicity)
data$ethnicity_hispanic <- ifelse(data$ethnicity == "Hispanic", 1, 0)
data$ethnicity_native <- ifelse(grepl("Native", data$ethnicity, ignore.case = T), 1, 0)
data$ethnicity_asian <- ifelse(grepl("Asian", data$ethnicity, ignore.case = T), 1, 0)
data$ethnicity_black <- ifelse(grepl("Black", data$ethnicity, ignore.case = T), 1, 0)
data$ethnicity_white <- ifelse(grepl("White", data$ethnicity, ignore.case = T), 1, 
                               ifelse(grepl("n/a", data$ethnicity, ignore.case = T), 1,0))

risk_factors <- c("obesity", "hypertension", "dyslipidemia", "diabetes", "smoker", "inactivity")
eth_var <- c("ethnicity_asian", "ethnicity_black", "ethnicity_hispanic", "ethnicity_white")

# Create percentiles and chi-squared test summary for publication tables.
# No Chi tests performed on ethnicity data due to very small sample sizes of non-white participants.
for(k in 1:length(death_var)){
  
  all_factors <- c(risk_factors, eth_var)
  df_summary <- data.frame(all_factors)
  
  # Get the correct mortality grouping.
  if(death_var[k] == "all"){
    temp_mor_df <- data
  } else {
    to_keep <- c(NA, death_var[k])
    temp_mor_df <- data[(data$mortality_grouping %in% to_keep),]
    rm(to_keep)    
  }
  
  
  # Chi Square tests for sex (excluding ethnicity)
  # Results used below for adding significance symbol.
  for(i in 1:length(all_factors)){
    if(substr(all_factors[i], 1, 9) != "ethnicity"){
      temp_tbl <- table(temp_mor_df$sex, temp_mor_df[[all_factors[i]]])
      df_summary[i, "Sex_Chi_p"] <- round(chisq.test(temp_tbl)$p.value, 4)
    }
  }
  
  
  # Summary percentages of individuals with risk factors and percentages of ethnicities.
  # For whole cohort and males and females.
  for(i in 1:length(all_factors)){
    for(s in 1:length(group_type)){
      
      if(group_type[s] == "all"){
        temp_df <- temp_mor_df
      } else {
        temp_df <- filter(temp_mor_df, sex ==  group_type[s])
      }
      
      # Add in a significance symbol (to femle column) if needed (excluding ethnicity).
      if(group_type[s] == "Female" & df_summary[i, "Sex_Chi_p"] < 0.05 & 
         substr(all_factors[i], 1, 9) != "ethnicity"){
        # Non-ethnicity factors are rounded to no decimals.
        if(substr(all_factors[i], 1, 9) != "ethnicity"){
          df_summary[i, group_type[s]] <- 
            paste(sprintf("%.0f", round(sum(temp_df[[all_factors[i]]], na.rm = T) / 
                                          sum(complete.cases(temp_df[[all_factors[i]]]))*100, digits = 0)), "*")
        } else {
          # Ethnicity rounded to 1 decimal place.
          df_summary[i, group_type[s]] <- 
            paste(sprintf("%.1f", round(sum(temp_df[[all_factors[i]]], na.rm = T) /
                                          sum(complete.cases(temp_df[[all_factors[i]]]))*100, digits = 1)), "*")
        }
      } else {
        # Non-ethnicity factors are rounded to no decimals.
        if(substr(all_factors[i], 1, 9) != "ethnicity"){
          df_summary[i, group_type[s]] <- 
            paste(sprintf("%.0f", round(sum(temp_df[[all_factors[i]]], na.rm = T) / 
                                          sum(complete.cases(temp_df[[all_factors[i]]]))*100, digits = 0)))
        } else {
          # Ethnicity rounded to 1 decimal place.
          df_summary[i, group_type[s]] <- 
            paste(sprintf("%.1f", round(sum(temp_df[[all_factors[i]]], na.rm = T) /
                                          sum(complete.cases(temp_df[[all_factors[i]]]))*100, digits = 1)))
        }
      }
    }
  }
  
  # Chi Square tests for mortality (excluding ethnicity tests).
  # Results used below for adding significance symbol.
  for(i in 1:length(all_factors)){
    # Exclude ethnicity in tests.
    if(substr(all_factors[i], 1, 9) != "ethnicity"){
      for(s in 1:length(group_type)){
        if(group_type[s] == "all"){
          df_mor <- temp_mor_df
        } else {
          df_mor <- filter(temp_mor_df, sex == group_type[s])
        }
        
        temp_tbl <- table(df_mor$mortality_status, df_mor[[all_factors[i]]])
        df_summary[i, paste(group_type[s], "mort_Chi_p", sep="_")] <- round(chisq.test(temp_tbl)$p.value, 4)
      }
    }
  }
  
  
  # Summary percentages of individuals with risk factors and percentages of ethnicities.
  # BY MORTALITY STATUS.
  for (i in 1:length(all_factors)){
    for(s in 1:length(group_type)){
      for (m in 1:2){
        if(group_type[s] == "all"){
          df_mor <- temp_mor_df
          # Determine whether chi squared test was significant for variable (excluding ethnicity).
          if(substr(all_factors[i], 1, 9) != "ethnicity"){
            sig_t_test <- df_summary[i, "all_mort_Chi_p"] < 0.05
          }
        } else {
          df_mor <- filter(temp_mor_df, sex == group_type[s])
          # Determine whether chi squared test was significant for variable (excluding ethnicity).
          if(substr(all_factors[i], 1, 9) != "ethnicity"){
            sig_t_test <- df_summary[i, paste(group_type[s], "mort_Chi_p", sep="_")] < 0.05
          }
        }
        
        if (m == 1){
          df_mor <- filter(df_mor, mortality_status == 0)
          
          # Non-ethnicity factors are rounded to no decimals.
          if(substr(all_factors[i], 1, 9) != "ethnicity"){
            df_summary[i, paste(group_type[s], "Alive", sep="_")] <- 
              paste(round(sum(df_mor[[all_factors[i]]], na.rm = T) /
                            sum(complete.cases(df_mor[[all_factors[i]]]))*100, digits = 0))
          } else {
            # Ethnicity rounded to 1 decimal place.
            df_summary[i, paste(group_type[s], "Alive", sep="_")] <- 
              paste(sprintf("%.1f", round(sum(df_mor[[all_factors[i]]], na.rm = T) /
                                            sum(complete.cases(df_mor[[all_factors[i]]]))*100, digits = 1)))
          }
        } else {
          df_mor <- filter(df_mor, mortality_status == 1)
          
          
          # Decide whether a significance symbol needs to be added.
          if(sig_t_test & substr(all_factors[i], 1, 9) != "ethnicity"){
            # Non-ethnicity factors are rounded to no decimals.
            if(substr(all_factors[i], 1, 9) != "ethnicity"){
              df_summary[i, paste(group_type[s], "Deceased", sep="_")] <- 
                paste(round(sum(df_mor[[all_factors[i]]], na.rm = T) /
                              sum(complete.cases(df_mor[[all_factors[i]]]))*100, digits = 0), "*")
            } else {
              # Ethnicity rounded to 1 decimal place.
              df_summary[i, paste(group_type[s], "Deceased", sep="_")] <- 
                paste(sprintf("%.1f", round(sum(df_mor[[all_factors[i]]], na.rm = T) /
                                              sum(complete.cases(df_mor[[all_factors[i]]]))*100, digits = 1)), "*")
            } 
          } else {
            # Non-ethnicity factors are rounded to no decimals.
            if(substr(all_factors[i], 1, 9) != "ethnicity"){
              df_summary[i, paste(group_type[s], "Deceased", sep="_")] <- 
                paste(round(sum(df_mor[[all_factors[i]]], na.rm = T) /
                              sum(complete.cases(df_mor[[all_factors[i]]]))*100, digits = 0))
            } else {
              # Ethnicity rounded to 1 decimal place.
              df_summary[i, paste(group_type[s], "Deceased", sep="_")] <- 
                paste(sprintf("%.1f", round(sum(df_mor[[all_factors[i]]], na.rm = T) /
                                              sum(complete.cases(df_mor[[all_factors[i]]]))*100, digits = 1)))
            }
          }
        }
      }
    }
  }
  
  # Add in the explanation of significance symbols.
  df_summary[nrow(df_summary)+2, "Female"] <- paste("* sig diff from males")
  df_summary[nrow(df_summary), "all_Deceased"] <- paste("* sig diff from those alive (all cohort)")
  df_summary[nrow(df_summary), "Male_Deceased"] <- paste("* sig diff from living males")
  df_summary[nrow(df_summary), "Female_Deceased"] <- paste("* sig diff from living females")
  
  
  # Rename summaries.
  assign(paste(death_var[k], "risk_ethnicity_data", sep="_"), df_summary)
  rm(all_factors, df_summary, df_mor, temp_df, temp_tbl)
}


###########################################################################################
# Save files.
###########################################################################################

y <- list("characteristics" = all_summary_data, 
          "risk_and_ethnicity" = all_risk_ethnicity_data,
          "ALL COP cox" = cox_all_COP_all, 
          "ALL concordance" = concordance_all_COP_all,
          
          "Male COP cox" = cox_Male_COP_all, 
          "Male concordance" = concordance_Male_COP_all,
          
          "Female COP cox" = cox_Female_COP_all, 
          "Female concordance" = concordance_Female_COP_all, 
          
          "data" = data)

write_xlsx(y, here::here("../COP Results_10_29_2021.xlsx"))