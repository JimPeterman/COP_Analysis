
library(readxl)
library(dplyr)
library(survival)

# Creating Kaplan Meier curves to illustrate survival data from study.
# Graph the 1st and 3rd COP tertiles.

data <- read_xlsx(here::here("../CLEANED_COP_dataset_2_7_2022.xlsx"))


# Create groups for COP tertiles.
data <- data %>%
  mutate(COP_Tertile = ntile(COP, 3)) %>%
  mutate(COP_Tertile = if_else(COP_Tertile == 1, "Good", 
                               if_else(COP_Tertile == 2, "Ok", "Bad"))) 

df <- filter(data, COP_Tertile == "Good" | COP_Tertile == "Bad")


##########################################################################
# Create figure with both sexes.

# Create the plot.
all_plot <- survminer::ggsurvplot(
  fit = survfit(Surv(follow_up_yrs, mortality_status) ~ COP_Tertile, data=df), 
  # palette = c("red3", "darkgreen"),
  pval = T,
  conf.int = T,
  xlab = "Years", 
  ylab = "Overall Survival Probability",
  title = "Kaplan-Meier Curve for COP",
  legend = "right",
  legend.title = "COP Tertiles",
  legend.labs = c("High COP (Bad)", "Low COP (Good)"))

# Another option for creating the plot with ggplot:
# km_fit <- survfit(Surv(follow_up_yrs, mortality_status)~ COP_Tertile, data=df)
# ggplot2::autoplot(km_fit) +
#   labs(x="Years",
#        y="Survival Percentage") +
#   theme_bw() +
#   theme(panel.grid = element_blank()) 
  
##########################################################################
# Create figure for only males.
df_M <- filter(df, sex == "Male")

# Create the plot.
m_plot = survminer::ggsurvplot(
  fit = survfit(Surv(follow_up_yrs, mortality_status) ~ COP_Tertile, data=df_M), 
  # palette = "Pastel2",
  palette = c("salmon", "skyblue4"),
  pval = T,
  conf.int = T,
  xlab = "Years", 
  ylab = "Overall Survival Probability",
  title = "Kaplan-Meier Curve for COP In Males",
  legend = "right",
  legend.title = "COP Tertiles",
  legend.labs = c("High COP (Bad)", "Low COP (Good)"))


##########################################################################
# Create figure for only females.
df_F <- filter(df, sex == "Female")

# Create the plot.
f_plot = survminer::ggsurvplot(
  fit = survfit(Surv(follow_up_yrs, mortality_status) ~ COP_Tertile, data=df_F), 
  # palette = "Pastel1",
  palette = c("paleturquoise3", "darkslateblue"),
  pval = T,
  conf.int = T,
  xlab = "Years", 
  ylab = "Overall Survival Probability",
  title = "Kaplan-Meier Curve for COP In Females",
  legend = "right",
  legend.title = "COP Tertiles",
  legend.labs = c("High COP (Bad)", "Low COP (Good)"))


##########################################################################
# Combine the male and female plots into one figure.
plot_list <- c(m_plot[1], f_plot[1])
gridExtra::grid.arrange(grobs=plot_list, nrow=1)
