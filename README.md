# COP_Analysis
Dataset creation and analysis for a study on the ability of cardiorespiratory optimal point (COP) to predict all-cause mortality. 

### Dataset creation.
The data for this study comes from the BALL ST cohort. To create the dataset, I combined two datasets:
1) Dataset consisting of health screening information, summary data from a cardiopulmonary exercise test (VO2max test), and mortality status/cause.
2) Dataset consisting of data from each minute of a cardiopulmonary exercise test (VO2max test). This dataset is in the "long" format, which adds to the difficulty.

The dataset creation involves locating those individuals in which we have minute test data for as this is required to calculate the COP. Then, for each individual/test, find the COP (the lowest value for VE/VO2 across the minute test data). 
This process involves:
- Picking which VE value to use in determining COP (a measured or calculated VE - the more historical VE data was calculated).
- Calculating the VE/VO2 at each minute then choosing the lowest for the COP.

### Analysis.
The analysis examines the ability of COP to predict all-cause mortality. 
A similar analysis is used in other studies of the BALLST cohort so this was designed robustly for examining other variables of interest.

Four different models are examined:
1) Univariate (just COP)
2) Multivariate (COP, age, sex, and test date)
3) Multivariate (COP, age, sex, test date, and risk factors as binary variables (obesity, hypertension, dyslipidemia, diabetes, inactivity, smoking status))
4) Multivariate (COP, age, sex, test date, risk factors as binary variables (obesity, hypertension, dyslipidemia, diabetes, inactivity, smoking status), and VO2max)

The analysis explores relationships across the whole cohort as well as within men and women separately. 

Included in the analysis is the creation of summary tables of variables of interest as well as cohort characteristics. The summary tables are created in a way so that they can be copied and pasted from the resulting Excel file into tables in Word (for publication).
