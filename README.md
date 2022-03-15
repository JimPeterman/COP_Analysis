# Examining the Prognostic Potential of COP

## Summary:
Cardiorespiratory optimal point (COP) is the minimum ventilatory equivalent for oxygen and can be determined during a submaximal incremental exercise test. This study investigated the relationship between COP and all-cause mortality in the BALL ST cohort with the goal of understanding the utility of COP assessments in clinical settings. **The results indicated COP is related to all-cause mortality in males but not females thus suggesting a determination of COP can have prognostic utility in healthy males aged 18–85 years old, which may be relevant when a maximal exercise test is not feasible or desirable.**

## The Rationale:
The American Heart Association [recommends]( https://pubmed.ncbi.nlm.nih.gov/27881567/) cardiopulmonary exercise testing in routine clinical practice to improve patient management and risk stratification. This testing though, requires an individual to perform a maximal effort and this maximal effort is speculated to be a reason why testing is not commonly performed in clinical practice.

Cardiorespiratory optimal point (COP) is defined as the minimum ventilatory equivalent for oxygen (ventilation divided by oxygen uptake [VE/VO<sub>2</sub>]) at any given minute during an incremental exercise test. COP reflects the optimal interaction between the respiratory and cardiovascular systems and follows a U-shaped curve during increasing exercise intensities (meaning COP occurs at submaximal intensities). Because COP can be assessed with a submaximal rather than a maximal exercise test, it could improve the uptake of exercise testing in clinical settings if it provides prognostic utility. 

Previous research highlights the potential prognostic utility of COP; however, additional research with larger sample sizes of males and (particularly) females is needed to substantiate this relationship in apparently healthy adults. Thus, **the purpose of this study was to evaluate the relationship between COP and all-cause mortality in a cohort of apparently healthy males and females.**


## The Final Product/Results:
Two scripts for R were used to create the dataset from the BALL ST cohort and then analyze the data. Prediction models using COP were related to mortality in males independent of traditional risk factors, including peak VO<sub>2</sub> (the traditional variable of interest from a maximal cardiopulmonary exercise test). There were sex differences in the predictive capability of COP though, as only the univariate COP model was significantly associated with mortality in females.

Further, the concordance index values from the models indicated the fully-adjusted COP models did not statistically differ compared to the fully-adjusted peak VO<sub>2</sub> models. Peak VO<sub>2</sub> also did not complement COP models and COP did not complement peak VO<sub>2</sub> models. These findings suggest determinations of COP alone could be beneficial when a determination of peak VO<sub>2</sub> from a maximal test is not feasible or desirable as peak VO<sub>2</sub> did not improve risk discrimination.

Following the data analysis, I summarized the findings of this study and submitted the scientific manuscript to the Journal of Cardiopulmonary Rehabilitation and Prevention. The manuscript was peer reviewed by experts in the field and accepted for publication (_link to be added once the manuscript is officially published_). 

## The Process:
### Dataset creation.
The data for this study comes from the BALL ST cohort (a database for a longitudinal fitness program). To create the dataset for analysis, I first had to wrangle, clean, analyze, and combine two datasets:
1) Dataset consisting of health screening information, summary data from a cardiopulmonary exercise test (cardiopulmonary exercise test), and mortality status/cause.
2) Dataset consisting of data from each minute of a cardiopulmonary exercise test. This dataset is in a "long" format and is not available for every participant. This dataset was also used to calculate the COP for each individual since the COP was not readily available in the database.

The data wrangling and cleaning relied heavily on the R package dplyr. Data were filtered to exclude extraneous values and to meet the inclusion criteria of the study (e.g., exercise test performed on a treadmill, aged 18-85 years old, at least 1 year of follow-up since the exercise test). 

A challenge associated with this dataset creation was calculating the COP since this value was not readily available in the database. The COP is defined as the lowest minute value for VE/ VO<sub>2</sub> and required:
- Picking which VE value to use in determining COP (a measured or calculated VE - the older test data in the database uses a calculation of VE rather than a measured VE and is found in different columns).
- Calculating the VE/ VO<sub>2</sub> at each minute of the test
- Selecting the lowest VE/ VO<sub>2</sub> as the COP and adding it to the main dataset.

During the peer review process, a reviewer asked for information regarding the proportion of individuals with metabolic syndrome. As such, coding was added to calculate whether each individual had metabolic syndrome.


### Analysis.
The analysis examines the ability of COP to predict all-cause mortality using Cox proportional hazard models as a means of assessing the prognostic potential of COP. Additionally, the script used for analysis outputs findings into tables that are saved as a .xlsx file to facilitate table preparation in manuscripts (i.e., allows for copy/pasting results into a Word file).

Similar analysis is used in other studies of the BALLST cohort, so this analysis was designed robustly for examining other variables of interest. Seven different models were examined:
1) Univariate (just COP [or peak VO<sub>2</sub>)])
2) Multivariate (COP [or peak VO<sub>2</sub>)], age, sex, and test date)
3) Multivariate (COP [or peak VO<sub>2</sub>)], age, sex, test date, and risk factors as binary variables (obesity, hypertension, dyslipidemia, diabetes, inactivity, smoking status))
4) Multivariate (COP, age, sex, test date, risk factors as binary variables (obesity, hypertension, dyslipidemia, diabetes, inactivity, smoking status), and peak VO<sub>2</sub>))

Schoenfeld residuals were examined to assess the assumption of proportional hazards underlying the Cox models. The relationships between the Schoenfeld residuals and the model covariates were either not statistically significant (_P_>0.05), indicating the proportional hazards assumption was met, or the residuals were plotted and the fit line of the plots fell within the confidence interval bounds for a horizontal linear trend suggesting statistically significant violations were not problematic

The analysis explored relationships across the whole cohort as well as within males and females separately. Comparisons of predictive capability were also made between COP and the traditional variable of interest from a maximal cardiopulmonary exercise test (peak VO<sub>2</sub>) by statistically comparing concordance values.

Descriptive statistics were also performed to summarize baseline characteristics of the participants. To test for differences between sexes and mortality status (survivor vs. deceased), independent samples t-tests and Chi-square tests were performed when appropriate. In cases when significant statistical results were found, significance symbols were added within the summary table to further facilitate the ease of copy/pasting results into publication-ready tables in Microsoft Word.
