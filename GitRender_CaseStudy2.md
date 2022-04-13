CaseStudy2DDS_HTML_to_render_on_Github
================
Tamas Toth
2022-04-16

 

#### Loading the necessary R libraries for the analysis

``` r
# Load the necessary libraries
library(knitr)
library(rmarkdown)
library(egg)
library(ggpubr)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(maps)
library(mapproj)
library(sf)
library(usmap)
library(urbnmapr)
library(tidyverse)
library(mice)
library(VIM)
library(lattice)
library(ggthemes)
library(e1071)
library(class)
library(caret)
library(stringr)
library(sjPlot)
library(data.table)
library(reshape2)
library(corrplot)
library(naivebayes)
library(car)
library(RColorBrewer)
```

``` r
# Turn off scientific notation
options(scipen = 100, digits = 4)
```

 

## **Background & Context**:

DDSAnalytics is an analytics company that specializes in talent
management solutions for Fortune 100 companies. There is a desire to
gain competitive advantage by leveraging Data Science to improve
actionable insight to talent management. The executive leadership
identified an opportunity to harness machine learning capabilities to
predict employee turnover. Further to this this they would like to see
Salary predictions for employees.

## **Objective**:

1.  Explore and visualize the dataset to provide job role specific
    trends.
2.  Build classification models to predict if an employee will leave the
    company or not. Use KNN and Naive Bayes models.
3.  Build regression model to predict salary for ONLY those employees
    who are still with the company using Multiple Linear Regression.
4.  Optimize the model using appropriate techniques
5.  Generate a set of insights and recommendations that will help the
    DDSAnalytics to to identify factors that lead to attrition.

### **Data Dictionary**:

> ##### -**ID** : int : Identification number in the Data Set
>
> ##### -**Age** : int : Employee’s age
>
> ##### -**Attrition** : chr : Yes/No - Employee left the company?
>
> ##### -**BusinessTravel** : chr : Frequency for business travel
>
> ##### -**DailyRate** : int : Daily Rate
>
> ##### -**Department** : chr : Which department the empoyee is working in
>
> ##### -**DistanceFromHome** : int : How far the employee lives from the work location
>
> ##### -**Education** : int : Level of education
>
> ##### -**EducationField** : chr : Filed of Education
>
> ##### -**EmployeeCount** : int : Employee count (all values are 1)
>
> ##### -**EmployeeNumber** : int : Employee Personal number
>
> ##### -**EnvironmentSatisfaction** : int : Environment Satisfaction
>
> ##### -**Gender** : chr : Gender
>
> ##### -**HourlyRate** : int : Hourly Rate
>
> ##### -**JobInvolvement** : int : Level the employee participates in the work
>
> ##### -**JobLevel** : int : Job level
>
> ##### -**JobRole** : chr : Role of the employee
>
> ##### -**JobSatisfaction** : int : Eployee job satisfaction level
>
> ##### -**MaritalStatus** : chr : Marital Status
>
> ##### -**MonthlyIncome** : int : Monthly Salary
>
> ##### -**MonthlyRate** : int : Monthly Rate includes what employer pays after the worker
>
> ##### -**NumCompaniesWorked** : int : Number of previous companies worked for
>
> ##### -**Over18** : chr : Everybody is over 18 in this dataset
>
> ##### -**OverTime** : chr : Employee overtime (yes/no)
>
> ##### -**PercentSalaryHike** : int : Salary increase in percentage
>
> ##### -**PerformanceRating** : int : Performance Rating
>
> ##### -**RelationshipSatisfaction**: int : How satisfied with the manager
>
> ##### -**StandardHours** : int : All values are 80 hours
>
> ##### -**StockOptionLevel** : int : Stock Option Level
>
> ##### -**TotalWorkingYears** : int : Total Number of years worked
>
> ##### -**TrainingTimesLastYear** : int : Number of trainings taken last year
>
> ##### -**WorkLifeBalance** : int : Level of work life balance
>
> ##### -**YearsAtCompany** : int : Years working for the current company
>
> ##### -**YearsInCurrentRole** : int : Years In Current Role
>
> ##### -**YearsSinceLastPromotion** : int : Years Since Last Promotion
>
> ##### -**YearsWithCurrManager** : int : Years With Current Manager

 

#### Read the data

``` r
#Read the data
setwd('/Users/ttoth76/Downloads/datasets')
tm = read.csv(file = 'CaseStudy2-data.csv',header = TRUE, sep = ",")
# take a sample of 15 from the dataframe
tm_sample = sample_n(tm, 5)
knitr::kable(tm_sample, "html")
```

<table>
<thead>
<tr>
<th style="text-align:right;">
ID
</th>
<th style="text-align:right;">
Age
</th>
<th style="text-align:left;">
Attrition
</th>
<th style="text-align:left;">
BusinessTravel
</th>
<th style="text-align:right;">
DailyRate
</th>
<th style="text-align:left;">
Department
</th>
<th style="text-align:right;">
DistanceFromHome
</th>
<th style="text-align:right;">
Education
</th>
<th style="text-align:left;">
EducationField
</th>
<th style="text-align:right;">
EmployeeCount
</th>
<th style="text-align:right;">
EmployeeNumber
</th>
<th style="text-align:right;">
EnvironmentSatisfaction
</th>
<th style="text-align:left;">
Gender
</th>
<th style="text-align:right;">
HourlyRate
</th>
<th style="text-align:right;">
JobInvolvement
</th>
<th style="text-align:right;">
JobLevel
</th>
<th style="text-align:left;">
JobRole
</th>
<th style="text-align:right;">
JobSatisfaction
</th>
<th style="text-align:left;">
MaritalStatus
</th>
<th style="text-align:right;">
MonthlyIncome
</th>
<th style="text-align:right;">
MonthlyRate
</th>
<th style="text-align:right;">
NumCompaniesWorked
</th>
<th style="text-align:left;">
Over18
</th>
<th style="text-align:left;">
OverTime
</th>
<th style="text-align:right;">
PercentSalaryHike
</th>
<th style="text-align:right;">
PerformanceRating
</th>
<th style="text-align:right;">
RelationshipSatisfaction
</th>
<th style="text-align:right;">
StandardHours
</th>
<th style="text-align:right;">
StockOptionLevel
</th>
<th style="text-align:right;">
TotalWorkingYears
</th>
<th style="text-align:right;">
TrainingTimesLastYear
</th>
<th style="text-align:right;">
WorkLifeBalance
</th>
<th style="text-align:right;">
YearsAtCompany
</th>
<th style="text-align:right;">
YearsInCurrentRole
</th>
<th style="text-align:right;">
YearsSinceLastPromotion
</th>
<th style="text-align:right;">
YearsWithCurrManager
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
320
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Travel_Rarely
</td>
<td style="text-align:right;">
265
</td>
<td style="text-align:left;">
Research & Development
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Life Sciences
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1269
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Female
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Research Scientist
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Single
</td>
<td style="text-align:right;">
2994
</td>
<td style="text-align:right;">
21221
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Travel_Rarely
</td>
<td style="text-align:right;">
189
</td>
<td style="text-align:left;">
Human Resources
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Human Resources
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1973
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Male
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Manager
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Married
</td>
<td style="text-align:right;">
19636
</td>
<td style="text-align:right;">
25811
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:right;">
466
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Travel_Rarely
</td>
<td style="text-align:right;">
371
</td>
<td style="text-align:left;">
Research & Development
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Life Sciences
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Male
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Research Scientist
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Single
</td>
<td style="text-align:right;">
3944
</td>
<td style="text-align:right;">
4306
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:right;">
627
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Travel_Rarely
</td>
<td style="text-align:right;">
1476
</td>
<td style="text-align:left;">
Sales
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Medical
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1445
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Female
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Sales Executive
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Married
</td>
<td style="text-align:right;">
4162
</td>
<td style="text-align:right;">
15211
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:right;">
852
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Travel_Rarely
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Research & Development
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Medical
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Male
</td>
<td style="text-align:right;">
83
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Laboratory Technician
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Single
</td>
<td style="text-align:right;">
2206
</td>
<td style="text-align:right;">
16117
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Y
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8
</td>
</tr>
</tbody>
</table>

 

#### Address the missing values in each column (NA as well as empty strings).

``` r
# Address the missing values in each column (NA as well as empty strings).
missing_df = as.data.frame(sapply(tm, function(x) sum(is.na(x))))
colnames(missing_df) = c("variable missing")
knitr::kable(missing_df, "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
variable missing
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ID
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Attrition
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
BusinessTravel
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DailyRate
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Department
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DistanceFromHome
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Education
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
EducationField
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
EmployeeCount
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
EmployeeNumber
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
EnvironmentSatisfaction
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gender
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
HourlyRate
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
JobInvolvement
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
JobLevel
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
JobRole
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
JobSatisfaction
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MaritalStatus
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MonthlyIncome
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MonthlyRate
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
NumCompaniesWorked
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Over18
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
OverTime
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
PercentSalaryHike
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
PerformanceRating
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
RelationshipSatisfaction
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
StandardHours
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
StockOptionLevel
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
TotalWorkingYears
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
TrainingTimesLastYear
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
WorkLifeBalance
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
YearsAtCompany
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
YearsInCurrentRole
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
YearsSinceLastPromotion
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
YearsWithCurrManager
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
empty_string_df = as.data.frame(sapply(tm, function(x) sum(x == "")))
colnames(empty_string_df) = c("variable empty")
knitr::kable(empty_string_df, "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
variable empty
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ID
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Attrition
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
BusinessTravel
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DailyRate
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Department
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DistanceFromHome
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Education
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
EducationField
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
EmployeeCount
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
EmployeeNumber
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
EnvironmentSatisfaction
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gender
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
HourlyRate
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
JobInvolvement
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
JobLevel
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
JobRole
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
JobSatisfaction
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MaritalStatus
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MonthlyIncome
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MonthlyRate
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
NumCompaniesWorked
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Over18
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
OverTime
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
PercentSalaryHike
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
PerformanceRating
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
RelationshipSatisfaction
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
StandardHours
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
StockOptionLevel
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
TotalWorkingYears
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
TrainingTimesLastYear
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
WorkLifeBalance
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
YearsAtCompany
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
YearsInCurrentRole
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
YearsSinceLastPromotion
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
YearsWithCurrManager
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

#### There are no missing values or empty strings in the dataset.

``` r
#set random seed
set.seed(329)
```

 

``` r
# Function to Identify different characteristics of the data frame 
# Getting a concise summary of the dataframe: str()
# Listing the column labels of the dataframe: colnames()
# Size of the dataset: dim()
# Checking for missing values isnull()
# Verify if there is any negative values in the dataset
dfinfo = function(df_name)
  {
  df_summary = str(df_name)
  df_colnames = colnames(df_name)
  df_dimensions = dim(df_name)
  df_na = sapply(df_name, function(x) sum(is.na(x)))
  df_white = sapply(df_name, function(x) sum(x == ""))
  df_neg = print(paste("Negative values in the Data Frame:", 
                       sapply(df_name, function(x) sum(x < 0))))
  outparam = list(df_summary, df_colnames, df_dimensions, df_na, df_white, df_neg)
  return (outparam)
}
```

``` r
# List of nominal categorical variables
nom_cat = noquote(unlist(tm %>% select(where(is.character)) %>% colnames()))
#convert the nominal categorical variables to factors
tm = as.data.frame(unclass(tm), stringsAsFactors = TRUE)
dfinfo(tm)
```

### Observations:

-   The dataset is comprised of 870 observations and 36 variables
-   There are numerical and categorical variables in the dataset
-   No missing values or empty strings in the dataset
-   No duplicated ‘EmployeeNumber’ records
-   ‘Attrition’ is the dependent variable - no missing value in the
    dependent variable
-   We need to predict Salary however there is no salary variable in the
    dataset but MonthlyIncome variable seems to be sufficient for this
    purpose.

#### **Categorical Ordinal variables**:

-   Education,
-   EnvironmentSatisfaction,
-   JobInvolvement,
-   JobLevel,
-   JobSatisfaction,
-   NumCompaniesWorked,
-   PerformanceRating,
-   RelationshipSatisfaction,
-   StockOptionLevel

 

#### Generate summary statistics

``` r
# Generate summary statistics
summary(tm)
```

    ##        ID           Age       Attrition           BusinessTravel   DailyRate   
    ##  Min.   :  1   Min.   :18.0   No :730   Non-Travel       : 94    Min.   : 103  
    ##  1st Qu.:218   1st Qu.:30.0   Yes:140   Travel_Frequently:158    1st Qu.: 472  
    ##  Median :436   Median :35.0             Travel_Rarely    :618    Median : 818  
    ##  Mean   :436   Mean   :36.8                                      Mean   : 815  
    ##  3rd Qu.:653   3rd Qu.:43.0                                      3rd Qu.:1166  
    ##  Max.   :870   Max.   :60.0                                      Max.   :1499  
    ##                                                                                
    ##                   Department  DistanceFromHome   Education  
    ##  Human Resources       : 35   Min.   : 1.00    Min.   :1.0  
    ##  Research & Development:562   1st Qu.: 2.00    1st Qu.:2.0  
    ##  Sales                 :273   Median : 7.00    Median :3.0  
    ##                               Mean   : 9.34    Mean   :2.9  
    ##                               3rd Qu.:14.00    3rd Qu.:4.0  
    ##                               Max.   :29.00    Max.   :5.0  
    ##                                                             
    ##           EducationField EmployeeCount EmployeeNumber EnvironmentSatisfaction
    ##  Human Resources : 15    Min.   :1     Min.   :   1   Min.   :1.0            
    ##  Life Sciences   :358    1st Qu.:1     1st Qu.: 477   1st Qu.:2.0            
    ##  Marketing       :100    Median :1     Median :1039   Median :3.0            
    ##  Medical         :270    Mean   :1     Mean   :1030   Mean   :2.7            
    ##  Other           : 52    3rd Qu.:1     3rd Qu.:1562   3rd Qu.:4.0            
    ##  Technical Degree: 75    Max.   :1     Max.   :2064   Max.   :4.0            
    ##                                                                              
    ##     Gender      HourlyRate    JobInvolvement    JobLevel   
    ##  Female:354   Min.   : 30.0   Min.   :1.00   Min.   :1.00  
    ##  Male  :516   1st Qu.: 48.0   1st Qu.:2.00   1st Qu.:1.00  
    ##               Median : 66.0   Median :3.00   Median :2.00  
    ##               Mean   : 65.6   Mean   :2.72   Mean   :2.04  
    ##               3rd Qu.: 83.0   3rd Qu.:3.00   3rd Qu.:3.00  
    ##               Max.   :100.0   Max.   :4.00   Max.   :5.00  
    ##                                                            
    ##                       JobRole    JobSatisfaction  MaritalStatus MonthlyIncome  
    ##  Sales Executive          :200   Min.   :1.00    Divorced:191   Min.   : 1081  
    ##  Research Scientist       :172   1st Qu.:2.00    Married :410   1st Qu.: 2840  
    ##  Laboratory Technician    :153   Median :3.00    Single  :269   Median : 4946  
    ##  Manufacturing Director   : 87   Mean   :2.71                   Mean   : 6390  
    ##  Healthcare Representative: 76   3rd Qu.:4.00                   3rd Qu.: 8182  
    ##  Sales Representative     : 53   Max.   :4.00                   Max.   :19999  
    ##  (Other)                  :129                                                 
    ##   MonthlyRate    NumCompaniesWorked Over18  OverTime  PercentSalaryHike
    ##  Min.   : 2094   Min.   :0.00       Y:870   No :618   Min.   :11.0     
    ##  1st Qu.: 8092   1st Qu.:1.00               Yes:252   1st Qu.:12.0     
    ##  Median :14074   Median :2.00                         Median :14.0     
    ##  Mean   :14326   Mean   :2.73                         Mean   :15.2     
    ##  3rd Qu.:20456   3rd Qu.:4.00                         3rd Qu.:18.0     
    ##  Max.   :26997   Max.   :9.00                         Max.   :25.0     
    ##                                                                        
    ##  PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel
    ##  Min.   :3.00      Min.   :1.00             Min.   :80    Min.   :0.000   
    ##  1st Qu.:3.00      1st Qu.:2.00             1st Qu.:80    1st Qu.:0.000   
    ##  Median :3.00      Median :3.00             Median :80    Median :1.000   
    ##  Mean   :3.15      Mean   :2.71             Mean   :80    Mean   :0.784   
    ##  3rd Qu.:3.00      3rd Qu.:4.00             3rd Qu.:80    3rd Qu.:1.000   
    ##  Max.   :4.00      Max.   :4.00             Max.   :80    Max.   :3.000   
    ##                                                                           
    ##  TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsAtCompany 
    ##  Min.   : 0        Min.   :0.00          Min.   :1.00    Min.   : 0.00  
    ##  1st Qu.: 6        1st Qu.:2.00          1st Qu.:2.00    1st Qu.: 3.00  
    ##  Median :10        Median :3.00          Median :3.00    Median : 5.00  
    ##  Mean   :11        Mean   :2.83          Mean   :2.78    Mean   : 6.96  
    ##  3rd Qu.:15        3rd Qu.:3.00          3rd Qu.:3.00    3rd Qu.:10.00  
    ##  Max.   :40        Max.   :6.00          Max.   :4.00    Max.   :40.00  
    ##                                                                         
    ##  YearsInCurrentRole YearsSinceLastPromotion YearsWithCurrManager
    ##  Min.   : 0.0       Min.   : 0.00           Min.   : 0.00       
    ##  1st Qu.: 2.0       1st Qu.: 0.00           1st Qu.: 2.00       
    ##  Median : 3.0       Median : 1.00           Median : 3.00       
    ##  Mean   : 4.2       Mean   : 2.17           Mean   : 4.14       
    ##  3rd Qu.: 7.0       3rd Qu.: 3.00           3rd Qu.: 7.00       
    ##  Max.   :18.0       Max.   :15.00           Max.   :17.00       
    ## 

### Observations:

-   The youngest employee is 18 and the oldest is 60. \~75% of the
    employees are between 18 and 43 years of age.
-   \~16% of the employees left the company. The attrition ratio is:
    140:870. This is a relatively balanced dataset for attrition
    prediction with \~16% of data containing sample of attrition.
-   Three level of Business Travels: Non-Travel, Travel Frequently and
    Travel Rarely. \~71% of the employees travel rarely.
-   The Daily rate ranges between 103 and 1499
-   There are three departments: Human Resources (35), R&D (562) and
    Sales (273). The most employees are working in R&D department.
-   Employees distance from home ranges between 1 to 29 units.
-   The median education level of the employees are 3.
-   Most of the employees are educated in Life Sciences followed by
    Medical. The least number of field studied is Human Resources.
-   There are more Male than Females in the company.
-   The median hourly rate is 66 while the min is 30 and the maximum is
    100.
-   There are 7 job roles. Most employees are Sales Executives followed
    by Research Scientists.
-   There are three marital categories Divorced, Married, Single. Most
    of the employees are married, followed by singles.
-   The Monthly income is a right skewed distribution (Mean\>Median).
    The minimum monthly income is 1081 and the maximum is 19999.
-   The min Monthly rate is 2094 and the max in 26997. The average
    monthly rate is 14326.
-   Diff between monthly income and monthly rate: the income is what the
    employee receives the rate includes all payments that the employer
    pays out after the employee monthly.
-   In average the employees work for at least 3 companies before.
-   All employees are over 18 years old. We can likely remove or just
    ignore this variable as there is no variance in it therefore would
    not make any difference to the model.
-   \~29% of the employees are working overtime.
-   Everybody received salary increase. The min increase was 11% and the
    max salary increase employee received is 25%. In average \~15%
    increase is what the employees received.
-   Standard hours are 80 so that likely will be not important for the
    model.
-   In average the employees have 10 years of work experience.
-   Last year in average employees took training 3 times. There were
    employees who did not take any training and employees who had 6.
-   Years At Company: This seems to be a right skewed distribution (Mean
    \> Median). This indicates that employees usually stays shorter
    period of time with the company. However there are employees with 40
    years with the company. Could be a unique case, maybe worth while
    investigating it. Also 75% of the employees are 0 to 10 years with
    the company so the long term retention of employees maybe a concern.
    -The Years in Current Role ranges between 0 and 18 years. Since the
    mean \> median it seems that most of the employees stay in their
    role for a shorter period of time.
-   Years Since Last Promotion: In average employees gets promoted every
    two years but there are employees with large number of years since
    they were promoted. Worth checking the reasons.
-   Employees in average stay with their managers for \~4 years.
-   Monthly Income will be our Dependent variable when predicting Salary

 

## Uni-variate analysis

``` r
#####################################################################################
#                               Uni-variate analysis                                #
#####################################################################################
# Let's plot the summary statistics
# Univariate analysis
num_discrete = c('Education','EnvironmentSatisfaction','JobInvolvement','JobLevel',
                  'JobSatisfaction','NumCompaniesWorked','PerformanceRating',
                  'RelationshipSatisfaction','StockOptionLevel', 'EmployeeCount', 'WorkLifeBalance', 'EmployeeNumber', 'ID', 'StandardHours')

num_exclude = c('EmployeeNumber', 'ID', 'StandardHours', 'EmployeeCount')

num_cols = tm %>% select(where(is.numeric)) %>% colnames()
num_cols_plots = noquote(unlist(num_cols[!( num_cols %in% num_discrete)]))
num_uni_plots = noquote(unlist(num_cols[!( num_cols %in% num_exclude)]))

nrows = length(num_cols_plots)

for (i in num_uni_plots)
{
box_p = tm %>%
  ggplot(aes(x="", y = .data[[i]])) +
  geom_boxplot(fill = "sandybrown", color = "black") + 
  coord_flip() + theme_classic() + xlab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylab(i)

hist_p = tm %>%
  ggplot() +
  geom_histogram(aes(x = .data[[i]], y = (..count..)/sum(..count..)),
                 position = "identity", binwidth = 1, 
                 fill = "sandybrown", color = "black") +
  ylab("Relative Frequency") +
  theme_classic() + xlab(i) + ggtitle(paste(i, "- Univariate Analysis")) + 
  theme(plot.title = element_text(hjust = 0.5))

egg::ggarrange(hist_p, box_p, heights = 2:1) 
}
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-1.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-2.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-3.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-4.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-5.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-6.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-7.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-8.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-9.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-10.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-11.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-12.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-13.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-14.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-15.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-16.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-17.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-18.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-19.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-20.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-21.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-22.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-9-23.png" angle=90 style="display: block; margin: auto;" />

### Observations:

-   Plotted the numerical variables using a histogram and a box plot.
-   The boxplot on top helps identifying extreme values.
-   Most variables are right skewed but based on CLT we can assume
    normality
-   We can observe some extreme values but after investigation I
    concluded that these are plausible data points.

 

### Categorical data plots

``` r
#####################################################################################
#                               Categorical data plots                              #
#####################################################################################

# drop "over18" variable
tm = select(tm, -c("Over18"))
num_ex = c('EmployeeNumber', 'ID', 'StandardHours', "Over18")
num_var = tm %>% select(where(is.numeric)) %>% colnames()
num_var_plots = noquote(unlist(num_var[!( num_var %in% num_ex)]))
cat_cols = tm %>% select(where(is.factor)) %>% colnames()
# Plot all categorical variables
for (c in cat_cols)
{
  cat_plot = tm %>% ggplot(aes(x= .data[[c]], group = 1)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent") +
    scale_y_continuous(labels = scales::percent) + theme(legend.position = "none") +
    ggtitle(paste(c, "Categorical Analysis")) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
    scale_fill_brewer(palette="Oranges")
    egg::ggarrange(cat_plot, ncol=2) 
}
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-10-1.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-10-2.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-10-3.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-10-4.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-10-5.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-10-6.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-10-7.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-10-8.png" angle=90 style="display: block; margin: auto;" />

### Observations:

-   16% of the total workforce has left the company already
-   71% of the employees rarely travel. 18.2% travels frequently.
-   65% of the employees are part of the R&D department and 31% is
    Sales. HR is only 4%.
-   41% of the employees are life scientist, 31% Medical and 11.5%
    Marketing.
-   Well balanced gender distribution. 59% Males and 41% Females.
-   Top three Job Roles are Sales Executive, Research Scientist and Lab.
    Technician.
-   47% of the employees are married, \~31% Single and 22% divorced.
-   71% of the employees don’t do overtime. The rest does.

 

### Staked % plots to better visualize the attrition ratio

``` r
#####################################################################################
#              Staked % plots to better visualize the attrition ratio               #
#####################################################################################

stack_plots = c('Education','EnvironmentSatisfaction','JobInvolvement','JobLevel',
                  'JobSatisfaction','NumCompaniesWorked','PerformanceRating',
                  'RelationshipSatisfaction','StockOptionLevel', 'EmployeeCount', 'WorkLifeBalance', "BusinessTravel", "Department", "EducationField", "Gender",        
"JobRole", "MaritalStatus", "OverTime")

for (c in stack_plots)
{
crosstab = table(tm[[c]], tm$Attrition)
cross_prop = round(prop.table(crosstab,1)*100,0)
tb.df = as.data.frame(cross_prop)
names(tb.df) <- c("V1", "Attrition", "Frequency")
stack = tb.df %>% ggplot() + aes(V1, Frequency, fill=Attrition) +
  geom_bar(stat="identity") +
  ylab("Relative frequencies") + xlab(c)+
  geom_text(aes(label=paste0(sprintf("%1.1f", Frequency),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle(paste("Proportion plot for Attrition vs", c)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_brewer(palette="Oranges")
egg::ggarrange(stack, ncol=2) 
}
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-1.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-2.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-3.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-4.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-5.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-6.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-7.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-8.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-9.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-10.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-11.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-12.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-13.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-14.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-15.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-16.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-17.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-11-18.png" angle=90 style="display: block; margin: auto;" />

### Observations:

#### The reason for this relative frequency plot is to reflect the true proportion of the resignations. e.g. Female resignations are 6% of the total number of employees. We may say that this is not very high. It is less then 40% of the total resignations so females tend to leave the company less often then males. This is not exactly the case here though. If we look into the resignations, proportionate to the number of females in the office than we will see that number is 15%. In other words the female resignations are very close to the male resignations and this can only be uncovered if we look into the relative frequency of each group vs attrition.

-   Top three resignations are from the lower educational levels
    relative to its population.
-   Those are resigning the most who are not satisfied with their
    environment but interesting is that the second largest percent of
    resignations come from those who are very satisfied with their
    environment.
-   Those are resigning the most who are not very involved with their
    jobs.
-   Lower job level employees tend to resign the most.
-   Lower job satisfaction employees resign the most.
-   Those who have worked for more then 4 companies are resigning the
    most or those who worked only for 1 company.
-   Male and Female employees are resigning at about the same.
-   Relationship with the manager is influencing resignations a bit but
    not significantly.
-   Those with stock option 0 and 3 are resigning the most.
-   As the work life balance increasing the resignations are decreasing.
-   The frequency of the travel is influencing the resignations. Those
    who travel a lot tend to resign the most.
-   Sales and HR departments are leading the number of resignations.
-   Performance rating is not influencing the resignations much.
-   45% of the Sales Representatives resigned, followed by HR (22%),
    Lab. tech (20%), Research Scientist (16%)
-   Single employees are resigning the most and divorced the least.
-   Over time is a big factor for resignation. 32% resigned who does
    overtime and only 10% of those who does not.

 

## Bi-variate analysis with dependent variable

``` r
#####################################################################################
#                     Bi-variate analysis with dependent variable                   #
#####################################################################################

for (i in num_cols_plots)
{
multibox = tm %>%
  ggplot(aes(x=Attrition, y = .data[[i]])) +
  geom_boxplot(fill = "sandybrown", color = "black") + 
  xlab("Attrition") +
  ylab(i) + stat_summary(fun=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  ggtitle(paste(i, "vs Attrition bi-variate analysis")) +
  theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_brewer(palette = "Oranges")  
egg::ggarrange(multibox, ncol=2)
}
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-1.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-2.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-3.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-4.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-5.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-6.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-7.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-8.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-9.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-10.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-11.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-12.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-12-13.png" angle=90 style="display: block; margin: auto;" />

### Observations:

-   The median age of those who resigned is lower.
-   Those who resigned are living further away from the office in
    average.
-   Median monthly income is lower for those who left the company.
-   The median salary increase is the same for those who left the
    company and stayed.
-   Those who have less work experience tend to resign more.
-   Training time is not a factor for resignation.
-   Who worked less for the company tend to resign more.
-   People who stay longer in their current role resign less.
-   Looks like promotion is not a factor for resignation.
-   Those who spent more time with their manager resign less.

 

### Bi-variate analysis with JobRole variable

``` r
#####################################################################################
#                     Bi-variate analysis with JobRole variable                     #
#####################################################################################
# Bi-variate analysis with JobRole variable plots  
for (i in num_uni_plots)
{
multibox = tm %>%
  ggplot(aes(x=JobRole, y = .data[[i]])) +
  geom_boxplot(fill = "sandybrown", color = "black") + 
  xlab("Job Role") +
  ylab(i) + stat_summary(fun=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  ggtitle(paste(i, "vs Job Role bi-variate analysis")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_brewer(palette = "Oranges")  
egg::ggarrange(multibox, ncol=2)
}
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-1.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-2.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-3.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-4.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-5.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-6.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-7.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-8.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-9.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-10.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-11.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-12.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-13.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-14.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-15.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-16.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-17.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-18.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-19.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-20.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-21.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-22.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-13-23.png" angle=90 style="display: block; margin: auto;" />

### Observations:

-   The youngest team is Sales Representatives based on median age.
-   The oldest is the managers and research directors followed by
    Healthcare reps based on median age.
-   Managers are living the closest to the office in median distance
    (\<5 miles).
-   Most of the employees are living between 5 and 10 miles.
-   The median monthly salary are very similar and the lowest for HR,
    Lab.Tech. Research Scientist and Sales Rep.
-   The median monthly salary are very similar and being the mid tier
    for Healthcare Reps., Manufacturing dir. and Sales Exec.
-   The highest paid job roles are Managers and Research Directors.
-   The median salary increase is well balanced between the job roles.
-   The median Education level is 3 across the job roles. There is a
    research director with a low level of education. It is not
    impossible therefore not considered as an outliar.
-   Environment satisfaction is at median level 3 for all job roles.
-   Median job involvement is 3 for all job roles.
-   The median job level is the lowest for HR, Lab. Tech. Research
    Scientist and Sales Rep.
-   Healthcare Reps, Manufacturing Dir., Sales Exec are on median job
    level 2.
-   The highest median job level is 4 for Managers and Research
    Directors
-   On average all job roles and satisfied equally.
-   The median number of jobs before is the highest for Research
    directors and Managers. The most inexperienced employees are Sales
    Reps., Research Scientist and Lab. Techs.
-   The median relationship satisfaction with the manager is equally 3
    for all role.
-   The median stock option levels are zero for HR and Sales Reps.
-   The most experience employees are the managers and Research
    directors in terms of number of years worked.
-   Work life balance is well balanced across job roles.
-   Management stays the longest with the company and Sales Reps stays
    the least.
-   Managers and directors are in the longest time in their roles.
-   Every role had a promotion in the last two years in median except
    the Managers.
-   In median, most of the roles are 5 years with the current manager
    except the managers and directors.

 

## Multi-variate analysis with dependent and JobRole variables

``` r
#####################################################################################
#             Multi-variate analysis with dependent and JobRole variables           #
#####################################################################################
for (i in num_cols_plots)
{
multibox = tm %>%
  ggplot(aes(x=JobRole, y = .data[[i]])) +
  geom_boxplot(aes(fill=Attrition)) + 
  xlab("Job Role") +
  ylab(i) + 
  ggtitle(paste(i, "vs Job Role vs Attrition multi-variate analysis")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Oranges")  
egg::ggarrange(multibox, ncol=2)
}
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-1.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-2.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-3.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-4.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-5.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-6.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-7.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-8.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-9.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-10.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-11.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-12.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-14-13.png" angle=90 style="display: block; margin: auto;" />

### Observations:

-   The median age for employees resigned from Sales Reps. and HR roles
    are below 30 years. The median age for resignations from other roles
    are between 30 and 40 years. The managers and directors are above a
    median age of 40 when it comes to resignation.
-   The median distance from the office for Healthcare reps, HR and
    Manufacturing director roles who resigned are more than 25 miles.
    Rest of the resignations happen below a median distance of 10 miles.
-   The median monthly income are in three bucket. Below 5000: HR, Lab.
    Tech. Research Scientist and Sales Reps. Between 5000 and 10000:
    Healthcare Reps., Manufacturing Dir., Sales Exec., Above 10000:
    Managers and Research director.
-   The Research Scientist who resigned had the highest median salary
    increase. For the rest of the roles we cannot observe significant
    difference.
-   Manufacturing director who had the least training last year
    resigned. Rest of the roles resigned at around the same number of
    training.
-   Managers with the highest median years at the company tend to
    resign. Most other resignations are below 10 years.
-   Managers with the highest median years in the current role tend to
    resign.
-   Managers in the current role under the same manager tend to resign
    more than other job roles.

 

### Employee profile who left the company

``` r
#####################################################################################
#                         Employee profile who left the company                     #
#####################################################################################
# Employee profile who left the company (filter attrition = YES)
# Job Role profile by Attrition
tmYES = tm[tm$Attrition == 'Yes',]
# Bi-variate analysis with JobRole variable plots  
for (i in num_cols_plots)
{
multibox = tmYES %>%
  ggplot(aes(x=JobRole, y = .data[[i]])) +
  geom_boxplot(fill = "sandybrown", color = "black") +  
  xlab("Job Role") +
  ylab(i) + stat_summary(fun=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  ggtitle(paste(i, "vs Job Role for those who resigned")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_brewer(palette = "Oranges")  
egg::ggarrange(multibox, ncol=2)
}
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-1.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-2.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-3.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-4.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-5.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-6.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-7.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-8.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-9.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-10.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-11.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-12.png" angle=90 style="display: block; margin: auto;" /><img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-15-13.png" angle=90 style="display: block; margin: auto;" />

### Observations:

-   Management resigns at higher age (\>40), other job roles are below
    40.
-   Those who resigned lived in a median distance of 20 miles or less
    from the company.
-   Most job roles who resigned had a median monthly salary below 10000
    except managers and Research directors.
-   Even though research scientists had a salary increase \> 15% they
    resigned.
-   Resignations happens for those who has less then a median 25 year of
    experience.
-   Most resignation are with a median years with the company of less
    than 10 years.
-   The median years of last promotion for those who resigned is less
    than 3 years.

 

### Data Validation

``` r
# Data Validation
a=0; b=0; c=0; d=0;
ifelse (tm$TotalWorkingYears < tm$YearsAtCompany, (a=a+1), ifelse (tm$TotalWorkingYears > tm$YearsAtCompany,a, a))
ifelse (tm$YearInCurrentRole > tm$YearsAtCompany, (b=b+1), ifelse (tm$YearInCurrentRole < tm$YearsAtCompany, b, b))
ifelse (tm$YearsSinceLastPromotion > tm$YearsAtCompany, (c=c+1), ifelse (tm$YearsSinceLastPromotion < tm$YearsAtCompany, c, c))
ifelse (tm$YearsWithCurrManager > tm$YearsAtCompany, (d=d+1), ifelse (tm$YearsWithCurrManager < tm$YearsAtCompany, d, d))
```

``` r
if ((a>0) | (b>0) | (c>0) | (d>0))
{
  cat("There is a data error")
  }else
{
    cat("No data error can be detected")
  }
```

    ## No data error can be detected

#### Veified if there is any unlikely scenario as follows. These would be incorrect records:

-   YearAtCompany \< YearInCurrentRole
-   TotalWorkingYears \< YearAtCompany
-   YearAtCompany \< YearsSinceLastPromotion
-   YearAtCompany \< YearsWithCurrentManager

 

### Zoomed-in Correlation Matrix

``` r
#####################################################################################
#                          Zoomed-in Correlation Matrix                             #
#####################################################################################

# Filter for data tob be included
num_incl = c('PerformanceRating', 'PercentSalaryHike', 
             'YearsInCurrentRole', 'YearsSinceLastPromotion',
             'YearsAtCompany','JobLevel',
             'TotalWorkingYears','MonthlyIncome',
             'Age')

tmcorr = tm[,num_incl]
corrplot(cor(tmcorr), method = 'square', order = 'AOE', addCoef.col = 'black', 
         cl.pos = 'n', col = COL2('BrBG'))
```

![](GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Observations:

-   Due to the large number of explanatory variables I zoomed in on the
    correlation matrix for visualization purposes.
-   We can observe some strong positive correlation between variables
    these will be important when building the model to avoid
    multicollinearity.

 

## Hypothesis Test for Feature Importance

``` r
#####################################################################################
#                      Hypothesis Test for Feature Importance                       #
#####################################################################################
var_exc = c('ID', 'EmployeeCount', 'EmployeeNumber', 'StandardHours', 'Over18')
num_tm_var = tm %>% select(where(is.numeric)) %>% colnames()
t_test_data = noquote(unlist(num_tm_var[!( num_tm_var %in% var_exc)]))

for (i in t_test_data)
{
varSel = t.test(tm[[i]] ~ Attrition, data = tm)
p = varSel$p.value
  if (p > 0.05)
  {
  print(paste("EXCLUDE:",i,"with p-value:", p))
    }else{
    print(paste("INCLUDE:",i,"with p-value:", p))}
}
```

    ## [1] "INCLUDE: Age with p-value: 0.0000504976363056325"
    ## [1] "EXCLUDE: DailyRate with p-value: 0.318874854465734"
    ## [1] "INCLUDE: DistanceFromHome with p-value: 0.0164051875698687"
    ## [1] "EXCLUDE: Education with p-value: 0.142131888189063"
    ## [1] "INCLUDE: EnvironmentSatisfaction with p-value: 0.0339727097336318"
    ## [1] "EXCLUDE: HourlyRate with p-value: 0.274479812097453"
    ## [1] "INCLUDE: JobInvolvement with p-value: 0.00000199729780321201"
    ## [1] "INCLUDE: JobLevel with p-value: 0.000000404185051444399"
    ## [1] "INCLUDE: JobSatisfaction with p-value: 0.0014972647186739"
    ## [1] "INCLUDE: MonthlyIncome with p-value: 0.000000241248750230928"
    ## [1] "EXCLUDE: MonthlyRate with p-value: 0.198094965180805"
    ## [1] "EXCLUDE: NumCompaniesWorked with p-value: 0.0978823461300551"
    ## [1] "EXCLUDE: PercentSalaryHike with p-value: 0.669229667435796"
    ## [1] "EXCLUDE: PerformanceRating with p-value: 0.661027834981479"
    ## [1] "EXCLUDE: RelationshipSatisfaction with p-value: 0.263966869253356"
    ## [1] "INCLUDE: StockOptionLevel with p-value: 0.0000386004250729309"
    ## [1] "INCLUDE: TotalWorkingYears with p-value: 0.000000659568228220838"
    ## [1] "EXCLUDE: TrainingTimesLastYear with p-value: 0.059483230057604"
    ## [1] "INCLUDE: WorkLifeBalance with p-value: 0.019008851910464"
    ## [1] "INCLUDE: YearsAtCompany with p-value: 0.000256302126976236"
    ## [1] "INCLUDE: YearsInCurrentRole with p-value: 0.00000152215212530173"
    ## [1] "EXCLUDE: YearsSinceLastPromotion with p-value: 0.898316528364275"
    ## [1] "INCLUDE: YearsWithCurrManager with p-value: 0.00000508422927849574"

### Observations:

-   Assumptions for a t-test has been plotted under the Uni-variate
    analysis. We can see that the distributions don’t look normal but we
    have enough samples so we can apply CLT therefore this is not a
    concern.
-   The standard deviations are not equal but t-test would be robust
    agiants it if the sample sizes are equal. Since the Attrition and No
    Attrition sample sizes are not equal we cannot assume that the
    standard deviations are equal. Due to this violation I ran Welch’s
    two sample t-test.
-   We assume independence. H0:μ_Attrition=μ_Not_Attrition
-   Ha:μ_Attrition≠μ_Not_Attrition
-   The test output above shows (incl. p-value) which variable should be
    included and excluded to the model.
-   Based on the hypothesis test if the p-value \> 0.05 then it means
    that there is not enough evidence to suggest that the “attrition”
    and “no attrition” groups are different therefore we should not
    include those variables into the model.

 

## Model Building

Defining the variables to include into the model based on the variable
selection. On top of that I have manually added and removed categorical
variables based on impact to the model performance.

``` r
#####################################################################################
#                                  Model Preparation                                #
#####################################################################################
# Defining the variables to include into the model based on the variable selection. On top of that I have manually added and removed categorical variables based on impact to the model performance.

incl_KNN= c('Attrition', 'Age', 'DistanceFromHome',  'JobInvolvement','JobLevel', 'EnvironmentSatisfaction',  'JobSatisfaction', 'MonthlyIncome', 'StockOptionLevel',  'TotalWorkingYears' ,'WorkLifeBalance', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsWithCurrManager', 'OverTime', 'EducationField')

incl_NB = c('Attrition', 'Age', 'DistanceFromHome',  'JobInvolvement','JobLevel', 'EnvironmentSatisfaction',  'JobSatisfaction', 'MonthlyIncome', 'StockOptionLevel',  'TotalWorkingYears' ,'WorkLifeBalance', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsWithCurrManager', 'OverTime', 'EducationField')

var_exc = c('ID', 'EmployeeCount', 'EmployeeNumber', 'StandardHours', 'Over18')
num_tm_var = tm %>% select(where(is.numeric)) %>% colnames()
num_mod_vars = noquote(unlist(num_tm_var[!( num_tm_var %in% var_exc)]))

# 5-fold cross validation
cv <- trainControl(
  method = "cv", 
  number = 5,
  savePredictions = TRUE
)
```

 

### Model building - KNN

``` r
#####################################################################################
#                                  Model building  - KNN                            #
#####################################################################################
set.seed(411)
KNN_tm = tm[,incl_KNN]
KNN_tm_all_num = KNN_tm %>% select(where(is.numeric))

caret::nearZeroVar(KNN_tm_all_num, saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)
```

    ## [1] rowname       freqRatio     percentUnique zeroVar       nzv          
    ## <0 rows> (or 0-length row.names)

``` r
cat("No variable with zero variance in the selected list of variables.") 
```

    ## No variable with zero variance in the selected list of variables.

``` r
# Scale the numerical variables as KNN is sensitive to that
KNN_tm_scale = KNN_tm %>% mutate_if(is.numeric, scale)

# 70/30 split of the data set to train the models:
TRAIN_KNN_tm_scale = sample(1:dim(KNN_tm_scale)[1], round(0.7*dim(KNN_tm_scale)[1]))
train = KNN_tm_scale[TRAIN_KNN_tm_scale,]
test = KNN_tm_scale[-TRAIN_KNN_tm_scale,]
train$Attrition = as.factor(train$Attrition)
test$Attrition = as.factor(test$Attrition)

# Search for optimal k
k_grid <- expand.grid(k = seq(3, 25, by = 2))

# Model Training
KNNM = train(
  Attrition ~.,
  data = train,
  method = "knn",
  tuneGrid = k_grid,
  trControl = cv
  )

# Predicting
prediction_train = predict(KNNM, train)
prediction_test = predict(KNNM, test)
```

### KNN performance on training set

``` r
# Scoring
confusionMatrix(train$Attrition, prediction_train, positive = 'Yes')
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  No Yes
    ##        No  505   6
    ##        Yes  70  28
    ##                                            
    ##                Accuracy : 0.875            
    ##                  95% CI : (0.846, 0.9)     
    ##     No Information Rate : 0.944            
    ##     P-Value [Acc > NIR] : 1                
    ##                                            
    ##                   Kappa : 0.372            
    ##                                            
    ##  Mcnemar's Test P-Value : 0.000000000000495
    ##                                            
    ##             Sensitivity : 0.8235           
    ##             Specificity : 0.8783           
    ##          Pos Pred Value : 0.2857           
    ##          Neg Pred Value : 0.9883           
    ##              Prevalence : 0.0558           
    ##          Detection Rate : 0.0460           
    ##    Detection Prevalence : 0.1609           
    ##       Balanced Accuracy : 0.8509           
    ##                                            
    ##        'Positive' Class : Yes              
    ## 

### KNN performance on validation set

``` r
confusionMatrix(test$Attrition, prediction_test, positive = 'Yes')
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  No Yes
    ##        No  216   3
    ##        Yes  35   7
    ##                                         
    ##                Accuracy : 0.854         
    ##                  95% CI : (0.806, 0.895)
    ##     No Information Rate : 0.962         
    ##     P-Value [Acc > NIR] : 1             
    ##                                         
    ##                   Kappa : 0.221         
    ##                                         
    ##  Mcnemar's Test P-Value : 0.000000493   
    ##                                         
    ##             Sensitivity : 0.7000        
    ##             Specificity : 0.8606        
    ##          Pos Pred Value : 0.1667        
    ##          Neg Pred Value : 0.9863        
    ##              Prevalence : 0.0383        
    ##          Detection Rate : 0.0268        
    ##    Detection Prevalence : 0.1609        
    ##       Balanced Accuracy : 0.7803        
    ##                                         
    ##        'Positive' Class : Yes           
    ## 

### Visualize ‘k’ and the most important features

``` r
# Visualize 'k' and the most important features
ggplot(KNNM) + ggtitle("Optimal k value for the highest accuracy") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
KNNvarImp = varImp(KNNM)
plot(KNNvarImp, top = 5, main='Top 5 reason of resignation (KNN)')
```

![](GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-23-2.png)<!-- -->

## Observations:

-   I split the data to train and test (used as validation) in a 70-30
    percent.
-   Created a grid for finding the optimal k-value.
-   Scaled the numerical variables as KNN is sensitive to non-normalized
    data
-   Used a k-fold cross validation to improve the training of the model.
-   As the result shows this model meets the 60% requirements for
    Sensitivity and Specificity.
-   Identified the top 5 most important variables using feature
    importance function in caret package. This result is backed by and
    matching with the result from the EDA above.

 

### Model building - naive Bayes

``` r
#####################################################################################
#                          Model building  - naive Bayes                            #
#####################################################################################
set.seed(411)
NB_tm = tm[,incl_NB]

# 70/30 split of the data set to train the models:
TRAIN_NB = sample(1:dim(NB_tm)[1], round(0.7*dim(NB_tm)[1]))
trainNB = NB_tm[TRAIN_NB,]
testNB = NB_tm[-TRAIN_NB,]
trainNB$Attrition = as.factor(trainNB$Attrition)
testNB$Attrition = as.factor(testNB$Attrition)

# Model Training
NB = train(
  Attrition ~.,
  data = trainNB,
  method = "naive_bayes",
  usepoisson = TRUE,
  trControl = cv)

# Predicting
prediction_train = predict(NB, trainNB)
prediction_test = predict(NB, testNB)
```

 

### Naive Bayes performance on training set

``` r
# Scoring
confusionMatrix(trainNB$Attrition, prediction_train, positive = 'Yes')
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  No Yes
    ##        No  509   2
    ##        Yes  83  15
    ##                                              
    ##                Accuracy : 0.86               
    ##                  95% CI : (0.83, 0.887)      
    ##     No Information Rate : 0.972              
    ##     P-Value [Acc > NIR] : 1                  
    ##                                              
    ##                   Kappa : 0.224              
    ##                                              
    ##  Mcnemar's Test P-Value : <0.0000000000000002
    ##                                              
    ##             Sensitivity : 0.8824             
    ##             Specificity : 0.8598             
    ##          Pos Pred Value : 0.1531             
    ##          Neg Pred Value : 0.9961             
    ##              Prevalence : 0.0279             
    ##          Detection Rate : 0.0246             
    ##    Detection Prevalence : 0.1609             
    ##       Balanced Accuracy : 0.8711             
    ##                                              
    ##        'Positive' Class : Yes                
    ## 

 

### Naive Bayes performance on validation set

``` r
confusionMatrix(testNB$Attrition, prediction_test, positive = 'Yes')
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  No Yes
    ##        No  218   1
    ##        Yes  39   3
    ##                                         
    ##                Accuracy : 0.847         
    ##                  95% CI : (0.797, 0.888)
    ##     No Information Rate : 0.985         
    ##     P-Value [Acc > NIR] : 1             
    ##                                         
    ##                   Kappa : 0.105         
    ##                                         
    ##  Mcnemar's Test P-Value : 0.00000000491 
    ##                                         
    ##             Sensitivity : 0.7500        
    ##             Specificity : 0.8482        
    ##          Pos Pred Value : 0.0714        
    ##          Neg Pred Value : 0.9954        
    ##              Prevalence : 0.0153        
    ##          Detection Rate : 0.0115        
    ##    Detection Prevalence : 0.1609        
    ##       Balanced Accuracy : 0.7991        
    ##                                         
    ##        'Positive' Class : Yes           
    ## 

 

### Visualize Naive Bayes most important features

``` r
# Visualize the most important features
NBvarImp = varImp(NB)
plot(NBvarImp, top = 5, main='Top 5 reason of resignation (Naive Bayes)')
```

![](GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

## Observations:

-   I split the data to train and test (used as validation) in a 70-30
    percent.
-   Used a k-fold cross validation to improve the training of the model.
-   As the result shows this model meets the 60% requirements for
    Sensitivity and Specificity.
-   Identified the top 5 most important variables using feature
    importance function in caret package. This result is backed by and
    matching with the result from the EDA above.

### Model comparision:

-   Both models are performing well however the Naive Bayes model
    outperforms the KNN model on the validation data set therefore I
    would recommend using this Naive Bayes model for predicting
    attrition.

 

### Model building - Linear Regression

Predicting Salary for employees. It only makes sense for those employees
who are still with the company so I have removed the employees who have
left the company from the dataset. Our dependent variable will be
MonthlyIncome. We will predict Monthly Salary with a multiple linear
regression model.

``` r
#####################################################################################
#                         Model building  - Linear Regression                       #
#####################################################################################
# Predicting Salary for employees. It only makes sense for those employees who are still with the company so I have removed the employees who have left the company from the dataset.
# Our dependent variable will be MonthlyIncome. We will predict Monthly Salary with a multiple linear regression model.

# Identify numeric variables which are linearly related to MonyhlyIncome
MLR_num_tm = tm[tm$Attrition=="No", num_mod_vars]
```

### Linear - Linear data

``` r
# Linear - Linear Model
pairs(MonthlyIncome~Age+DailyRate+DistanceFromHome+Education, data=MLR_num_tm, col="green")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-29-1.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~EnvironmentSatisfaction+HourlyRate+JobInvolvement+JobLevel, data=MLR_num_tm, col="green")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-29-2.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~JobSatisfaction+MonthlyRate+NumCompaniesWorked+PercentSalaryHike, data=MLR_num_tm, col="green")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-29-3.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears, data=MLR_num_tm, col="green")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-29-4.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole, data=MLR_num_tm, col="green")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-29-5.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~YearsSinceLastPromotion+YearsWithCurrManager, data=MLR_num_tm, col="green")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-29-6.png" angle=90 style="display: block; margin: auto;" />

 

### Log - Linear Transformation

``` r
# Log - Linear Model
LOG_num_tm = MLR_num_tm
LOG_num_tm$MonthlyIncome = log(MLR_num_tm$MonthlyIncome) 
pairs(MonthlyIncome~Age+DailyRate+DistanceFromHome+Education, data=MLR_num_tm, col="red")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-30-1.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~EnvironmentSatisfaction+HourlyRate+JobInvolvement+JobLevel, data=LOG_num_tm, col="red")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-30-2.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~JobSatisfaction+MonthlyRate+NumCompaniesWorked+PercentSalaryHike, data=LOG_num_tm, col="red")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-30-3.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears, data=LOG_num_tm, col="red")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-30-4.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole, data=LOG_num_tm, col="red")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-30-5.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~YearsSinceLastPromotion+YearsWithCurrManager, data=MLR_num_tm, col="red")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-30-6.png" angle=90 style="display: block; margin: auto;" />

 

### Log - Log Transformation

``` r
# Log - Log Model
# Just add 1 to values which are 0.
LOG_num_tm = MLR_num_tm
MLR_num_tm[MLR_num_tm==0] = 1
LOG_num_tm = log(MLR_num_tm)

pairs(MonthlyIncome~Age+DailyRate+DistanceFromHome+Education, data=MLR_num_tm, col="blue")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-31-1.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~EnvironmentSatisfaction+HourlyRate+JobInvolvement+JobLevel, data=LOG_num_tm, col="blue")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-31-2.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~JobSatisfaction+MonthlyRate+NumCompaniesWorked+PercentSalaryHike, data=LOG_num_tm, col="blue")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-31-3.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears, data=LOG_num_tm, col="blue")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-31-4.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole, data=LOG_num_tm, col="blue")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-31-5.png" angle=90 style="display: block; margin: auto;" />

``` r
pairs(MonthlyIncome~YearsSinceLastPromotion+YearsWithCurrManager, data=MLR_num_tm, col="blue")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-31-6.png" angle=90 style="display: block; margin: auto;" />

``` r
# Based on the pair plots we can observe linear relationship between MonthlyIncome and Age, JobLevel, TotalWorkingYears, YearAtCompany
```

## Observation:

Based on the pair plots the following variables have linear relationship
with MonthlyIncome:

-   Age,
-   JobLevel,
-   TotalWorkingYears,
-   YearAtCompany

therefore I will include these continuous variables to the Multiple
Linear Regression model.

### Check VIF for the selected numerical variables

``` r
# Check VIF for the selected variables
# Let's fit a temporary model
MLR = lm(MonthlyIncome ~ Age+JobLevel+TotalWorkingYears+YearsAtCompany, data = MLR_num_tm)

# sore the temp model
summary(MLR)
```

    ## 
    ## Call:
    ## lm(formula = MonthlyIncome ~ Age + JobLevel + TotalWorkingYears + 
    ##     YearsAtCompany, data = MLR_num_tm)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -5117   -913     51    751   3992 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)       -1766.93     266.20   -6.64       0.000000000063 ***
    ## Age                  -3.19       8.07   -0.40              0.69252    
    ## JobLevel           3748.81      75.97   49.35 < 0.0000000000000002 ***
    ## TotalWorkingYears    82.16      14.45    5.69       0.000000018843 ***
    ## YearsAtCompany      -40.86      11.33   -3.61              0.00033 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1400 on 725 degrees of freedom
    ## Multiple R-squared:  0.91,   Adjusted R-squared:  0.91 
    ## F-statistic: 1.84e+03 on 4 and 725 DF,  p-value: <0.0000000000000002

### Visualize VIF

``` r
MLR_VIF = vif(MLR)
barplot(MLR_VIF, main = 'VIF Values', horiz = TRUE, col="blue")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-33-1.png" angle=90 style="display: block; margin: auto;" />

#### There is no multicollinierity in the model (VIF \< 10) but based on the p-values Age is not significant (its slope is not different than zero) so we can remove it from the model.

 

### Let’s proceed with building and training the Multiple Linear Regression Model.

``` r
set.seed(411)
# 70/30 split of the data set to train the models:
TRAIN_MLR = sample(1:dim(MLR_num_tm)[1], round(0.7*dim(MLR_num_tm)[1]))
trainMLR = MLR_num_tm[TRAIN_MLR,]
testMLR = MLR_num_tm[-TRAIN_MLR,]

# Model Training
MLRT = train(
  MonthlyIncome ~ JobLevel+TotalWorkingYears+YearsAtCompany,
  data = trainMLR,
  method = "lm",
  trControl = cv)

# Predicting
train_pred = predict(MLRT, trainMLR)
valid_pred = predict(MLRT, testMLR)

# Scoring the final model on Training and Validation set
summary(MLRT$finalModel)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -4772   -895     69    758   4028 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)        -1968.6      139.7  -14.09 < 0.0000000000000002 ***
    ## JobLevel            3751.6       92.2   40.70 < 0.0000000000000002 ***
    ## TotalWorkingYears     80.2       14.5    5.53          0.000000052 ***
    ## YearsAtCompany       -27.8       13.4   -2.07                0.039 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1420 on 507 degrees of freedom
    ## Multiple R-squared:  0.909,  Adjusted R-squared:  0.909 
    ## F-statistic: 1.7e+03 on 3 and 507 DF,  p-value: <0.0000000000000002

``` r
residuals = resid(MLRT$finalModel)
postResample(pred = train_pred, obs = trainMLR$MonthlyIncome)
```

    ##      RMSE  Rsquared       MAE 
    ## 1419.3592    0.9095 1090.8591

``` r
postResample(pred = valid_pred, obs = testMLR$MonthlyIncome)
```

    ##      RMSE  Rsquared       MAE 
    ## 1355.9009    0.9127 1060.0776

### Checking Multiple Liner Regression model assumptions

``` r
# Checking model assumptions
fit = lm(MonthlyIncome ~ JobLevel+TotalWorkingYears+YearsAtCompany, trainMLR)
confint(fit)
```

    ##                      2.5 %    97.5 %
    ## (Intercept)       -2243.00 -1694.156
    ## JobLevel           3570.48  3932.655
    ## TotalWorkingYears    51.67   108.653
    ## YearsAtCompany      -54.15    -1.351

``` r
hist(residuals, main = "Histogram of Residuals")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-35-1.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-35-2.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit, which = 2)
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-35-3.png" angle=90 style="display: block; margin: auto;" />

``` r
plot(fit, which = 4)
```

<img src="GitRender_CaseStudy2_files/figure-gfm/unnamed-chunk-35-4.png" angle=90 style="display: block; margin: auto;" />

## Observations:

-   First I have verified which variables are linearly correlated with
    the response variable.
-   I log transformed the data and verified if it increases the linear
    relationship with our dependent variable.
-   Fit a temporary model then verified VIF for the variables included.
    There was no multicollinearity observed.
-   Removed Age from the model as its p-value suggested that it is no
    significant for the model performance.
-   Added categorical variable to the model (trial and error method
    since we did not have a large number of categorical variables).
-   I split the data to train and test (used as validation) in a 70-30
    percent.
-   Used a k-fold cross validation to improve the training of the model.
-   Finally verified the model assumptions and conculded that all are
    met.

 

## Recommendations:

1.  I suggest to use the Naive Bayes model to predict attrition as the
    model is performing well.
2.  Pay attention to employees working overtime as they tend to resign
    more frequently.
3.  Implement a program that will encourage younger employees to stay
    with the company as people with less then 10 years of work
    experience tend to leave the company more often.
4.  Salary in many cases is an important factor for attrition. Make sure
    that the salary of the employees stays competitive, even extending
    the Stock options will help.
5.  I also recommend using the Regression model for predicting monthly
    salary. This could help the company to retain employees and stay
    competitive on the job market by better planning salaries.
