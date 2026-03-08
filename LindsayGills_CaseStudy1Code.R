# Case 1 Study
# By: Lindsay Gills
# Objective of Case Study: Identify the top three factors that contribute to turnover and
# build a model(s) to predict attrition and be able to measure the cost or savings impact of your model(s)
# Link to Presentation: https://drive.google.com/file/d/14igJKwoXIlWU43CH6wwDdcgvnpEiGLdi/view?usp=drive_link
# Load Packages
library(class)
library(caret)
library(e1071)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(GGally)

# Import CaseStudy1-data.csv Data CSV file
attrition = read.csv(file.choose("C:\\\\Users\\\\pearl\\\\OneDrive\\\\Documents\\\\GradSchoolDoingDataScienceClass\\\\Project1\\\\CaseStudy1-data (1).csv"),header = TRUE)
head(attrition)

# Summarizing Data
summary(attrition)

# Cleaning Data

# Identifying Null Values
is.na(attrition)
sum(is.na(attrition))
colSums(is.na(attrition))
which(!complete.cases(attrition))
# There are no missing values in the dataset.

# Omit Null Values
na.omit(attrition)

# Change categorical columns into factors
Gender <- as.factor(attrition$Gender)
EnvironmentSatisfaction <- as.factor(attrition$EnvironmentSatisfaction)
JobSatisfaction <- as.factor(attrition$JobSatisfaction)
PerformanceRating <- as.factor(attrition$PerformanceRating)
BusinessTravel <- as.factor(attrition$BusinessTravel)
Department <- as.factor(attrition$Department)
EducationField <- as.factor(attrition$EducationField)
JobInvolvment <- as.factor(attrition$JobInvolvement)
JobLevel <- as.factor(attrition$JobLevel)
JobRole <- as.factor(attrition$JobRole)
MaritalStatus <- as.factor(attrition$MaritalStatus)
RelationshipSatisfaction <- as.factor(attrition$RelationshipSatisfaction)
StockOptionLevel <- as.factor(attrition$StockOptionLevel)
WorkLifeBalance <- as.factor(attrition$WorkLifeBalance)

# Number of Employees who left and Stayed at the Company
table(attrition$Attrition)
attrition %>% count(Attrition) %>% ggplot(aes(x = Attrition, fill = Attrition, y = n)) + geom_col() + ggtitle("Number of Employees Who Stayed and Left The Company") +
  geom_text(aes(label = n, vjust = 0))
# 730 employees stayed at the company.
# 140 left the company.

# Percent of Employees who left and Stayed at the Company
prop.table(table(attrition$Attrition))
attrition %>% group_by(Attrition) %>% summarize(pct = round(n()/870 * 100,2)) %>%
  ggplot(aes(x = Attrition, fill = Attrition, y = pct)) + geom_col() + ggtitle("Percent of Employees Who Stayed and Left The Company") +
  geom_text(aes(label = pct, vjust = 0))

# About 83.91% of employees stayed at the company.
# About 16.09% of employees left the company.

# Exploring Data

# Pairplot 1
attrition %>%
  select(MonthlyIncome, WorkLifeBalance, Attrition) %>%
  ggpairs()
# pattern: No attriton has a higher monthly income

# Pairplot 2
attrition %>%
  select(MonthlyIncome, JobInvolvement, Attrition) %>%
  ggpairs()

# Bar Graph 1
ggplot(attrition, aes(x = YearsAtCompany, y = TotalWorkingYears)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Years at Company vs. Total Number of Years at Company",
    x = "Years at Company",
    y = "Total Number of Working Years at Company"
  ) +
  theme_minimal() 
# pattern: The graph is right skewed

# Bar Graph 2
ggplot(attrition, aes(x = DistanceFromHome, y = YearsAtCompany , fill = JobSatisfaction)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Distance From Home vs. Total Number of Years at Company",
    x = "Distance From Home",
    y = "Total Number of Working Years at Company"
  ) +
  facet_wrap(~JobSatisfaction)
theme_minimal() 
# pattern: There is a bimodel distritbution for the bottom right graph from
# a distance of 17 - 30 miles from home.

# Bar Graph 3
ggplot(attrition, aes(x = YearsSinceLastPromotion, y = YearsAtCompany , fill = JobSatisfaction)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Years Since Last Promotion vs. Total Number of Years at Company",
    x = "Years Since Last Promotion",
    y = "Total Number of Working Years at Company"
  ) +
  facet_wrap(~JobSatisfaction)
theme_minimal() 
# pattern: The data is more right skewed for a job satisfaction of 4.

# Bar Graph 4
ggplot(attrition, aes(x = WorkLifeBalance, y = YearsAtCompany, fill = JobSatisfaction)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Work Life Balance vs. Total Number of Years at Company",
    x = "Work Life Balance",
    y = "Total Number of Working Years at Company"
  ) +
  facet_wrap(~JobSatisfaction)
theme_minimal()
# pattern: A work life balance of 3 yields the highest number of working years at the company

# Bar Graph 5
ggplot(attrition, aes(x = JobLevel, y = YearsAtCompany)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Job Level vs. Number of Working Years at Company",
    x = "Job Level",
    y = "Total Number of Working Years at Company")
# pattern: Starting at job level of 2, it appears that the higher the job level, the employee worked less years worked at the company. 

# Bar Graph 6
ggplot(attrition, aes(x = YearsInCurrentRole, y = YearsAtCompany)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Years in Current Job Level vs. Number of Working Years at Company",
    x = "Years in Current Job Level",
    y = "Total Number of Working Years at Company")
# pattern: The graph is right skewed from 7-18 years in the current job level

# Bar Graph 7
ggplot(attrition, aes(x = NumCompaniesWorked, y = YearsAtCompany, fill = JobSatisfaction)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Number of Companies Worked vs. Number of Working Years at Company",
    x = "Number of Companies Worked",
    y = "Total Number of Working Years at Company") +
  facet_grid(~JobSatisfaction)
theme_minimal()
# pattern: The less companies a person works for, the higher the tenure at the company.

# Bar Graph 8
ggplot(attrition, aes(x = OverTime, y = YearsAtCompany)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Worked Overtime vs. Number of Working Years at Company",
    x = "Worked Overtime",
    y = "Total Number of Working Years at Company") +
  theme(legend.position = "none")
# pattern: Overtime hours influenced tenure. 

# Bar Graph 9
ggplot(attrition, aes(x = Department, y = YearsAtCompany)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Department vs. Number of Working Years at Company",
    x = "Department",
    y = "Total Number of Working Years at Company")
# pattern: The department, Research and Development, had the highest total of working years at the company.

# Bar Graph 10
ggplot(attrition, aes(x = JobRole, y = YearsAtCompany)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Department vs. Number of Working Years at Company",
    x = "Department",
    y = "Total Number of Working Years at Company") 
# pattern: HR and sales representatives have least tenure because both of these departments had a small sample.

# Count by Department
table(attrition$Department)
table(attrition$JobRole)

# Calculating Attrition Means for Each Factor

# Factor: Department
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>% 
  group_by(Department) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE)* 100, 2)) %>%
  ggplot(aes(y = Department, x = AttritionRate, fill = Department)) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Department") +
  geom_text(aes(label = AttritionRate, hjust = 1))

# Result: The mean attrition rate for HR is 0.171
# Result: The mean attrition rate for Research and Development is 0.133
# Result: The mean attrition rate for Sales is 0.216.

# Factor: Job Role
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>%
  group_by(JobRole) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE) * 100, 2)) %>%
ggplot(aes(y = JobRole, x = AttritionRate, fill = JobRole)) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Job Role") +
  geom_text(aes(label = AttritionRate, hjust = 1))

# Result: The mean attrition for healthcare representatives is 0.105
# Result: The mean attrition for HR is 0.222
# Result: The mean attrition for laboratory techinicians is 0.196
# Result: The mean attrition for managers is 0.0784
# Result: The mean attrition for manufactoring directors is 0.0230
# Result: The mean attrition for research directors is 0.0196
# Result: The mean attrition for research scientists is 0.186
# Result: The mean attrition for sales executive is 0.165
# Result: The mean attrition for sales representatives is 0.453

# Factor: Gender
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>%
  group_by(Gender) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE) * 100, 2)) %>%
ggplot(aes(y = Gender, x = AttritionRate, fill = Gender)) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Gender") +
  geom_text(aes(label = AttritionRate, hjust = 1))
# Result: The mean attrition is 0.150 for females.
# Result: The mean attrition is 0.169 for males.

# Factor: Marital Status
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>%
  group_by(MaritalStatus) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE) * 100, 2)) %>%
ggplot(aes(y = MaritalStatus, x = AttritionRate, fill = MaritalStatus)) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Marital Status") +
  geom_text(aes(label = AttritionRate, hjust = 1))
# Result: The mean attrition for divorced is 0.0628
# Result: The mean attrition for married is 0.141
# Result: The mean attrition for single is 0.260

# Factor: Job Satisfaction
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>%
  group_by(JobSatisfaction) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE) * 100, 2)) %>%
  ggplot(aes(y = factor(JobSatisfaction), x = AttritionRate, fill = factor(JobSatisfaction))) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Job Satisfaction Score") +
  geom_text(aes(label = AttritionRate), hjust = 1)
# Result: The mean attrition for a job satisfaction of 1 is 0.212
# Result: The mean attrition for a job satisfaction of 2 is 0.187
# Result: The mean attrition for a job satisfaction of 3 is 0.169
# Result: The mean attrition for a job satisfaction of 4 is 0.103

# Factor: Performance Rating
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>%
  group_by(PerformanceRating) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE) * 100, 2)) %>% 
ggplot(aes(y = factor(PerformanceRating), x = AttritionRate, fill = factor(PerformanceRating))) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Performance Rating") +
  geom_text(aes(label = AttritionRate), hjust = 1)
# Result: The mean attrition for a performance ranking of 3 is 0.159
# Result: The mean attrition for a performance ranking of 4 is 0.174

# Factor: Relationship Satisfaction
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>%
  group_by(RelationshipSatisfaction) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE) * 100, 2)) %>%
ggplot(aes(y = factor(RelationshipSatisfaction), x = AttritionRate, fill = factor(RelationshipSatisfaction))) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Relationship Satisfaction") +
  geom_text(aes(label = AttritionRate), hjust = 1)
# Result: The mean attrition for a job satisfaction of 1 is 0.201
# Result: The mean attrition for a job satisfaction of 2 is 0.158
# Result: The mean attrition for a job satisfaction of 3 is 0.159
# Result: The mean attrition for a job satisfaction of 4 is 0.159

# Factor: Job Involvement
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>%
  group_by(JobInvolvement) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE) * 100, 2)) %>%
ggplot(aes(y = factor(JobInvolvement), x = AttritionRate, fill = factor(JobInvolvement))) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Job Involvement") +
  geom_text(aes(label = AttritionRate), hjust = 1)
# Result: The mean attrition for a job involvement score of 1 is 0.468
# Result: The mean attrition for a job involvement score of 2 is 0.193
# Result: The mean attrition for a job involvement score of 3 is 0.130
# Result: The mean attrition for a job involvement score of 4 is 0.0864
# Job involvement is big factor for attrition.

# Factor: Business Travel
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>%
  group_by(BusinessTravel) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE)* 100, 2)) %>%
ggplot(aes(y = factor(BusinessTravel), x = AttritionRate, fill = factor(BusinessTravel))) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Business Travel") +
  geom_text(aes(label = AttritionRate), hjust = 1)
# Result: The mean attrition for non-travel is 0.117
# Result: The mean attrition for travel frequently is 0.222
# Result: The mean attrition for travel rarely is 0.152

# Factor: Education
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>%
  group_by(Education) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE) * 100, 2)) %>%
ggplot(aes(y = factor(Education), x = AttritionRate, fill = factor(Education))) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Education") +
  geom_text(aes(label = AttritionRate), hjust = 1)
# Result: The mean attrition for an education level of 1 is 0.184
# Result: The mean attrition for an education level of 2 is 0.176
# Result: The mean attrition for an education level of 3 is 0.170
# Result: The mean attrition for an education level of 4 is 0.133
# Result: The mean attrition for an education level of 5 is 0.115

# Factor: Environment Satisfaction
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>%
  group_by(EnvironmentSatisfaction) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE) * 100, 2)) %>%
ggplot(aes(y = factor(EnvironmentSatisfaction), x = AttritionRate, fill = factor(EnvironmentSatisfaction))) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Environment Satisfaction") +
  geom_text(aes(label = AttritionRate), hjust = 1)
# Result: The mean attrition for an environmental satisfaction score of 1 is 0.244
# Result: The mean attrition for an environmental satisfaction score of 2 is 0.135
# Result: The mean attrition for an environmental satisfaction score of 3 is 0.136
# Result: The mean attrition for an environmental satisfaction score of 4 is 0.149

# Factor: OverTime
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>%
  group_by(OverTime) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE) * 100, 2)) %>%
ggplot(aes(y = factor(OverTime), x = AttritionRate, fill = factor(OverTime))) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Over Time") +
  geom_text(aes(label = AttritionRate), hjust = 1)
# Result: The attrition rate for employees that worked overtime is 0.317
# Result: The attrition rate for employees that did not work overtime is 0.0971
# Overtime is big factor that affects attrition.

# Factor: Work Life Balance
attrition %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0)) %>%
  group_by(WorkLifeBalance) %>% summarize(AttritionRate = round(mean(Attrition, na.rm = TRUE) * 100, 2)) %>%
ggplot(aes(y = factor(WorkLifeBalance), x = AttritionRate, fill = factor(WorkLifeBalance))) + 
  geom_col() + guides(fill = FALSE) +
  ggtitle("Percent Attrition Rate By Work Life Balance") +
  geom_text(aes(label = AttritionRate), hjust = 1)
# Result: The mean attrition for a work life balance score of 1 is 0.354
# Result: The mean attrition for a work life balance score of 2 is 0.156
# Result: The mean attrition for a work life balance score of 3 is 0.150
# Result: The mean attrition for a work life balance score of 4 is 0.133

# T-Tests for Each Factor

# t-test 1
# Factor: Number of Companies Worked
# 95% confidence level, alpha = 0.05
t.test(NumCompaniesWorked ~ Attrition, data = attrition)
# Step 1: Null Hypothesis and Alternative Hypothesis
# Null Hypothesis: The mean number of companies an employee worked for who left the company
# is equal to the mean number of companies an employee worked for who did not leave the company.
# Alternative: Hypothesis: The mean number of companies an employee worked for who left the company
# is not equal to the mean number of companies an employee worked for who did not leave the company.
# Skip step 2: shading
# Step 3: Find the t-test statistic
# Results from Step 3:
# The t statistic is -1.6637. The degrees of freedom is 183.57. The p-value is 0.09788
# 95 % confidence interval is (-0.91435932, 0.07776441)
# The number of companies worked for sample mean of the employees who stayed is 2.660274
# The number of companies worked for sample mean of the employees who did not stay is 3.078571 
# Step 4: Find the p-value.
# The p-value is 0.09788 which is greater than the confidence level (alpha) 0.05.
# Step 5: Fail to reject the null hypothesis or reject the null hypothesis
# Fail to reject the null hypothesis.
# Step 6: Conclusion
# Conclusion: There is not sufficient evidence to suggest (p-value = 0.09788) that the 
# mean number of companies an employee worked for who left the company
# is not equal to the mean number of companies an employee worked for who did not leave the company.
# Not statistically significant


# ttest 2
# Factor: Monthly Rate
# 95% confidence level, alpha = 0.05
# Step 1: Null Hypothesis and Alternative Hypothesis
# Null Hypothesis: The mean monthly rate of an employee who left the company
# is equal to the mean monthly rate of an employee who did not leave the company.
# Alternative: Hypothesis: The mean monthly rate of an employee who left the company
# is not equal to the mean monthly rate of an employee who did not leave the company.
# Skip step 2: shading
# Step 3: Find the t-test statistic
t.test(MonthlyRate ~ Attrition, data = attrition)
# The t statistic is 1.2913 and the degrees of freedom is 198.38.
# The 95% confidence interval is (-440.5815, 2112.2567)
# Step 4: Find the p-value.
# The p-value is 0.1981 which is greater than the confidence level (alpha) 0.05.
# Step 5: Fail to reject the null hypothesis or reject the null hypothesis
# Fail to reject the null hypothesis.
# Step 6: Conclusion
# Conclusion: There is not sufficient evidence to suggest (p-value = 0.1981) that the 
# mean monthly rate for an employee who left the company
# is not equal to the mean monthly rate of an employee who did not leave the company.
# Not statistically significant


# ttest 3
# Factor Distance From Home
# 95% confidence level, alpha = 0.05
# Step 1: Null Hypothesis and Alternative Hypothesis
# Null Hypothesis: The mean commute of an employee who left the company
# is equal to the mean commute of an employee who did not leave the company.
# Alternative: Hypothesis: The mean commute of an employee who left the company
# is not equal to the mean commute of an employee who did not leave the company.
# Skip step 2: shading
# Step 3: Find the t-test statistic
t.test(DistanceFromHome ~ Attrition, data = attrition)
# The t statistic is -2.4218 and the degrees of freedom is 186.03.
# The 95% confidence interval is (-3.4992554, -0.3574961)
# Step 4: Find the p-value.
# The p-value is 0.01641 which is less than the confidence level (alpha) 0.05.
# Step 5: Fail to reject the null hypothesis or reject the null hypothesis
# Reject the null hypothesis.
# Step 6: Conclusion
# Conclusion: There is sufficient evidence to suggest (p-value = 0.01641) that the 
# mean commute for an employee who left the company
# is not equal to the mean commute of an employee who did not leave the company.
# statistically significant


# ttest 4
t.test(YearsSinceLastPromotion ~ Attrition, data = attrition)
# Factor Years Since Last Promotion
# 95% confidence level, alpha = 0.05
# Step 1: Null Hypothesis and Alternative Hypothesis
# Null Hypothesis: The mean years since last promotion of an employee who left the company
# is equal to the mean years since last promotion of an employee who did not leave the company.
# Alternative: Hypothesis: The mean years since last promotion of an employee who left the company
# is not equal to the mean years since last promotion of an employee who did not leave the company.
# Skip step 2: shading
# Step 3: Find the t-test statistic
# The t-statistic is 0.12796 and the degrees of freedom is 187.59. 
# The 95% confidence interval is(-0.5712911, 0.6505475)
# Step 4: Find the p-value.
# The p-value is 0.8983 which is greater than the confidence level (alpha) 0.05.
# Step 5: Fail to reject the null hypothesis or reject the null hypothesis
# Fail to reject the null hypothesis.
# Step 6: Conclusion
# Conclusion: There is not sufficient evidence to suggest (p-value = 0.8983) that the 
# mean years since last promotion for an employee who left the company
# is not equal to the mean years since last promotion of an employee who did not leave the company.
# Not statistically significant

# t-test 5
# Factor Total Working Years
# 95% confidence level, alpha = 0.05
# Step 1: Null Hypothesis and Alternative Hypothesis
# Null Hypothesis: The mean total number of working years of an employee who left the company
# is equal to the mean total number of working years of an employee who did not leave the company.
# Alternative: Hypothesis: The mean total number of working years of an employee who left the company
# is not equal to the mean total number of working years of an employee who did not leave the company.
# Skip step 2: shading
# Step 3: Find the t-test statistic
t.test(TotalWorkingYears ~ Attrition, data = attrition)
# The t-statistic is 5.1364. The degrees of freedom is 201.19.
# The 95% confidence interval is (2.105259,4.728792)
# Step 4: Find the p-value.
# The p-value is 6.596e-07 which is less than the confidence level (alpha) 0.05.
# Step 5: Fail to reject the null hypothesis or reject the null hypothesis
# Reject the null hypothesis.
# Step 6: Conclusion
# Conclusion: There is sufficient evidence to suggest (p-value = 6.596e-07) that the 
# mean total number of working years for an employee who left the company
# is not equal to the mean total number of working years of an employee who did not leave the company.
# statistically significant

# t-test 6
# Factor: Years In Current Role
# 95% confidence level, alpha = 0.05
# Step 1: Null Hypothesis and Alternative Hypothesis
# Null Hypothesis: The mean years in a current role of an employee who left the company
# is equal to the mean years in a current role of an employee who did not leave the company.
# Alternative: Hypothesis: The mean years in a current role of an employee who left the company
# is not equal to the mean years in a current role of an employee who did not leave the company.
# Skip step 2: shading
# Step 3: Find the t-test statistic
t.test(YearsInCurrentRole ~ Attrition, data = attrition)
# The t-statistic is 4.9513 and the degrees of freedom is 208.
# Step 4: Find the p-value
# The p-value is 1.522e-06 which is less than the confidence level (alpha) 0.05.
# Step 5: Fail to reject the null hypothesis or reject the null hypothesis
# Reject the null hypothesis.
# Step 6: Conclusion
# Conclusion: There is sufficient evidence to suggest (p-value = 1.522e-06) that the 
# mean years in current role for an employee who left the company
# is not equal to the mean years in a current role of an employee who did not leave the company.
# statistically significant

# t-test 7
t.test(Age ~ Attrition, data = attrition)
# Factor: Age
# 95% confidence level, alpha = 0.05
# Step 1: Null Hypothesis and Alternative Hypothesis
# Null Hypothesis: The mean age an employee who left the company
# is equal to the mean age of an employee who did not leave the company.
# Alternative: Hypothesis: The mean age of an employee who left the company
# is not equal to the mean age of an employee who did not leave the company.
# Skip step 2: shading
# Step 3: Find the t-test statistic
# The t-statistic is 4.1509 and the degrees of freedom is 184.91.
# The 95% confidence interval is (1.902905, 5.350324)
# Step 4: Find the p-value
# The p-value is 5.05e-05 which is less than the confidence level (alpha) 0.05.
# Step 5: Fail to reject the null hypothesis or reject the null hypothesis
# Reject the null hypothesis.
# Step 6: Conclusion
# Conclusion: There is sufficient evidence to suggest (p-value = 5.05e-05) that the 
# mean age of an employee who left the company
# is not equal to the mean age of an employee who did not leave the company.
# statistically significant

# t-test 8
t.test(PercentSalaryHike ~ Attrition, data = attrition)
# Factor: Percent Salary Hike
# 95% confidence level, alpha = 0.05
# Step 1: Null Hypothesis and Alternative Hypothesis
# Null Hypothesis: The mean percent salary hike of an employee who left the company
# is equal to the mean percent salary hike of an employee who did not leave the company.
# Alternative: Hypothesis: The mean percent salary hike of an employee who left the company
# is not equal to the mean percent salary hike of an employee who did not leave the company.
# Skip step 2: shading
# Step 3: Find the t-test statistic
# The t-statistic is -0.42788 and the degrees of freedom is 187.22
# The 95% confidence interval is (-0.8596808, 0.5532229)
# Step 4: Find the p-value
# The p-value is 0.6692 is greater than the confidence level (alpha) 0.05.
# Step 5: Fail to reject the null hypothesis or reject the null hypothesis
# Fail to reject the null hypothesis.
# Step 6: Conclusion
# Conclusion: There is not sufficient evidence to suggest (p-value = 0.6692) that the 
# mean percent salary hike for an employee who left the company
# is not equal to the mean percent salary hike of an employee who did not leave the company.
# Not statistically significant

# t-test 9
t.test(HourlyRate ~ Attrition, data = attrition)
# Factor: Hourly Rate
# 95% confidence level, alpha = 0.05
# Step 1: Null Hypothesis and Alternative Hypothesis
# Null Hypothesis: The mean hourly rate of an employee who left the company
# is equal to the mean hourly rate of an employee who did not leave the company.
# Alternative: Hypothesis: The mean hourly rate of an employee who left the company
# is not equal to the mean hourly rate of an employee who did not leave the company.
# Skip step 2: shading
# Step 3: Find the t-test statistic
# The t-statistic is -1.0958 and the degrees of freedom is 199.1
# The 95% confidence interval is  (-5.602043, 1.599890)
# Step 4: Find the p-value
# The p-value is 0.2745 which is greater than the confidence level (alpha) 0.05.
# Step 5: Fail to reject the null hypothesis or reject the null hypothesis
# Fail to reject the null hypothesis.
# Step 6: Conclusion
# Conclusion: There is not sufficient evidence to suggest (p-value = 0.2745) that the 
# mean hourly rate of an employee who left the company
# is not equal to the mean hourly rate of an employee who did not leave the company.
# not statistically significant

# t-test 10
t.test(MonthlyIncome ~ Attrition, data = attrition)
# Factor: Monthly Income
# 95% confidence level, alpha = 0.05
# Step 1: Null Hypothesis and Alternative Hypothesis
# Null Hypothesis: The mean monthly income of an employee who left the company
# is equal to the mean monthly income an employee who did not leave the company.
# Alternative: Hypothesis: The mean monthly income of an employee who left the company
# is not equal to the mean monthly income of an employee who did not leave the company.
# Skip step 2: shading
# Step 3: Find the t-test statistic
# The t-statistic is 5.3249 and the degrees of freedom is 228.45
# The 95% confidence interval is (1220.382, 2654.047)
# Step 4: Find the p-value
# The p-value is 2.412e-07 which is less than the confidence level (alpha) 0.05.
# Step 5: Fail to reject the null hypothesis or reject the null hypothesis
# Reject the null hypothesis.
# Step 6: Conclusion
# Conclusion: There is sufficient evidence to suggest (p-value = 2.412e-07) that the 
# mean monthly income of an employee who left the company
# is not equal to the mean monthly income of an employee who did not leave the company.
# statistically significant

# t-test 11
t.test(TrainingTimesLastYear ~ Attrition, data = attrition)
# Factor: Training Times Last Year
# 95% confidence level, alpha = 0.05
# Step 1: Null Hypothesis and Alternative Hypothesis
# Null Hypothesis: The mean training times last year of an employee who left the company
# is equal to the mean training times last year of an employee who did not leave the company.
# Alternative: Hypothesis: The mean training times last year of an employee who left the company
# is not equal to the mean training times last year of an employee who did not leave the company.
# Skip step 2: shading
# Step 3: Find the t-test statistic
# The t-statistic is 1.8954. The degrees of freedom is 200.36.
# The confidence interval is(-0.00876413  0.44301071)
# Step 4: Find the p-value
# The p-value is 0.05948 which is greater than the confidence level (alpha) 0.05.
# Step 5: Fail to reject the null hypothesis or reject the null hypothesis
# Fail to reject the null hypothesis.
# Step 6: Conclusion
# Conclusion: There is not sufficient evidence to suggest (p-value = 0.05948) that the 
# mean training times last year of an employee who left the company
# is not equal to the mean training hours of an employee who did not leave the company.
# not statistically significant

# t-test 12
t.test(YearsWithCurrManager ~ Attrition, data = attrition)
# Factor: Years With Current Manager
# 95% confidence level, alpha = 0.05
# Step 1: Null Hypothesis and Alternative Hypothesis
# Null Hypothesis: The mean years with current manager of an employee who left the company
# is equal to the mean years with current manager of an employee who did not leave the company.
# Alternative: Hypothesis: The mean years with current manager of an employee who left the company
# is not equal to the mean years with current manager of an employee who did not leave the company.
# Skip step 2: shading
# Step 3: Find the t-test statistic
# The t-statistic is 4.6826. The degrees of freedom is 209.75.
# The 95% confidence interval is(0.8262439, 2.0277679)
# Step 4: Find the p-value
# The p-value is 5.084e-06 which is less than the confidence level (alpha) 0.05.
# Step 5: Fail to reject the null hypothesis or reject the null hypothesis
# Reject the null hypothesis.
# Step 6: Conclusion
# Conclusion: There is sufficient evidence to suggest (p-value = 5.084e-06) that the 
# mean years with current manager of an employee who left the company
# is not equal to the mean years with current manager of an employee who did not leave the company.
# not statistically significant

# Naive Bayes Theorem for Each Factor(s)

# Factor: Monthly Income
#Plot OverTime, Monthly Income vs. Attrition
attrition %>% ggplot(aes(x = OverTime , y = MonthlyIncome, col = Attrition)) + geom_point() + ggtitle("Attrition vs. OverTime and Monthly Income") + 
  xlab("OverTime")  + 
  geom_jitter()

# Predict Attrition probability for a monthly income of $5000 for working overtime and not working overtime
model = naiveBayes(attrition[,c("MonthlyIncome","OverTime")],attrition$Attrition)
preds = predict(model,data.frame(MonthlyIncome = c(5000,5000), OverTime = c("No", "Yes")),type = "raw")
predsDF = as.data.frame(preds)

#Factor: Distance From Home
# Plot OverTime, Distance from Home vs. Attrition
attrition %>% ggplot(aes(x = OverTime , y = DistanceFromHome, col = Attrition)) + geom_point() + ggtitle("Attrition vs. OverTime and Distance From Home") + 
  xlab("OverTime")  + 
  geom_jitter() 

# Predict Attrition probability for a commute of 2 miles working overtime and not working overtime.
model = naiveBayes(attrition[,c("DistanceFromHome","OverTime")],attrition$Attrition)
preds = predict(model,data.frame(DistanceFromHome = c(2,2), OverTime = c("No", "Yes")),type = "raw")
predsDF = as.data.frame(preds)

# Factors: OverTime and Total Working Years
# Plot OverTime, Total Working Years vs. Attrition
attrition %>% ggplot(aes(x = OverTime , y = TotalWorkingYears, col = Attrition)) + geom_point() + ggtitle("Attrition vs. OverTime and Total Working Years") + 
  xlab("OverTime")  + 
  geom_jitter() 

# Predict Attrition probability for total working years of 4 and not working overtime and working overtime.
model = naiveBayes(attrition[,c("TotalWorkingYears","OverTime")],attrition$Attrition)
preds = predict(model,data.frame(TotalWorkingYears = c(4,4), Attrition = c("No", "Yes")),type = "raw")
predsDF = as.data.frame(preds)

# Factors: OverTime and Years in Current Role
#Plot OverTime, Years in Current Role vs. Attrition
attrition %>% ggplot(aes(x = OverTime , y = YearsInCurrentRole, col = Attrition)) + geom_point() + ggtitle("Attrition vs. OverTime and Years in Current Role") + 
  xlab("OverTime")  + 
  geom_jitter()

# Predict Attrition probability for 3 years in current role and not working overtime and working overtime.
model = naiveBayes(attrition[,c("YearsInCurrentRole","OverTime")],attrition$Attrition)
preds = predict(model,data.frame(YearsInCurrentRole = c(3,3), Attrition = c("No", "Yes")),type = "raw")
predsDF = as.data.frame(preds)

# Factors: OverTime and Age
#Plot OverTime, Age vs. Attrition
attrition %>% ggplot(aes(x = OverTime , y = Age, col = Attrition)) + geom_point() + ggtitle("Attrition vs. OverTime and Age") + 
  xlab("OverTime")  + 
  geom_jitter()

# Predict Attrition probability for an age of 29 and not working overtime and working overtime.
model = naiveBayes(attrition[,c("Age","OverTime")],attrition$Attrition)
preds = predict(model,data.frame(Age = c(29,29), Attrition = c("No", "Yes")),type = "raw")
predsDF = as.data.frame(preds)

# Factors: OverTime and Age
#Plot OverTime, Age vs. Attrition
attrition %>% ggplot(aes(x = OverTime , y = Age, col = Attrition)) + geom_point() + ggtitle("Attrition vs. OverTime and Age") + 
  xlab("OverTime")  + 
  geom_jitter()

# Predict Attrition probability for an age of 29 and not working overtime and working overtime.
model = naiveBayes(attrition[,c("Age","OverTime")],attrition$Attrition)
preds = predict(model,data.frame(Age = c(29,29), Attrition = c("No", "Yes")),type = "raw")
predsDF = as.data.frame(preds)


# NB with only Monthly Income, Over Time, Years In Current Role, Job Involvement (Use this code to change seed and see variances of accuracy etc.)
set.seed(7)
trainIndices = sample(seq(1:length(attrition$Attrition)),round(.7*length(attrition$Attrition)))
trainattrition = attrition[trainIndices,]
testattrition = attrition[-trainIndices,]
model = naiveBayes(trainattrition[,c("MonthlyIncome","OverTime", "YearsInCurrentRole", "JobInvolvement")],trainattrition$Attrition)

CM = confusionMatrix(table(predict(model,testattrition[,c("MonthlyIncome","OverTime", "YearsInCurrentRole", "JobInvolvement")]),testattrition$Attrition), mode = "everything")
CM


# Find average accuracy etc. of 100 train / test splits
AccHolder = numeric(100)
SensHolder = numeric(100)
SpecHolder = numeric(100)

for (seed in 401:300)
{
  set.seed(seed)
  trainIndices = sample(seq(1:length(attrition$Attrition)),round(.7*length(attrition$Attrition)))
  trainattrition = attrition[trainIndices,]
  testattrition = attrition[-trainIndices,]
  model = naiveBayes(trainattrition[,c("MonthlyIncome","OverTime", "YearsInCurrentRole", "JobInvolvement")],trainattrition$Attrition)
  CM = confusionMatrix(table(predict(model,testattrition[,c("MonthlyIncome","OverTime", "YearsInCurrentRole", "JobInvolvement")]),testattrition$Attrition))
  AccHolder[seed-400] = CM$overall[1]
  SensHolder[seed-400] = CM$byClass[1]
  SpecHolder[seed-400] = CM$byClass[2]
}

mean(AccHolder)
#Standard Error of the Mean
sd(AccHolder)/sqrt(100) 
mean(SensHolder)
#Standard Error of the Mean
sd(SensHolder)/sqrt(100) 
mean(SpecHolder)
#Standard Error of the Mean
sd(SensHolder)/sqrt(100)


# Cost of Replacement and Cost of Incentives

# For models predicting cost of replacement, I need to mutate (add a new column) 
# that uses an employees salary based on the joblevel and multiplies the salary by the percent cost of replacement
# Entry level replacement cost: 30% - 50% of employee's annual salary
# Mid level employee replacement cost: 125% - 150% of employee's annual salary
# High level employee replacement cost: around 400% of employee's annual salary

# Incentives cost
# $200 per employee

attrition = attrition %>% mutate(AnnualSalary = attrition$MonthlyIncome * 12)
head(attrition)
EntryLevel = 0.4
MidLevel = 1.40
SeniorLevel = 4.00
attrition = attrition %>%
  mutate(
    CostofReplacement = case_when(
      JobLevel == 1 ~ AnnualSalary * EntryLevel,
      JobLevel == 2 ~ AnnualSalary * EntryLevel,
      JobLevel == 3 ~ AnnualSalary * MidLevel,
      JobLevel == 4 ~ AnnualSalary * SeniorLevel,
      JobLevel == 5 ~ AnnualSalary * SeniorLevel,
      TRUE ~ AnnualSalary * EntryLevel
    )
  )
print(attrition$CostofReplacement)


# Naive Bayes Classifier Final Model
threshold = 0.2
attrition$Attrition = if_else(attrition$Attrition == "Yes" | attrition$Attrition == 1, 1, 0)
attrition$OverTime = factor(attrition$OverTime)
set.seed(7)
trainIndices = sample(seq(1:length(attrition$Attrition)),round(.7*length(attrition$Attrition)))
trainattrition = attrition[trainIndices,]
testattrition = attrition[-trainIndices,]
model = naiveBayes(trainattrition[,c("MonthlyIncome","OverTime", "YearsInCurrentRole", "JobInvolvement", "YearsWithCurrManager", "YearsAtCompany", "TotalWorkingYears", "Age", "Gender")],trainattrition$Attrition)
probs = predict(model,testattrition[,c("MonthlyIncome","OverTime", "YearsInCurrentRole", "JobInvolvement", "YearsWithCurrManager", "YearsAtCompany", "TotalWorkingYears", "Age", "Gender")], type = "raw")[,2]
predictions = if_else(probs > threshold, 1, 0 )
CM = confusionMatrix(table(predictions, testattrition$Attrition), mode = "everything")
CM
cost = sum(predictions) * 200
savings = sum((predictions == 1 & testattrition$Attrition == 1) * testattrition$CostofReplacement)
savings - cost

probs= predict(model,attrition[,c("MonthlyIncome","OverTime", "YearsInCurrentRole", "JobInvolvement", "YearsWithCurrManager", "YearsAtCompany", "TotalWorkingYears")], type = "raw")[,2]
predictions = if_else(probs > threshold, 1, 0 )
CM = confusionMatrix(table(predictions, attrition$Attrition), mode = "everything")
CM
cost = sum(predictions) * 200
savings = sum((predictions == 1 & attrition$Attrition == 1) * attrition$CostofReplacement)
savings - cost
# Net savings = savings from employees who did not leave - cost of program

# KNN Classifier
# k = 7
set.seed(7)
trainIndices = sample(1:dim(attrition)[1],round(0.7* dim(attrition)[1]))
attrition$Female = if_else(attrition$Gender == "Female", 1, 0)
train = attrition[trainIndices,]
test = attrition[-trainIndices,]
threshold = 0.2
classifications = 1-attr(knn(train[,c("MonthlyIncome", "YearsInCurrentRole", "Attrition", "TotalWorkingYears", "YearsAtCompany", "YearsWithCurrManager", "Female")],test[,c("MonthlyIncome", "YearsInCurrentRole", "Attrition", "TotalWorkingYears", "YearsAtCompany", "YearsWithCurrManager", "Female")],train$Attrition, prob = TRUE, k = 7), "prob")
predictions = ifelse(classifications > threshold, 1, 0)
table(predictions,test$Attrition)
confusionMatrix(table(predictions,test$Attrition))
cost = sum(predictions) * 200
savings = sum((predictions == 1 & test$Attrition == 1) * test$CostofReplacement)
savings - cost

# Find average accuracy etc. of 100 train / test splits
AccHolder = numeric(100)
SensHolder = numeric(100)
SpecHolder = numeric(100)

for (seed in 401:300)
{
  set.seed(seed)
  trainIndices = sample(seq(1:length(attrition$Attrition)),round(.7*length(attrition$Attrition)))
  trainattrition = attrition[trainIndices,]
  testattrition = attrition[-trainIndices,]
  classifications = knn(trainattrition[,c("MonthlyIncome","YearsInCurrentRole", "Attrition")], testattrition[,c("MonthlyIncome", "YearsInCurrentRole", "Attrition")],trainattrition$Attrition, prob = TRUE, k = 3)
  CM = confusionMatrix(table(classifications,testattrition$Attrition))
  AccHolder[seed-400] = CM$overall[1]
  SensHolder[seed-400] = CM$byClass[1]
  SpecHolder[seed-400] = CM$byClass[2]
}

mean(AccHolder)
#Standard Error of the Mean
sd(AccHolder)/sqrt(100) 
mean(SensHolder)
#Standard Error of the Mean
sd(SensHolder)/sqrt(100) 
mean(SpecHolder)
#Standard Error of the Mean
sd(SensHolder)/sqrt(100)


# Competition Data
competitionData = read.csv(file.choose("C:\\\\Users\\\\pearl\\\\OneDrive\\\\Documents\\\\GradSchoolDoingDataScienceClass\\\\Project1\\\\CaseStudy1CompSet No Attrition.csv"),header = TRUE)
head(attrition)

# Change columns into factors
Gender <- as.factor(competitionData$Gender)
EnvironmentSatisfaction <- as.factor(competitionData$EnvironmentSatisfaction)
JobSatisfaction <- as.factor(competitionData$JobSatisfaction)
PerformanceRating <- as.factor(competitionData$PerformanceRating)
BusinessTravel <- as.factor(competitionData$BusinessTravel)
Department <- as.factor(competitionData$Department)
EducationField <- as.factor(competitionData$EducationField)
JobInvolvment <- as.factor(competitionData$JobInvolvement)
JobLevel <- as.factor(competitionData$JobLevel)
JobRole <- as.factor(competitionData$JobRole)
MaritalStatus <- as.factor(competitionData$MaritalStatus)
RelationshipSatisfaction <- as.factor(competitionData$RelationshipSatisfaction)
StockOptionLevel <- as.factor(competitionData$StockOptionLevel)
WorkLifeBalance <- as.factor(competitionData$WorkLifeBalance)

probs = predict(model,competitionData, type ="raw")[,2]
prediction = if_else(probs > threshold, "Yes", "No")
submission = data.frame(ID = competitionData$ID, Attrition = prediction)
write.csv(submission, "Case1PredictionsGillsAttrition.csv")

# Part 3: Conclusion
# Conclusion: The 3 biggest factors that affect attrition are job involvement, overtime and 
# monthly income as shown using the Naive Bayes Classifier Model, KNN Classifier Model and t-tests. 
# Age was another important factor for attrition.
