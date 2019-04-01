# Analyze the Healthcare cost and Utilization in Wisconsin hospitals
# Import hospital datafile

hosp <- read.csv(file.choose())
str(hosp)
summary(hosp)
head(hosp)

# Question 1 : To record the patient statistics,the agency wants to find
# the age category of people who frequent the hospital and has the maximum expenditure.
hist(AGE, xlab = "AGE", ylab = "Freq")
hosp$AGE <- factor(hosp$AGE) # tapply works on vector, hence converting to factor.
summary(hosp$AGE)
str(hosp$AGE)

tapply(TOTCHG,AGE,sum) # Max expenditure : 678118.
# Using group by fn.
library(dplyr)
hosp %>%
  group_by(AGE) %>%
    summarise(count = n(), total_exp = sum(TOTCHG)) %>%
      arrange(desc(count,TOTCHG))

# Answer 1 : The age category of people who frequest the hospital and has max expenditure 
# is "0" year category,ie, infants.
# Age of "0" category has an expense of 678118.
#AGE   count     total_exp
#0    307        678118    

#********************************************************************************************************************

# Question 2 : In order of severity of the diagnosis and treatments and to find out
# the expensive treatments, the agency wants to find the diagnosis related group that has
# maximum hospitalization and expenditure.
APRDRG
table(APRDRG)
max(table(APRDRG))
which.max(table(APRDRG))

hosp$APRDRG <- factor(hosp$APRDRG)
tapply(TOTCHG,APRDRG,sum)

# Using group by fn.
library(dplyr)
hosp %>%
  group_by(APRDRG) %>%
  summarise(count = n(),totalstay = sum(LOS),total_exp = sum(TOTCHG)) %>%
  arrange(desc(count,APRDRG,totalstay))

# Answer 2 : From the analysis it is clear that the  diagnosis related group 640 has
# maximum entries of hospitalization and also has the highest hospitalization cost (437978)
#APRDRG count totalstay total_exp
# 640    267     652     437978

#********************************************************************************************************************

# Question 3 : To make sure that there is no malpractice, the agency needs to analyze
# if the race of the patient is related to the hospitalization costs.
hosp$RACE

# H0 : There is no relation with race and HospitalCosts
# H1 : Race and HospitalCosts are relalated

hosp$RACE <- as.factor(hosp$RACE)
summary(RACE)
str(hosp)

# Omit the NA's
hospna <- na.omit(hosp)
anova_mod <- aov(TOTCHG ~ RACE)
summary(anova_mod)

# Answer 3 : pvalue = 0.686 which is >0.05. Hence we cannot reject the null hypothesis.
# Hence we can prove that there is no relationship with race and hospitalization costs.

#********************************************************************************************************************

# Question 4 : To properly utilize the costs, the agency has to analyze the severity of
# the hospital costs by age and gender for proper allocation of resources. 

# Build the linear regression model.
hosp_mod <- lm(TOTCHG ~ AGE + FEMALE)
summary(hosp_mod)

# Answer 4 
# pvalue (Age) = 0.000763 < 0.05, Hence age is an important factor in the HospitalCosts
# pvalue (Gende) = 0.034967 < 0.05. Hence Gender is also an important factor in the hospital costs.

#********************************************************************************************************************

# Question 5 : Since the length of stay is the crucial factor for inpatients, the agency wants 
# to find if the length of stay can be predicted from age, gender, and race. 
str(hosp)
LOS

# Build regression model to find if LOS is related to age, gender and race.

hosp_mod2 <- lm(LOS ~ AGE + FEMALE + RACE)
summary(hosp_mod2)

hosp_mod2 <- lm(LOS ~ AGE + FEMALE)
summary(hosp_mod2)

hosp_mod2 <- lm(LOS ~ AGE + RACE)
summary(hosp_mod2)

hosp_mod2 <- lm(LOS ~ FEMALE + RACE)
summary(hosp_mod2)

hosp_mod2 <- lm(LOS ~ AGE)
summary(hosp_mod2)

hosp_mod2 <- lm(LOS ~ FEMALE)
summary(hosp_mod2)

hosp_mod2 <- lm(LOS ~ RACE)
summary(hosp_mod2)

# Answer5 : From the analysis, only intercept is having significance. Hence there is no linear relationship between
# given variables. Hence with age, gender, and race we can't predict length of stay.

#********************************************************************************************************************

# Question 6 : To perform a complete analysis, the agency wants to find the variable
# that mainly affects the hospital costs. 

# Build regression model to find out the factor that affect hospital costs.
str(hosp)
hosp$AGE <- as.integer(hosp$AGE)
hosp$APRDRG <- as.integer(hosp$APRDRG)
hosp$FEMALE <- as.factor(hosp$FEMALE)

hosp_mod3 <- lm(TOTCHG ~ AGE + FEMALE + LOS + RACE + APRDRG, data = hosp)
summary(hosp_mod3)

# Dropping the insignificant variables FEMALE and RACE and rebuild the model.
hosp_mod3 <- lm(TOTCHG ~ AGE + LOS + APRDRG, data = hosp)
summary(hosp_mod3)

# Answer 6 :
# pvalue (AGE) < 0.05, hence it is a significant variable to predict hospital cost
# pvalue (LOS) < 0.05, hence it is a significant variable to predict hospital cost
# pvalue (APRDRG) < 0.05, hence it is a significant variable to predict hospital cost

#From the analysis, the variables that affect hospital costs are AGE, Length of stay and All Patient Refined Diagnosis Related Groups
