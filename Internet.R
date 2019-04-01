# Analyze the internet data of www.datadb.com 

# Import the dataset
library(readxl)
internet <- read_excel(file.choose())
str(internet)

# Qn1 - The team wants to analyze each variable of the data collected through data summarization
# to get a basic understanding of the dataset and to prepare for further analysis.
summary(internet)
# Bounces - min = 0, max = 30.000
# Exits - min = 0, max = 36
# From the result of std dataset, it is observed that the numerical data includes information related
# to min,max and mean.Categorical data like Continenet includes the number of times the category has been repeated
#in the ds. 

# Bounces - It represents the percentage of visitors who enter the site and "bounce" (leave the site) rather than
# continuing to view other pages within the same site
table(internet$Bounces)

# Exists - Exits: It represents the percentage of visitors to a site who actively click away to a different site 
# from a specific page, after possibly having visited any other page on the site
table(internet$Exits)

# Continet - Continent: It shows the continent from which the site has been accessed. 
internet$Continent <- as.factor(internet$Continent)
table(internet$Continent)

# Source group - It shows how the visitor has accessed the site. - Time on page: It shows how long the user
# has spent on that particular page of the website.
table(internet$Sourcegroup)
internet$Sourcegroup <- as.factor(internet$Sourcegroup)

# Unique page view: It represents the number of sessions during which that page was viewed one or more times
table(internet$Uniquepageviews)

# Visits: A visit counts all visitors, no matter how many times the same visitor may have been to your site
table(internet$Visits)

# Qn 2 - As mentioned earlier, a unique page view represents the number of sessions during which that page was viewed
# one or more times. A visit counts all instances, no matter how many times the same visitor may have been to your site.
# So the team needs to know whether the unique page view value depends on visits.

str(internet)
cor(internet$Uniquepageviews,internet$Visits)
anova_res <- aov(internet$Uniquepageviews~internet$Visits)
summary(anova_res)

# Conclusion - Since the correlation % is 81.4% and also from anova results, it shows that visit is a significant
# variable, we can prove that visit variable has a significant impact on Uniquepageviews.

# Qn3 - Find out the probable factors from the dataset, which could affect the exits.
# Exit Page Analysis is usually required to get an idea about why a user leaves the website for a session and moves
#on to another one. Please keep in mind that exits should not be confused with bounces.
# Finding relation with corrplot. Correlation only checks the numeric variables.

library(corrplot)
corrplot(cor(internet[,sapply(internet,is.numeric)]))

# Exit as dependent variable
anova_res2 <- aov(Exits ~., data = internet)
summary(anova_res2)
# Conclusion - From the anova results, we can infer that Bounces,Continet,Sourcegroup,Timeinpage,Uniquepageviews,
# are more significant with Exits. Vist has comparatively less significance.
# Hence we can say that exit from site is affected by the factors Bounces,Continet,Sourcegroup,Timeinpage,Uniquepageviews

# Qn4 - Every site wants to increase the time on page for a visitor. This increases the chances of the visitor
# understanding the site content better and hence there are more chances of a transaction taking place. 
# Find the variables which possibly have an effect on the time on page

# Timeinpage as dependent variable
anova_res3 <- aov(Timeinpage ~., data = internet)
summary(anova_res3)
# Conclusion - From anova results, we can infer that Bounces, Exits,Continent,Uniquepageviews and Visits
# have significance with Timeinpage. Only the variable Sourcegroup is not significant.

# Qn5 - A high bounce rate is a cause of alarm for websites which depend on visitor engagement. 
# Help the team in determining the factors that are impacting the bounce..
str(internet)
table(internet$Bounces)
internet$BouncesNew <- internet$Bounces*0.01
internet$Continent <- as.factor(internet$Continent)
internet$Sourcegroup <- as.factor(internet$Sourcegroup)
log_mod <- glm(BouncesNew ~ Timeinpage + Exits + Continent + Sourcegroup + Uniquepageviews + Visits, data = internet,
            family = "binomial")
summary(log_mod)
# Drop the insignifant variables
log_mod <- glm(BouncesNew ~ Timeinpage + Exits + Uniquepageviews + Visits, data = internet,
               family = "binomial")
summary(log_mod)

# Conclusion :
# pvalues of (Exits,Uniquepageviews,Visits ) <0.05
# As per the linear regression model "Exits,Uniquepageviews,Visits" are the variables impact the target variable


