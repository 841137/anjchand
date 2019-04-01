# Insurance project.
ins <- read.csv(file.choose())
str(ins)
summary(ins)

# Question 1 : The committee is interested to know each field of the data collected through descriptive analysis to 
# gain basic insights into the data set and to prepare for further analysis. 
summary(ins)
head(ins)
# conclusion
# From this you can understand the spread of data. 
# We can see that claims and payment also have null or zero values, 
# however the insured column does not have a zero value. 
# This specifies that there are few entries where the car has been insured for a given period of time. 
# However, no claim or payment has been made for that combination of car make, zone, and kilometres.

# Question 2 : The total value of payment by an insurance company is an important factor to be monitored.
# So the committee has decided to find whether this payment is related to number of claims and the number of insured
# policy years. They also want to visualize the results for better understanding. 

attach(ins)
cor(Payment,Claims) # 0.9954003
cor(Payment,Insured) # 0.933217
cor(Claims,Insured) # 0.9103478
plot(Payment,Claims)
plot(Payment,Insured)

# Conclusion
# The results show that Claims is 99 percent positively correlated with payment. 
# and insured is 93 percent positively correlated with payment. 
# The scatter plot shows that the relationship between the variables are strong 
# as there is a linear trend in the graph, that is, as the value of claims increases, 
# the payment value also increases and the same trend will occur for the insured and the payment.

# Question 3 - The committee wants to figure out the reasons for insurance payment increase and decrease. 
# So they have decided to find whether distance, location, bonus, make, and insured amount or claims are affecting
# the payment or all or some of these are affecting it. 

rm <- lm(Payment ~ Kilometres + Zone + Make + Insured + Claims + Bonus)
summary(rm)

# Drop the insignificant variables Make and Bonus
rm <- lm(Payment ~ Kilometres + Zone + Insured + Claims)
summary(rm)

# Conclusion - The variables distance, location, insured amount and claims are affecting the payment.

# Question 4 - The insurance company is planning to establish a new branch office, so they are interested to find
# at what location(zone), kilometer, and bonus level their insured amount, claims, and payment get increased. 
# (Hint: Aggregate Dataset)
str(ins)
table(Zone)
table(Kilometres)
table(Bonus)

head(ins[,c(5,6,7)])

# Finding best zone
tapply(Insured,Zone,mean)
tapply(Claims,Zone,mean)
tapply(Payment, Zone, mean)

# Result - Zone 4 (Rural areas in southern Sweden) has highest number of claims, insured years and payments.
# Finding optimum Kilometer point
table(Kilometres)
tapply(Insured,Kilometres,mean)
max(tapply(Insured,Kilometres,mean))

tapply(Claims,Kilometres,mean)
max(tapply(Claims,Kilometres,mean))

tapply(Payment,Kilometres,mean)
max(tapply(Payment,Kilometres,mean))

# Result : Kilometer = 2, KM range = 1000-15000 has highest number of claims and payments. 
# but it has bit less Insured amount compared to Kilometer group 1

# Finding optimum Bonus point
tapply(Insured,Bonus,mean)
max(tapply(Insured,Bonus,mean))

tapply(Claims, Bonus, mean)
max(tapply(Claims, Bonus, mean))

tapply(Payment, Bonus, mean)
max(tapply(Payment, Bonus, mean))

# Result - There is not much variation in groups of bonus except for 7 
# with unusually high number of insured years, claims, and payments.

# Question 5 - The committee wants to understand what affects their claim rates so as to decide the right premiums
# for a certain set of situations. Hence, they need to find whether the insured amount, zone, kilometer, bonus, 
# or make affects the claim rates and to what extent. 

reg_mod <- lm(Claims ~ Insured + Zone + Kilometres + Bonus + Make)
summary(reg_mod)
# Conclusion
# all the p values of independent variables, such as kilometres, zone, bonus, make, and insured 
# are highly significant and are making an impact on the claims.







