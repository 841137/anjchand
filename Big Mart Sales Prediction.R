# Import the libraries

library(data.table)
library(dplyr)
library(ggplot2)
library(corrplot)

# Read the data
train = read.csv(file.choose())
test = read.csv(file.choose())

############################### Understanding the data##########################
dim(train)
dim(test)

# Features of data
names(train)
names(test)

#Structure of data
str(train)
str(test)

#Combine test and train
#test[,Item_Outlet_Sales :=NA] 
test$Item_Outlet_Sales = NA

combi = rbind(train,test)

####################Univariate analysis#####################################

# Since the target variable is continuous, lets plot it using histogram
hist(combi$Outlet_Sales)
ggplot(train) + aes(train$Item_Outlet_Sales) + geom_histogram(binwidth=100,
                          fill='Darkgreen') + xlab("Item Outlet Sales")             
# Its a right skewed histogram and would need some data transformation to treat the skewness

#Independent continuous variables
ggplot(combi) + aes(Item_Weight) + geom_histogram(binwidth =0.5,fill='blue')

ggplot(combi) + aes(Item_Visibility) + geom_histogram(binwidth =0.005,fill='blue')

ggplot(combi) + aes(Item_MRP) + geom_histogram(binwidth =1,fill='blue')
                                                            
# Conclusions
# 1. There is no clear pattern for Item_Weight
# 2. Item_Visibility is right skewed and it should be transformed to curb its skewness
# 3. Can see 4 different distributions Item_MRP

# Independent Variables(Categorical)
combi %>% 
  group_by(Item_Fat_Content) %>%
    summarise(Count = n())

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +
            geom_bar(aes(Item_Fat_Content,Count), stat='identity',fill='yellow')

# From the box plot its clear that the columns LF,low fat can be renamed as Low Fat
combi$Item_Fat_Content[combi$Item_Fat_Content=='LF'] = 'Low Fat'
combi$Item_Fat_Content[combi$Item_Fat_Content=='low fat'] = 'Low Fat'
combi$Item_Fat_Content[combi$Item_Fat_Content=='reg'] = 'Regular'

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +
  geom_bar(aes(Item_Fat_Content,Count), stat='identity',fill='yellow')

#Plot the categorical variable Item_Type
combi %>% group_by(Item_Type) %>% summarise(Count = n())
ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +
       geom_bar(aes(Item_Type,Count),stat='identity',fill='coral1') +
        geom_label(aes(Item_Type,Count,label=Count),vjust=0.5) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot for the categorical variable - Outlet_Identifier
combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())
ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +
  geom_bar(aes(Outlet_Identifier,Count),stat = 'identity',fill='coral1') +
    geom_label(aes(Outlet_Identifier,Count,label=Count)) +
      theme(axis.text.x = element_text(angle = 45,hjust = 1))

# Plot for the categorical variable - Outlet_Size
combi %>% group_by(Outlet_Size) %>% summarise(Count = n())
ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +
  geom_bar(aes(Outlet_Size,Count), stat = 'identity') +
    geom_label(aes(Outlet_Size,Count,label=Count))

# In "Outlet_Size", 4016 observations are blank, it should be handled via imputation

# Plot for the categorical variable = "Outlet_Location_Type"
combi %>% group_by(Outlet_Location_Type) %>% summarise(Count = n())
ggplot(combi %>% group_by(Outlet_Location_Type) %>% summarise(Count = n())) +
  geom_bar(aes(Outlet_Location_Type,Count),stat = 'identity',fill='green') +
    geom_label(aes(Outlet_Location_Type,Count,label = Count))

# Plot for the categorical variable = "Outlet_Type"
combi %>% group_by(Outlet_Type) %>% summarise(Count = n())
ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +
  geom_bar(aes(Outlet_Type,Count),stat = 'identity',fill ='red') +
    geom_label(aes(Outlet_Type,Count,label=Count)) +
      theme(axis.text.x = element_text(angle = 45,hjust = 1))

# Plot for the categorical variable = "Outlet_Establishment_Year"
combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())
ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +
    geom_bar(aes(factor(Outlet_Establishment_Year),Count),stat = 'identity') +
      geom_label(aes(factor(Outlet_Establishment_Year),Count,label=Count)) +
        theme(axis.text.x = element_text(size = 10))
# Conclusions
#Lesser number of observations in the data for the outlets established in the year 1998 as compared to the other years.
#Supermarket Type 1 seems to be the most popular category of Outlet_Type.

#################### Bivariate Analysis ###################################
# Scatter plot for continuous variables
# Violin plot for categ variables
nrow(train)
# train = combi[1:nrow(train)]

# Target variable Vs Independent Numerical Variables
# Target Vs Item_Weight
ggplot(train) + geom_point(aes(Item_Weight,Item_Outlet_Sales),
                           color='violet') +
                             theme(axis.title = element_text(size = 8.5))
# There are 1463 missing values

#Target Vs Item_Visibility
ggplot(train) + geom_point(aes(Item_Visibility,Item_Outlet_Sales),
                           color='violet') +
  theme(axis.title = element_text(size = 8.5))
# A right skewed curve

#Target Vs Item_MRP
ggplot(train) + geom_point(aes(Item_MRP,Item_Outlet_Sales),
                           color='violet') +
  theme(axis.title = element_text(size = 8.5))
#Observations
############################
#1. Item_Outlet_Sales is spread well across the entire range of the Item_Weight
#without any obvious pattern.
#2. In Item_Visibility vs Item_Outlet_Sales, there is a string of points at
# Item_Visibility = 0.0 which seems strange as item visibility cannot be completely zero. 
# We will take note of this issue and deal with it in the later stages.
#3. In the third plot of Item_MRP vs Item_Outlet_Sales, we can clearly see 
# four segments of prices that can be used in feature engineering to create a new variable.

# Target variable Vs Independent Categorical Variable
#Target Vs Item_Type
ggplot(train) + geom_violin(aes(Item_Type,Item_Outlet_Sales),fill='red') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),axis.title = element_text(size = 8.5))
#Conclusion : Distribution of Item_Outlet_Sales across the categories of 
#Item_Type is not is not very distinct.
  
#Target Vs Item_Fat_Content
ggplot(train) + geom_violin(aes(Item_Fat_Content,Item_Outlet_Sales), fill='red') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),axis.title = element_text(size = 8.5))    
#Conclusion : Distribution of Item_Outlet_Sales across the categories of 
#Item_Fat_Content is not is not very distinct.

#Target Vs Outlet_Identifier
ggplot(train) + geom_violin(aes(Outlet_Identifier,Item_Outlet_Sales), fill='red') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),axis.title = element_text(size = 8.5))    
#Conclusion : Distribution of OUT010 and 19 look similar and is different from
#the rest of the categories.

#Target Vs Outlet_Size
ggplot(train) + geom_violin(aes(Outlet_Size,Item_Outlet_Sales),fill='red')
#Conclusion - The distribution of "Small" Outlet_Size is almost similar as
#the blank category. It was notticed in the univariate analysis too.
#We can substitute the missing values of blank with Small.

#Target Vs Outlet_Location_Type
ggplot(train) + geom_violin(aes(Outlet_Location_Type,Item_Outlet_Sales),fill='red')
#Conclusion - Distribution of Tier1 and Tier2 look similar.

#Target Vs Outlet_Type
ggplot(train) + geom_violin(aes(Outlet_Type,Item_Outlet_Sales),fill='red') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Conclusion : Distribution of Grocery Store has less sales compared to the categories.

######################### Missing Value Treatment###########################
sapply(combi, function(df)
{
  sum(is.na(df)==T)
})
# Item_Weight has missing values (2439) and Item_Outlet_Sales has 5681 missing values.
#Or
sum(is.na(combi$Item_Weight))
# Missing data in Item_Outlet_Sales can be ignored since they belong to the test dataset

#Impute Item_Weight with the mean value.
combi$Item_Weight[is.na(combi$Item_Weight)] <- mean(combi$Item_Weight,na.rm = T)
sum(is.na(combi$Item_Weight))

#During the univariate and bivariate analysis, it is found that there are some
# columns where Item_Visibility is 0 and logically its incorrect.
#Lets replace it with the mean value of the Item_Identifier
index <- which(combi$Item_Visibility == 0)
for (i in index)
{
item = combi$Item_Identifier[i]
combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier==item],na.rm = T)
}

#Lets plot the visibility again. #### some issue here
ggplot(combi) + geom_histogram(aes(Item_Visibility),bins=100)

#################### Feature Engineering ####################################
# Create new feature for Item_Type
perishable = c('Bread','Breakfast','Dairy','Fruits and Vegetables','Meat','Seafood')
non_perishable = c('Baking Goods', 'Canned', 'Frozen Foods',
                   'Hard Drinks','Health and Hygiene', 'Household', 'Soft Drinks')

# Create a new feature Item_Type_New 
combi$Item_Type_New = ifelse(combi$Item_Type %in% perishable,"Perishable",
       ifelse(combi$Item_Type %in% non_perishable,"Non Perishable",
              "Not Sure"))
         
table(combi$Item_Type,substr(combi$Item_Identifier,1,2))
combi['Item_Category'] = substr(combi$Item_Identifier,1,2)
        
# Change the values of Item_Fat_Content wherever Item_category is 'NC' 
# because non-consumable items cannot have any fat content.
combi$Item_Fat_Content[combi$Item_Category == 'NC'] = 'Non-Editable'

#New column - Outlet_Years
combi['Outlet_Years'] = 2013 - combi$Outlet_Establishment_Year

#New column for price per unit weight
combi['price_per_unit_wt'] = combi$Item_MRP / combi$Item_Weight
combi['Item_MRP_Clusters'] = ifelse(combi$Item_MRP <70, "1st",
                                    ifelse(combi$Item_MRP >70 & combi$Item_MRP<140, "2nd",
                                           ifelse(combi$Item_MRP >140 & combi$Item_MRP <203,"3rd","4th")))
#Encoding categorical variables
#1 - Label Encoding : Mainly for ordinal numbers - convert to 0/1
#Convert Outlet_Size
combi$Outlet_Size = ifelse(combi$Outlet_Size == 'Small',0,
                           ifelse(combi$Outlet_Size == 'Medium',1,2))
#Convert Outlet_Location_Type
combi$Outlet_Location_Type = ifelse(combi$Outlet_Location_Type == 'Tier3',0,
                                    ifelse(combi$Outlet_Location_Type == 'Tier2',1,2))

#One hot encoding for the categorical variable
#each category of a categorical variable is converted into a new binary column (1/0).
library(caret)
data1 = combi[c(1,5,8)]
head(data1)
##data = combi[,-c(1,8,'Item_Identifier', 'Outlet_Establishment_Year', 'Item_Type')]

?dummyVars
ohe = dummyVars(~., data = data1,fullRank = T)
ohe_df = data.table(predict(ohe,data1))
combi = cbind(combi[,'Item_Identifier'],ohe_df)
View(combi)

######################## Data Pre-processing ###############################
# We need to deal with the skewness of data and the scale the numerical variables
# The variables Item_Visibility and price_unit_wt and highly skewed, so we need to
# take its log transformation.
combi$Item_Visibility = log(combi$Item_Visibility +1) # log +1 is to avoid the division by 0
combi$price_unit_wt = log(combi$price_unit_wt +1)

# Scale the numerica variables to make mean as 0, SD as 0 and scale of 0 to 1
#Get the index of the numeric variables
num_vars = which(sapply(combi, is.numeric))
#Names of the numeric variables
num_vars_names = names(num_vars)
?setdiff()

#not including the target variable in normalization
combi_numeric = combi[,setdiff(num_vars_names,combi$Item_Outlet_Sales),with=F]
combi_numeric
#Pre-process
prep_num = preProcess(combi_numeric,method = c('center','scale'))

#TRansform the dataset
combi_numeric_norm = predict(prep_num,combi_numeric)
combi_numeric_norm

#Remove the numeric independent variables
combi[,setdiff(num_vars_names,'Item_Output_Sales')] = NULL
combi = cbind(combi,combi_numeric_norm)
View(combi)

#Split the ds into train and test
train = combi[1:nrow(train)]
test = combi[nrow(train)+1 : nrow(combi)]

#Checking Correlation ############## Errored out
cor_train = cor(train[,-c('Item_Identifier')])
corrplot(cor_train,method = 'pie',type = 'lower',tl.cex = 0.9)

############### Model Building ############################################
#Linear Regression
lm_model = lm(train$Item_Outlet_Sales ~ ., data = train[, -c('Item Identifier')])

#Predict on test data
predict$Item_Outlet_Sales = predict(lm_model,test[, -c('Item Identifier')])

























