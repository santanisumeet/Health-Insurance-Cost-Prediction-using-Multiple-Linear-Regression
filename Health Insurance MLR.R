#Multiple Linear Regression to determine causes Health Insurance cost predictions

#R Code

getwd() 

#Step 1
# Importing the dataset
dataset = read.csv('insurance.csv')
View(dataset)


#Step 2
#Let's encode the categrocial data
dataset$smoker = factor(dataset$smoker,
                       levels = c('yes', 'no'),
                       labels = c(0, 1))

dataset$region

dataset$sex = factor(dataset$sex,
                     levels = c('female', 'male'),
                     labels = c(1,2))
dataset$region = factor(dataset$region,
                        levels = c('southeast', 'southwest', 'northeast', 'northwest'),
                        labels = c(1,2,3,4))


#Step 3

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$charges, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

View(training_set)
training_set$

#Step 4

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = charges ~ .,
               data = training_set)

#Step 5
# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

View(test_set)

#Let's find out the more significant independent variables

#Step 6
summary(regressor)
#Lower the p-value, higher the statistical significance. More Asterisk means more statistical significance 


y_pred
test_set


#Let's remove the independent variables that  have lesser or no statistical significance

#Due to a relatively higher p-value, we can conclude that sex of a person and region don't really play a significant role in determining his/her insurance charges

#Building a new regressor after removing statistically insignificant independent variables

regressorUpdate = lm(formula = charges ~ age + bmi + children + smoker,
               data = training_set)
summary(regressorUpdate)

test_set

#On comparing the predicted results and the data from the test_set, we can see more accuracy in predictions now

#Insurance Companies check following attributes of a customer in determining premium charges
# Age 
# Body Mass Index
# Smoking habit
