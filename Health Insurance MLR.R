#Multiple Linear Regression to determine causes behind Death Rates

getwd()

# Importing the dataset
dataset = read.csv('insurance.csv')
View(dataset)



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



# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$charges, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

View(training_set)
training_set$


# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = charges ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

View(test_set)

#Let's find out the more significant independent variables

summary(regressor)

y_pred
test_set


#Let's remove the independent variables that are have less or no statistical significance

#Sex of the person and region don't really play a significant role in determining charges


regressorUpdate = lm(formula = charges ~ age + bmi + children + smoker,
               data = training_set)
summary(regressorUpdate)

test_set

#Insurance Companies check following attributes of a customer in determining premium charges
#Age 
#Body Mass Index
#Smoking habit
