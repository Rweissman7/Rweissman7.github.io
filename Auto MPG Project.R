#In this project I will investigate the impact of a number of automobile engine factors on the vehicle’s mpg.
#The data set auto-mpg.csv contains information for 398 different automobile models.
#Information regarding the number of cylinders, displacement, horsepower, weight, acceleration, model year, origin, and car name as well as mpg are contained in the file.
# Load the MASS package for backward and forward selection models

library(MASS)
# Load the dataset
MPG_Dataset <- read.csv("auto-mpg(1).csv")
#MPG_Dataset <- read.csv("C:\\Users\\ruchy\\OneDrive\\Documents\\auto-mpg(1).csv")
#MPG_Dataset <- read.csv(file.choose()")
#1. Perform exploratory data analysis, I am looking for output that summarizes the columns in this dataset (mean, number of rows, min, max, etc.). 
head(MPG_Dataset)
summary(MPG_Dataset)

#Performed some initial analysis and create visualizations using Tableau Public
#https://public.tableau.com/app/profile/ruchel.weissman/viz/ImpactsonMPG/ImpactsonMPG
#scatter plot if horsepower affects mpg- miles per gallon seems to be clear negative linear correlation  with more horsepower lowering mpg number of cylinders also seems to be related but might be correlation not necessarily causation 
#average mpg is highest is most for 4 cylinder car becomes less for more cylinder but 3 cylinder might be specialty cars which also have more factors causing less mpg 
#scatter plot acceleration does not seem to be influencing mpg
#scatter plot weight seems to have negative linear correlation as well
#sheet 4 cars seem to be getting more efficient on average, the general upward trend of later cars going more miles per gallon.
#displacement negative linear relationship higher displacement less mpg
#displacement weight and horsepower seem to have similar curves, try to see why and maybe they are correlated. 
#The origin has an impact with cars produced in continent 3 have the highest mpg while in continent 1 the lowest.

#convert horsepower to numeric
MPG_Dataset$horsepower <- as.numeric(MPG_Dataset$horsepower)
#divide the dataset for training and testing
Train_Data <- MPG_Dataset[1:300, ]
Test_Data <- MPG_Dataset[301:398, ]

# Create an empty data frame to store the log
model_log <- data.frame(
  Model_Name = character(),
  Multiple_R_Squared = numeric(),
  Adjusted_R_squared = numeric(),
  Linear_Regression_Equation = character()
)

#run anova to see which factors are statistically significant at alpha 0.05. Only acceleration is not significant
anov_test <- aov(mpg ~ ., data=Train_Data)
summary(anov_test)


#simple linear regression on cylinder
model_cylinder <- lm(mpg~cylinder,data=Train_Data)
model_cylinder_summary <- summary(model_cylinder)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "model_cylinder",
    Multiple_R_Squared = model_cylinder_summary$r.squared,
    Adjusted_R_squared = model_cylinder_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(model_cylinder)[1], "+", coef(model_cylinder)[2], "* cylinder")
  )
)


#simple linear regression on displacement
model_displacement <- lm(mpg~displacement,data=Train_Data)
model_displacement_summary <- summary(model_displacement)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "model_displacement",
    Multiple_R_Squared = model_displacement_summary$r.squared,
    Adjusted_R_squared = model_displacement_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(model_displacement)[1], "+", coef(model_displacement)[2], "* displacement")
  )
)

#simple linear regression on horsepower
model_horsepower <- lm(mpg~horsepower,data=Train_Data)
model_horsepower_summary <- summary(model_horsepower)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "model_horsepower",
    Multiple_R_Squared = model_horsepower_summary$r.squared,
    Adjusted_R_squared = model_horsepower_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(model_horsepower)[1], "+", coef(model_horsepower)[2], "* horsepower")
  )
)

#simple linear regression on weight
model_weight <- lm(mpg~weight,data=Train_Data)
model_weight_summary <- summary(model_weight)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "model_weight",
    Multiple_R_Squared = model_weight_summary$r.squared,
    Adjusted_R_squared = model_weight_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(model_weight)[1], "+", coef(model_weight)[2], "* weight")
  )
)

#simple linear regression on acceleration
model_acceleration <- lm(mpg~acceleration,data=Train_Data)
model_acceleration_summary <- summary(model_acceleration)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "model_acceleration",
    Multiple_R_Squared = model_acceleration_summary$r.squared,
    Adjusted_R_squared = model_acceleration_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(model_acceleration)[1], "+", coef(model_acceleration)[2], "* acceleration")
  )
)

#simple linear regression on model.year
model_model.year <- lm(mpg~model.year,data=Train_Data)
model_model.year_summary <- summary(model_model.year)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "model_model.year",
    Multiple_R_Squared = model_model.year_summary$r.squared,
    Adjusted_R_squared = model_model.year_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(model_model.year)[1], "+", coef(model_model.year)[2], "* model.year")
  )
)

#simple linear regression on origin
model_origin <- lm(mpg~origin,data=Train_Data)
model_origin_summary <- summary(model_origin)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "model_origin",
    Multiple_R_Squared = model_origin_summary$r.squared,
    Adjusted_R_squared = model_origin_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(model_origin)[1], "+", coef(model_origin)[2], "* Origin")
  )
)

#Build a multivariate linear regression model using all of the columns besides for car name with mpg as the target variable.
full_model <- lm(mpg~.-car.name,data=Train_Data)
full_model_summary <- summary(full_model)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "full_model",
    Multiple_R_Squared = full_model_summary$r.squared,
    Adjusted_R_squared = full_model_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(full_model)[1], "+(", coef(full_model)[2], "*",names(coef(full_model)[2]),")+(",coef(full_model)[3],
                                       "*",names(coef(full_model)[3]),")+(", coef(full_model)[4], "*",names(coef(full_model)[4]),")+(",
                                       coef(full_model)[5], "*",names(coef(full_model)[5]),")+(",coef(full_model)[6], "*",names(coef(full_model)[6]),")+(",
                                       coef(full_model)[7], "*",names(coef(full_model)[7]),")+(",coef(full_model)[8], "*",names(coef(full_model)[8]),")")))

print(full_model_summary)
#multiple regression model with only weight model year and origin as only those are statistically significant in the full model
reduced_model <- lm(mpg ~ weight + model.year + origin, data = Train_Data)
reduced_model_summary <- summary(reduced_model)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "reduced_model",
    Multiple_R_Squared = reduced_model_summary$r.squared,
    Adjusted_R_squared = reduced_model_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(reduced_model)[1], "+(", coef(reduced_model)[2], "*",names(coef(reduced_model)[2]),")+(",coef(reduced_model)[3],
                                       "*",names(coef(reduced_model)[3]),")+(", coef(reduced_model)[4],"*",names(coef(reduced_model)[4]),")")))

#second reduced multiple regression model with only weight and origin if want to take away year factor for making future predictions
reduced_model2 <- lm(mpg ~ weight + origin, data = Train_Data)
reduced_model2_summary <- summary(reduced_model)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "reduced_model2",
    Multiple_R_Squared = reduced_model2_summary$r.squared,
    Adjusted_R_squared = reduced_model2_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(reduced_model2)[1], "+(", coef(reduced_model2)[2], "*",names(coef(reduced_model2)[2]),")+(",coef(reduced_model2)[3],
                                       "*",names(coef(reduced_model2)[3]),")")))

#will try adding cylinder as well because that is one thing that seems to be affecting the intercept more than the others
reduced_model_cy <- lm(mpg ~ weight + origin, data = Train_Data)
reduced_model_cy_summary <- summary(reduced_model)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "reduced_model_cy",
    Multiple_R_Squared = reduced_model_cy_summary$r.squared,
    Adjusted_R_squared = reduced_model_cy_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(reduced_model_cy)[1], "+(", coef(reduced_model_cy)[2], "*",names(coef(reduced_model_cy)[2]),")+(",coef(reduced_model_cy)[3],
                                       "*",names(coef(reduced_model_cy)[3]),")+(",coef(reduced_model_cy)[4],
                                       "*",names(coef(reduced_model_cy)[4]),")")))
print(model_log)
# Backward Elimination
backward_model <- stepAIC(full_model, direction = "backward")
backward_model_summary <- summary(backward_model)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "backward_model",
    Multiple_R_Squared = backward_model_summary$r.squared,
    Adjusted_R_squared = backward_model_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(backward_model)[1], "+(", coef(backward_model)[2], "*",names(coef(backward_model)[2]),")+(",coef(backward_model)[3],
                                       "*",names(coef(backward_model)[3]),")+(", coef(backward_model)[4], "*",names(coef(backward_model)[4]),")+(",
                                       coef(backward_model)[5], "*",names(coef(backward_model)[5]),")+(",coef(backward_model)[6], "*",names(coef(backward_model)[6]),")+(",
                                       coef(backward_model)[7], "*",names(coef(backward_model)[7]),")")))


#backwards model removed acceleration now try the forward model
forward_model <- stepAIC(full_model, direction = "forward")
forward_model_summary <- summary(forward_model)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "forward_model",
    Multiple_R_Squared = forward_model_summary$r.squared,
    Adjusted_R_squared = forward_model_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(forward_model)[1], "+(", coef(forward_model)[2], "*",names(coef(forward_model)[2]),")+(",coef(forward_model)[3],
                                       "*",names(coef(forward_model)[3]),")+(", coef(forward_model)[4], "*",names(coef(forward_model)[4]),")+(",
                                       coef(forward_model)[5], "*",names(coef(forward_model)[5]),")+(",coef(forward_model)[6], "*",names(coef(forward_model)[6]),")+(",
                                       coef(forward_model)[7], "*",names(coef(forward_model)[7]),")+(",coef(forward_model)[8], "*",names(coef(forward_model)[8]),")")))


step_model <- stepAIC(full_model, direction = "both")
step_model_summary <- summary(step_model)
# Add a new row to the model_log data frame
model_log <- rbind(
  model_log,
  data.frame(
    Model_Name = "step_model",
    Multiple_R_Squared = step_model_summary$r.squared,
    Adjusted_R_squared = step_model_summary$adj.r.squared,
    Linear_Regression_Equation = paste("mpg =", coef(step_model)[1], "+(", coef(step_model)[2], "*",names(coef(step_model)[2]),")+(",coef(step_model)[3],
                                       "*",names(coef(step_model)[3]),")+(", coef(step_model)[4], "*",names(coef(step_model)[4]),")+(",
                                       coef(step_model)[5], "*",names(coef(step_model)[5]),")+(",coef(step_model)[6], "*",names(coef(step_model)[6]),")+(",
                                       coef(step_model)[7], "*",names(coef(step_model)[7]),")")))

print(model_log)

#forward model uses same columns as full and backwards same as step model.
#full model has has highest r squared however backwards has highest adjusted R squared
#but for sake of simplicity since all are a real minute difference will go with the reduced model since it has the fewest variables
#and its adjusted r squared is even better than the full model

#get predictions for test data set and compare them to actual outcome
predictions <- predict(reduced_model, newdata=Test_Data)
res <- Test_Data$mpg - predictions
#produce residual vs. fitted plot
plot(res, main = "Test Data Residuals Plot", xlab = "Observation", ylab = "Residuals")
#add a horizontal line at 0 
abline(0,0)

#Residual Plot on train data
predictions_tr <- predict(reduced_model, newdata=Train_Data)
res_tr <- Train_Data$mpg - predictions_tr
res_tr
#produce residual vs. fitted plot
plot(res_tr, main = "Training Data Residuals Plot", xlab = "Observation", ylab = "Residuals")
#add a horizontal line at 0 
abline(0,0)

#when run it to the train data it is even with few outliers should check out problem that is sorted according to year with older
#models only in test data set, will remove year from model to accurately predict future models when not
#sure what the change that year will be. 
#many are above 0 and not so many below so isn't fitting data so well might be skewed need to check it out
#histogram
hist(res, main = "Histogram of Residuals", xlab = "Residuals")

#quite a few outliers doesn't fit so well tried if model without year performs better but doesn't make a difference see if can add something else that 
#might influence like diesel cars as those with diesel in car name seem to have higher mpg

#tried other models on the test data but none seem to work much better than the reduced model
predictions2 <- predict(model_cylinder, newdata=Test_Data)
res <- Test_Data$mpg - predictions2
#produce residual vs. fitted plot
plot(res, main = "Test Data Model 2 Residuals Plot", xlab = "Observation", ylab = "Residuals")
#add a horizontal line at 0 
abline(0,0)
#find r squared of test data
test_fit <- lm(Test_Data$mpg~predictions2)
summary(test_fit)
#While the train data had a .82 r squared the test data only had a .52 r squared much lower.


  
