################################################################################
#
# The following libraries will need to be installed for this workshop.
# Uncomment the following lines if you don't already have them installed.
#
################################################################################

#install.packages('tidyverse')
#install.packages('MatchIt')
#install.packages('lmtest')
#install.packages('sandwich')
#install.packages('mice')

################################################################################
#
# In this example, we will illustrate the use of propensity score matching to
# to estimate the average treatment effect on the treated (ATT).
#
################################################################################
library(tidyverse)
currentDataset <- read_csv("https://raw.githubusercontent.com/gckc123/ExampleData/main/smoking_psyc_distress.csv")

#Since remoteness is a categorical variable with more than two categories. It is necessary to convert 
#it into a factor variable.
#For other categorical variable with only 2 levels, this is optional if the variable is coded as 0 and 1.
currentDataset$remoteness <- factor(currentDataset$remoteness, exclude = c("", NA))

#The MatchIt, lmtest and sandwich libraries are used.
library(MatchIt)
library(lmtest)
library(sandwich)

#Using the mathcit function from MatchIt to match each smoker with a non-smoker (1 to 1 matching) based on
#sex, indigeneity status, high school completion, marital status (partnered or not),
#region of residence (major cities, inner regional, outer regional), language background (English speaking Yes/No) 
#and risky alcohol drinking (Yes/No)
match_obj <- matchit(smoker ~ sex + indigeneity + high_school + partnered + remoteness + language + risky_alcohol + age,
                     data = currentDataset, method = "nearest", distance ="glm",
                     ratio = 1,
                     replace = FALSE)
summary(match_obj)

#platting the balance between smokers and non-smokers
plot(match_obj, type = "jitter", interactive = FALSE)
plot(summary(match_obj), abs = FALSE)

#Extract the matched data and save the data into the variable matched_data
matched_data <- match.data(match_obj)

#Run regression model with psychological distress as the outcome, and smoker as the only predictor
#We need to specify the weights - Matched participants have a weight of 1, unmatched participants 
res <- lm(psyc_distress ~ smoker, data = matched_data, weights = weights)

#Test the coefficient using cluster robust standard error
coeftest(res, vcov. = vcovCL, cluster = ~subclass)
#Calculate the confidence intervals based on cluster robust standard error
coefci(res, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

################################################################################
#
# In this example, we will illustrate the use of propensity score matching to
# to estimate the average treatment effect on the treated (ATT) in the presence
# of missing data.
#
################################################################################

library(tidyverse)
currentDataset <- 
  read_csv("https://raw.githubusercontent.com/gckc123/ExampleData/main/smoking_psyc_distress_with_missing.csv")

#Since remoteness is a categorical variable with more than two categories. It is necessary to convert 
#it into a factor variable.
#For other categorical variable with only 2 levels, this is option if the variable is coded as 0 and 1.
currentDataset$remoteness <- factor(currentDataset$remoteness, exclude = c("", NA))

#The mice, MatchIt, lmtest and sandwich libraries are used.
library(mice)
library(MatchIt)
library(lmtest)
library(sandwich)

#Below we specified the formula we used to impute each of the variables
formulas <- make.formulas(currentDataset)

#For example, to impute missing value in sex, we specify that we use the variable indigeneity, high_school, partnered, remoteness, language, #smoker, risky_alcohol, psyc_distress and age.
formulas$sex =sex ~ indigeneity + high_school + partnered + remoteness + language + smoker + risky_alcohol + psyc_distress + age
formulas$indigeneity =indigeneity ~ sex + high_school + partnered + remoteness + language + smoker + risky_alcohol + psyc_distress + age
formulas$high_school =high_school ~ sex + indigeneity + partnered + remoteness + language + smoker + risky_alcohol + psyc_distress + age
formulas$partnered =partnered ~ sex + indigeneity + high_school + remoteness + language + smoker + risky_alcohol + psyc_distress + age
formulas$remoteness =remoteness ~ sex + indigeneity + high_school + partnered + language + smoker + risky_alcohol + psyc_distress + age
formulas$language =language ~ sex + indigeneity + high_school + partnered + remoteness + smoker + risky_alcohol + psyc_distress + age
formulas$smoker =smoker ~ sex + indigeneity + high_school + partnered + remoteness + language + risky_alcohol + psyc_distress + age
formulas$risky_alcohol =risky_alcohol ~ sex + indigeneity + high_school + partnered + remoteness + language + smoker + psyc_distress + age
formulas$psyc_distress =psyc_distress ~ sex + indigeneity + high_school + partnered + remoteness + language + smoker + risky_alcohol + age
formulas$age =age ~ sex + indigeneity + high_school + partnered + remoteness + language + smoker + risky_alcohol + psyc_distress

#By default, categorical variables are imputed using multinomial logistic regression and continuous variables
#are imputed using predictive mean matching.
#For variable that we don't intend to impute, we set the "method" to blank ("").
#For example, we don't use the "rand" variable and we set meth["rand"] <- ""

meth <- make.method(currentDataset)
meth["rand"] <- ""

#We use the parlmice function to create the imputed dataset. The parlmice can take advantage of parallel computing
#to accelerate imputation.
#the m parameters specifies that at least 20 datasets will be imputed.
#The n.core parameters specifies the number of cores in the computer. In this example, we used 6 cores.
#The n.imp.core parameters specifies the number of imputations per core.
imputedDataset <- parlmice(currentDataset,
                           method = meth,
                           formulas = formulas,
                           m = 20,
                           n.core = 2, 
                           n.imp.core = 10)

#We can plot the output from the imputed dataset. The line in each of the plots should be well intermingled. 
plot(imputedDataset)

#We save the full imputed dataset to the currentDataset variable.
currentDataset <- complete(imputedDataset, action = "long", include = TRUE) 

library(MatchIt)
library(lmtest)
library(sandwich)

library(mice)
match_obj <- NULL
res <- NULL
covar <- NULL
b <- data.frame()
m <- max(currentDataset$.imp)
coeftest_obj <- NULL
matched_data <- NULL

#conduct matching and outcome analysis in each imputed dataset
for (i in (1:m)) {
  match_obj[[i]] <- matchit(smoker ~ sex + indigeneity + high_school + partnered + remoteness + language + risky_alcohol + age,
                            data = currentDataset[currentDataset$.imp == i,], method = "nearest", distance ="glm",
                            ratio = 1,
                            replace = FALSE)
  if (i<=3) {
    print(summary(match_obj[[i]]))
    plot(match_obj[[i]], type = "jitter", interactive = FALSE)
    plot(summary(match_obj[[i]]), abs = FALSE)
  }
  
  #Extract the matched data for each individual imputed dataset
  matched_data[[i]] <- match.data(match_obj[[i]])
  
  #Run a regression model on the matched data to estimate the effect of smoking on psychological distress
  res[[i]] <- lm(psyc_distress ~ smoker, data =   matched_data[[i]], weights = weights)
  #Extract the regression coefficient
  b <- rbind(b, res[[i]]$coefficients)
  #Calculate and extract the cluster-robust variance
  covar[[i]] <- vcovCL(res[[i]], cluster = ~subclass)
}

#Pool the results based on Rubin's rule
colnames(b) <- attributes(res[[1]]$coefficients)$names
U_bar <- Reduce("+", covar)/m
U_bar <- diag(U_bar)
B <- sapply(b, var)
T <- U_bar + (1 + 1/m)*B
SE <- T^0.5

#format the results into a dataframe
est <- data.frame(var = names(b))
est$b <- sapply(b, mean)
est$SE <- SE
est$Z <- est$b/est$SE
est$p_value <- pnorm(abs(est$Z), lower.tail = FALSE)*2
"Tests using cluster-robust standard error"

#display the results
print(est)