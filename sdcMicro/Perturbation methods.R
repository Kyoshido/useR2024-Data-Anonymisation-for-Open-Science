###############################################################################

# Perturbation methods

###############################################################################
###############################################################################

### Load the required libraries
# install.packages("sdcMicro")
# install.packages("data.table")
library(sdcMicro)
library(data.table)

### Read data
# A real-world data set on household income and expenditures 
# (The International Household Survey)
data(testdata)
# https://rdrr.io/cran/sdcMicro/man/testdata.html
head(testdata)

# categorical variables should be saved as a factor
vars <- c("urbrur", "water", "sex", "age", "relat", "walls", "roof")
testdata[, vars] <- lapply(testdata[, vars], as.factor)

# setting up the SDC object
sdc <- createSdcObj(testdata,
                    keyVars = c("urbrur", "water", "sex", "age", "relat"),
                    numVars = c("expend", "income", "savings"),
                    pramVars = c("walls"), 
                    w = "sampling_weight",
                    hhId = "ori_hid",
                    strataVar = "hhcivil"
)

# Post-randomization Method (PRAM) --------------------------------------------
# PRAM (Gouweleeuw et al. 1998) is a probabilistic, perturbative method for protecting
# categorical variables. The method swaps the categories for selected variables
# based on a pre-defined transition matrix, which specifies the probabilities for each
# category to be swapped with other categories.

sdc <- pram(sdc)
print(sdc, "pram")

# The transition matrix shows the probabilities for each category of the walls 
# variable to be swapped with other categories.

sdc <- pram(sdc, 
            alpha = 0.1) # amount of perturbation
print(sdc, "pram")

# Define a custom transition matrix
levels(testdata$walls)
custom_transition_matrix <- matrix(c(
   0.85, 0.10, 0.05,   # Probabilities for category 1
   0.10, 0.80, 0.10,   # Probabilities for category 2
   0.05, 0.15, 0.80    # Probabilities for category 3
), nrow = 3, byrow = TRUE, 
dimnames = list(levels(testdata$walls),
                levels(testdata$walls)
                ))
sdc <- pram(sdc, 
            pd = custom_transition_matrix) # amount of perturbation
print(sdc, "pram")

# Microaggregation -------------------------------------------------------------
# The method first partitions records into groups, then assigns an aggregate value 
# (typically the arithmetic mean) to each variable in the group.
# setting up the SDC object

sdc <- createSdcObj(testdata,
                    keyVars = c("urbrur", "water", "sex", "age", "relat"),
                    numVars = c("expend", "income", "savings"),
                    pramVars = c("walls"), 
                    w = "sampling_weight",
                    hhId = "ori_hid",
                    strataVar = "hhcivil"
)

sdc <- microaggregation(sdc)
print(sdc, "numrisk")
# Numerical key variables: expend, income, savings
# 
# Disclosure risk is currently between [0.00%; 6.14%]
# 
# Current Information Loss:
#    - IL1: 545012.50
# - Difference of Eigenvalues: 0.060%

# The numerical key variables that were subjected to microaggregation are 
# numVars: expend, income, savings

# Disclosure Risk
# Range [0.00%; 6.14%]: The disclosure risk for the numerical key variables 
# after microaggregation ranges from 0.00% to 6.14%.
# 0.00%: Some records have no risk of re-identification.
# 6.14%: The maximum disclosure risk is 6.14%, indicating a significant reduction 
# in the risk of re-identification. This means the microaggregation has
# effectively anonymized the data, reducing the risk to a relatively low level.

# Information Loss
# IL1: 545012.50: IL1 is a measure of information loss due to the microaggregation
# process. A higher IL1 value indicates a greater loss of information. In this case,
# IL1 is quite high (545012.50), suggesting significant changes to the data due 
# to microaggregation.
# This high value may be due to the scale of the numerical variables, which might
# have large absolute values, resulting in a high IL1 score.

# Difference of Eigenvalues: 0.060%: This metric indicates the change in the 
# variance-covariance structure of the data before and after microaggregation.
# A difference of 0.060% is very small, suggesting that the overall structure 
# of the data, in terms of its variance-covariance matrix, is well preserved 
# despite the anonymization process.

sdc <- createSdcObj(testdata,
                    keyVars = c("urbrur", "water", "sex", "age", "relat"),
                    numVars = c("expend", "income", "savings"),
                    pramVars = c("walls"), 
                    w = "sampling_weight",
                    hhId = "ori_hid",
                    strataVar = "hhcivil"
)

# different parameters
sdc <- microaggregation(sdc,
                        aggr = 5,
                        method="mdav")
print(sdc, "numrisk")

# Noise Addition --------------------------------------------------------------
# The idea is to add or multiply a stochastic or randomized number to the original
# values to protect data from exact matching with external files.

sdc <- createSdcObj(testdata,
                    keyVars = c("urbrur", "water", "sex", "age", "relat"),
                    numVars = c("expend", "income", "savings"),
                    pramVars = c("walls"), 
                    w = "sampling_weight",
                    hhId = "ori_hid",
                    strataVar = "hhcivil"
)

sdc <- addNoise(sdc, 
                method="additive")
print(sdc, "numrisk")

sdc <- addNoise(sdc, 
                method="correlated")
print(sdc, "numrisk")
