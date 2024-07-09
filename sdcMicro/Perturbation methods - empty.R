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


# Post-randomization Method (PRAM) --------------------------------------------
# PRAM (Gouweleeuw et al. 1998) is a probabilistic, perturbative method for protecting
# categorical variables. The method swaps the categories for selected variables
# based on a pre-defined transition matrix, which specifies the probabilities for each
# category to be swapped with other categories.

# setting up the SDC object
sdc <- createSdcObj(testdata,
                    keyVars = c("urbrur", "water", "sex", "age", "relat"),
                    numVars = c("expend", "income", "savings"),
                    pramVars = c("walls"), 
                    w = "sampling_weight",
                    hhId = "ori_hid",
                    strataVar = "hhcivil"
)

sdc <- pram(sdc)

print(sdc, "pram")


levels(testdata$walls)

levels(testdata$walls)
custom_transition_matrix <- matrix(c(
   0.85, 0.10, 0.05,   # Probabilities for category 1
   0.10, 0.80, 0.10,   # Probabilities for category 2
   0.05, 0.15, 0.80    # Probabilities for category 3
), nrow = 3, byrow = TRUE, 
dimnames = list(levels(testdata$walls),
                levels(testdata$walls)
))
levels(testdata$walls))


sdc <- pram(sdc,
            pd =custom_transition_matrix )

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

sdc <- microaggregation(sdc,
                        aggr = 20)



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
                method  = "additive"
                )

print(sdc, "numrisk")
sdc <- addNoise(sdc,
                method  = "correlated"
)
