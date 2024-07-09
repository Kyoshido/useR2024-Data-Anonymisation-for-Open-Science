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

