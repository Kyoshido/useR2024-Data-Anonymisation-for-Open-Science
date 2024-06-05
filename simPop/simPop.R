# Packages --------------------------------------------------------------------
library(simPop)

# Data ------------------------------------------------------------------------
data(eusilc13puf, package = "simPop")
??eusilc13puf

df <- eusilc13puf[,c(1:6, 8:9,14, 16, 46)]
df$age <- as.numeric(df$age)
df$pid <- as.factor(df$pid)

vars <- c("hhid", "hsize", "region", "age", "sex", "pid", "eco_stat", "citizenship", 
          "pgrossIncome","hgrossIncome","weight")

colnames(df) <- vars

df <- head(df)


# Synthesis -------------------------------------------------------------------

## Define input----
# Identify in the microdata which variables provide info on clustering, hhsize, 
# subpopulations and sampling weights. The sample weights can be calibrated by 
# iterative  proportional fitting to the population totals.      
inp <-
  specifyInput(
    data = df,
    hhid = "hhid",
    hhsize = "hsize",
    strata = "region",
    weight = "weight"
  )


## Calibrate sample weights -----
# Not going to this today
# data("totalsRG", package = "simPop")
# data("totalsRGtab", package = "simPop")
# 
# weights.df <- calibSample(inp, totalsRG)
# weights.tab <- calibSample(inp, totalsRGtab)
# identical(weights.df, weights.tab)
# addWeights(inp) <- calibSample(inp, totalsRGtab)



## Define structure ----
# The household structure of the synthetic population is created by extrapolating
# the sample using the sample weights/calibrated weights and re-sampling (alias 
# sampling) when method = "distribution" and contains a set of user-defined "basic vars".
# (if you do not want to create a population use sample weights = 1 as a variable
# in your data). Other methods can be chosen such as "direct", using the Horvitz-Thompson
# estimator to estimate the population totals for each combination of stratum and
# household size or "multinom" estimating of the conditional probabilities within
# the strata using a multinomial log-lineaar model and random draws from the res-
# ulting distribution. 


simPop <-
  simStructure(
    data = inp,
    method = "distribution",
    basicHHvars = c("age", "sex", "region") 
  )

# Define categorical vars ) ----
# Categorical variables are simulated using the defined household structure and 
# the weights as an input.
# The package offers two methodological ways to  add the categorical vars. One 
# is a model based simulation, the other a synthetic reconstruction. Here we use
# the model based simulation. A multinomial model is fit to sample data, so that
# the categorical var wished to be simulated is the response var and the other vars
# serve as predictors. This results in the fit of the regression \beta. ...
#
simPop <-
  simCategorical(
    simPop,
    additional = c("eco_stat", "citizenship"), 
    method = "multinom"# also try xgboost
    # nr_cpus = 1
  )



# Define continuous vars ----
# Continuous vars are added to the structure using a modeling approach.
simPop <- simContinuous(
  simPop,
  additional = "pgrossIncome",
  # used to be netIncome
  regModel = ~ sex + hsize + eco_stat + citizenship +
    age,
  upper = 200000,
  equidist = FALSE#, nr_cpus=1
) # also try xgboost


simPop <- simContinuous(
  simPop,
  additional = "hgrossIncome",
  # used to be netIncome
  regModel = ~sex+hsize+eco_stat+citizenship+pgrossIncome,
  upper = 200000,
  equidist = FALSE#, nr_cpus=1
)

simPop
# add components
# TBD

# Synthetic data analysis -----------------------------------------------------

tab <- spTable(simPop, select = c("sex", "region", "hsize"))
spMosaic(tab, labeling = labeling_border(abbreviate = c(ID = TRUE))
spCdfplot(simPop, "pgrossIncome", cond = "sex", layout = c(1, 2))
spBwplot(simPop, x = "pgrossIncome", cond = "sex", layout = c(1, 2))
