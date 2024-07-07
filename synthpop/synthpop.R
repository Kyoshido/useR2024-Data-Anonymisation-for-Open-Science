###############################################################################
###############################################################################
###############################################################################

# Packages --------------------------------------------------------------------

library(synthpop)

# Data ------------------------------------------------------------------------

vars <- c("sex", "age", "edu", "marital", "income", "ls", "wkabint")
ods <- SD2011[, vars] # Social Diagnosis 2011 - Objective and Subjective Quality
# of Life in Poland
head(ods)

# Synthesis -------------------------------------------------------------------

# cart
my.seed <- 1
sds.default <- syn(ods, 
                   method = "cart",
                   seed = my.seed)
sds.default

# parametric
sds.parametric <- syn(ods,
                      method = "parametric", 
                      seed = my.seed)
sds.parametric$method


# Synthetic data analysis -----------------------------------------------------

# We use the wkabint variable which specifies the intentions of work migration 
# but we adjust it to disregard the destination country group. Besides we recode
# the current missing data code of variable income (-8) into the R missing data 
# code NA.

ods$wkabint <- as.character(ods$wkabint)
ods$wkabint[ods$wkabint == "YES, TO EU COUNTRY" |
              ods$wkabint == "YES, TO NON-EU COUNTRY"] <- "YES"
ods$wkabint <- factor(ods$wkabint)
ods$income[ods$income == -8] <- NA


# Generate five synthetic data sets

sds <- syn(ods,
           method = "ctree", 
           m = 5, 
           seed = my.seed)


# Compare some descriptive statistics of the observed and synthetic data sets
summary(ods)
summary(sds)

# Summary of individual data sets
summary(sds, msel = 1:5)
summary(sds, msel = 2)

# Compare the synthesised variables with the original ones
# continuous
compare(sds, ods, 
        vars = "income")
# categorical
compare(sds, ods, 
        vars = "ls",
        msel = 1:3)

# Comparison based on propensity scores
utility.tables(sds, ods)

# log reg on group adherence with two variables. 
# predict prop score for all observation
# MSE based on group adherence 
# devide by number of observation to standardize


# -----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################