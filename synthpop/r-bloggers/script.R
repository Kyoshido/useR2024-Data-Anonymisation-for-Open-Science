###############################################################################
###############################################################################
###############################################################################

# Generating Synthetic Data Sets with ‘synthpop’ in R
# https://www.r-bloggers.com/2019/01/generating-synthetic-data-sets-with-synthpop-in-r/

###############################################################################

suppressPackageStartupMessages(library(synthpop))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(sampling))
suppressPackageStartupMessages(library(partykit))
mycols <- c("darkmagenta", "turquoise")
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
myseed <- 20190110
# filtering the dataset
original.df <- SD2011 %>% dplyr::select(sex, age, socprof, income, marital, depress, sport, nofriend, smoke, nociga, alcabuse, bmi)
head(original.df)


# setting continuous variable NA list
cont.na.list <- list(income = c(NA, -8), nofriend = c(NA, -8), nociga = c(NA, -8))


# apply rules to ensure consistency
rules.list <- list(
  marital = "age < 18", 
  nociga = "smoke == 'NO'")

rules.value.list <- list(
  marital = "SINGLE", 
  nociga = -8)


SD2011[which.max(SD2011$bmi),]

# getting around the error: synthesis needs to occur before the rules are applied
original.df$bmi <- ifelse(original.df$bmi > 75, NA, original.df$bmi)


# synthesise data
synth.obj <- syn(original.df, 
                 cont.na = cont.na.list, 
                 rules = rules.list, 
                 rvalues = rules.value.list, 
                 seed = myseed)
synth.obj

# compare the synthetic and original data frames
compare(synth.obj, 
        original.df, 
        nrow = 3, 
        ncol = 4,
        cols = mycols)





###############################################################################
###############################################################################
###############################################################################