#########################
##### CASE STUDY 1 ######
#########################
rm(list=ls())

##### Step 1 - Need for confidentiality protection ####
# Statistical units and sensitive variables require confidentiality protection

#### Step 2 - Data preparation and exploring characteristics ####
# Install packages if not yet installed
list.of.packages <- c("foreign", "sdcMicro", "laeken", "reldist", "bootstrap", "ineq")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load required packages
library(foreign)    # for read/write function for STATA
library(sdcMicro)	  # sdcMicro package
library(laeken)     # for GINI
library(reldist)    # for GINI
library(bootstrap)  # for bootstrapping
library(ineq)       # for Lorenz curves

# Get current working directory
getwd()

# Set working directory - set to the path on your machine
setwd("C:/")

# Save working directory
mainDir <- getwd()
mainDir

# Specify file name/path
fname <- "case_1_data.dta"
  
# Read-in file
file <- read.dta(fname, convert.factors = F) # factors as numeric code

# Dimensions of file (observations, variables)
dim(file)

# Variable names (more elaborate\descriptive names for the short variable names in the file)
# Names of variables in the data frame file
colnames(file)  

# Descriptive names
vardescr <- c("Region", "District", "Area of residence", "Individual weighting coefficient", "Population weighting coefficient",
              "Household ID", "Individual ID", "Household size", "Gender", "Relationship to Household Head",
              "Marital Status", "Age in completed years", "Age of child in completed months", 
              "Religion of household head", "Ethnicity of household head", "Language of household head",
              "Morbidity last x weeks", "Child immunized against Measles",
              "Sought medical attention", "Weight of the child (Kg)", "Height of the child (cms)", "Currently enrolled in school",
              "Highest level of education completed",  "Years of education",  "Years of education for currently enrolled", "Type of school attending",
              "Literacy", "Type of employment", "Unemployed", "Industry", "Employment categories", "Hours worked last week", 
              "Ownership of dwelling", "Main material used for roof", "Main toilet facility", "Connection of electricity in dwelling",
              "Main cooking fuel", "Main source of water", "Ownership of agricultural land", "Land size owned by household (ha) (agric and non ag)",
              "Ownership of motorcycle", "Ownership of car", "Ownership of television", "Number of large-sized livestock owned", 
              "Income - remittances", "Income - wages and salaries", "Income - Bonuses and social allowances derived from wage jobs", 
              "Income - Gross income from household farm businesses", "Income - Gross income from household nonfarm businesses", 
              "Income - rent", "Income - financial", "Income - pensions/social assistance", "Income - other",
              "Income - total", "Farm employment", "Total expenditure on housing", "Total expenditure on food", 
              "Total expenditure on alcoholic beverages, tobacco and narcotics", 
              "Total expenditure on clothing",  "Total expenditure on furnishing", 
              "Total expenditure on health", "Total expenditure on transport", "Total expenditure on communication", 
              "Total expenditure on recreation", "Total expenditure on education", "Total expenditure on hotels and restaurants", 
              "Total expenditure on miscellaneous spending", "Total annual nominal household expenditures")
cbind(colnames(file), vardescr)

# Evaluate characteristics of variables
# Look at tabulations of categorical variables (and cross-tabulations)
# Pay attention to missing values
# Automatic loop to look at tabulations for all variables
varnum <- 1
while (varnum != "0"){
  print(colnames(file)[as.integer(varnum)]) # variable name
  print(vardescr[as.integer(varnum)])       # variable description
  print(table(file[,as.integer(varnum)], useNA = "ifany")) # tabulation
  flush.console()
  varnum <- readline("# Enter variable number (1-66) or enter 0 to continue: ")
}

# Look at summary statistics of continuous variables
# Automatic loop to look at summary statistics for all variables
varnum <- 1
while (varnum != "0"){
  print(colnames(file)[as.integer(varnum)])
  print(summary(file[,as.integer(varnum)]))
  flush.console()
  varnum <- readline("# Enter variable number (1-66) or enter 0 to continue: ")
}

# Set different NA codes to R missing value NA
file[,'RELIG'][file[,'RELIG'] == 99999] <- NA
file[,'EMPTYP1'][file[,'EMPTYP1'] == 99] <- NA
file[,'LIVESTOCK'][file[,'LIVESTOCK'] == 9999] <- NA

# Drop variables containing only missings
file <-  file[,!names(file) %in% c('LANGUAGE', 'ETHNICITY')]

# Look at first 20 lines and last 20 lines
head(file, n = 20)
tail(file, n = 20)

#### Step 3: Type of release ####
# SUF file, file only available for researchers under contract

#### Step 4: Intruder scenarios and choice of key variables ####
# No direct identifiers available in raw data

# Household id and individual id in file

# The data contains a hierarchical structure (households)

##### Step 5 - Determine key uses and select utility measures ####
# See case study description

############# HOUSEHOLD LEVEL ##################
### Select variables (household level)
# Key variables (household level)
# REGION    (region)                          1-11 regions
# URBRUR    (area of residence)               1 - rural, 2 - urban  
# HHSIZE    (household size)
# OWNAGLAND (agricultural land ownership)     1 - yes, 2 - cannot be determined, 3 - no
# RELIG     (religion of household head)      1, 5, 6, 7, 9

selectedKeyVarsHH = c('URBRUR', 'REGION', 'HHSIZE', 'OWNAGLAND', 'RELIG') 
file$URBRUR    <- as.factor(file$URBRUR)
file$REGION    <- as.factor(file$REGION)
file$OWNHOUSE  <- as.factor(file$OWNHOUSE)
file$OWNAGLAND <- as.factor(file$OWNAGLAND)
file$RELIG     <- as.factor(file$RELIG)

# Numerical variables
# TANHHEXP   (total expenditures, sum of components)                    
# TFOODEXP, TALCHEXP, TCLTHEXP, TCLTHEXP, THOUSEXP, TFURNEXP, THLTHEXP, TTRANSEXP, TCOMMEXP, TRECEXP, TEDUEXP, TRESTHOTEXP, TMISCEXP (expenditures by category)
# INCTOTGROSSHH (total income, sum of components)
# INCRMT, INCWAGE, INCBONSOCALL, INCFARMBSN, INCNFARMBSN, INCRENT, INCFIN, INCPENSN, INCSTUDYSUPP, INCOTHER (income by source)
# LANDSIZEHA (Land size owned by household (ha))
numVarsHH = c('LANDSIZEHA', 'TANHHEXP', 'TFOODEXP', 'TALCHEXP', 'TCLTHEXP', 'THOUSEXP', 'TFURNEXP', 'THLTHEXP', 'TTRANSEXP', 'TCOMMEXP', 'TRECEXP', 'TEDUEXP', 'TRESTHOTEXP', 'TMISCEXP', 
              'INCTOTGROSSHH', 'INCRMT', 'INCWAGE', 'INCFARMBSN', 'INCNFARMBSN', 'INCRENT', 'INCFIN', 'INCPENSN', 'INCOTHER')

# Spontaneous recognition variables
# ROOF          (roof type)                   1 = Concrete/cement/ brick/stone, 2 = Wood, 3 = Bamboo/thatch, 4 = Tiles/shingles, 
#                                             5 = Tin/metal sheets, 6 = Makeshift, 9 = Other             
# WATER         (main source of water)        1 = Pipe (own tap), 2 = Public standpipe, 3 = Borehole, 4 = Wells (protected), 
#                                             5 = Wells (unprotected), 6 = Surface water, 7 = Rain water, 8 = Vendor/truck, 9 = Other
# TOILET        (main toilet facility)        1 = Flush toilet, 2 = Improved pit latrine, 3 = Pit latrine, 4 = No facility, 9 = Other
# ELECTCON      (electricity)                 1 = Central/local, 2 = Solar/wind, 3 = Generator, 9 = Other, 0 = None
# FUELCOOK      (main cooking fuel)           1 = Firewood, 2 = Kerosene, 3 = Charcoal, 4 = Electricity, 5 = Gas, 9 = Other
# OWNMOTORCYCLE (ownership of motorcycle)     1 = YES, 0 = No
# CAR           (ownership of car)            1 = YES, 0 = No
# TV            (ownershipof television)      1 = YES, 0 = No
# LIVESTOCK     (number of large-sized livestock owned)
pramVarsHH = c('ROOF', 'TOILET', 'WATER', 'ELECTCON', 'FUELCOOK', 'OWNMOTORCYCLE', 'CAR', 'TV', 'LIVESTOCK')

# sample weight (WGTPOP)
weightVarHH = c('WGTPOP')

# All household level variables (id, key variables, pram variables, numerical variables, weights)
HHVars <- c('IDH', selectedKeyVarsHH, pramVarsHH, numVarsHH, weightVarHH)

#### Step 6a: Assessing disclosure risk (household level) ####
# Create subset of file with households and HH variables
fileHH <- file[,HHVars]
# Remove duplicated rows based on IDH, one row per household in fileHH
fileHH <- fileHH[which(!duplicated(fileHH$IDH)),]
dim(fileHH)

# Create initial sdc object for household level variables
sdcHH <- createSdcObj(dat=fileHH, keyVars=selectedKeyVarsHH, pramVars=pramVarsHH, weightVar=weightVarHH, numVars = numVarsHH)
numHH <- length(fileHH[,1]) # number of households

# Number of observations violating k-anonymity
print(sdcHH)

# Calculate sample frequencies and count number of obs. violating k (3,5) - anonymity
kAnon3 <- sum(sdcHH@risk$individual[,2] <3) 
kAnon5 <- sum(sdcHH@risk$individual[,2] <5)
kAnon5

# As percentage of total
kAnon3 / numHH
kAnon5 / numHH

# Show values of key variable of records that violate k-anonymity
fileHH[sdcHH@risk$individual[,2] < 2, selectedKeyVarsHH] # 2-anonymity
fileHH[sdcHH@risk$individual[,2] < 3, selectedKeyVarsHH] # 3-anonimity
  
# SUDA score (SUDA DIS score)
# Save the number of observations with suda scores equal to 0 and between 0-0.1, 0.1-0.2, ..., 0.6-0.7
SUDA <- rep(0, 8)
SUDA[1] <- sum(suda2(sdcHH)@risk$suda2$disScore == 0)

for (i in 1:7)
{
  SUDA[i+1] <- sum(suda2(sdcHH)@risk$suda2$disScore <= i/10) - sum(SUDA[1:i])
}

names(SUDA) <- c('SUDA0', 'SUDA01', 'SUDA02', 'SUDA03', 'SUDA04','SUDA05','SUDA06','SUDA07')

# Histogram of SUDA scores
hist(suda2(sdcHH)@risk$suda2$disScore)

# Individual risk (per houshold)
# Number of observations with relatively high risk
dim(fileHH[sdcHH@risk$individual[, "risk"] > 0.01,])
# Highest individual risk
max(sdcHH@risk$individual[, "risk"])
# Look at high risk households
fileHH[sdcHH@risk$individual[, "risk"] > 0.01,]

# Global risk on household level
print(sdcHH, 'risk')
# Global risk percentage
sdcHH@risk$global$risk
# Number of expected re-identifications
sdcHH@risk$global$risk_ER

#### Step 7a: Assessing utility measures (household level) ####
# Individual weights
curW <- sdcHH@origData[,'WGTPOP']

# Number of observations
numObs <- length(sdcHH@origData[,1])

# a) Mean monthly per capita expenditure by rur / urb
# Expenditure matrix by category (from origData but treated, separation HH and IND)
# origData in sdcCombined contains the treated household variables
expByCat <- cbind(sdcHH@origData$TFOODEXP, sdcHH@origData$TALCHEXP, sdcHH@origData$TCLTHEXP, sdcHH@origData$THOUSEXP,
                  sdcHH@origData$TFURNEXP, sdcHH@origData$THLTHEXP, sdcHH@origData$TTRANSEXP, sdcHH@origData$TCOMMEXP, 
                  sdcHH@origData$TRECEXP, sdcHH@origData$TEDUEXP, sdcHH@origData$TRESTHOTEXP, sdcHH@origData$TMISCEXP)
colnames(expByCat) <- c("TFOODEXP", "TALCHEXP", "TCLTHEXP", "THOUSEXP", "TFURNEXP", "THLTHEXP", "TTRANSEXP", "TCOMMEXP", "TRECEXP", "TEDUEXP", "TRESTHOTEXP", "TMISCEXP")

# expPerCapita, monthly: /12 and /HHSIZE
expPerCapita <- rowSums(expByCat, na.rm=TRUE)/(12 * as.numeric(sdcHH@origData$HHSIZE))
MMERur  <- weighted.mean(x = expPerCapita[sdcHH@manipKeyVars$URBRUR == 1], w = curW[sdcHH@manipKeyVars$URBRUR == 1])
MMEUrb  <- weighted.mean(x = expPerCapita[sdcHH@manipKeyVars$URBRUR == 2], w = curW[sdcHH@manipKeyVars$URBRUR == 2])
MMETot  <- weighted.mean(x = expPerCapita, w = curW)
MMERes  <- as.data.frame(cbind(MMERur, MMEUrb, MMETot))
print(MMERes)

# c) Mean monthly per capita income (by rur/urb)
# Income matrix by category
incByCat <- cbind(sdcHH@origData$INCRMT, sdcHH@origData$INCWAGE, 
                  sdcHH@origData$INCFARMBSN, sdcHH@origData$INCNFARMBSN, sdcHH@origData$INCRENT,
                  sdcHH@origData$INCFIN, sdcHH@origData$INCPENSN, sdcHH@origData$INCOTHER)

colnames(incByCat) <- c('INCRMT', 'INCWAGE', 'INCFARMBSN', 'INCNFARMBSN', 'INCRENT', 'INCFIN', 'INCPENSN', 'INCOTHER')

# incPerCapita, monthly: /12 and /HHSIZE
incPerCapita <- rowSums(incByCat, na.rm=TRUE)/(12 * as.numeric(sdcHH@origData$HHSIZE))
MMIRur  <- weighted.mean(x = incPerCapita[sdcHH@manipKeyVars$URBRUR == 1], w = curW[sdcHH@manipKeyVars$URBRUR == 1])
MMIUrb  <- weighted.mean(x = incPerCapita[sdcHH@manipKeyVars$URBRUR == 2], w = curW[sdcHH@manipKeyVars$URBRUR == 2])
MMITot  <- weighted.mean(x = incPerCapita, w = curW)
MMIRes  <- as.data.frame(cbind(MMIRur, MMIUrb, MMITot))
print(MMIRes)

# d) # Components of expenditure
# average share of each expenditure component/category
AveShareExp <- colSums(expByCat / rowSums(expByCat, na.rm=TRUE), na.rm=TRUE) / numObs
names(AveShareExp) <- paste0( "AveShareExp", colnames(expByCat))
print(AveShareExp)

#### Step 8a: Choice and application of SDC methods (household variables) ####
# Remove large households (14 or more household members) from file and fileHH
table(sdcHH@manipKeyVars$HHSIZE)
dim(fileHH[fileHH[,'HHSIZE'] >= 14,]) # number of households od size 14 or larger
file   <- file[!file[,'HHSIZE'] >= 14,]
fileHHnew <- fileHH[!fileHH[,'HHSIZE'] >= 14,]

# Recreate sdcMicro object with fileHH excluding households with 14 or more members
sdcHH <- createSdcObj(dat=fileHHnew, keyVars=selectedKeyVarsHH, pramVars=pramVarsHH, weightVar=weightVarHH, numVars = numVarsHH)

# Look at distribution of variable LANDSIZE
# 1st - 100th percentiles of landsize
quantile(fileHH$LANDSIZE, probs = (90:100)/100 , na.rm= TRUE)

# Values of landsize for largest 50 plots
tail(sort(fileHH$LANDSIZE), n = 50)

# Rounding values of LANDSIZEHA to 1 digits for plots smaller than 1 and to no digits for plots larger than 1
sdcHH@manipNumVars$LANDSIZEHA[sdcHH@manipNumVars$LANDSIZEHA <= 1  & !is.na(sdcHH@manipNumVars$LANDSIZEHA)] <- round(sdcHH@manipNumVars$LANDSIZEHA[sdcHH@manipNumVars$LANDSIZEHA <= 1 & !is.na(sdcHH@manipNumVars$LANDSIZEHA)], digits = 1)
sdcHH@manipNumVars$LANDSIZEHA[sdcHH@manipNumVars$LANDSIZEHA > 1 & !is.na(sdcHH@manipNumVars$LANDSIZEHA)] <- round(sdcHH@manipNumVars$LANDSIZEHA[sdcHH@manipNumVars$LANDSIZEHA > 1 & !is.na(sdcHH@manipNumVars$LANDSIZEHA)], digits = 0)

# Grouping values of LANDSIZEHA in intervals 5-19, 20-39
sdcHH@manipNumVars$LANDSIZEHA[sdcHH@manipNumVars$LANDSIZEHA >= 5 & sdcHH@manipNumVars$LANDSIZEHA  < 20 & !is.na(sdcHH@manipNumVars$LANDSIZEHA)] <- 13
sdcHH@manipNumVars$LANDSIZEHA[sdcHH@manipNumVars$LANDSIZEHA >= 20 & sdcHH@manipNumVars$LANDSIZEHA  < 40 & !is.na(sdcHH@manipNumVars$LANDSIZEHA)] <- 30

# Top coding values of LANDSIZEHA larger than 40
sdcHH <- topBotCoding(sdcHH, value = 40, replacement = 40, kind = 'top', column = 'LANDSIZEHA')

# Check results for LANDSIZEHA
table(sdcHH@manipNumVars$LANDSIZEHA)

# Local suppression
sdcHH <- localSuppression(sdcHH, k = 2, importance = NULL)          # no importance vector
print(sdcHH, "ls")
table(fileHHnew$HHSIZE); table(sdcHH@manipKeyVars$HHSIZE)              # Check which values of the variable HHSIZE are suppressed

sdcHH <- undolast(sdcHH) # undo suppressions to see the effect of an importance vector

# Redo local suppression minimizing the number of suppressions in HHSIZE (and REGION and URBRUR)
sdcHH <- localSuppression(sdcHH, k = 2, importance = c(3, 2, 1, 5, 5)) 
print(sdcHH, "ls")

table(fileHHnew$URBRUR); table(sdcHH@manipKeyVars$URBRUR)              # Check which values of the variable URBRUR are suppressed
table(fileHHnew$REGION); table(sdcHH@manipKeyVars$REGION)              # Check which values of the variable REGION are suppressed
table(fileHHnew$OWNAGLAND); table(sdcHH@manipKeyVars$OWNAGLAND)          # Check which values of the variable OWNAGLAND are suppressed
table(fileHHnew$RELIG); table(sdcHH@manipKeyVars$RELIG)              # Check which values of the variable RELIG are suppressed
table(fileHHnew$HHSIZE); table(sdcHH@manipKeyVars$HHSIZE)              # Check which values of the variable HHSIZE are suppressed

# Remove household with suppressed value of HHSIZE
sdcHH@origData[is.na(sdcHH@manipKeyVars$HHSIZE), "IDH"] #IDH
file   <- file[file[,'IDH'] != 1782,]
fileHHnew <- fileHHnew[fileHHnew[,'IDH'] != 1782,]

# Pram
set.seed(536747)
sdcHH <- pram(obj = sdcHH, strata_variables = "REGION", pd = 0.8)
table(sdcHH@manipPramVars[,3], sdcHH@manipKeyVars$REGION, useNA = "ifany")

# Add noise to income and expenditure variables by category
compExp <- c('TFOODEXP', 'TALCHEXP', 'TCLTHEXP', 'THOUSEXP', 'TFURNEXP', 'THLTHEXP', 'TTRANSEXP', 'TCOMMEXP', 'TRECEXP', 'TEDUEXP', 'TRESTHOTEXP', 'TMISCEXP')
set.seed(123)
# Add noise to expenditure variables
sdcHH   <- addNoise(noise = 0.01, obj = sdcHH, variables = compExp, method = "additive")
# Add noise to outliers
sdcHH   <- addNoise(noise = 0.05, obj = sdcHH, variables = compExp, method = "outdect")
# Sum over expenditure categories to obtain consistent totals
sdcHH@manipNumVars[,'TANHHEXP'] <- rowSums(sdcHH@manipNumVars[,compExp], na.rm = TRUE)

compInc <- c('INCRMT', 'INCWAGE', 'INCFARMBSN', 'INCNFARMBSN', 'INCRENT', 'INCFIN', 'INCPENSN', 'INCOTHER')
set.seed(123)
# Add noise to expenditure variables
sdcHH   <- addNoise(noise = 0.01, obj = sdcHH, variables = compInc, method = "additive")
# Add noise to outliers
sdcHH   <- addNoise(noise = 0.05, obj = sdcHH, variables = compInc, method = "outdect")
# Sum over income categories to obtain consistent totals
sdcHH@manipNumVars[,'INCTOTGROSSHH'] <- rowSums(sdcHH@manipNumVars[,compInc], na.rm = TRUE)

# recalculate risks after manually changing values in sdcMicro object
calcRisks(sdcHH)

#### Step 9a: Remeasure risk ####
# Number of observations violating k-anonimity
numHH <- length(fileHH[,1]) # number of households
# Number of observations violating k-anonimity
print(sdcHH)

# Calculate sample frequencies and count number of obs. violating k (3,5) - anonimity
kAnon3 <- sum(sdcHH@risk$individual[,2] <3) 
kAnon5 <- sum(sdcHH@risk$individual[,2] <5)
kAnon3; kAnon5

# As percentage of total
kAnon3 / numHH
kAnon5 / numHH

# SUDA score (SUDA DIS score)
# Save the number of observations with suda scores equal to 0 and between 0-0.1, 0.1-0.2, ..., 0.7-0.8
SUDA <- rep(0, 8)
SUDA[1] <- sum(suda2(sdcHH)@risk$suda2$disScore == 0) 

for (i in 1:7)
{
  SUDA[i+1] <- sum(suda2(sdcHH)@risk$suda2$disScore <= i/10) - sum(SUDA[1:i])
}

names(SUDA) <- c('SUDA0', 'SUDA01', 'SUDA02', 'SUDA03', 'SUDA04','SUDA05','SUDA06','SUDA07')

# Individual risk 
# Number of observations with relatively high risk
dim(fileHH[sdcHH@risk$individual[, "risk"] > 0.01,])
# Look at high risk households
fileHH[sdcHH@risk$individual[, "risk"] > 0.01,]

# Global risk on household level
print(sdcHH, 'risk')
# Global risk percentage
sdcHH@risk$global$risk
# Number of expected reindentifications
sdcHH@risk$global$risk_ER

# Risk evaluation continuous variables
# Expenditure components
dRisk(sdcHH@origData[,compExp], xm = sdcHH@manipNumVars[,compExp], k = 0.01)
dRisk(sdcHH@origData[,compExp], xm = sdcHH@manipNumVars[,compExp], k = 0.02)
dRisk(sdcHH@origData[,compExp], xm = sdcHH@manipNumVars[,compExp], k = 0.05)

#### Step 10a: Remeasure utility ####
# a) GINI on sum of expenditures and CI
set.seed(123) # set seed for bootstrap
myGini <- function(x, dat){gc <- gini(dat[x,1], weights = dat[x,2])} # Gini function to use in bootstrap

# Original / raw data
a <- cbind(fileHH$TANHHEXP / fileHH$HHSIZE, fileHH$WGTPOP)
apos <- a[a[,1] > 0,]
point0 <- gini(apos[,1], weights = apos[,2])
r0 <- bootstrap(1:dim(apos)[1], 200, myGini, apos)$thetastar
left0 <- (mean(r0) - 1.96 * sd(r0)) # left
right0 <- (mean(r0) + 1.96 * sd(r0)) # left

# Treated data after anonymization
b <- cbind(sdcHH@manipNumVars[, "TANHHEXP"] / sdcHH@origData$HHSIZE, sdcHH@origData$WGTPOP)
bpos <- b[b[,1] > 0,]
point1 <- gini(bpos[,1], weights = bpos[,2])
r1 <- bootstrap(1:dim(bpos)[1], 200, myGini, bpos)$thetastar
left1 <- (mean(r1) - 1.96 * sd(r1)) # left
right1 <- (mean(r1) + 1.96 * sd(r1)) # left

# Point and confidence intervals comparison
rbind(c(point0, left0, right0), c(point1, left1, right1))

# b) Lorenz curve of sum expenditures
plot(Lc(apos[, 1]), col = 1, lwd = 3, lty = 1, main = "", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
par(new=TRUE)
plot(Lc(bpos[,1]), col = 2, lwd = 3, lty = 2, main = "", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
legend("topleft", c("before", "after"), col = c(1:2), lty = 1:2, lwd=3, cex = 1)

# Summary statistics of sum of expenditures
summary(a[,1]); summary(b[,1])

# Individual weights
curW <- sdcHH@origData[,'WGTPOP']

# Number of observations
numObs <- length(sdcHH@origData[,1])

# c) Mean monthly per capita expenditure by rural / urban
# Expenditure matrix by category (from origData but treated, separation HH and IND)
expByCatA <- cbind(sdcHH@manipNumVars$TFOODEXP, sdcHH@manipNumVars$TALCHEXP, sdcHH@manipNumVars$TCLTHEXP, sdcHH@manipNumVars$THOUSEXP,
                  sdcHH@manipNumVars$TFURNEXP, sdcHH@manipNumVars$THLTHEXP, sdcHH@manipNumVars$TTRANSEXP, sdcHH@manipNumVars$TCOMMEXP, 
                  sdcHH@manipNumVars$TRECEXP, sdcHH@manipNumVars$TEDUEXP, sdcHH@manipNumVars$TRESTHOTEXP, sdcHH@manipNumVars$TMISCEXP)
colnames(expByCatA) <- c("TFOODEXP", "TALCHEXP", "TCLTHEXP", "THOUSEXP", "TFURNEXP", "THLTHEXP", "TTRANSEXP", "TCOMMEXP", "TRECEXP", "TEDUEXP", "TRESTHOTEXP", "TMISCEXP")

# expPerCapita, monthly: /12 and /HHSIZE
expPerCapitaA <- rowSums(expByCatA, na.rm=TRUE)/(12 * as.numeric(sdcHH@manipKeyVars$HHSIZE))
MMERurA  <- weighted.mean(x = expPerCapitaA[sdcHH@manipKeyVars$URBRUR == 1], w = curW[sdcHH@manipKeyVars$URBRUR == 1], na.rm = T)
MMEUrbA  <- weighted.mean(x = expPerCapitaA[sdcHH@manipKeyVars$URBRUR == 2], w = curW[sdcHH@manipKeyVars$URBRUR == 2], na.rm = T)
MMETotA  <- weighted.mean(x = expPerCapitaA, w = curW, na.rm = T)
MMEResA  <- as.data.frame(cbind(MMERurA, MMEUrbA, MMETotA))
print(MMERes)
print(MMEResA)

# d) # Components of expenditure
# average share of each expenditure component/category
AveShareExpA <- colSums(expByCatA / rowSums(expByCatA, na.rm=TRUE), na.rm=TRUE) / numObs
names(AveShareExpA) <- paste0( "AveShareExp", colnames(expByCat))
print(AveShareExp)
print(AveShareExpA)

# e) Mean monthly per capita income (by rur/urb)
# Income matrix by category
incByCatA <- cbind(sdcHH@manipNumVars$INCRMT, sdcHH@manipNumVars$INCWAGE, 
                  sdcHH@manipNumVars$INCFARMBSN, sdcHH@manipNumVars$INCNFARMBSN, sdcHH@manipNumVars$INCRENT,
                  sdcHH@manipNumVars$INCFIN, sdcHH@manipNumVars$INCPENSN, sdcHH@manipNumVars$INCOTHER)

colnames(incByCatA) <- c('INCRMT', 'INCWAGE', 'INCFARMBSN', 'INCNFARMBSN', 'INCRENT', 'INCFIN', 'INCPENSN', 'INCOTHER')

# incPerCapita, monthly: /12 and /HHSIZE
incPerCapitaA <- rowSums(incByCatA, na.rm=TRUE)/(12 * as.numeric(sdcHH@manipKeyVars$HHSIZE))
MMIRurA  <- weighted.mean(x = incPerCapitaA[sdcHH@manipKeyVars$URBRUR == 1], w = curW[sdcHH@manipKeyVars$URBRUR == 1], na.rm = T)
MMIUrbA  <- weighted.mean(x = incPerCapitaA[sdcHH@manipKeyVars$URBRUR == 2], w = curW[sdcHH@manipKeyVars$URBRUR == 2], na.rm = T)
MMITotA  <- weighted.mean(x = incPerCapitaA, w = curW, na.rm = T)
MMIResA  <- as.data.frame(cbind(MMIRurA, MMIUrbA, MMITotA))
print(MMIRes)
print(MMIResA)

############# INDIVIDUAL LEVEL ##################
### Select variables (individual level)
# key variables (individual level)
# GENDER (gender)                           0 - female, 1 - male
# REL (relationship to household head):     1 - Head, 2 - Spouse, 3 - Child, 4 - Father/Mother, 6 - Son/Daughter in law
#                                           7 - Other relative, 8 - Domestic help, 9 - Non-relative  
# MARITAL (marital status):                 1 - never married, 2 - married monogamous, 3 - married polygamous
#                                           5 - divorced/separated, 6 - widowed
# AGEYRS (age in completed years)           numeric in years
# EDUCY (highest level of educ compl)       0 - no educ, 1 - Pre-school/ primary, not completed,
#                                           2 - Completed primary, but less than completed, 3 - Completed lower secondary but less than
#                                           4 - Completed upper secondary, 5 -  Post-secondary technical , 6 - University and higher 
# EDYRSCURRAT
# ATSCHOOL (currently enrolled in school)   0 - no, 1 - yes
# INDUSTRY1 (1 digit industry classific.)   1-10   
selectedKeyVarsIND = c('GENDER', 'REL', 'MARITAL', 'AGEYRS', 'EDUCY', 'EDYRSCURRAT', 'ATSCHOOL', 'INDUSTRY1') # list of selected key variables

# sample weight (WGTHH, individual weight)
selectedWeightVarIND = c('WGTHH')
  
# Household ID
selectedHouseholdID = c('IDH')

# No strata

# All individual level variables
INDVars <- c(selectedKeyVarsIND)

# Recombining anonymized HH data sets and individual level variables
indVars <- c("IDH", "IDP", selectedKeyVarsIND, "WGTHH") # HID and all non HH vars
fileInd <- file[indVars] # subset of file without HHVars
HHmanip <- extractManipData(sdcHH) # manipulated variables HH
HHmanip <- HHmanip[HHmanip[,'IDH'] != 1782,] # Remove household with suppressed value of HHSIZE

fileCombined <- merge(HHmanip, fileInd, by.x = c('IDH'))
fileCombined <- fileCombined[order(fileCombined[,'IDH'], fileCombined[,'IDP']),]
dim(fileCombined)

# SDC objects with all variables and treated HH vars for anonymization of individual level variables
sdcCombined <- createSdcObj(dat = fileCombined, keyVars = c(selectedKeyVarsIND), weightVar = selectedWeightVarIND, hhId = selectedHouseholdID)

#### Step 6b: Risk assesment before anonymization ####
# k-anonymity
numIND <- length(fileCombined[,1]) # number of households
# Number of observations violating k-anonymity
sdcCombined

# Calculate sample frequencies and count number of obs. violating k (3,5) - anonymity
kAnon3 <- sum(sdcCombined@risk$individual[,2] <3) 
kAnon5 <- sum(sdcCombined@risk$individual[,2] <5)
kAnon5

# As percentage of total
kAnon3 / numIND
kAnon5 / numIND

# Show lines with variables that violate k-anonymity
fileCombined[sdcCombined@risk$individual[,2] < 2,]
fileCombined[sdcCombined@risk$individual[,2] < 3,]

# SUDA score (SUDA DIS score)
# Save the number of observations with suda scores equal to 0 and between 0-0.1, 0.1-0.2, ..., 0.6-0.7
SUDA <- rep(0, 8)
SUDA[1] <- sum(suda2(sdcCombined)@risk$suda2$disScore == 0)

for (i in 1:7)
{
  SUDA[i+1] <- sum(suda2(sdcCombined)@risk$suda2$disScore <= i/10) - sum(SUDA[1:i])
}

names(SUDA) <- c('SUDA0', 'SUDA01', 'SUDA02', 'SUDA03', 'SUDA04','SUDA05','SUDA06','SUDA07')
print(SUDA)

# Individual risk 
# Look at high risk 
dim(fileCombined[sdcCombined@risk$individual[, "risk"] > 0.01,])
fileCombined[sdcCombined@risk$individual[, "risk"] > 0.01,]

# Global risk on individual level
print(sdcCombined, 'risk')
# Global risk percentage
sdcCombined@risk$global$risk
# Number of expected re-identifications
sdcCombined@risk$global$risk_ER
# Hierarchical risk percentage
sdcCombined@risk$global$hier_risk
# Hierarchical risk: number of expected identifications
sdcCombined@risk$global$hier_risk_ER

#### Step 7b: Assessing utility (individual level) ####
# Individual weights
curW <- sdcCombined@origData[,'WGTHH']

# Number of observations
numObs <- length(sdcCombined@origData[,1])

# a) # Net enrolment in primary education, by gender
# Definition (UN): Net primary enrolment rate in primary education is the number of children of official primary 
# school age (according to ISCED971) (6-12) who are enrolled in primary education as a percentage of the total children 
# of the official school age population. Total net primary enrolment rate also includes children of primary school 
# age enrolled in secondary education.
# Net enrolment:in education & in age group / in age group
# Gross enrolment: in education / in age group 

# EDYRSCURRAT (years of education for currently enrolled)  
# Primary:            (EDYRSCURRAT>=1 & EDYRSCURRAT<=6)
# Secondary (lower):  (EDYRSCURRAT>=7 & EDYRSCURRAT<=10)
# By gender (0 - female, 1 - male)
educPrimMale    <-  sum(curW * (sdcCombined@manipKeyVars$EDYRSCURRAT >= 1 & sdcCombined@manipKeyVars$EDYRSCURRAT <= 6 & sdcCombined@manipKeyVars$AGEYRS >= 6 &  sdcCombined@manipKeyVars$AGEYRS <= 12  & sdcCombined@manipKeyVars$GENDER == 1), na.rm=TRUE) / sum(curW * (sdcCombined@manipKeyVars$AGEYRS >= 6 &  sdcCombined@manipKeyVars$AGEYRS <= 12  & sdcCombined@manipKeyVars$GENDER == 1), na.rm=TRUE)
educPrimFemale  <-  sum(curW * (sdcCombined@manipKeyVars$EDYRSCURRAT >= 1 & sdcCombined@manipKeyVars$EDYRSCURRAT <= 6 & sdcCombined@manipKeyVars$AGEYRS >= 6 &  sdcCombined@manipKeyVars$AGEYRS <= 12  & sdcCombined@manipKeyVars$GENDER == 0), na.rm=TRUE)/ sum(curW * (sdcCombined@manipKeyVars$AGEYRS >= 6 &  sdcCombined@manipKeyVars$AGEYRS <= 12  & sdcCombined@manipKeyVars$GENDER == 0), na.rm=TRUE)
educPrimTot     <-  sum(curW * (sdcCombined@manipKeyVars$EDYRSCURRAT >= 1 & sdcCombined@manipKeyVars$EDYRSCURRAT <= 6 & sdcCombined@manipKeyVars$AGEYRS >= 6 &  sdcCombined@manipKeyVars$AGEYRS <= 12  ), na.rm=TRUE)/ sum(curW * (sdcCombined@manipKeyVars$AGEYRS >= 6 &  sdcCombined@manipKeyVars$AGEYRS <= 12  ), na.rm=TRUE)                                                                                                                  
educSecMale     <-  sum(curW * (sdcCombined@manipKeyVars$EDYRSCURRAT >= 7 & sdcCombined@manipKeyVars$EDYRSCURRAT <= 10 & sdcCombined@manipKeyVars$GENDER == 1), na.rm=TRUE) / sum(curW * (sdcCombined@manipKeyVars$AGEYRS >= 7 &  sdcCombined@manipKeyVars$AGEYRS <= 10  & sdcCombined@manipKeyVars$GENDER == 1), na.rm=TRUE)
educSecFemale   <-  sum(curW * (sdcCombined@manipKeyVars$EDYRSCURRAT >= 7 & sdcCombined@manipKeyVars$EDYRSCURRAT <= 10 & sdcCombined@manipKeyVars$GENDER == 0), na.rm=TRUE)/ sum(curW * (sdcCombined@manipKeyVars$AGEYRS >= 7 &  sdcCombined@manipKeyVars$AGEYRS <= 10  & sdcCombined@manipKeyVars$GENDER == 0), na.rm=TRUE)
educSecTot      <-  sum(curW * (sdcCombined@manipKeyVars$EDYRSCURRAT >= 7 & sdcCombined@manipKeyVars$EDYRSCURRAT <= 10), na.rm=TRUE)/ sum(curW * (sdcCombined@manipKeyVars$AGEYRS >= 7 &  sdcCombined@manipKeyVars$AGEYRS <= 10  ), na.rm=TRUE)                                                                                                                  

educEnrolRes <- cbind(educPrimMale, educPrimFemale, educPrimTot, educSecMale, educSecFemale, educSecTot)
names(educEnrolRes) <- c('educPrimMale', 'educPrimFemale', 'educPrimTot', 'educSecMale', 'educSecFemale', 'educSecTot')      
print(educEnrolRes)

#### Step 8b: Choice and application of SDC methods (individual level) ####
# Recoding age (10 year age group 15-65), top coding age (top code age 65), children aged under 1 are recoded 0 (previously in months)
sdcCombined@manipKeyVars$AGEYRS[sdcCombined@manipKeyVars$AGEYRS >= 0 & sdcCombined@manipKeyVars$AGEYRS < 1] <- 0
sdcCombined@manipKeyVars$AGEYRS[sdcCombined@manipKeyVars$AGEYRS >= 15 & sdcCombined@manipKeyVars$AGEYRS < 25] <- 20
sdcCombined@manipKeyVars$AGEYRS[sdcCombined@manipKeyVars$AGEYRS >= 25 & sdcCombined@manipKeyVars$AGEYRS < 35] <- 30
sdcCombined@manipKeyVars$AGEYRS[sdcCombined@manipKeyVars$AGEYRS >= 35 & sdcCombined@manipKeyVars$AGEYRS < 45] <- 40
sdcCombined@manipKeyVars$AGEYRS[sdcCombined@manipKeyVars$AGEYRS >= 45 & sdcCombined@manipKeyVars$AGEYRS < 55] <- 50
sdcCombined@manipKeyVars$AGEYRS[sdcCombined@manipKeyVars$AGEYRS >= 55 & sdcCombined@manipKeyVars$AGEYRS < 65] <- 60

# topBotCoding also recalculates risk based on manual recoding above
sdcCombined <- topBotCoding(obj = sdcCombined, value = 65, replacement = 65, kind = 'top', column = 'AGEYRS')
table(sdcCombined@manipKeyVars$AGEYRS) # check results

sdcCombined
# Importance vectors for local suppression (depending on utility measures)
impVec1 <- NULL # for optimal suppression
impVec2 <- rep(length(selectedKeyVarsIND), length(selectedKeyVarsIND)); impVec2[match('AGEYRS', selectedKeyVarsIND)] <- 1  # AGEYRS
impVec2[match('GENDER', selectedKeyVarsIND)] <- 2  # GENDER

# Local suppression without importance vector
sdcCopy <- sdcCombined
sdcCombined <- localSuppression(sdcCombined, k = 2, importance = NULL)
# Number of suppressions per variable
print(sdcCombined, "ls")
# Number of suppressions for each value of AGEYRS
table(sdcCopy@manipKeyVars$AGEYRS) - table(sdcCombined@manipKeyVars$AGEYRS)
# Undo local suppression
sdcCombined <- undolast(sdcCombined)

# Local suppression with importance vector on AGEYRS and GENDER
sdcCombined <- localSuppression(sdcCombined, k = 2, importance = impVec2)
# Number of suppressions per variable
print(sdcCombined, "ls")
# Number of suppressions per for each value of 
table(sdcCopy@manipKeyVars$AGEYRS) - table(sdcCombined@manipKeyVars$AGEYRS)

#### Step 9b: Remeasure risk (individual level) ####
# k-anonymity
numIND <- length(fileCombined[,1]) # number of households

# Number of observations violating k-anonymity
print(sdcCombined)

# Calculate sample frequencies and count number of obs. violating k (3,5) - anonymity
kAnon3 <- sum(sdcCombined@risk$individual[,2] <3) 
kAnon5 <- sum(sdcCombined@risk$individual[,2] <5)
kAnon5

# As percentage of total
kAnon3 / numIND
kAnon5 / numIND

# Show lines with variables that violate k-anonymity
fileCombined[sdcCombined@risk$individual[,2] < 3,]
fileCombined[sdcCombined@risk$individual[,2] < 5,]

## SUDA score (SUDA DIS score)
# Save the number of observations with suda scores equal to 0 and between 0-0.1, 0.1-0.2, ..., 0.6-0.7
SUDA <- rep(0, 8)
SUDA[1] <- sum(suda2(sdcCombined)@risk$suda2$disScore == 0)

for (i in 1:7)
{
  SUDA[i+1] <- sum(suda2(sdcCombined)@risk$suda2$disScore <= i/10) - sum(SUDA[1:i])
}

names(SUDA) <- c('SUDA0', 'SUDA01', 'SUDA02', 'SUDA03', 'SUDA04','SUDA05','SUDA06','SUDA07')
print(SUDA)

# Look at high risk 
dim(fileCombined[sdcCombined@risk$individual[, "risk"] > 0.1,])
fileCombined[sdcCombined@risk$individual[, "risk"] > 0.1,]

# Check high risk individuals
dim(fileCombined[sdcCombined@risk$individual[, "risk"] > 0.1,])

# Global risk on individual level
print(sdcCombined, 'risk')
# Global risk percentage
sdcCombined@risk$global$risk
# Number of expected re-identifications
sdcCombined@risk$global$risk_ER

# Highest individual hierarchical risk
max(sdcCombined@risk$individual[,'hier_risk'])

#### Step 10b: Remeasure utility ####
# Individual weights
curW <- sdcCombined@origData[,'WGTHH']

# Number of observations
numObs <- length(sdcCombined@origData[,1])

# a) # Net enrolment in primary education, by gender
# Definition (UN): Net primary enrolment rate in primary education is the number of children of official primary 
# school age (according to ISCED971) (6-12) who are enrolled in primary education as a percentage of the total children 
# of the official school age population. Total net primary enrolment rate also includes children of primary school 
# age enrolled in secondary education.
# Net enrolment:in education & in age group / in age group
# Gross enrolment: in education / in age group 

# EDYRSCURRAT (years of education for currently enrolled)  
# Primary:            (EDYRSCURRAT>=1 & EDYRSCURRAT<=6)
# Secondary (lower):  (EDYRSCURRAT>=7 & EDYRSCURRAT<=10)
# By gender (0 - female, 1 - male)
educPrimMaleA    <-  sum(curW * (sdcCombined@manipKeyVars$EDYRSCURRAT >= 1 & sdcCombined@manipKeyVars$EDYRSCURRAT <= 6 & sdcCombined@manipKeyVars$AGEYRS >= 6 &  sdcCombined@manipKeyVars$AGEYRS <= 12  & sdcCombined@manipKeyVars$GENDER == 1), na.rm=TRUE) / sum(curW * (sdcCombined@manipKeyVars$AGEYRS >= 6 &  sdcCombined@manipKeyVars$AGEYRS <= 12  & sdcCombined@manipKeyVars$GENDER == 1), na.rm=TRUE)
educPrimFemaleA  <-  sum(curW * (sdcCombined@manipKeyVars$EDYRSCURRAT >= 1 & sdcCombined@manipKeyVars$EDYRSCURRAT <= 6 & sdcCombined@manipKeyVars$AGEYRS >= 6 &  sdcCombined@manipKeyVars$AGEYRS <= 12  & sdcCombined@manipKeyVars$GENDER == 0), na.rm=TRUE)/ sum(curW * (sdcCombined@manipKeyVars$AGEYRS >= 6 &  sdcCombined@manipKeyVars$AGEYRS <= 12  & sdcCombined@manipKeyVars$GENDER == 0), na.rm=TRUE)
educPrimTotA     <-  sum(curW * (sdcCombined@manipKeyVars$EDYRSCURRAT >= 1 & sdcCombined@manipKeyVars$EDYRSCURRAT <= 6 & sdcCombined@manipKeyVars$AGEYRS >= 6 &  sdcCombined@manipKeyVars$AGEYRS <= 12  ), na.rm=TRUE)/ sum(curW * (sdcCombined@manipKeyVars$AGEYRS >= 6 &  sdcCombined@manipKeyVars$AGEYRS <= 12  ), na.rm=TRUE)                                                                                                                  
educSecMaleA    <-  sum(curW * (sdcCombined@manipKeyVars$EDYRSCURRAT >= 7 & sdcCombined@manipKeyVars$EDYRSCURRAT <= 10 & sdcCombined@manipKeyVars$GENDER == 1), na.rm=TRUE) / sum(curW * (sdcCombined@manipKeyVars$AGEYRS >= 7 &  sdcCombined@manipKeyVars$AGEYRS <= 10  & sdcCombined@manipKeyVars$GENDER == 1), na.rm=TRUE)
educSecFemaleA   <-  sum(curW * (sdcCombined@manipKeyVars$EDYRSCURRAT >= 7 & sdcCombined@manipKeyVars$EDYRSCURRAT <= 10 & sdcCombined@manipKeyVars$GENDER == 0), na.rm=TRUE)/ sum(curW * (sdcCombined@manipKeyVars$AGEYRS >= 7 &  sdcCombined@manipKeyVars$AGEYRS <= 10  & sdcCombined@manipKeyVars$GENDER == 0), na.rm=TRUE)
educSecTotA      <-  sum(curW * (sdcCombined@manipKeyVars$EDYRSCURRAT >= 7 & sdcCombined@manipKeyVars$EDYRSCURRAT <= 10), na.rm=TRUE)/ sum(curW * (sdcCombined@manipKeyVars$AGEYRS >= 7 &  sdcCombined@manipKeyVars$AGEYRS <= 10  ), na.rm=TRUE)                                                                                                                  

educEnrolResA <- cbind(educPrimMaleA, educPrimFemaleA, educPrimTotA, educSecMaleA, educSecFemaleA, educSecTotA)
names(educEnrolResA) <- c('educPrimMale', 'educPrimFemale', 'educPrimTot', 'educSecMale', 'educSecFemale', 'educSecTot')      
print(educEnrolRes)
print(educEnrolResA)

#### Step 11: Audit and reporting ####
# Create reports with sdcMicro report() function
report(sdcHH, internal = F, filename = 'externalReport1.html') # external (brief) report 
report(sdcHH, internal = T, filename = 'internalReport1.html') # internal (extended) report 

# Create reports with sdcMicro report() function
report(sdcCombined, internal = F, filename = 'externalReport2.html') # external (brief) report 
report(sdcCombined, internal = T, filename = 'internalReport2.html') # internal (extended) report 

# Information for internal report
# Anonymization methods: see Steps 7a, 7b
# Selected (key) variables: see under HOUSEHOLD LEVEL and INDIVIDUAL LEVEL
# Risk measures: see Steps 5a, 5b, 8a and 8b

# 10 combinations with highest risk on household and individual level 
# (we have to recreate the sdc objects, since the results in the current ones pertain to the anonymized data)
sdcHHrisk       <- createSdcObj(dat=fileHH, keyVars=selectedKeyVarsHH, pramVars=pramVarsHH, weightVar=weightVarHH, numVars = numVarsHH)
sdcHHrisk@origData[sdcHHrisk@risk$individual[,'risk'] >= sort(sdcHHrisk@risk$individual[,'risk'], TRUE)[10], selectedKeyVarsHH]  # household level
sdcCombinedrisk <- createSdcObj(dat = fileCombined, keyVars = selectedKeyVarsIND, weightVar = selectedWeightVarIND, hhId = selectedHouseholdID)
sdcCombinedrisk@origData[sdcCombinedrisk@risk$individual[,'risk'] >= sort(sdcCombinedrisk@risk$individual[,'risk'], TRUE)[10], selectedKeyVarsIND]  # household level

# Frequencies before anonymization (see Step 1)
lapply(1:5, function(x){table(fileHH[,selectedKeyVarsHH][,x], useNA = "ifany")})
lapply(1:8, function(x){table(file[,selectedKeyVarsIND][,x], useNA = "ifany")})

# Frequencies after anonymization
selectedKeyVarsHH # (household level)
lapply(1:5, function(x){table(sdcHH@manipKeyVars[,selectedKeyVarsHH][,x], useNA = "ifany")})
selectedKeyVarsIND # (individual level)
lapply(1:8, function(x){table(sdcCombined@manipKeyVars[,selectedKeyVarsIND][,x], useNA = "ifany")})

# Number of suppressions per variable
print(sdcHH, 'ls')        # household level
print(sdcCombined, 'ls')  # individual level

# Summary statistics of continuous variables before anonymization
numVarsHH
lapply(1:23, function(x){summary(fileHH[,numVarsHH][,x])})

# Summary statistics of continuous variables after anonymization
numVarsHH
lapply(1:23, function(x){summary(sdcHH@manipNumVars[,numVarsHH][,x])})

# Information loss measures
sdcHH@utility

#### Step 12: Data release ####
# Anonymized dataset
# Household variables and individual variables
dataAnon <- extractManipData(sdcCombined, ignoreKeyVars = F, ignorePramVars = F, ignoreNumVars = F, ignoreStrataVar = F) #extracts all variables, not just the manipulated ones
names(dataAnon)
dim(dataAnon)

# Create STATA file
write.dta(dataframe = dataAnon, file= 'Case1DataAnon.dta', convert.dates=TRUE)
