#########################
##### CASE STUDY 2 ######
#########################
#### Step 1: Need for confidentiality protection ####
# Statistical units and sensitive variables require confidentiality protection

#### Step 2: Data preparation and exploring characteristics ####
rm(list=ls())

# Install packages if not yet installed
list.of.packages <- c("foreign", "sdcMicro")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load required packages
library(foreign)    # for read/write function for STATA
library(sdcMicro)   # sdcMicro package

# Get current working directory
getwd()

# Set working directory (also writes output files here)
setwd("/Users/")

# Save working directory
mainDir <- getwd()
mainDir

# Specify file name of SUF file from case study 1
fname <- "Case1DataAnon.dta"

# Specify file name of original dataset (raw data)
fnameOrig <- "case_1_data.dta"

# Read-in files
file <- read.dta(fname, convert.factors = TRUE)         # SUF (case study 1)
fileOrig <- read.dta(fnameOrig, convert.factors = TRUE) # orginal dataset

# Dimensions of file (observations, variables)
dim(file)
dim(fileOrig)

# Names of variables in the data frame file
colnames(file)  

# Take subset for testing (if necessary)
#file <- file[1:3000,]

# Evaluate characteristics of variables
# Look at tabulations of categorical variables (and cross-tabulations)
# Pay attention to missing values
# Automatic loop to look at tabulations for all variables
varnum <- 1
while (varnum != "0"){
  print(colnames(file)[as.integer(varnum)]) # variable name
  print(table(file[,as.integer(varnum)], useNA = "ifany")) # tabulation
  flush.console()
  varnum <- readline("# Enter variable number (1-49) or enter 0 to continue: ")
}

# Look at summary statistics of continuous variables
# Automatic loop to look at summary statistics for all variables
varnum <- 1
while (varnum != "0"){
  print(colnames(file)[as.integer(varnum)])
  print(summary(file[,as.integer(varnum)]))
  flush.console()
  varnum <- readline("# Enter variable number (1-49) or enter 0 to continue: ")
}

# Look at first 20 lines and last 20 lines
head(file, n = 20)
tail(file, n = 20)

#### Step 3: Type of release ####
# PUF file, file available to the general public

#### Step 4: Intruder scenarios and choice of key variables ####
# No direct identifiers available (SUF file used)

# Household id and individual id in file

# The data contains a hierarchical structure (households)

##### Step 5: Determine key uses and select utility measures ####
# See case study description

############# HOUSEHOLD LEVEL ##################
# Categorical key variables at household level with their levels
# URBRUR        (area)                            1 - rural, 2 - urban  
# REGION        (region)                          6 regions, numbered 1-6
# HHSIZE        (household size)                  1 - 13
selectedKeyVarsHH <- c('URBRUR', 'REGION', 'HHSIZE')

# Continuous key variables
# TANHHEXP      (Total annual nominal husehold expenditures)
# INCGROSSHH    (Income - total)
numVarsHH         <- c('TANHHEXP', 'INCTOTGROSSHH')

# PRAM variables
# ROOF          (roof type)                   1 = Concrete/cement/ brick/stone, 2 = Wood, 3 = Bamboo/thatch, 4 = Tiles/shingles, 
#                                             5 = Tin/metal sheets, 6 = Makeshift, 9 = Other             
# WATER         (main source of water)        1 = Pipe (own tap), 2 = Public standpipe, 3 = Borehole, 4 = Wells (protected), 
#                                             5 = Wells (unprotected), 6 = Surface water, 7 = Rain water, 8 = Vendor/truck, 9 = Other
# TOILET        (main toilet facility)        1 = Flush toilet, 2 = Improved pit latrine, 3 = Pit latrine, 4 = No facility, 9 = Other
# ELECTCON      (electrcity)                  1 = Central/local, 2 = Solar/wind, 3 = Generator, 9 = Other, 0 = None
# FUELCOOK      (main cooking fuel)           1 = Firewood, 2 = Kerosene, 3 = Charcoal, 4 = Electricity, 5 = Gas, 9 = Other
# OWNMOTORCYCLE (ownership of motorcycle)     1 = YES, 0 = No
# CAR           (ownership of car)            1 = YES, 0 = No
# TV            (ownershipof television)      1 = YES, 0 = No
# LIVESTOCK     (number of large-sized livestock owned)
pramVarsHH        <- c('ROOF', 'TOILET', 'WATER', 'ELECTCON', 'FUELCOOK', 'OWNMOTORCYCLE', 'CAR', 'TV', 'LIVESTOCK')

# Household weight
weightVarHH       <- c('WGTPOP')

# Variables not suitable for release in PUF (HH level)
varsNotToBeReleasedHH   <- c("OWNAGLAND", "RELIG", "LANDSIZEHA")

# Vector with names of all HH level variables
HHVars <- c('IDH', selectedKeyVarsHH, pramVarsHH, numVarsHH, weightVarHH)

# Create subset of file with only HH variables
fileHH <- file[,HHVars]

# Remove duplicated rows based on IDH, one row per household in fileHH
fileHH      <- fileHH[which(!duplicated(fileHH$IDH)),]      # SUF file
fileOrigHH  <- fileOrig[which(!duplicated(fileOrig$IDH)),]  # original dataset

# Dimensions of fileHH
dim(fileHH)
dim(fileOrigHH)

# Create initial sdc object for household level variables
sdcHH <- createSdcObj(dat = fileHH, keyVars = selectedKeyVarsHH, 
                      pramVars = pramVarsHH, weightVar = weightVarHH, numVars = numVarsHH)
numHH <- length(fileHH[,1]) # number of households
numHH

# Summary overview of sdcHH object
sdcHH 

#### Step 6a: Assessing disclosure risk (household level) ####
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
fileHH[sdcHH@risk$individual[,2] < 3, selectedKeyVarsHH] # 3-anonimity
fileHH[sdcHH@risk$individual[,2] < 5, selectedKeyVarsHH] # 5-anonimity

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

# Global risk on household level
print(sdcHH, 'risk')
# Global risk percentage
sdcHH@risk$global$risk
# Number of expected re-identifications
sdcHH@risk$global$risk_ER

# Individual risk (per houshold)
# Highest individual risk
max(sdcHH@risk$individual[, "risk"])

# Show 10 households with highest risk
sdcHH@origData[which(sdcHH@risk$individual[, "risk"] %in% tail(sort(sdcHH@risk$individual[, "risk"]), 10)), selectedKeyVarsHH]
sdcHH@risk$individual[which(sdcHH@risk$individual[, "risk"] %in% tail(sort(sdcHH@risk$individual[, "risk"]), 10)), c("fk", "Fk")]

#### Step 7a: Assessing utility measures (household level) ####
# Only utility measures based on variables that are in the released PUF dataset

#### Step 8a: Choice and application of SDC methods (household variables) ####
# Removing variables

# Local suppression to achieve 5-anonimity
sdcHH <- localSuppression(sdcHH, k = 5, importance = NULL)          # no importance vector
print(sdcHH, "ls")

sdcHH <- undolast(sdcHH) # undo suppressions to see the effect of an importance vector

# Redo local suppression minimizing the number of suppressions in HHSIZE
sdcHH <- localSuppression(sdcHH, k = 5, importance = c(2, 3, 1)) 
print(sdcHH, "ls")

sdcHH <- undolast(sdcHH) # undo suppressions to see the effect of a  different importance vector

# Redo local suppression minimizing the number of suppressions in HHSIZE
sdcHH <- localSuppression(sdcHH, k = 5, importance = c(3, 2, 1)) 
print(sdcHH, "ls")

# PRAM
set.seed(10987)
# Replace PRAM variables in sdcMicro object sdcHH with the original raw values
sdcHH@origData[,pramVarsHH] <- fileHH[match(fileHH$IDH, fileOrigHH$IDH), pramVarsHH]
sdcHH@manipPramVars         <- fileHH[match(fileHH$IDH, fileOrigHH$IDH), pramVarsHH]

sdcHH <- pram(obj = sdcHH, pd = 0.6)

# Create bands (deciles) for income and expenditure variables (aggregates) based on the original data
decExp <- as.numeric(cut(fileOrigHH[match(fileHH$IDH, fileOrigHH$IDH), "TANHHEXP"], quantile(fileOrigHH[match(fileHH$IDH, fileOrigHH$IDH), "TANHHEXP"],(0:10)/10, na.rm = T), include.lowest = TRUE, labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
decInc <- as.numeric(cut(fileOrigHH[match(fileHH$IDH, fileOrigHH$IDH), "INCTOTGROSSHH"], quantile(fileOrigHH[match(fileHH$IDH, fileOrigHH$IDH), "INCTOTGROSSHH"],(0:10)/10, na.rm = T), include.lowest = TRUE, labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))

# Mean values of deciles
decExpMean <- round(sapply(split(fileOrigHH[match(fileHH$IDH, fileOrigHH$IDH), "TANHHEXP"], decExp), mean))
decIncMean <- round(sapply(split(fileOrigHH[match(fileHH$IDH, fileOrigHH$IDH), "INCTOTGROSSHH"], decInc), mean))

# Replace with mean value of decile
sdcHH@manipNumVars$TANHHEXP      <- decExpMean[match(decExp, names(decExpMean))]
sdcHH@manipNumVars$INCTOTGROSSHH <- decIncMean[match(decInc, names(decIncMean))]

# Recalculate risks after manually changing values in sdcMicro object
calcRisks(sdcHH)

# Extract data from sdcHH
HHmanip <- extractManipData(sdcHH) # manipulated variables HH

# Keep components of expenditures and income as share of total, use original data since previous data was perturbed
compExp <- c('TFOODEXP', 'TALCHEXP', 'TCLTHEXP', 'THOUSEXP', 'TFURNEXP', 'THLTHEXP', 'TTRANSEXP', 'TCOMMEXP', 'TRECEXP', 'TEDUEXP', 'TRESTHOTEXP', 'TMISCEXP')
compInc <- c('INCRMT', 'INCWAGE', 'INCFARMBSN', 'INCNFARMBSN', 'INCRENT', 'INCFIN', 'INCPENSN', 'INCOTHER')

HHmanip <- cbind(HHmanip, round(fileOrigHH[match(fileHH$IDH, fileOrigHH$IDH), compExp] / fileOrigHH[match(fileHH$IDH, fileOrigHH$IDH), "TANHHEXP"], 2))
HHmanip <- cbind(HHmanip, round(fileOrigHH[match(fileHH$IDH, fileOrigHH$IDH), compInc] / fileOrigHH[match(fileHH$IDH, fileOrigHH$IDH), "INCTOTGROSSHH"], 2))

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
# Highest individual risk
max(sdcHH@risk$individual[,1])

# Global risk on household level
print(sdcHH, 'risk')
# Global risk percentage
sdcHH@risk$global$risk
# Number of expected reindentifications
sdcHH@risk$global$risk_ER

# Risk evaluation continuous variables
dRisk(sdcHH@origData[,c("TANHHEXP", "INCTOTGROSSHH")], xm = sdcHH@manipNumVars[,c("TANHHEXP", "INCTOTGROSSHH")], k = 0.01)
dRisk(sdcHH@origData[,c("TANHHEXP", "INCTOTGROSSHH")], xm = sdcHH@manipNumVars[,c("TANHHEXP", "INCTOTGROSSHH")], k = 0.02)
dRisk(sdcHH@origData[,c("TANHHEXP", "INCTOTGROSSHH")], xm = sdcHH@manipNumVars[,c("TANHHEXP", "INCTOTGROSSHH")], k = 0.05)

#### Step 10a: Remeasure utility ####
# Decile dispersion ratio 
# raw data
mean(tail(sort(fileOrigHH$INCTOTGROSSHH), n = 200)) / mean(head(sort(fileOrigHH$INCTOTGROSSHH), n = 200))
mean(tail(sort(HHmanip$INCTOTGROSSHH), n = 197)) / mean(head(sort(HHmanip$INCTOTGROSSHH), n = 197))

# Share of total consumption by the poorest decile households
sum(head(sort(fileOrigHH$TANHHEXP), n = 200)) / sum(fileOrigHH$TANHHEXP)
sum(head(sort(HHmanip$TANHHEXP), n = 197)) / sum(HHmanip$TANHHEXP)

############# INDIVIDUAL LEVEL ##################
### Select variables (individual level)
# GENDER (gender)                           0 - female, 1 - male
# REL (relationship to household head):     1 - Head, 2 - Spouse, 3 - Child, 4 - Father/Mother, 6 - Son/Daughter in law
#                                           7 - Other relative, 8 - Domestic help, 9 - Non-relative  
# MARITAL (marital status):                 1 - never married, 2 - married monogamous, 3 - married polygamous
#                                           5 - divorced/separated, 6 - widowed
# AGEYRS (age in completed years)           numeric in years
# EDUCY (highest level of educ compl)       0 - no educ, 1 - Pre-school/ primary, not completed,
#                                           2 - Completed primary, but less than completed, 3 - Completed lower secondary but less than
#                                           4 - Completed upper secondary, 5 -  Post-secondary technical , 6 - University and higher 
# INDUSTRY1 (1 digit industry classific.)   1-10 
selectedKeyVarsIND = c('GENDER', 'REL', 'MARITAL', 'AGEYRS', 'EDUCY', 'INDUSTRY1') # list of selected key variables

# sample weight (WGTHH, individual weight)
selectedWeightVarIND = c('WGTHH')

# Household ID
selectedHouseholdID = c('IDH')

# Variables not suitable for release in PUF (IND level)
varsNotToBeReleasedIND   <- c("ATSCHOOL", "EDYRSCURRAT")

# All individual level variables
INDVars <- c(selectedKeyVarsIND)

# Recombining anonymized HH data sets and individual level variables
indVars <- c("IDH", "IDP", selectedKeyVarsIND, "WGTHH") # HID and all non HH vars
fileInd <- file[indVars] # subset of file without HHVars
fileCombined <- merge(HHmanip, fileInd, by.x = c('IDH'))
fileCombined <- fileCombined[order(fileCombined[,'IDH'], fileCombined[,'IDP']),]
dim(fileCombined)

# SDC objects with only IND level variables
sdcCombined    <- createSdcObj(dat = fileCombined, keyVars = c(selectedKeyVarsIND), weightVar = selectedWeightVarIND, hhId = selectedHouseholdID)
# SDC objects with both HH and IND level variables
sdcCombinedAll <- createSdcObj(dat = fileCombined, keyVars = c(selectedKeyVarsIND, selectedKeyVarsHH ), weightVar = selectedWeightVarIND, hhId = selectedHouseholdID)

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
dim(fileCombined[sdcCombined@risk$individual[, "hier_risk"] > 0.01,])
fileCombined[sdcCombined@risk$individual[, "hier_risk"] > 0.01,]
max(sdcCombined@risk$individual[, "hier_risk"])

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

# Show 10 individuals with highest risk
sdcCombined@origData[which(sdcCombined@risk$individual[, "risk"] %in% tail(sort(sdcCombined@risk$individual[, "risk"]), 10)), selectedKeyVarsIND]
sdcCombined@risk$individual[which(sdcCombined@risk$individual[, "risk"] %in% tail(sort(sdcCombined@risk$individual[, "risk"]), 10)), c("fk", "Fk")]

#### Step 7b: Assessing utility measures (household level) ####
# See step case study 1 for comparison of utility measures before anonymization
# Only utility measures based on variables that are in the released PUF dataset

#### Step 8b: Choice and application of SDC methods (individual level) ####
# GENDER
table(sdcCombined@manipKeyVars$GENDER, useNA = "ifany")

# Recode REL (relation to household head)
table(sdcCombined@manipKeyVars$REL, useNA = "ifany")
#  1 - Head, 2 - Spouse, 3 - Child, 4 - Father/Mother, 5 - Grandchild, 6 - Son/Daughter in law
#  7 - Other relative, 8 - Domestic help, 9 - Non-relative  
sdcCombined <- groupVars(sdcCombined, var = "REL", before = c("4", "5", "6", "7"), after = c("7", "7", "7", "7")) # other relative
sdcCombined <- groupVars(sdcCombined, var = "REL", before = c("8", "9"), after = c("9", "9")) # other
table(sdcCombined@manipKeyVars$REL, useNA = "ifany")

# Recode MARITAL (marital status)
table(sdcCombined@manipKeyVars$MARITAL, useNA = "ifany")
# 1 - Never married, 2 - Married monogamous, 3 - Married polygamous, 
# 4 - Common law, union coutumiere, union libre, living together, 5 - Divorced/Separated, 6 - Widowed
sdcCombined <- groupVars(sdcCombined, var = "MARITAL", before = c("2", "3", "4"), after = c("2", "2", "2")) # married/living together
sdcCombined <- groupVars(sdcCombined, var = "MARITAL", before = c("5", "6"), after = c("9", "9")) # divorced/seperated/widowed
table(sdcCombined@manipKeyVars$MARITAL, useNA = "ifany")

# Recode AGEYRS (0-15 years)
table(sdcCombined@manipKeyVars$AGEYRS, useNA = "ifany")
sdcCombined <- groupVars(sdcCombined, var = "AGEYRS", before = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
                         , after = rep("7", 15))
table(sdcCombined@manipKeyVars$AGEYRS, useNA = "ifany")
sdcCombined <- calcRisks(sdcCombined)

# Recode EDUCY (highest level of educ compl) 
table(sdcCombined@manipKeyVars$EDUCY, useNA = "ifany")
# 0 - No education, 1 - Pre-school/ Primary not completed, 2 - Completed primary, but less than completed lower secondary
# 3 - Completed lower secondary (or post-primary vocational education) but less than completed upper secondary
# 4 - Completed upper secondary (or extended vocational/technical education), 5 - Post secondary technical
# 6 - University and higher
sdcCombined <- groupVars(sdcCombined, var = "EDUCY", before = c("3", "4", "5", "6"), after = c("3", "3", "3", "3")) # Completed lower secondary or higher
table(sdcCombined@manipKeyVars$EDUCY, useNA = "ifany")

# Recode INDUSTRY1 (industry type) 
table(sdcCombined@manipKeyVars$INDUSTRY1, useNA = "ifany")
# 1 - Agriculture and Fishing, 2 - Mining, 3 - Manufacturing, 4 - Electricity and Utilities
# 5 - Construction, 6 - Commerce, 7 - Transportation, Storage and Communication, 8 - Financial, Insurance and Real Estate
# 9 - Services: Public Administration, 10 - Other Services, 11 - Unspecified
sdcCombined <- groupVars(sdcCombined, var = "INDUSTRY1", before = c("1", "2"), after = c("1", "1")) # primary
sdcCombined <- groupVars(sdcCombined, var = "INDUSTRY1", before = c("3", "4", "5"), after = c("2", "2", "2")) # secondary
sdcCombined <- groupVars(sdcCombined, var = "INDUSTRY1", before = c("6", "7", "8", "9", "10"), after = c("3", "3", "3", "3", "3")) # tertiary
table(sdcCombined@manipKeyVars$INDUSTRY1, useNA = "ifany")

sdcCombined <- calcRisks(sdcCombined)

# Local suppression without importance vector
sdcCopy <- sdcCombined
sdcCombined <- localSuppression(sdcCombined, k = 5, importance = NULL)

# Number of suppressions per variable
print(sdcCombined, "ls")

# Undo local suppression
sdcCombined <- undolast(sdcCombined)

sdcCombined <- localSuppression(sdcCombined, k = 5, importance =  c(2,4,3,1,6,6))

# Number of suppressions per variable
print(sdcCombined, "ls")

# Undo local suppression
sdcCombined <- undolast(sdcCombined)

# Redo local suppression without importance vector
sdcCombined <- localSuppression(sdcCombined, k = 5, importance =  NULL)

# Number of suppressions per variable
print(sdcCombined, "ls")

# Extract data from sdcCombined
dataAnon <- extractManipData(sdcCombined, ignoreKeyVars = F, ignorePramVars = F, ignoreNumVars = F, ignoreStrataVar = F) #extracts all variables, not just the manipulated ones

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

## SUDA score (SUDA DIS score)
# Save the number of observations with suda scores equal to 0 and between 0-0.1, 0.1-0.2, ..., 0.6-0.7
SUDA <- rep(0, 8)
SUDA[1] <- sum(suda2(sdcHH)@risk$suda2$disScore == 0)

for (i in 1:7)
{
  SUDA[i+1] <- sum(suda2(sdcHH)@risk$suda2$disScore <= i/10) - sum(SUDA[1:i])
}

names(SUDA) <- c('SUDA0', 'SUDA01', 'SUDA02', 'SUDA03', 'SUDA04','SUDA05','SUDA06','SUDA07')
print(SUDA)

# Look at high risk 
dim(fileCombined[sdcCombined@risk$individual[, "risk"] > 0.01,])
fileCombined[sdcCombined@risk$individual[, "risk"] > 0.01,]

# Highest individual hierarchical risk
max(sdcCombined@risk$individual[,'hier_risk'])

# Global risk on individual level
print(sdcCombined, 'risk')
# Global risk percentage
sdcCombined@risk$global$hier_risk
# Number of expected re-identifications
sdcCombined@risk$global$hier_risk_ER

#### Step 10b: Remeasure utility ####
# Examples of tabulations
table(file$MARITAL, file$URBRUR) # before anonymization
table(sdcCombined@manipKeyVars$MARITAL, sdcCombined@origData$URBRUR) # after anonymization, origData includes the treated HH level variables

table(file$INDUSTRY1, file$EDUCY) # before anonymization
table(sdcCombined@manipKeyVars$INDUSTRY1, sdcCombined@manipKeyVars$EDUCY) # after anonymization

#### Step 11: Audit and reporting ####
# Information for custom reports
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
selectedKeyVarsHH # (household level)
lapply(1:3, function(x){table(fileOrigHH[,selectedKeyVarsHH][,x], useNA = "ifany")})
selectedKeyVarsIND # (individual level)
lapply(1:6, function(x){table(fileOrig[,selectedKeyVarsIND][,x], useNA = "ifany")})

# Frequencies after anonymization
selectedKeyVarsHH # (household level)
lapply(1:3, function(x){table(sdcHH@manipKeyVars[,selectedKeyVarsHH][,x], useNA = "ifany")})
selectedKeyVarsIND # (individual level)
lapply(1:6, function(x){table(sdcCombined@manipKeyVars[,selectedKeyVarsIND][,x], useNA = "ifany")})

# Number of suppressions per variable
print(sdcHH, 'ls')        # household level
print(sdcCombined, 'ls')  # individual level

# Summary statistics of continuous variables before anonymization
numVarsHH
lapply(1:2, function(x){summary(fileOrigHH[,numVarsHH][,x])})

# Summary statistics of continuous variables after anonymization
numVarsHH
lapply(1:2, function(x){summary(sdcHH@manipNumVars[,numVarsHH][,x])})

# Information loss measures
sdcHH <- dUtility(sdcHH)
sdcHH@utility

#### Step 12: Data release ####
# Anonymized dataset
# Household variables and individual variables
# Check dimensions and first 6 lines
dim(dataAnon)
head(dataAnon)

# Randomize order of households dataAnon and recode IDH to random number (sort file by region)
set.seed(97254)

# Sort by region
dataAnon <- dataAnon[order(dataAnon$REGION),]

# Number of records per region
indperregion <- table(dataAnon$REGION, useNA="ifany")

# Number of households per region
hhperregion <- table(dataAnon[match(unique(dataAnon$IDH), dataAnon$IDH), "REGION"], useNA = "ifany")

# Randomized IDH (household ID)
randomHHid <-  c(sample(1:hhperregion[1], hhperregion[1]), unlist(lapply(1:(length(hhperregion)-1), function(i){sample((sum(hhperregion[1:i]) + 1): sum(hhperregion[1:(i+1)]), hhperregion[(i+1)])})))
dataAnon$IDH <- rep(randomHHid, table(dataAnon$IDH)[match(unique(dataAnon$IDH), as.numeric(names(table(dataAnon$IDH))))])

# Sort by IDH (and region implicitly)
dataAnon <- dataAnon[order(dataAnon$IDH),]

# Create STATA file
write.dta(dataframe = dataAnon, file= 'Case2DataAnon.dta', convert.dates=TRUE)
