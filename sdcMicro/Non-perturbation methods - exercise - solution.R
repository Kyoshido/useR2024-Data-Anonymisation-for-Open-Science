###############################################################################

# sdcMicro

###############################################################################

# Load the required libraries
# install.packages("sdcMicro")
library(sdcMicro)

# Load the data
library(simPop) 
data("eusilc13puf")
data <- eusilc13puf
rm(eusilc13puf)
# https://rdrr.io/cran/simPop/man/eusilc13puf.html

# REVIEW DATA STRUCTURE
str(data)

# EXPLORE RELATIONSHIP BETWEEN VARIABLES

#  For our assessment, we want to pay particular attention to common key 
# variables like age, location and marital status.

# One way to explore the relationship between two categorical variables is 
# to create a crosstab. You can do this using the function table().
table(data$pb220a,
      data$rb090)

# For numeric variables, such as age, you can use the hist() function 
# to explore whether or not there are noticeable outliers.
hist(as.numeric(data$age))

# SELECTING KEY VARIABLES USING SDCMICRO

# The sdcMicro package is built around objects of class sdcMicroObj.
# In this section, we will walk through how to select key variables, set your 
# sample weight variable and create a subset of your datafile. 

# You will then use these new objects (selectedKeyVars, selectedWeights, 
# and dataSub) as arguments of the function ‘createSdcObj()’ in order to create 
# the sdcMicro object that you will use to assess the disclosure risk and apply
# disclosure control techniques.

# DEVELOP DISCLOSURE SCENARIOS
# To develop a disclosure scenario, you will need to think through 
# the motivations of any malicious actors, describe the data that they may have
# access to, and specify how this data and other publicly available data could 
# be linked to your data and lead to disclosure. This requires you to make 
# assumptions about what types of data and information others are likely to have 
# access to. If you are not sure, we recommend creating multiple disclosure
# scenarios based on different assumptions and run the disclosure risk 
# assessment on each.

# SELECT WEIGHT & KEY VARIABLE

# Now we need to select our key variables and sample weights. 
# After exploring the data and developing disclosure risk scenarios, 
# we identified the following key variables: 

data$hsize <- as.numeric(data$hsize)
data$age <- as.numeric(data$age)

selectedKeyVars <- c('age', # the person's age.
                    'rb090', # the person's gender
                    'db040', # the federal state in which the household is located
                    'hsize', # the number of persons in the household.
                    'pb220a' #  the person's citizenship
                    )
                    
selectedWeights <- c('rb050') # the personal sample weights

# CREATE SUBSET OF THE DATA FILE

# Now that we know the keys variables that we want to focus on we are going 
# to create a subset of the data file that contains only these variables and 
# our sample weights. Before we create our subset of the data file, we need to 
# convert the categorical variables from chr variables to factors. 

## Create Subset of Data File
subVars <- c(selectedKeyVars, 
             selectedWeights)
subData <- data[,subVars]

# CREATE SDCMICRO OBJECT

# Finally, you are ready to create the sdcMicro object. Use the arguments of 
# the function createSdcObj() to specify the data file (subData), the sample 
# weights (selectedWeights) and the keyVars (selectedKeyVars).

## Create sdcMicro object
objSDC <- createSdcObj(dat=subData,
                       keyVars = selectedKeyVars, 
                       weightVar = selectedWeights)

# CALCULATE INDIVIDUAL RISK, SAMPLE AND POPULATION FREQUENCY

# With your key variables selected and your sdcMicro object created, you are 
# ready to run the assessment. First, we are going to create a new data frame 
# that will combine the sample frequency (fk), population frequency (Fk) and 
# individual risk measures to the subset of the data file that we created in 
# step one (fileRes). Next, we will calculate the individual risk using the
# sdcMicro object that we just created. In order to make it easier to explore
# this data, we will assign its values to a new object (individual_risk) and 
# then create a new data frame using the cbind() function.

individual_risk <- objSDC@risk$individual
indRisk_table <- cbind(subData,individual_risk)
View(indRisk_table)

# REVIEW K-ANONYMITY

# Our next step is to review k-anonymity. Here, we will simply print 
# 2-. 3-, and 5- anonymity.

## print K-anonymity
print(objSDC, type="kAnon")

# As you can see, we have 2042 records that violate 2-anonymity.
# It is recommend that no record violates 3-anonymity. 
# From here, it’s # clear that we will need to apply statistical disclosure control before sharing
# this data publicly but before we do, we should review the global disclosure
# risk.

# CALCULATE GLOBAL RISK

# The final risk measure that we will calculate is the global risk measures. 
# The global risk of re-identification is 99.24% (very high!). 
# As we anticipated, we will need to apply disclosure control techniques before 
# sharing this data widely.


## Print global risk
print(objSDC, "risk")

# REDUCE THE DISCLOSURE RISK

# Given this high risk of re-identification, we will have to apply statistical 
# disclosure control techniques before sharing this data publicly. 
# In this section, we will walk through how to use the sdcMicro package to 
# apply two disclosure control techniques – global recoding and local 
# suppression.

# APPLY GLOBAL RECODING

# Global Recoding is a common disclosure control technique that involves 
# reducing the number of values a given variable can take. It is an effective 
# method for reducing the detail in the data while maintaining some of its 
# analytical power. For numeric variables, recoding involves creating intervals 
# or brackets (i.e. income or age). Recoding can also be used for categorical 
# variables. For example, a geographic variable could be aggregated into groups 
# like ‘north’, ‘south’, ‘east’ and ‘west’ or ‘urban’, ‘rural’ and ‘suburban’.

# In the example below, the age variable is converted into ten age brackets with 
# ten year increments and the total members variable is converted into five. 
# After recoding each variable, you can use the table() function to see 
# the distribution.

## Recode Age Variable
objSDC <- globalRecode(objSDC, 
                       column = c('age'), 
                       breaks = 10 * c(0:10))
table(objSDC@manipKeyVars$age)

## Recode Total Members Variables
objSDC <- globalRecode(objSDC, 
                       column = c('hsize'), 
                       breaks = c(0,1,2,3,4,5,9))
table(objSDC@manipKeyVars$hsize)

# Now that we have done this recoding, we want to determine the impact it has 
# had reducing the global risk of disclosure. As you can see below, recoding 
# these two variables reduces our global risk from 99.24% to 24.90%.
# A nearly 25% risk of disclosure is still quite high and so we will need apply
# additional disclosure control techniques before we share the data publicly.

print (objSDC,"risk")

# APPLY LOCAL SUPPRESSION

# If you want to further reduce the disclosure risk of your data, a next step 
# might be to apply local suppression. WIth local suppression, individual values 
# are suppressed (deleted) and replaced with NA (missing value). 
# In the example below, local suppression is applied to achieve 3-anonymity. 
# You can set K to whatever you’d like.

objSDC <- localSuppression(objSDC, 
                           k = 3, 
                           importance = NULL)
calcRisks(objSDC)


# REASSESS DISCLOSURE RISK

# Now that we have applied global recoding and local suppression, we again want 
# to reassess the disclosure risk before applying any additional disclosure 
# control. Ultimately, we have reduced the risk of disclosure from 99.24% to
# 3.93%. This would still be too high to share on HDX.

print(objSDC,"risk")

# QUANTIFY INFORMATION LOSS

# Before we decide to further reduce the risk of disclosure, we are going to 
# quantify the information loss to decide whether it makes sense or if we should 
# be exploring other ways to share the data. There are quite a few methods for 
# quantifying information loss and in this tutorial, we will not go into these 
# in too much detail.

# Below are two tables showing the distribution of the ethnolinguistic group 
# variable before and after treatment (local suppression). Overall, for this
# variable, 123 values were replaced with missing values and all but one of 
# these come from non-arab groups. For example, you can see in the second table 
# that all 5 Pamiri responses were suppressed. We could do the same of the other
# variables to see what impact the information loss will have on different forms
# of analysis. In the end, there is no objective way to determine whether the 
# information loss. We have to assess this in regards to the needs of downstream 
# users.

table(objSDC@origData[, c('pb220a','rb090')])
table(objSDC@manipKeyVars[, c('pb220a','rb090')])

table(objSDC@origData[, c('age','rb090')])
table(objSDC@manipKeyVars[, c('age','rb090')])

# From here, we could decide that the level of information loss is tolerable and 
# apply additional statistical disclosure techniques to further reduce the risk 
# of disclosure (at least below the 3% threshold for sharing data on HDX). 
# Alternatively, we could decide that the information loss is too high, 
# especially in regards to a few of our key variables, and instead explore other
# ways to share the data safely. 



