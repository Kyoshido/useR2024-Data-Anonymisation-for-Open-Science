library(laeken)
data(eusilc)
data <- eusilc


KeyVars <- c("age",
             "rb090",
             "db040",
             "hsize",
             "pb220a"
             )

Weights<- "rb050" 

subVars <- c(KeyVars, 
             Weights)
subData <- data[,subVars]

library(sdcMicro)
objSDC <- createSdcObj(dat = subData,
                       keyVars = KeyVars,
                       weightVar = Weights)

individual_risk <- objSDC@risk$individual
indRisk_table <- cbind(subData,individual_risk)
indRisk_table
                
print(objSDC, type="kAnon")

## Print global risk
print(objSDC, "risk")

table(objSDC@manipKeyVars$age)


objSDC <- globalRecode(objSDC, 
                       column = c('age'), 
                       breaks = 10 * c(0:10))

table(objSDC@manipKeyVars$age)
# print K-anonymity
print(objSDC, type="kAnon")
## Top and Bottom Coding
table(objSDC@manipKeyVars$hsize)

objSDC <- topBotCoding(objSDC, 
                       column = c('hsize'), 
                       value = 8, 
                       replacement = 8, 
                       kind = "top")

table(objSDC@manipKeyVars$hsize)


print(objSDC, type="kAnon")


objSDC <- topBotCoding(objSDC, 
                       column = c('hsize'), 
                       value = 6, 
                       replacement = 6, 
                       kind = "top")
print(objSDC, type="kAnon")


## Create sdcMicro object
objSDC <- createSdcObj(dat = subData,
                       keyVars = KeyVars, 
                       weightVar = Weights)

objSDC <- localSuppression(objSDC, 
                           k = 3, 
importance = NULL)


calcRisks(objSDC)
