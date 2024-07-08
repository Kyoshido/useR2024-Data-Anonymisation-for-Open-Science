###############################################################################

# Target record swapping

###############################################################################

### Load the required libraries
# install.packages("sdcMicro")
# install.packages("data.table")
library(sdcMicro)
library(data.table)

### Read data
data(test_data_100k)
# dat <- fread("data/test_data_100k.csv.gz")
dat <- as.data.table(test_data_100k)
class(dat)

### look at data
View(dat)
print(dat)
summary(dat)

# number of persons
dat[,.N]
nrow(dat) # both the same

# number of households
uniqueN(dat$HID) # number of unique household ids

### set parameter
hierarchy <- c("NUTS1","NUTS2")
hid <- "HID"
risk_variables <- c("COC.M", # Country of citizenship
                    "POB.M") # Place of birth
k_anonymity <- 3
swaprate <- 0.05
similar <- "Size"
seed <- 123

### apply record swapping
dat_swapped <- recordSwap(data = dat, 
                          hid = hid,
                          hierarchy = hierarchy,
                          similar = similar,
                          risk_variables = risk_variables,
                          k_anonymity = k_anonymity,
                          swaprate = swaprate,
                          return_swapped_id = TRUE,
                          seed = seed)

### check swapped households
# filter all records where HID is not equal HID_swapped
# and count number of unique household IDs
dat_swapped[HID!=HID_swapped,
            uniqueN(HID)]

### check geographic variables
# filter all records where HID not equal HID_swapped
# and select columns NUTS1, NUTS2, NUTS3 and LAU2
dat_swapped[HID!=HID_swapped,.(NUTS1,NUTS2,NUTS3,LAU2)]
# geographic variables are mixed up
# first digits of NUTS3 are not identical with NUTS2

### apply new record swapping
# use parameter carry_along to fix this
carry_along = c("NUTS3",
                "LAU2")

dat_swapped <- recordSwap(data = dat, hid = hid,
                          hierarchy = hierarchy,
                          similar = similar,
                          risk_variables = risk_variables,
                          k_anonymity = k_anonymity,
                          carry_along = carry_along, # insert paramter
                          swaprate = swaprate,
                          return_swapped_id = TRUE,
                          seed = seed)

### check if NUTS2 is not equal the first 2 digits of NUTS3
dat_swapped[NUTS2 != substr(NUTS3,1,2)]






### information loss measures
# use this function to calculate tables for infromation loss
calc_table <- function(dat, dat_swapped, keyVars){
   
   tab1 <- dat[,.(N1=.N),by=c(keyVars)]
   tab2 <- dat_swapped[,.(N2=.N),by=c(keyVars)]
   
   tab <- merge(tab1,tab2, all=TRUE)
   tab[is.na(N1),N1:=0]
   tab[is.na(N2),N2:=0]
   return(tab)
}

# Create table for keyVars=c("NUTS3","SEX","COC.L")
tab_1 <- calc_table(dat,dat_swapped,
                     keyVars=c("NUTS3","SEX","COC.L"))
tab_1


# mean absolute deviation per NUTS3
ad_1 <- tab_1[,mean(abs(N1-N2)),by=.(NUTS3)]
ad_1
ad_1[,mean(V1)] # mean over AD

# sum of RAD by NUTS3
rad_1 <- tab_1[,sum(abs(N1-N2)/N1),by=.(NUTS3)]
# take average
rad_1[,mean(V1)]

# hellingers distance by NUTS3
hd_1 <- tab_1[,sqrt(1/2*sum((sqrt(N1)-sqrt(N2))^2)),
             by=.(NUTS3)]
hd_1[,mean(V1)]

###############################################################################

# Exercise  ---------------------------------
# repeate using swaprate 10% and 2.5%

