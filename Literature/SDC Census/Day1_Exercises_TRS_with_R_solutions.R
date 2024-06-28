#####################################################
# ESTP Course on Statistical disclosure control (SDC) methods and tools for census 2021
# 
# Exercises TRS with R
#

##
# load packages
library(data.table)
library(recordSwapping)

# read data
dat <- fread("~/sdctestdata/data/testdata/test_data_100k.csv.gz")
##

## Exercise 1 ---------------------------------
# 1.a look at data
View(dat)
print(dat)
summary(dat)

# number of persons
dat[,.N]
nrow(dat) # both the same

# number of households
uniqueN(dat$HID) # number of unique household ids

# check non-integer type
not_integer <- !sapply(dat,is.integer)
not_integer[not_integer]

# 1.b convert variables  
dat[,Y_coord:=as.integer(substr(L001000,5,8))]
dat[,X_coord:=as.integer(substr(L001000,10,13))]
dat[,L001000:=NULL]

dat[,.N,by=.(Size)] # number of persons per household size
dat[,Size:=pmin(5,Size)] # truncate Size with 5
dat[,.N,by=.(Size)] # number of persons per household size after transformation

age_levels <- levels(cut(1:101,breaks=c(seq(0,100,5),Inf),include.lowest = TRUE,right=FALSE))
dat[,AGE.M:=as.integer(factor(AGE.M,levels=age_levels))]


# 1.c save data
fwrite(dat,file="SDC Census/test_data_100k_cleaned.csv.gz",sep=";")

## --------------------------------------------

## Exercise 2 ---------------------------------
# set parameter
hierarchy <- c("NUTS1","NUTS2")
hid <- "HID"
risk_variables <- c("COC.M","POB.M")
k_anonymity <- 3
swaprate <- 0.05
similar <- "Size"
seed <- 202103

# 2.a apply record swapping
dat_swapped <- recordSwap(data = dat, hid = hid,
                          hierarchy = hierarchy,
                          similar = similar,
                          risk_variables = risk_variables,
                          k_anonymity = k_anonymity,
                          swaprate = swaprate,
                          return_swapped_id = TRUE,
                          seed = seed)

# swapped records = 19510
dat_swapped[HID!=HID_swapped,]
nrow(dat_swapped[HID!=HID_swapped,])

# 2.b swapped households
# filter all records where HID not equal HID_swapped
# and count number of unique household IDs
dat_swapped[HID!=HID_swapped,uniqueN(HID)]

# 2.c geographic variables are mixed up
# filter all records where HID not equal HID_swapped
# and select columns NUTS1, NUTS2, NUTS3 and LAU2
dat_swapped[HID!=HID_swapped,.(NUTS1,NUTS2,NUTS3,LAU2)]
# first digits of NUTS3 are not identical with NUTS2

# 2.d use parameter carry_along to fix this
carry_along = c("NUTS3",
                "LAU2","Y_coord",
                "X_coord")

dat_swapped <- recordSwap(data = dat, hid = hid,
                          hierarchy = hierarchy,
                          similar = similar,
                          risk_variables = risk_variables,
                          k_anonymity = k_anonymity,
                          carry_along = carry_along, # insert paramter
                          swaprate = swaprate,
                          return_swapped_id = TRUE,
                          seed = seed)

# check if NUTS2 is not equal the first 2 digits of NUTS3
dat_swapped[NUTS2 != substr(NUTS3,1,2)]
## --------------------------------------------

## Exercise 3 ---------------------------------
# 3.a calculate table 8.1
tab_vars <- c("NUTS3","SEX","COC.L")
tab1 <- dat[,.(N1=.N),by=c(tab_vars)]
tab2 <- dat_swapped[,.(N2=.N),by=c(tab_vars)]

tab_81 <- merge(tab1,tab2, all=TRUE)
tab_81[is.na(N1),N1:=0]
tab_81[is.na(N2),N2:=0]

## 3.b
# information loss measures

# mean absolute deviation per NUTS3
ad <- tab_81[,mean(abs(N1-N2)),by=.(NUTS3)]
ad
ad[NUTS3==111]
ad[,mean(V1)] # mean over AD

# sum of RAD by NUTS3
rad <- tab_81[,sum(abs(N1-N2)/N1),by=.(NUTS3)]
# take average
rad[,mean(V1)]

# hellingers distance by NUTS3
hd <- tab_81[,sqrt(1/2*sum((sqrt(N1)-sqrt(N2))^2)),
          by=.(NUTS3)]
hd[,mean(V1)]

res_81_1 <- infoLoss(dat, dat_swapped, table_vars = c("NUTS3","SEX","COC.L"))
res_81_1$measures
res_81_1$cumdistrabsD

## 3.c
# repeate exercise using swaprate 10% and 2.5%
dat_swapped_2 <- recordSwap(data = dat, hid = hid,
                          hierarchy = hierarchy,
                          similar = similar,
                          risk_variables = risk_variables,
                          k_anonymity = k_anonymity,
                          carry_along = carry_along,
                          swaprate = 0.1,
                          return_swapped_id = TRUE,
                          seed = seed)

dat_swapped_3 <- recordSwap(data = dat, hid = hid,
                             hierarchy = hierarchy,
                             similar = similar,
                             risk_variables = risk_variables,
                             k_anonymity = k_anonymity,
                             carry_along = carry_along,
                             swaprate = 0.025,
                             return_swapped_id = TRUE,
                             seed = seed)


res_81_2 <- infoLoss(dat, dat_swapped_2, table_vars = c("NUTS3","SEX","COC.L"))
res_81_2$measures
res_81_2$cumdistrabsD

res_81_3 <- infoLoss(dat, dat_swapped_3, table_vars = c("NUTS3","SEX","COC.L"))
res_81_3$measures
res_81_3$cumdistrabsD

# obvious drop of information loss when swaping fewer households
# Regardless of swaping rate high absolute difference in some cells
# this is can be caused by households with large household size
# large absolute difference should also only occure for cells with#
# high cell count

## --------------------------------------------


## Exercise 4 ---------------------------------
# 
similar <- c("Size","HST")

dat_swapped_4 <- recordSwap(data = dat, hid = hid,
                            hierarchy = hierarchy,
                            similar = similar,
                            risk_variables = risk_variables,
                            k_anonymity = k_anonymity,
                            carry_along = carry_along,
                            swaprate = swaprate,
                            return_swapped_id = TRUE,
                            seed = seed)

il <- infoLoss(dat, dat_swapped_4, table_vars = c("NUTS3","SEX","COC.L"))
il$measures
il$cumdistrabsD
il$cumdistrabsD[cat=="10"]
# ~ 68.4% of cells have difference not greater than 10
# slight improvement


# use more similarity files
similar <- list(c("NUTS1","Size","HST"),c("Size","HST"),"Size")

dat_swapped_5 <- recordSwap(data = dat, hid = hid,
                            hierarchy = hierarchy,
                            similar = similar,
                            risk_variables = risk_variables,
                            k_anonymity = k_anonymity,
                            carry_along = carry_along,
                            swaprate = 0.01,
                            return_swapped_id = TRUE,
                            seed = seed)

il <- infoLoss(dat, dat_swapped_5, table_vars = c("NUTS3","SEX","COC.L"))
il$measures
il$cumdistrabsD


