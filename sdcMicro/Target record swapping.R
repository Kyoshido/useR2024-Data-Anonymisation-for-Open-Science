###############################################################################

# Target record swapping

###############################################################################

# Load the required libraries
# install.packages("sdcMicro")
# install.packages("data.table")
library(sdcMicro)
library(data.table)

# Read data
dat <- fread("data/test_data_100k.csv.gz")
class(dat)

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

dat[,AGE.M:=as.integer(factor(AGE.M))]

# 1.c save data
fwrite(dat,file="test_data_100k_cleaned.csv.gz",sep=";")

## --------------------------------------------

## Exercise 2 ---------------------------------
# set parameter
hierarchy <- c("NUTS1","NUTS2")
hid <- "HID"
risk_variables <- c("COC.M","POB.M")
k_anonymity <- 3
swaprate <- 0.05
similar <- "Size"
seed <- 123

# 2.a apply record swapping
dat_swapped <- recordSwap(data = dat, 
                          hid = hid,
                          hierarchy = hierarchy,
                          similar = similar,
                          risk_variables = risk_variables,
                          k_anonymity = k_anonymity,
                          swaprate = swaprate,
                          return_swapped_id = TRUE,
                          seed = seed)

# 2.b swapped households
# filter all records where HID is not equal HID_swapped
# and count number of unique household IDs
dat_swapped[HID!=HID_swapped,uniqueN(HID)]

# 2.c geographic variables are mixed up
# filter all records where HID not equal HID_swapped
# and select columns NUTS1, NUTS2, NUTS3 and LAU2
dat_swapped[HID!=HID_swapped,.(NUTS1,NUTS2,NUTS3,LAU2)]
# first digits of NUTS3 are not identical with NUTS2

# 2.d use parameter carry_along to fix this
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

# optionally make helpfunction
calc_table <- function(dat, dat_swapped, keyVars){
  
  tab1 <- dat[,.(N1=.N),by=c(keyVars)]
  tab2 <- dat_swapped[,.(N2=.N),by=c(keyVars)]
  
  tab <- merge(tab1,tab2, all=TRUE)
  tab[is.na(N1),N1:=0]
  tab[is.na(N2),N2:=0]
  return(tab)
}
tab_81 <- calc_table(dat,dat_swapped,
                     keyVars=c("NUTS3","SEX","COC.L"))
tab_81[,NUTS2:=substr(NUTS3,1,2)]

## 3.b
# information loss measures

# mean absolute deviation per NUTS3
ad <- tab_81[,mean(abs(N1-N2)),by=.(NUTS3)]
ad
ad[,mean(V1)] # mean over AD

# sum of RAD by NUTS3
rad <- tab_81[,sum(abs(N1-N2)/N1),by=.(NUTS3)]
# take average
rad[,mean(V1)]

# hellingers distance by NUTS3
hd <- tab_81[,sqrt(1/2*sum((sqrt(N1)-sqrt(N2))^2)),
             by=.(NUTS3)]
hd[,mean(V1)]

res_81_1 <- cellKey::ck_cnt_measures(tab_81$N1,tab_81$N2)
res_81_1$measures

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


tab_81_2 <- calc_table(dat,dat_swapped_2,
                       keyVars=c("NUTS3","SEX","COC.L"))
tab_81_3 <- calc_table(dat,dat_swapped_3,
                       keyVars=c("NUTS3","SEX","COC.L"))

# information loss for 81_2
res_81_2 <- cellKey::ck_cnt_measures(tab_81_2$N1,tab_81_2$N2)
res_81_2$measures

# mean absolute deviation per NUTS3
ad <- tab_81_2[,mean(abs(N1-N2)),by=.(NUTS3)]
ad
ad[,mean(V1)] # mean over AD

# sum of RAD by NUTS3
rad <- tab_81_2[,sum(abs(N1-N2)/N1),by=.(NUTS3)]
# take average
rad[,mean(V1)]

# hellingers distance by NUTS3
hd <- tab_81_2[,sqrt(1/2*sum((sqrt(N1)-sqrt(N2))^2)),
               by=.(NUTS3)]
hd[,mean(V1)]


# information loss for 81_3
res_81_3 <- cellKey::ck_cnt_measures(tab_81_3$N1,tab_81_3$N2)
res_81_3$measures

# mean absolute deviation per NUTS3
ad <- tab_81_3[,mean(abs(N1-N2)),by=.(NUTS3)]
ad
ad[,mean(V1)] # mean over AD

# sum of RAD by NUTS3
rad <- tab_81_3[,sum(abs(N1-N2)/N1),by=.(NUTS3)]
# take average
rad[,mean(V1)]

# hellingers distance by NUTS3
hd <- tab_81_3[,sqrt(1/2*sum((sqrt(N1)-sqrt(N2))^2)),
               by=.(NUTS3)]
hd[,mean(V1)]


# obvious drop of information loss when swaping fewer households
# Regardless of swaping rate high absolute difference in some cells
# this is can be caused by households with large household size
# large absolute difference should also only occure for cells with#
# high cell count

tab_81_3[,summary(N1)] # median cell count 375.5
tab_81_3[abs(N1-N2)>30] # only occurs in cells with cell count > 1000
## --------------------------------------------


## Exercise 4 ---------------------------------
# 4.a and 4.b
# Construct tables 26.2 and 26.3
tab_262 <- calc_table(dat,dat_swapped,
                      keyVars=c("NUTS2","SEX","AGE.M","HST","POB.L"))
res_262 <- cellKey::ck_cnt_measures(tab_262$N1,tab_262$N2)
res_262

# mean absolute deviation per NUTS2
ad <- tab_262[,mean(abs(N1-N2)),by=.(NUTS2)]
ad
ad[,mean(V1)] # mean over AD

# aggregate NUTS2, SEX, HST
rad <- tab_262[,.(N1=sum(N1),N2=sum(N2)),by=.(NUTS2,HST,POB.L)]
# sum of RAD by NUTS2
rad <- rad[,sum(abs(N1-N2)/N1),by=.(NUTS2)]
# take average
rad[,mean(V1)] # this is infinite... switched zero cell to non zero cell

# hellingers distance by NUTS2
hd <- tab_262[,sqrt(1/2*sum((sqrt(N1)-sqrt(N2))^2)),
              by=.(NUTS2)]
hd[,mean(V1)]



tab_263 <- calc_table(dat,dat_swapped,
                      keyVars=c("NUTS1","SEX","AGE.M","HST","POB.L","COC.L"))
res_263 <- cellKey::ck_cnt_measures(tab_263$N1,tab_263$N2)
res_263

# mean absolute deviation per NUTS1
ad <- tab_263[,mean(abs(N1-N2)),by=.(NUTS1)]
ad
ad[,mean(V1)] # mean over AD

# aggregate NUTS2, SEX, HST, POB.L
rad <- tab_263[,.(N1=sum(N1),N2=sum(N2)),by=.(NUTS1,HST,POB.L)]
# sum of RAD by NUTS1
rad <- rad[,sum(abs(N1-N2)/N1),by=.(NUTS1)]
# take average
rad[,mean(V1)]

# hellingers distance by NUTS1
hd <- tab_263[,sqrt(1/2*sum((sqrt(N1)-sqrt(N2))^2)),
              by=.(NUTS1)]
hd[,mean(V1)]


# 4.c
dat_swapped_4 <- recordSwap(data = dat, hid = hid,
                            hierarchy = hierarchy,
                            similar = c("Size","HST"),
                            risk_variables = risk_variables,
                            k_anonymity = k_anonymity,
                            carry_along = carry_along,
                            swaprate = swaprate,
                            return_swapped_id = TRUE,
                            seed = seed)


# calculate information loss on table 26.2
tab_262_4 <- calc_table(dat,dat_swapped_4,
                        keyVars=c("NUTS2","SEX","AGE.M","HST","POB.L"))
res_262_4 <- cellKey::ck_cnt_measures(tab_262_4$N1,tab_262_4$N2)
res_262_4

# mean absolute deviation per NUTS1
ad <- tab_262_4[,mean(abs(N1-N2)),by=.(NUTS2)]
ad
ad[,mean(V1)] # mean over AD

# aggregate NUTS2, SEX, HST, POB.L
rad <- tab_262_4[,.(N1=sum(N1),N2=sum(N2)),by=.(NUTS2,HST,POB.L)]
# sum of RAD by NUTS2
rad <- rad[,sum(abs(N1-N2)/N1),by=.(NUTS2)]
# take average
rad[,mean(V1)]

# hellingers distance by NUTS2
hd <- tab_262[,sqrt(1/2*sum((sqrt(N1)-sqrt(N2))^2)),
              by=.(NUTS2)]
hd[,mean(V1)]

# calculate information loss on table 26.3
tab_263_4 <- calc_table(dat,dat_swapped_4,
                        keyVars=c("NUTS1","SEX","AGE.M","HST","POB.L","COC.L"))
res_263_4 <- cellKey::ck_cnt_measures(tab_263_4$N1,tab_263_4$N2)
res_263_4

# mean absolute deviation per NUTS1
ad <- tab_263_4[,mean(abs(N1-N2)),by=.(NUTS1)]
ad
ad[,mean(V1)] # mean over AD

# aggregate NUTS2, SEX, HST, POB.L
rad <- tab_263_4[,.(N1=sum(N1),N2=sum(N2)),by=.(NUTS1,HST,POB.L)]
# sum of RAD by NUTS1
rad <- rad[,sum(abs(N1-N2)/N1),by=.(NUTS1)]
# take average
rad[,mean(V1)] # no longer infinte value for RAD
# this is due to the similarity variable HST

# hellingers distance by NUTS1
hd <- tab_263_4[,sqrt(1/2*sum((sqrt(N1)-sqrt(N2))^2)),
                by=.(NUTS1)]
hd[,mean(V1)]

# information loss slightly reduced when introducing mutliple similarity vaiables
# since households which are more similar to each other are swapped

