###############################################################################

# Target record swapping

###############################################################################

### Load the required libraries
# install.packages("sdcMicro")
# install.packages("data.table")
library(sdcMicro)
library(data.table)

# Load the data
library(laeken) 
data("eusilc")
dat <- as.data.table(eusilc)
class(dat)

### look at data
View(dat)
print(dat)
summary(dat)

# number of persons
dat[,.N]
nrow(dat) # both the same

# number of households
uniqueN(dat$db030) # number of unique household ids

### set parameter
hierarchy <- c("db040")
hid <- "db030"
risk_variables <- c('age', 
                    'rb090', 
                    'db040', 
                    'hsize',
                    'pb220a')
k_anonymity <- 3
swaprate <- 0.05
similar <- "hsize"
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

dat$db040 <- as.numeric(factor(dat$db040))
dat$rb090 <- as.numeric(factor(dat$rb090))
dat$pb220a <- as.numeric(factor(dat$pb220a))

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

sum(is.na(dat$pb220a))
dat <- dat[!is.na(dat$pb220a), ]

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
dat_swapped[db030!=db030_swapped,
            uniqueN(db030)]

### check geographic variables
# filter all records where HID not equal HID_swapped
# and select columns NUTS1, NUTS2, NUTS3 and LAU2
dat_swapped[db030!=db030_swapped]


