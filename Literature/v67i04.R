###################################################
### create sdcMicroObj
###################################################
library("sdcMicro")
data("testdata", package = "sdcMicro")

sdc <- createSdcObj(testdata,
   keyVars = c("urbrur", "water", "sex", "age"), 
   numVars = c("expend", "income", "savings"),
   pramVars = "walls", 
   w = "sampling_weight", 
   hhId = "ori_hid")


###################################################
### slots
###################################################
slotNames(sdc)


###################################################
### show-method
###################################################
sdc


###################################################
### microaggregation
###################################################
sdc <- microaggregation(sdc)


###################################################
### access
###################################################
ut <- get.sdcMicroObj(sdc, "utility")
cat <- get.sdcMicroObj(sdc, "keyVars")
dat <- extractManipData(sdc)

str(dat)


###################################################
### print risk
###################################################
print(sdc, "risk")


###################################################
### print methods
###################################################
print(sdc)
print(sdc, "ls")
print(sdc, type = "recode")
print(sdc, type = "risk")
print(sdc, type = "numrisk")
print(sdc, type = "pram")


###################################################
### undo last
###################################################
sdc <- undolast(sdc)


###################################################
### get individual risk
###################################################
head(get.sdcMicroObj(sdc, type="risk")$individual)


###################################################
### print method
###################################################
print(sdc)


###################################################
### ldiversity
###################################################
res1 <- ldiversity(testdata, 
  keyVars = c("urbrur", "water", "sex", "age"),
  ldiv_index = "income")
print(res1)


###################################################
### suda2
###################################################
sdc <- suda2(sdc)
get.sdcMicroObj(sdc, type = "risk")$suda


###################################################
### indivRisk
###################################################
head(get.sdcMicroObj(sdc, "risk")$individual)


###################################################
### print risk 3
###################################################
print(sdc, "risk")


###################################################
### model-based Risk
###################################################
sdc <- LLmodGlobalRisk(sdc, form = ~ urbrur + water + sex + age)
get.sdcMicroObj(sdc, "risk")$model$gr1


###################################################
### print numeric risk
###################################################
print(sdc, "numrisk")


###################################################
### robust risk
###################################################
sdc <- dRiskRMD(sdc)


###################################################
### global recoding
###################################################
sdc <- globalRecode(sdc, column = "age",
  breaks = c(1, 9, 19, 29, 39, 49, 59, 69, 100), labels = 1:8)
print(sdc)


###################################################
### local suppression
###################################################
sdc <- localSuppression(sdc)
print(sdc, "risk")


###################################################
### riskafterls
###################################################
print(sdc, "ls")


###################################################
### dataForPram
###################################################
set.seed(1234)
A <- as.factor(rep(c("A1", "A2", "A3"), each = 5))
A


###################################################
### pram
###################################################
Apramed <- pram(A)
Apramed


###################################################
### summary pram
###################################################
summary(Apramed)


###################################################
### pram2
###################################################
sdc <- pram(sdc)
print(sdc, "pram")


###################################################
### microaggregation3
###################################################
sdc <- microaggregation(sdc, aggr = 4, 
            strata_variables = "age", method = "mdav")
print(sdc, "numrisk")


###################################################
### addNoise
###################################################
sdc <- undolast(sdc)
sdc <- addNoise(sdc, method = "correlated2")
print(sdc, "numrisk")


###################################################
### shuffle
###################################################
sdc <- undolast(sdc)
form <- formula(expend + income + savings ~ age + urbrur + water +
  electcon + age + sex, data = testdata)
sdc <- shuffle(sdc, form)
print(sdc, "numrisk")


###################################################
### ulitily
###################################################
get.sdcMicroObj(sdc, "utility")


###################################################
### additional-results
###################################################
require("laeken")
sdc@additionalResults$gpg <- gpg(inc = "income",
      method = "mean", gender = "sex", weights = "sampling_weight",
      breakdown = "water", 
      data = extractManipData(sdc))$valueByStratum$value


###################################################
### absolute error
###################################################
testdata$sex <- as.factor(testdata$sex)
res <- gpg(inc = "income",
      method = "mean", gender = "sex", weights = "sampling_weight",
      breakdown = "water", data = testdata)$valueByStratum$value
options(scipen=999)
100 * abs((res - sdc@additionalResults$gpg)/res)


###################################################
### absolute error 2
###################################################
sdc <- undolast(sdc)
sdc <- microaggregation(sdc)
sdc@additionalResults$gpg <- gpg(inc = "income",
      method = "mean", gender = "sex", weights = "sampling_weight",
      breakdown = "water", data = extractManipData(sdc))$valueByStratum$value
100 * abs((res - sdc@additionalResults$gpg) / res)


###################################################
### Figure 2: 
### Computation time for new sdcMicro:
###################################################

library("sdcMicro")
library("scales")
library("ggplot2")
data("testdata")

## resample data function
genData <- function(x, n) {
  s <- sample(1:nrow(x), n, replace = TRUE)
  x[s, ]
}

## data sizes
SEQ <- c(10000, 50000, 1e+05, 250000, 5e+05, 750000, 1e+06, 1500000, 2e+06, 3e+06, 
  4e+06, 5e+06, 7500000, 1e+07)

## make sdc objects
sdc <- list()
for (i in 1:length(SEQ)) {
  g <- genData(testdata, SEQ[i])
  cat("\n SEQ", SEQ[i])
  sdc[[i]] <- createSdcObj(g, keyVars = c("urbrur", "water", "sex", "age"), w = "sampling_weight", 
    hhId = "ori_hid")
  sdc[[i]] <- globalRecode(sdc[[i]], column = "age", breaks = c(1, 9, 19, 29, 39, 
    49, 59, 69, 100), labels = 1:8)
}

## Computation time for new sdcMicro:
measureTime <- function(sdc, method) {
  n <- length(sdc)
  res <- lapply(sdc, function(x) {
    print(nrow(x@origData))
    system.time(method(x))[1]
  })
  return(res)
}

r <- measureTime(sdc, measure_risk)
r3 <- measureTime(sdc, localSuppression)

# save(r, r3, file = "rr3.RData")
rm(sdc)


### Computation time for old version of sdcMicro restart R, eventually do
### remove.packages('sdcMicro')
detach("package:sdcMicro")
library("devtools")
install_github("alexkowa/sdcMicro", ref = "sdcMicro-4.0.4forTesting")
library("scales")
library("ggplot2")
library("sdcMicro")
packageVersion("sdcMicro")
# [1] "4.0.4"
data("testdata")

## data sizes
SEQold <- c(10000, 50000, 1e+05, 250000)

## make sdc objects
sdc <- list()
for (i in 1:length(SEQold)) {
  g <- genData(testdata, SEQold[i])
  rownames(g) <- 1:nrow(g)
  cat("\n SEQ", SEQold[i])
  sdc[[i]] <- createSdcObj(g, keyVars = c("urbrur", "water", "sex", "age"), w = "sampling_weight", 
    hhId = "ori_hid")
  sdc[[i]] <- globalRecode(sdc[[i]], column = "age", breaks = c(1, 9, 19, 29, 39, 
    49, 59, 69, 100), labels = 1:8)
}
rold <- measureTime(sdc, measure_risk)
r3old <- measureTime(sdc, localSuppression)
# save(SEQ, SEQold, r, r3, rold, r3old, file = "r.RData")

## plot
df <- data.frame(size = c(SEQ, SEQ[1:4]), time = c(unlist(r), unlist(rold)), method = factor(c(rep("sdcMicro 4.1.0", 
  14), rep("IHSN C++", 4))))
g1 <- ggplot(data = df, aes(x = size, y = time, group = method)) + geom_line() + 
  geom_point()
g2 <- ggplot(data = df, aes(x = size, y = time, group = method)) + ggtitle("frequency estimation + risk measurement") + 
  geom_line(aes(colour = method)) + geom_point(aes(colour = method), size = 4) + 
  geom_point(aes(fill = method)) + scale_x_continuous(labels = comma) + ylab("time in seconds") + 
  xlab("number of observations")
pdf("compTime1btd.pdf")
print(g2)
dev.off()

df <- data.frame(size = c(SEQ[1:length(unlist(r3))], SEQ[1:4]), time = c(unlist(r3), 
  unlist(r3old)), method = factor(c(rep("sdcMicro 4.1.0", length(unlist(r3))), 
  rep("IHSN C++", 4))))

g2 <- ggplot(data = df, aes(x = size, y = time, group = method)) + ggtitle("optimal local suppression") + 
  geom_line(aes(colour = method)) + geom_point(aes(colour = method), size = 4) + 
  geom_point(aes(fill = method)) + scale_x_continuous(labels = comma) + ylab("time in seconds") + 
  xlab("number of observations")
pdf("compTime2btd.pdf")
print(g2)
dev.off()
