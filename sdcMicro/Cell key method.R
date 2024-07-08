###############################################################################

# Cell key method

###############################################################################

# Load the required libraries
# install.packages("data.table")
# install.packages("cellKey")
# install.packages("ptable")
library(data.table)
library(cellKey)
library(ptable)

# Load the data
data(test_data_10k)
# dat <- fread("data/test_data_10k.csv.gz")
dat <- as.data.table(test_data_10k)
class(dat)

dat <- data.table(
   id = 1:100,
   region = sample(c("North", "South", "East", "West"), 100, replace = TRUE),
   gender = sample(c("1", "2"), 100, replace = TRUE),
   income = rnorm(100, mean = 50000, sd = 15000)
)

# Generate Record Keys
# Generate record keys for the data to ensure each record is uniquely identified.
dat$rkey <- ck_generate_rkeys(dat = dat, 
                              seed = 123)

# Create Hierarchies
# Create hierarchies for categorical variables in the dataset. For simplicity, 
# let's use gender and region variables.

# Create a hierarchy for gender
d_sex <- hier_create(nodes = c("1", "2"), 
                     root = "Total")

# Create a hierarchy for region (assuming 'region' is a column in the data)
regions <- unique(as.character(dat$region))
d_region <- hier_compute(inp = regions, 
                         dim_spec = c(1, 1, 1), 
                         root = "Total", 
                         method = "len")

# Set Up the Perturbation Table
# Define the perturbation table using the hierarchies created.

# Define the table
tab <- ck_setup(
   x = dat,
   rkey = "rkey",
   dims = list(Gender = d_sex, 
               Region = d_region)
)

# Create a simple ptable
ptab <- create_cnt_ptable(D = 2, V = 1.08, pstay = 0.6, optim = 4)

# Prepare and perturb the table
ptab_input <- ck_params_cnts(ptab = ptab)
tab$params_cnts_set(val = ptab_input, v = "total")
tab$perturb(v = "total")









## ptable-Package 
ptab1 <- create_cnt_ptable(D = 2, 
                           V = 1.08, 
                           js = 1, 
                           mono = c(T,T,F,T))

# table
ptab1@pTable
# plot
plot(ptab1, type='p')


## (2) Please design a ptable object 'ptab2' with the following specifications: 
## a maximum noise of D=8, 
## a high variance of V=3 and a probability of 60%, 
# that frequencies won't be changed.

# Solution:
ptab2 <- create_cnt_ptable(D = 8, 
                           V = 3, 
                           pstay = 0.6, 
                           optim=4)

plot(ptab2, type = "t")
ptab2@empResults

## cellKey ##

# record keys
dat$rkey <- ck_generate_rkeys(dat = dat,
                              seed = 123)
dat$rkey

# dimensions and hierarchy
d_sex <- 
  hier_create(
    nodes = c("1","2"), 
    root = "Total"
  )

coc.m_cat <- unique(as.character(dat$COC.M))

d_coc.m <- 
  hier_compute(
    inp = coc.m_cat, # inp = c("1","21","221", ...) 
    dim_spec = c(1,1,1), 
    root = "Total",
    method = "len"
  ); 


# define the table
tab <- ck_setup(
  x = dat,
  rkey = "rkey",
  dims = list(SEX = d_sex, COC.M = d_coc.m)
)

# prepare and perturb the table
ptab_input <- ck_params_cnts(ptab = ptab1)
tab$params_cnts_set(val = ptab_input, v = "total")
tab$perturb(v = "total")





## (4a) What is the maximum relative absolute distance between original and perturbed values? Give an
## interpretation of the value and search the table cell (give the defintion of the table cell)-

# Solution:
tab$summary()

# Answer: The maximum relative absolute distance is 0.5. That means, the maximum relative error in the table is 50%. It is 
# the table cell (SEX=1; COC.M=226) which hast been changed from 2 to 3: |(3-2)| / 2 = 0.5.


## (4b) There is exactly one table cell, that has been changed by `+2`. Which one (give the definition of the table cell)?

# Solution

tab$mod_cnts()
tab$mod_cnts()[pert == 2]

tab$freqtab()[(puwc-uwc) == 2]

result1 <- tab$freqtab()
result1[, noise :=  puwc-uwc]
result1$noise2 <- result1$puwc - result1$uwc

result1[noise == 2]
# Answer: SEX=1 and COC.M=224





## (5) Now, extend the two-dimensional table by a geographical variable.

# (5a) Create the variable hierarchy/dimension for NUTS3. NUTS3 has 3 levels each of length 1. Please use
# 'hier_compute(...)' (similar to the hierarchy of the variable COC.M).


# Solution:
nuts3 <- unique(as.character(dat$NUTS3))

d_nuts3 <- 
  hier_compute(
    inp = nuts3, 
    dim_spec = c(1,1,1), 
    root = "Total",
    method = "len"
  ); 

hier_display(d_nuts3)


# (5b) Update the following setup using the hierarchy of NUTS3 you created in (5a) and assign it to 
# the object 'tab5'.

tab5 <- ck_setup(
  x = dat,
  rkey = "rkey",
  dims = list(SEX = d_sex, COC.M = d_coc.m)
)

# Remark: The list for the argument 'dims = ...' must be entered case-sensitively!



# Solution:
tab5 <- ck_setup(
  x = dat,
  rkey = "rkey",
  dims = list(SEX = d_sex, COC.M = d_coc.m, NUTS3 = d_nuts3)
)


# (5c) Question: How many cells does the newly generated 3-dimensional table have?

# Solution:
tab5

# Answer: 1.584 cells



# (5d) Apply the perturbation using the ptable 'ptab2'. How many 1's are in original table and how many 1's 
# have been changed by +8 (try to explain)?

# Solution:
ptab_input <- ck_params_cnts(ptab = ptab2)
tab5$params_cnts_set(val = ptab_input, v = "total")
tab5$perturb(v = "total")
tab5$freqtab(v = c("total"))[ uwc == 1,]
tab5$freqtab(v = c("total"))[ uwc == 1 & puwc == 9,]

# Answer: 7 out of 137 1's have been changed by +8 to 9.



# (5e) How many (absolute or relative) cells are still original (i.e. remain unchanged) after perturbation? 

# Solution
tab5$measures_cnts(v = "total")$overview

# Answer: 1094 or 69%


# (5f) How large are the three mean distances (utility measures) when you take original zero counts
# into account:
# - d1: absolute distance between original and perturbed values
# - d2: relative absolute distance between original and perturbed values
# - d3: absolute distance between square-roots of original and perturbed values


# Solution

tab5$summary()
# or
tab5$measures_cnts(v = "total")$measures

# Answer: without zeros: 0.835	0.101	0.089



## (6) Compare different perturbations.

## (6a) Produce the table from exercise 5 again and assign it to the object 'tab6'.
tab6 <- ck_setup(...)


# Solution:
tab6 <- ck_setup(
  x = dat,
  rkey = "rkey",
  dims = list(SEX = d_sex, COC.M = d_coc.m, NUTS3 = d_nuts3)
)

# Hint: Don't copy the object like: tab6 <- tab5 (!! doesn't work)

# Remark: If you try to perturb a table and receive the message 
# "--> Variable "total" was already perturbed: parameters are not updated." then you have to rerun
# the 'ck_setup(..)' step. you can't perturb the object twice.



## (6b) Perturb 'tab6' by the following ptable:

ptab6 <- create_cnt_ptable(D = 8, V = 2, pstay = 0.6, optim=4)



#Solution:
tab6$params_cnts_set(val = ck_params_cnts(ptab = ptab6), v = "total")
tab6$perturb(v = "total")


## (6c)  Compare the measure "relative absolute distance" between the two different 
## perturbations in (5) and (6). Which perturbation comes along with a lower information loss?

# Solution:
tab5$measures_cnts(v = "total")$measures
tab6$measures_cnts(v = "total")$measures

# Answer: ptab6 has a lower variance --> tab6 has a lower loss of information


## (6d) [Advanced] Compare the distributions of the two ptables `ptab2` (which was used to perturb `tab5`)
## and `ptab6` (which was used to perturb `tab6`) and try to explain the result in (6c).

# Solution
plot(ptab2, type="d")
plot(ptab6, type="d")

# Answer: The distributions are almost identical. However, the variance that differs is the main reason.




## (7) [Advanced]

## (7a) Use `dat` and create a household data set (with `HID`, `LAU2`, size of the household `Size` and mean age using `AGE.H`). Then, assign a record key to each household. 

# Solution

dat <- fread("test_data_10k.csv.gz")
dat$rkey <- ck_generate_rkeys(dat = dat, seed = 123)


# compute mean age of the household
dat[, hh_mean_age:=mean(AGE.H), by=HID]

# compute cell-key for households (i.e. aggregate record keys of the household members)
dat[, hh_ckey:=sum(rkey), by=HID]

# remove integer before the decimal points (i.e. modulo operation)
dat[, hh_ckey := hh_ckey %% 1]


# household data set
hh_dat <- unique(dat, by = "HID")

## select variables
hh_dat <- hh_dat[,  .(LAU2, Size, hh_mean_age, hh_ckey)]

# result
hh_dat


# Important: The household *cell-key* could also be interpreted as the *record-key* of the household. That is, 
# if you are going to produce a household table and perturb it, the cell-key could be interpreted as record-key. Therefore:


setnames(hh_dat, "hh_ckey", "hh_rkey")
hh_dat


## (7b) How many households do we have? What will be the cell-key for this total number (don't use the cellKey-package; compute it manually) 
## and what would be the noise if we use `ptab1` (manual lookup using the graph or look into the ptable)?

ptab1 <- create_cnt_ptable(D = 2, V = 1.08, js = 1, mono = c(T,T,F,T))


#Solution
nrow(hh_dat)
sum(hh_dat$hh_rkey) %% 1

ptab1@pTable






### Exercise (8) [Advanced]

# Create a one-dimensional table with the hierarchical variable NUTS3 and apply a filter.

## (8a) Create a table object 'tab8' with a filter (argument 'countvars = ...'). The filter shall only count 
## females (variable 'sex == 2'). (i.e. call the filter ). Use the help page '?cellkey_pkg' to define the argument 'countvars'.

tab8 <-
  
  
  
  # Solution
  dat[, female := ifelse(SEX == 2, 1, 0)]

tab8 <- ck_setup(
  x = dat,
  rkey = "rkey",
  dims = list(NUTS3 = d_nuts3),
  countvars = "female"
)


## (8b) Perturb the table using the new countvar and ptable 'ptab1'.

# Solution

tab8$params_cnts_set(val = ck_params_cnts(ptab = ptab1), v = "female")

tab8$perturb(v = "female")
tab8$freqtab(v = c("female"))



## Exercise (9) [Advanced] 

## (9a) Compare the distributions of the two following ptables.

ptab91 <- create_cnt_ptable(D = 5, V = 0.5, optim=4)
ptab92 <- create_cnt_ptable(D = 5, V = 2, optim=4)

## What is the main difference between the distributions (look at the graph)?

# Solution

plot(ptab91, type="d")
plot(ptab92, type="d")

# Answer: Leptocurtic curve (`ptab91`) with high probabilitiy for noise 0 versus normal curtosis (`ptab92`).


## (9b) What would you expect: Which ptable has a lower loss of information? Perturb the table you have designed in (8) twice and perturb the tables with the two ptables.

tab91 <- ...
tab92 <- ...


# Solution

tab91 <- ck_setup(
  x = dat,
  rkey = "rkey",
  dims = list(NUTS3 = d_nuts3)
)
tab92 <- ck_setup(
  x = dat,
  rkey = "rkey",
  dims = list(NUTS3 = d_nuts3)
)
tab91$params_cnts_set(val = ck_params_cnts(ptab = ptab91), v = "total")
tab92$params_cnts_set(val = ck_params_cnts(ptab = ptab92), v = "total")
tab91$perturb(v = "total")
tab92$perturb(v = "total")

tab91$measures_cnts(v = "total")$measures
tab92$measures_cnts(v = "total")$measures


# Answer: `ptab91` has a lower variance and, hence, a lower loss of information.