###############################################################################

# Non-perturbation methods

###############################################################################
# 3.1 Table redesign
###############################################################################

# Data with number of products
df_prod <- data.frame(
  row.names = c('Harps', 'Organs', 'Pianos', 'Other', 'Total'),
  A = c(58, 71, 92, 800, 1021),
  B = c(47, 124, 157, 934, 1262),
  C = c(36, 24, 59, 651, 770),
  D = c(89, 31, 28, 742, 890),
  Total = c(230, 250, 336, 3127, 3943)
)
df_prod

# Data with number of companies
df_comp <- data.frame(
  row.names = c('Harps', 'Organs', 'Pianos', 'Other', 'Total'),
  A = c(151, 16, 5, 302, 474),
  B = c(2, 21, 12, 362, 397),
  C = c(98, 9, 7, 287, 401),
  D = c(23, 8, 1, 227, 259),
  Total = c(274, 54, 25, 1178, 1531)
)
df_comp

### Solution 
df_prod$"B+C" <- df_prod$B + df_prod$C
df_prod["Organs + Pianos",] <- df_prod["Organs",] + df_prod["Pianos",]
df_prod <- df_prod[,c(-2,-3)]
df_prod <- df_prod[,c(1,4,2,3)]
df_prod <- df_prod[c(-2,-3),]
df_prod <- df_prod[c(1,4,2,3),]
df_prod

df_comp$"B+C" <- df_comp$B + df_comp$C
df_comp["Organs + Pianos",] <- df_comp["Organs",] + df_comp["Pianos",]
df_comp <- df_comp[,c(-2,-3)]
df_comp <- df_comp[,c(1,4,2,3)]
df_comp <- df_comp[c(-2,-3),]
df_comp <- df_comp[c(1,4,2,3),]
df_comp






