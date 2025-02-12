
library(MASS)
library(tidyLPA)
library(tidyverse)
library(lavaan)

detach("package:dplyr")
library(dplyr)

omicron <- read_sas('//cdc.gov/project/CCID_NCPDCID_NHSN_SAS/Data/work/_Projects/LTC/COVID-19/Codes/Jason/KML_Shape_Epi_Curve_Omicron/omicron.sas7bdat')

omi <- spread(omicron, key = week, value = rate )

# Rename multiple columns for old to new
names(omi) <-  c('orgid', 'week82','week83','week84', 'week85', 'week86', 'week87', 'week88', 
                 'week89', 'week90', 'week91','week92','week93','week94', 'week95',
                 'week96', 'week97')
    


omi %>%
  select(2:17) %>%
  estimate_profiles(1:6) %>%
  compare_solutions(statistics = c("AIC", "BIC"))




# https://data-edu.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html

mod <- omi %>%
  dplyr::select(2:17) %>%
  estimate_profiles(2)

plot_profiles(mod)

mod.dat <- get_data(mod)

table(dat$group, mod.dat$Class)







#MODEL STUFF BELOW 

# https://blogs.baylor.edu/rlatentvariable/sample-page/r-syntax/#Chapter_4_Latent_Variable_Models_with_Multiple_Groups

lfa.vars <- c(paste0("y", 1:10), paste0("x", 1:3))
lfa.cov <- cov(dat[lfa.vars])
lfa.mean <- colMeans(dat[lfa.vars])
names(lfa.mean) <- colnames(lfa.cov) <- rownames(lfa.cov) <- c(paste0("Time", 1:10), paste0("x", 1:3))


# State and poverty are predictors
lfa.model7 <- '
# intercept
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4 + 1*Time5 + 1*Time6 + 1*Time7 + 1*Time8 + 1*Time9 + 1*Time10
# slope
s =~ 1*Time1 + 1*Time2 + 1*Time3 + 2*Time4 + 2*Time5 + 2*Time6 - 3*Time7 - 3*Time8 + 1*Time9 + 1*Time10
# regression
s + i ~ x1 + x2 + x3
'
lfa.fit7 <- growth(lfa.model7, sample.cov=lfa.cov, sample.mean=lfa.mean, sample.nobs=300)


