############################################
############################################
############################################
#                                          #
#                                          #
#  kml Shape Epi Curve for Omicron Winter  #
#  Date last edited: 9/8/2023              #
#  Last editor: Jason Massey (JM)          #
#                                          #
#                                          #
############################################
############################################
############################################




###########################################

#  import libraries 

###########################################

# Libraries for kmlshape 
library(haven)
library(dplyr)
library(lcsm)
library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)
library(caret)
library(MASS)
library(tidyLPA)
library(tidyverse)
library(boot)
library(coxed)
library(glmtoolbox)
library(rms)
library(summarytools)

detach("package:dplyr")
library(dplyr)

# Libraries for OLR Model
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)





###########################################

#  Install Data 

###########################################

# installing old version of kmlShape and Devtools
# Must first go online and download RTools42 on computer 

require(devtools)
install_version("kmlShape", version = "0.9.5", repos = "http://cran.us.r-project.org")

library(kmlShape)  # <-- Comes after old installation! 


# import data set from your path 
omicron <- read_sas('//cdc.gov/project/CCID_NCPDCID_NHSN_SAS/Data/work/_Projects/LTC/COVID-19/Codes/Jason/KML_Shape_Epi_Curve_Omicron/omicron.sas7bdat')

# Pulling in covariate data 
model_data <- read_sas("//cdc.gov/project/CCID_NCPDCID_NHSN_SAS/Data/work/_Projects/LTC/COVID-19/Codes/Jason/KML_Shape_Epi_Curve_Omicron/jason_new.sas7bdat")






#################################################

#  kmlShape before Bootstrap 

#################################################

# create trajectory data frame 
omi <- data.frame(omicron$orgid, omicron$week, omicron$rate)

# object designed to store long format trajectory data frame with three variables only
myClds <- cldsLong(omi)
myClds <- na.omit(cldsLong(omi)) 

#Create "senators" to reduce total size can choose 16, 30, 100, 200 (need to try 500 w fast internet)
reduceTraj(myClds, 250)


#  using 2 groups 
par(ask=FALSE)
kmlShape(myClds,2)

#  using 3 groups
par(ask=FALSE)
kml_omi <- kmlShape(myClds,3)

#  using 4 groups 
par(ask=FALSE)
kmlShape(myClds,4)


#Create New DF
clusterid <- data.frame(orgid=myClds@id, cluster = myClds@clusters)

#Check clusters freqs
dfSummary(clusterid, style = "grid", plain.ascii = TRUE)

#Write final dataset to csv file 
 write.csv(clusterid,"//cdc.gov/project/CCID_NCPDCID_NHSN_SAS/Data/work/_Projects/LTC/COVID-19/Codes/Jason/KML_Shape_Epi_Curve_Omicron/omicron_clusters.csv", row.names = FALSE)







#################################################

#  Using Clusters in Ordinal Logistic Regression  

#################################################

# # Looking at Avg Covid Booster  
# hist(model_data$avg_booster_cov)
# plot(model_data$avg_booster_cov)
# 
# # Looking at SVI
# hist(model_data$SVI)
# plot(model_data$SVI)
# plot(model_data$SVI, model_data$cluster)

# # EXPLORING TERTILES 
# 
# hist(model_data$avg_numLTCFBeds)
# hist(model_data$avg_numres)
# 
# # create hsitogram with 200 bars - number of beds 
# ggplot( model_data, aes( x= avg_numLTCFBeds ) ) +
#   geom_histogram( bins= 50 )
# 
# # Tertiles 
# numBed_Tertiles <- quantile(model_data$avg_numLTCFBeds, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
# numBed_Tertiles
# 
# 
# # number of res 
# ggplot( model_data, aes( x= avg_numres ) ) +
#   geom_histogram( bins= 50 )
# 
# # Tertiles 
# numRes_Tertiles <- quantile(model_data$avg_numres, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
# numRes_Tertiles
 


# DROP OLD CLUSTER VARIABLE
model_data <- subset(model_data, select = -29)

# MERGING CLUSTERS WITH COVARIATE DATA
model_data = merge(x=clusterid,y=model_data,by="orgid")


#RECODING INACCURATE COVARIATES: 

# Recode # 5 - Region, uncontiguous - 19 observations  --> add uncontig to pacific 
model_data$Region <- ifelse(model_data$Region == "Uncontigu", "Pacific", model_data$Region )

# Recode 14 - #Beds, large -  7 observations  --> recategorize # of beds numltcfbeds or # residents
#                                         combine data from Iram and explore
model_data <- mutate(model_data, beds_tertiles = 
                case_when(
                  model_data$avg_numLTCFBeds <= 66 ~ "small",
               66 < model_data$avg_numLTCFBeds & model_data$avg_numLTCFBeds <= 115 ~ "medium",
                  115 < model_data$avg_numLTCFBeds ~ "large"))

# Recode 19 - ltcCert, State - 4 observations --> only use medicare, medicaid, and dual;
#                                             drop state and put under *Other:
#                                             (excludes medicare and medicaid )   
model_data$ltcCert <- ifelse(model_data$ltcCert == "STATE", "" , model_data$ltcCert )


# Recode 26 - facowner, VA - 15 observations -->  try to combine GOV and VA 
model_data$facowner <- ifelse(model_data$facowner== "VA", "GOV", model_data$facowner )

# Rename new SVI variable
model_data$SVI <- model_data$RPL_THEMES

# Combine NP and GOV to Non-Private 
model_data$facowner <- ifelse(model_data$facowner == "GOV", "NP", model_data$facowner )

# LTC Affiliation: Combine independent, combine hospital 
model_data$ltcAff <- ifelse(model_data$ltcAff == "HSA", "Non-Ind", model_data$ltcAff )
model_data$ltcAff <- ifelse(model_data$ltcAff == "HSFS", "Non-Ind", model_data$ltcAff )
model_data$ltcAff <- ifelse(model_data$ltcAff == "MFO", "Non-Ind", model_data$ltcAff )
model_data$ltcAff <- ifelse(model_data$ltcAff == "ICC", "Ind", model_data$ltcAff )
model_data$ltcAff <- ifelse(model_data$ltcAff == "IFS", "Ind", model_data$ltcAff )


# Check Ordered Levels of Covariates
levels(as.factor(model_data$Region))
levels(as.factor(model_data$U_or_R))
levels(as.factor(model_data$SVI))
levels(as.factor(model_data$beds_tertiles))
levels(as.factor(model_data$ltcCert))
levels(as.factor(model_data$ltcAff))
levels(as.factor(model_data$facowner))

# Set References for each Covariate 
model_data$Region <- relevel(factor(model_data$Region), "Northeast" )
model_data$U_or_R <- relevel(factor(model_data$U_or_R), "SmallUrban1")
model_data$beds_tertiles <- relevel(factor(model_data$beds_tertiles), "small")
model_data$ltcCert <- relevel(factor(model_data$ltcCert), "DUAL")
model_data$facowner <-  relevel(factor(model_data$facowner), "NP")

# Delete missings from independent and dependent variables in model
model_data <- model_data[is.na(model_data$Region)  <1, ] 
model_data <- model_data[is.na(model_data$U_or_R)  <1, ] 
model_data <- model_data[is.na(model_data$SVI) < 1, ] 
model_data <- model_data[is.na(model_data$beds_tertiles)  <1, ] 
model_data <- model_data[is.na(model_data$ltcCert)  <1, ] 
model_data <- model_data[is.na(model_data$ltcAff)  <1, ] 
model_data <- model_data[is.na(model_data$facowner) <1, ] 

# Delete Empty Strings and drop unused levels
model_data <- model_data[model_data$orgid != "", ] 
model_data <- model_data %>% mutate(orgid=droplevels(orgid))

model_data <- model_data[model_data$cluster != "", ] 
model_data <- model_data %>% mutate(cluster=droplevels(cluster))

model_data <- model_data %>% mutate(Region=droplevels(Region))

model_data <- model_data[model_data$U_or_R != "", ] 
model_data <- model_data %>% mutate(U_or_R=droplevels(U_or_R))

model_data <- model_data[model_data$beds_tertiles != "", ] 
model_data <- model_data %>% mutate(beds_tertiles=droplevels(beds_tertiles))

model_data <- model_data[model_data$ltcCert != "", ] 
model_data <- model_data %>% mutate(ltcCert=droplevels(ltcCert))

# Delete Duplicates
model_data <- model_data[!duplicated(model_data$orgid), ]

# Check missings of All Variables
sapply(model_data, function(x) sum(is.na(x)))

# Frequencies of All Variables 
dfSummary(model_data, style = "grid", plain.ascii = TRUE)



###############
# Ordinal Model 
###############
  # polr measures ordinal groups of dependent (Y) outcome var
  # clusters 1,2,3 represent peaks varying by intensity of peak shape
  # Negative binomial,
  # offset not needed in R 
  # equivalent to class for categorical vars --> as.factor() 


# Fully Adjusted Model  
m <- polr(as.factor(cluster) ~ as.factor(Region) + as.factor(U_or_R) + SVI + as.factor(beds_tertiles) + 
            avg_booster_cov +
            as.factor(ltcCert) + as.factor(ltcAff) + as.factor(facowner), data = model_data, Hess=TRUE)

# Crude Models
m1 <- polr(as.factor(cluster) ~ as.factor(Region) , data = model_data, Hess=TRUE)
m2 <- polr(as.factor(cluster) ~ as.factor(U_or_R) , data = model_data, Hess=TRUE)
m3 <- polr(as.factor(cluster) ~  SVI , data = model_data, Hess=TRUE)
m4 <- polr(as.factor(cluster) ~  as.factor(beds_tertiles) , data = model_data, Hess=TRUE)
m5 <- polr(as.factor(cluster) ~ avg_primseries_cov , data = model_data, Hess=TRUE)
m6 <- polr(as.factor(cluster) ~  avg_booster_cov , data = model_data, Hess=TRUE)
m7 <- polr(as.factor(cluster) ~ as.factor(ltcCert) , data = model_data, Hess=TRUE)
m8 <- polr(as.factor(cluster) ~  as.factor(ltcAff) , data = model_data, Hess=TRUE)
m9 <- polr(as.factor(cluster) ~  as.factor(facowner), data = model_data, Hess=TRUE)

# Check for Multicollinearity 
vif(m)
vif(m1)
vif(m2)
vif(m3)
vif(m4)
vif(m5)
vif(m6)
vif(m7)
vif(m8)
vif(m9)

# Confidence Limits  
ci <- confint.default(m)

# Exp for estimates and limits for OR and CI
est <- exp(cbind(OR = coef(m), ci))






#################
# Logistic Model 
#################
m <- glm(as.factor(cluster) ~ as.factor(Region) + as.factor(U_or_R) + SVI + as.factor(beds_tertiles) + 
           avg_booster_cov +
            as.factor(ltcCert)  + as.factor(facowner), data = model_data, family = "binomial")

# Crude Models
m1 <- glm(as.factor(cluster) ~ as.factor(Region) , data = model_data, family="binomial")
m2 <- glm(as.factor(cluster) ~ as.factor(U_or_R) , data = model_data, family="binomial")
m3 <- glm(as.factor(cluster) ~  SVI , data = model_data, family="binomial")
m4 <- glm(as.factor(cluster) ~  as.factor(beds_tertiles) , data = model_data, family="binomial")
m5 <- glm(as.factor(cluster) ~ avg_primseries_cov , data = model_data, family="binomial")
m6 <- glm(as.factor(cluster) ~  avg_booster_cov , data = model_data, family="binomial")
m7 <- glm(as.factor(cluster) ~ as.factor(ltcCert) , data = model_data, family="binomial")
m8 <- glm(as.factor(cluster) ~  as.factor(ltcAff) , data = model_data, family="binomial")
m9 <- glm(as.factor(cluster) ~  as.factor(facowner), data = model_data, family = "binomial")

# Check for Multicollinearity 
vif(m)
vif(m1)
vif(m2)
vif(m3)
vif(m4)
vif(m5)
vif(m6)
vif(m7)
vif(m8)
vif(m9)

# Exp for estimates and limits for OR and CI
est <- exp(cbind(OR = coef(m), confint(m)))


# Export Model Dataset for Tables in SAS 
write.csv(model_data, "//cdc.gov/project/CCID_NCPDCID_NHSN_SAS/Data/work/_Projects/LTC/COVID-19/Codes/Jason/KML_Shape_Epi_Curve_Omicron/model_data.csv")



#################################################

#  kmlShape with Bootstrap Method 

#################################################

# Create Wide Data 
omi_wide <- spread(omicron, key = week, value = rate)

# Merge model and wide data
data_merge <- merge(omi_wide, model_data, by = "orgid" ) 

# Drop Cluster Variable 
omi_boot <- select(data_merge, -cluster)



# Formatting Bootstrapping Data: here we place everything we did earlier to be held in a function  
  # set seed, create function (kmlboot)
  # create dataframe (dboot) with indices from your original data
  # create trajectory object (dks) using cldsWide function based on dboot DF -> define size and IDs
  # reduce trajectories on dks object
  # use kmlshape to create our output object (bootfit) with 3 clusters and no plotting 
  # add cluster variable to DF dboot from bootstrap output 
  # model clusters based on bootstrap DF
  # return point estimates from model  

#####################
# Ordinal Bootstrap
#####################
kml.boot <- function(data, indices){
  
  d.boot <- data[indices,]
  
  d.ks <- cldsWide(d.boot[,1:17], 2:17)
  
  reduceTraj(d.ks, 250)
  
  boot.fit <- kmlShape(d.ks, 3, toPlot = "none")
  
  d.boot$clust <- boot.fit@clusters
  
  boot.mod <- polr(as.factor(clust) ~ as.factor(Region) + as.factor(U_or_R) + SVI + as.factor(beds_tertiles) + 
                    avg_booster_cov +
                     as.factor(ltcCert) + as.factor(facowner), data = d.boot, Hess=TRUE)
  
  return(boot.mod$coefficients)
  
}
  

#####################
# Logistic Bootstrap
#####################
kml.boot <- function(data, indices){
  
  d.boot <- data[indices,]
  
  d.ks <- cldsWide(d.boot[,1:17], 2:17)
  
  reduceTraj(d.ks, 250)
  
  boot.fit <- kmlShape(d.ks, 2, toPlot = "none")
  
  d.boot$clust <- boot.fit@clusters
  
  boot.mod <- glm(as.factor(clust) ~ as.factor(Region) + as.factor(U_or_R) + SVI + as.factor(beds_tertiles) + 
                    avg_booster_cov +
                     as.factor(ltcCert) + as.factor(facowner), data = d.boot, family = "binomial")
  
  return(boot.mod$coefficients)
  
}


# Call Bootstrap function with N iterations using merged data                   
set.seed(5)
boot.out <- boot(data=omi_boot, statistic=kml.boot, R=5000) #try 10, 100, 1000, 2000, 5000


# Look at t t0 data and evaluate estimates 
boot.out$t

# Computing Confidence Intervals for each covariate category compared to refs      
x1.ci <- boot.ci(boot.out, type="bca", index = 1)
x2.ci <- boot.ci(boot.out, type="bca", index = 2)
x3.ci <- boot.ci(boot.out, type="bca", index = 3)
x4.ci <- boot.ci(boot.out, type="bca", index = 4)
x5.ci <- boot.ci(boot.out, type="bca", index = 5)
x6.ci <- boot.ci(boot.out, type="bca", index = 6)
x7.ci <- boot.ci(boot.out, type="bca", index = 7)
x8.ci <- boot.ci(boot.out, type="bca", index = 8)
x9.ci <- boot.ci(boot.out, type="bca", index = 9)
x10.ci <- boot.ci(boot.out, type="bca", index = 10)
x11.ci <- boot.ci(boot.out, type="bca", index = 11)
x12.ci <- boot.ci(boot.out, type="bca", index = 12)
x13.ci <- boot.ci(boot.out, type="bca", index = 13)
x14.ci <- boot.ci(boot.out, type="bca", index = 14)
x15.ci <- boot.ci(boot.out, type="bca", index = 15)
x16.ci <- boot.ci(boot.out, type="bca", index = 16)
x17.ci <- boot.ci(boot.out, type="bca", index = 17)
x18.ci <- boot.ci(boot.out, type="bca", index = 18)
x19.ci <- boot.ci(boot.out, type="bca", index = 19)
x20.ci <- boot.ci(boot.out, type="bca", index = 20)
x21.ci <- boot.ci(boot.out, type="bca", index = 21)
x22.ci <- boot.ci(boot.out, type="bca", index = 22)
x23.ci <- boot.ci(boot.out, type="bca", index = 23)


# Estimates to get names 
x1.ci$t0
x2.ci$t0
x3.ci$t0
x4.ci$t0
x5.ci$t0
x6.ci$t0
x7.ci$t0
x8.ci$t0
x9.ci$t0
x10.ci$t0
x11.ci$t0
x12.ci$t0
x13.ci$t0
x14.ci$t0
x15.ci$t0
x16.ci$t0
x17.ci$t0
x18.ci$t0
x19.ci$t0
x20.ci$t0
x21.ci$t0
x22.ci$t0
x23.ci$t0


# BCA Exponentiated Confidence Intervals 
boostrap_estimates <- rbind.data.frame(
  
region.midwest.est <- exp(cbind(x1.ci$t0, x1.ci$bca)),
region.mountain.est <- exp(cbind(x2.ci$t0, x2.ci$bca)),
region.northeast.est <- exp(cbind(x3.ci$t0, x3.ci$bca)),
region.south.est <- exp(cbind(x4.ci$t0, x4.ci$bca)),
U_or_R.LargeUrban1.est <- exp(cbind(x5.ci$t0, x5.ci$bca)),
U_or_R.LageUrban2.est <- exp(cbind(x6.ci$t0, x6.ci$bca)),
U_or_R.Metropolitan1.est <- exp(cbind(x7.ci$t0, x7.ci$bca)),
U_or_R.Metropolitan2.est <- exp(cbind(x8.ci$t0, x8.ci$bca)),
U_or_R.Metropolitan3.est <- exp(cbind(x9.ci$t0, x9.ci$bca)),
SVI.est <- exp(cbind(x10.ci$t0, x10.ci$bca)),
beds.large.est <- exp(cbind(x11.ci$t0, x11.ci$bca)),
beds.medium.est <- exp(cbind(x12.ci$t0, x12.ci$bca)),
avg.cov.est <- exp(cbind(x13.ci$t0, x13.ci$bca)),
avg.booster.est <- exp(cbind(x14.ci$t0, x14.ci$bca)),
ltcCert.MCAID.est <- exp(cbind(x15.ci$t0, x15.ci$bca)),
ltcCert.MCARE.est <- exp(cbind(x16.ci$t0, x16.ci$bca)),
facowner.GOV.est <- exp(cbind(x17.ci$t0, x17.ci$bca)),
facowner.P.est <- exp(cbind(x18.ci$t0, x18.ci$bca))
)

#Just estimates and CI 
boostrap_est <- subset(boostrap_estimates, select = -c(2,3,4) )

#Combine Non-BS and BS estimates
boostrap_est <- cbind.data.frame(est, boostrap_est)

  
# NOTES:
  # Chose 250 senators b/c maintains convergence 
  # Chose 2 clusters/ binary logistic regression b/c statistically significant
  # Table 1 and Table 2






