d <- read.csv("/Users/junopinder/Desktop/Research/Project COPE/cleaned_cope_data_randomized.csv")
d2 <- d
View(d2)
names(d2)
library(labelled)
look_for(d2, "mean")
sapply(d2[,c(358:361, 363:367)], function(x) sum(is.na(x)))

## CDI-2-SF
# find columns
library(labelled)
look_for(d2, "b_cdi")
head(d2[,72:83])
# append new total
d2$b_cdi_tot <- apply(d2[,72:83], 1, sum)
# check 
all(round(d2$b_cdi_mean * 12, 1) == round(d2$b_cdi_tot, 1), na.rm = T)
# investigate nas
sum(is.na(d$b_cdi_mean))
sum(is.na(d2$b_cdi_tot))
m_cdi <- which(is.na(d2$b_cdi_tot))
d2[m_cdi, c(72:83, 358, 535)]

## BADS-SF
# find columns
look_for(d2, "b_bads")
# recode 110, 115, 116, 117
d2[,c(110, 115:117)] <- 6 - d2[,c(110, 115:117)]
# check
 head(d[,c(110, 115:117)])
head(d2[,c(110, 115:117)])
round(cor(d2[,110:118], use = "pairwise.complete.obs"), 2)
library(psych)
alpha(d[,110:118])$total
alpha(d2[,110:118])$total
# append new total
d2$b_bads_tot <- apply(d2[,110:118], 1, sum)
# check 
cor(d2$b_bads_mean, d2$b_cdi_tot, use = "pairwise.complete.obs")
cor(d2$b_bads_tot, d2$b_cdi_tot, use = "pairwise.complete.obs")
# investigate nas
sum(is.na(d$b_bads_mean))
sum(is.na(d2$b_bads_tot))
m_bads <- which(is.na(d2$b_bads_tot))
d2[m_bads, c(110:118, 359, 536)]


## ACES
# find columns
look_for(d2, "aces")
head(d2[,132:136])
# append new total
d2$b_aces_tot <- apply(d2[,132:136], 1, sum)
# check 
all(round(d2$b_aces_mean * 5, 1) == round(d2$b_aces_tot, 1), na.rm = T)
cor
# investigate nas
sum(is.na(d$b_aces_mean))
sum(is.na(d2$b_aces_tot))
m_aces <- which(is.na(d2$b_aces_tot))
d2[m_aces, c(132:136, 363, 537)]


## BHS-4
look_for(d2, "b_bhs")
head(d2[,125:128])
# append new total
d2$b_bhs_tot <- apply(d2[,125:128], 1, sum)
# check 
all(round(d2$b_bhs_mean * 4, 1) == round(d2$b_bhs_tot, 1), na.rm = T)
# investigate nas
sum(is.na(d$b_bhs_mean))
sum(is.na(d2$b_bhs_tot))
m_bhs <- which(is.na(d2$b_bhs_tot))
d2[m_bhs, c(125:128, 361, 538)]

## SHS
look_for(d2, "b_shs")
head(d2[,119:124])
# append new total
d2$b_shs_tot <- apply(d2[,119:124], 1, sum)
# check 
all(round(d2$b_shs_mean * 6, 1) == round(d2$b_shs_tot, 1), na.rm = T)
# investigate nas
sum(is.na(d$b_shs_mean))
sum(is.na(d2$b_shs_tot))
m_shs <- which(is.na(d2$b_shs_tot))
d2[m_shs, c(119:124, 360, 539)]


## MPVS
# find columns
look_for(d2, "b_mvps")
head(d2[,174:189])
# append new total
d2$b_mvps_tot <- apply(d2[,174:189], 1, sum)
# check 
all(round(d2$b_mvps_mean * 16, 1) == round(d2$b_mvps_tot, 1), na.rm = T)
# investigate nas
sum(is.na(d$b_mvps_mean))
sum(is.na(d2$b_mvps_tot))
m_mvps <- which(is.na(d2$b_mvps_tot))
d2[m_mvps, c(174:189, 364, 539)]


## (E)EDS
# find columns
look_for(d2, "eds_events")
head(d2[,210:219])
# append new total
d2$eds_events_tot <- apply(d2[,210:219], 1, sum)
# check 
all(round(d2$eds_events_mean * 10, 1) == round(d2$eds_events_tot, 1), na.rm = T)
# investigate nas
sum(is.na(d$eds_events_mean))
sum(is.na(d2$eds_events_tot))
m_eds <- which(is.na(d2$eds_events_tot))
d2[m_eds, c(210:219, 366, 540)]


## GAD
# find columns
look_for(d2, "b_gad")
head(d2[,220:226])
# append new total
d2$b_gad_tot <- apply(d2[,220:226], 1, sum)
# check 
all(round(d2$b_gad_mean * 7, 1) == round(d2$b_gad_tot, 1), na.rm = T)
# investigate nas
sum(is.na(d$b_gad_mean))
sum(is.na(d2$b_gad_tot))
m_eds <- which(is.na(d2$b_gad_tot))
d2[m_eds, c(220:226, 367, 541)]


## ULS
look_for(d2, "uls")
head(d2[190:209])
# recode 1, 5, 6, 9, 10, 15, 16, 19, 20
head(d2[,c(190, 194, 195, 198, 199, 204, 205, 208, 209)])
d2[,c(190, 194, 195, 198, 199, 204, 205, 208, 209)] <- 3 - d2[,c(190, 194, 195, 198, 199, 204, 205, 208, 209)]
# check
head(d[,c(190, 194, 195, 198, 199, 204, 205, 208, 209)])
head(d2[,c(190, 194, 195, 198, 199, 204, 205, 208, 209)])
round(cor(d2[,190:209], use = "pairwise.complete.obs"), 2)
alpha(d[,190:209])$total
alpha(d2[,190:209])$total
# append new total
d2$b_uls_tot <- apply(d2[,190:209], 1, sum)
# check 
cor(d2$b_uls_mean, d2$b_cdi_tot, use = "pairwise.complete.obs")
cor(d2$b_uls_tot, d2$b_cdi_tot, use = "pairwise.complete.obs")
# investigate nas
sum(is.na(d$b_uls_mean))
sum(is.na(d2$b_uls_tot))
m_uls <- which(is.na(d2$b_uls_tot))
d2[m_uls, c(190:209, 365, 542)]


## Export
write.csv(d2, "/Users/junopinder/Desktop/Research/Project COPE/cleaned_cope_data_randomized_corrected2.csv", row.names = T)

## Compare NAs
nas <- sapply(d2[,c(358, 535, 359, 536, 363, 537, 361, 360, 538, 364, 539, 366, 540, 367, 541, 365, 542)], function(x) sum(is.na(x)))
nas

head(d2[,535:543])
      