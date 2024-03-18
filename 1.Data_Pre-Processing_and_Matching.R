#if you have access to the ABCD data, please setwd() here to whichever file location the tables are stored in csv format

library(psych)
library(dplyr)
library(purrr) 
library(tidyr)
library(stringr)
library(tableone)
library(MatchIt)
library(car)
library(ggplot2)
library(ggstatsplot)
library(jtools)
library(sensemakr)
library(reshape2)

################################################################################################
################################################################################################
#Data Preparation
################################################################################################
################################################################################################


######################################################################
#Identify children with an mTBI in the Baseline Dataset
######################################################################

Baseline <- read.csv2("abcd_otbi01.csv")
Long <- read.csv2("abcd_lpohstbi01.csv")

tbi_BL <- subset(Baseline, tbi_1b == 1 | tbi_1c == 1 | tbi_2b == 1 | tbi_2c == 1 | tbi_3b == 1 | tbi_3c == 1 | tbi_4b == 1 | tbi_4c == 1 | tbi_5b == 1 | tbi_5c == 1 | tbi_6o == 1 | tbi_7c1 == 1| tbl_7c2 == 1 | tbi_7g == 1) #people with LOC or memory loss after any of the injury mechanisms
BL_modsev <- subset(tbi_BL, tbi_1b == 2 | tbi_1b == 3 | tbi_3b == 2 | tbi_3b == 3 ) #remove all moderate / severe
tbi_BL <- anti_join(x = tbi_BL, y = BL_modsev, by = "src_subject_id")
#449 mTBI at Baseline

tbi_1y <- subset(Long, eventname == '1_year_follow_up_y_arm_1')
tbi_1y <- subset(tbi_1y, tbi_1b_l == 1 | tbi_1c_l == 1 | tbi_2b_l == 1 | tbi_2c_l == 1 | tbi_3b_l == 1 | tbi_3c_l == 1 | tbi_4b_l == 1 | tbi_4c_l == 1 | tbi_5b_l == 1 | tbi_5c_l == 1 | tbi_6o_l == 1 | tbi_7c1_l == 1| tbl_7c2_l == 1) #people with LOC or memory loss after any of the injury mechanisms
#138 mTBI at 1y-FU

tbi_2y <- subset(Long, eventname == '2_year_follow_up_y_arm_1')
tbi_2y <- subset(tbi_2y, tbi_1b_l == 1 | tbi_1c_l == 1 | tbi_2b_l == 1 | tbi_2c_l == 1 | tbi_3b_l == 1 | tbi_3c_l == 1 | tbi_4b_l == 1 | tbi_4c_l == 1 | tbi_5b_l == 1 | tbi_5c_l == 1 | tbi_6o_l == 1 | tbi_7c1_l == 1| tbl_7c2_l == 1) #people with LOC or memory loss after any of the injury mechanisms
#142 mTBI at 2y-FU 

tbi_3y <- subset(Long, eventname == '3_year_follow_up_y_arm_1')
tbi_3y <- subset(tbi_3y, tbi_1b_l == 1 | tbi_1c_l == 1 | tbi_2b_l == 1 | tbi_2c_l == 1 | tbi_3b_l == 1 | tbi_3c_l == 1 | tbi_4b_l == 1 | tbi_4c_l == 1 | tbi_5b_l == 1 | tbi_5c_l == 1 | tbi_6o_l == 1 | tbi_7c1_l == 1| tbl_7c2_l == 1) #people with LOC or memory loss after any of the injury mechanisms
#62 mTBI at 3y-FU

#exclude those children who had an additional moderate-severe TBI
modsev <- BL_modsev
tbi_l_modsev <- subset(Long, tbi_1b_l == 2 | tbi_3b_l == 2 | tbi_4b_l == 2 | tbi_4b_l == 3 | tbi_6r_l == 1 )
modsev <- bind_rows(BL_modsev,tbi_l_modsev) 
sample_modsev <- data.frame(modsev$src_subject_id)
colnames(sample_modsev) <- c("src_subject_id")
tbi_BL <- anti_join(x=tbi_BL, y=modsev, by="src_subject_id")

##############  ##############  ##############  

sampleBaseline <- data.frame(tbi_BL$src_subject_id)
colnames(sampleBaseline) <- c("src_subject_id")
sample1y <- data.frame(tbi_1y$src_subject_id)
colnames(sample1y) <- c("src_subject_id")
sample2y <- data.frame(tbi_2y$src_subject_id)
colnames(sample2y) <- c("src_subject_id")
sample3y <- data.frame(tbi_3y$src_subject_id)
colnames(sample3y) <- c("src_subject_id")

allTBI <- rbind(sampleBaseline, sample1y)
allTBI <- rbind(allTBI, sample2y)
allTBI <- rbind(allTBI, sample3y) 
allTBI <- rbind(allTBI, sample_modsev) 
allTBI_nd = data.frame(allTBI[!duplicated(allTBI$src_subject_id),]) # (unique, all those reporting mTBI at one point during the 4 years)
colnames(allTBI_nd) <- c("src_subject_id")

sampleTBI <- sampleBaseline
sampleTBI$group <- as.factor(1)

######################################################################
#Create TBI-specific variables: Age-at-Injury and type of TBI
######################################################################

abcd_otbi01_TBI <- inner_join(x=Baseline, y=sampleTBI, by="src_subject_id")
typeofTBIs <- abcd_otbi01_TBI

typeofTBIs$Sit1_treated <- ifelse(typeofTBIs$tbi_1b==1 |	typeofTBIs$tbi_1c == 1, typeofTBIs$Sit1_treated <- 1, typeofTBIs$Sit1_treated <- 0)
typeofTBIs$Sit1_age <- ifelse(typeofTBIs$Sit1_treated==1, typeofTBIs$Sit1_age <- typeofTBIs$tbi_1d, typeofTBIs$Sit1_age <- NA)

typeofTBIs$Sit2_accident <- ifelse(typeofTBIs$tbi_2b==1 |	typeofTBIs$tbi_2c == 1, typeofTBIs$Sit2_accident <- 1, typeofTBIs$Sit2_accident <- 0)
typeofTBIs$Sit2_age <- ifelse(typeofTBIs$Sit2_accident==1, typeofTBIs$Sit2_age <- typeofTBIs$tbi_2d, typeofTBIs$Sit2_age <- NA)

typeofTBIs$Sit3_fall <- ifelse(typeofTBIs$tbi_3b==1 |	typeofTBIs$tbi_3c == 1, typeofTBIs$Sit3_fall <- 1, typeofTBIs$Sit3_fall <- 0)
typeofTBIs$Sit3_age <- ifelse(typeofTBIs$Sit3_fall==1, typeofTBIs$Sit3_age <- typeofTBIs$tbi_3d, typeofTBIs$Sit3_age <- NA)

typeofTBIs$Sit4_violence <- ifelse(typeofTBIs$tbi_4b==1 |	typeofTBIs$tbi_4c == 1, typeofTBIs$Sit4_violence <- 1, typeofTBIs$Sit4_violence <- 0)
typeofTBIs$Sit4_age <- ifelse(typeofTBIs$Sit4_violence==1, typeofTBIs$Sit4_age <- typeofTBIs$tbi_4d, typeofTBIs$Sit4_age <- NA)

typeofTBIs$Sit5_blast <- ifelse(typeofTBIs$tbi_5b==1 |	typeofTBIs$tbi_5c == 1, typeofTBIs$Sit5_blast <- 1, typeofTBIs$Sit5_blast <- 0)
typeofTBIs$Sit5_age <- ifelse(typeofTBIs$Sit5_blast==1, typeofTBIs$Sit5_age <- typeofTBIs$tbi_5d, typeofTBIs$Sit5_age <- NA)

typeofTBIs$Sit6_morewithLOC <- ifelse(typeofTBIs$tbi_6o==1, typeofTBIs$Sit6_morewithLOC <- typeofTBIs$tbi_6p, typeofTBIs$Sit6_morewithLOC <- 0)
typeofTBIs$Sit6_age <- ifelse(typeofTBIs$Sit6_morewithLOC!=0, typeofTBIs$Sit6_age <- typeofTBIs$tbi_6s, typeofTBIs$Sit6_age <- NA)

typeofTBIs$Sit7_repimpact1 <- ifelse(typeofTBIs$tbi_7c1==1 | typeofTBIs$tbl_7c2 == 1, typeofTBIs$Sit7_repimpact1 <- 1, typeofTBIs$Sit7_repimpact1 <- 0)
typeofTBIs$Sit7_age <- ifelse(typeofTBIs$Sit7_repimpact1==1, typeofTBIs$Sit7_age <- typeofTBIs$tbi_7e, typeofTBIs$Sit7_age <- NA)

typeofTBIs$Sit8_repimpact2 <- ifelse(typeofTBIs$tbi_7i==1 | typeofTBIs$tbi_7i==2, typeofTBIs$Sit8_repimpact2  <- 1, typeofTBIs$Sit8_repimpact2 <- 0)
typeofTBIs$Sit8_age <- ifelse(typeofTBIs$Sit8_repimpact2==1, typeofTBIs$Sit8_age <- typeofTBIs$tbi_7k, typeofTBIs$Sit8_age <- NA)

typeofTBIs$Sit9_repimpact3 <- ifelse(typeofTBIs$tbi_8i==1 | typeofTBIs$tbi_8i==2, typeofTBIs$Sit9_repimpact3  <- 1, typeofTBIs$Sit9_repimpact3 <- 0)
typeofTBIs$Sit9_age <- ifelse(typeofTBIs$Sit9_repimpact3==1, typeofTBIs$Sit9_age <- typeofTBIs$tbi_8k, typeofTBIs$Sit9_age <- NA)

agecols <- c("interview_age", "Sit1_age", "Sit2_age", "Sit3_age", "Sit4_age", "Sit5_age", "Sit6_age", "Sit7_age", "Sit8_age", "Sit9_age", "Sit1_treated", "Sit2_accident", "Sit3_fall", "Sit4_violence", "Sit5_blast", "Sit6_morewithLOC", "Sit7_repimpact1", "Sit8_repimpact2", "Sit9_repimpact3")
typeofTBIs[agecols] <- sapply(typeofTBIs[agecols],as.numeric)

typeofTBIs$agefirst <- with(typeofTBIs, pmin(Sit1_age, Sit2_age, Sit3_age, Sit4_age, Sit5_age, Sit6_age, Sit7_age, Sit8_age, Sit9_age, na.rm=TRUE))
typeofTBIs$agefirst <- as.numeric(typeofTBIs$agefirst)

typeofTBIs$group <- as.factor(1)
typeofTBIs <- select(typeofTBIs, c("src_subject_id", "sex", "group","interview_age", "Sit1_treated", "Sit2_accident", "Sit3_fall", "Sit4_violence", "Sit5_blast", "Sit6_morewithLOC", "Sit7_repimpact1", "Sit8_repimpact2", "Sit9_repimpact3", "Sit1_age", "Sit2_age", "Sit3_age", "Sit4_age", "Sit5_age", "Sit6_age", "Sit7_age", "Sit8_age", "Sit9_age", "agefirst"))

#create an age category
typeofTBIs <- typeofTBIs %>% mutate(age_cat=cut(agefirst, breaks=c(-Inf, 3, 7, Inf), labels=c("0-3","4-7","8-10")))
typeofTBIs$age_cat <- as.factor(typeofTBIs$age_cat)

######################################################################
#Load parent demographics for the matching
######################################################################
dem <- read.csv2("pdem02.csv")
dem <- select(dem, c('src_subject_id', 'demo_comb_income_v2')) 

#################################
#Socioeconomic status
#################################
dem$demo_comb_income_v2 <- as.factor(as.numeric(dem$demo_comb_income_v2))
dem[, 2][dem[, 2] == 999] <- NA
dem[, 2][dem[, 2] == 777] <- NA

#################################
#Site
#################################
site.data <- read.csv2("abcd_lt01.csv")
site.data <- subset(site.data, eventname == 'baseline_year_1_arm_1')
site.data <- select(site.data, c('src_subject_id', 'site_id_l'))
Baseline <- inner_join(x=Baseline, y=site.data, by="src_subject_id")
Baseline$site_id_l <- as.factor(Baseline$site_id_l)

#################################
#Race (create new variable)
#################################
dem_race <- read.csv2("pdem02.csv")

#select all variables specifying race, add an empty column for the adapted race
dem_race <- select(dem_race, c('src_subject_id', 'demo_race_a_p___10','demo_race_a_p___11',	'demo_race_a_p___12',	'demo_race_a_p___13',	'demo_race_a_p___14',	'demo_race_a_p___15', 'demo_race_a_p___16',	'demo_race_a_p___17', 'demo_race_a_p___18',	'demo_race_a_p___19', 'demo_race_a_p___20',	'demo_race_a_p___21',	'demo_race_a_p___22',	'demo_race_a_p___23',	'demo_race_a_p___24',	'demo_race_a_p___25',	'demo_race_a_p___77', 'demo_race_a_p___99',	'demo_race_notes_v2', 'demo_ethn_v2')) 
dem_race$demo_race_adapted <- NA
dem_race[,2:20] <- sapply(dem_race[,2:20],as.numeric)
dem_race$numrace <- rowSums(dem_race[,2:17]) 

#Assign the categories from Dick, A. S., Lopez, D. A., Watts, A. L., Heeringa, S., Reuter, C., Bartsch, H., ... & Thompson, W. K. (2021). Meaningful associations in the adolescent brain cognitive development study. Neuroimage, 239, 118262.
dem_race[, 22][dem_race[, 17] == 1] <- 'Other'
dem_race[, 22][dem_race[, 16] == 1] <- 'Asian'
dem_race[, 22][dem_race[, 15] == 1] <- 'Asian'
dem_race[, 22][dem_race[, 14] == 1] <- 'Asian'
dem_race[, 22][dem_race[, 13] == 1] <- 'Asian'
dem_race[, 22][dem_race[, 12] == 1] <- 'Asian'
dem_race[, 22][dem_race[, 11] == 1] <- 'Asian'
dem_race[, 22][dem_race[, 10] == 1] <- 'Asian'
dem_race[, 22][dem_race[, 9] == 1] <- 'NHPI'
dem_race[, 22][dem_race[, 8] == 1] <- 'NHPI'
dem_race[, 22][dem_race[, 7] == 1] <- 'NHPI'
dem_race[, 22][dem_race[, 6] == 1] <- 'NHPI'
dem_race[, 22][dem_race[, 5] == 1] <- 'AIAN'
dem_race[, 22][dem_race[, 4] == 1] <- 'AIAN'
dem_race[, 22][dem_race[, 3] == 1] <- 'Black'
dem_race[, 22][dem_race[, 2] == 1] <- 'White'
dem_race[, 22][dem_race[, 23] >= 2] <- 'Multiple'

dem_onlyrace <- select(dem_race, c('src_subject_id','demo_race_adapted'))

Baseline <- inner_join(x=Baseline, y=dem_onlyrace, by="src_subject_id")
Baseline$demo_race_adapted <- as.factor(Baseline$demo_race_adapted )

#################################
#BMI (calculate and exclude obvious data entry errors)
#################################
BMI.data <- read.csv2("abcd_ant01.csv")
BMI.data <- subset(BMI.data, eventname == 'baseline_year_1_arm_1')
BMI.data <- select(BMI.data, c('src_subject_id', 'anthroheightcalc', 'anthroweight1lb','anthroweight2lb', 'anthroweight3lb'))
EHI.data <- read.csv2("abcd_ehis01.csv")
EHI.data <- select(EHI.data, c('src_subject_id', 'ehi_y_ss_scoreb'))

control.variables <- inner_join(x=site.data, y=BMI.data, by='src_subject_id')
control.variables <- inner_join(x=control.variables, y=EHI.data, by='src_subject_id')
cols.num2 <- c('anthroheightcalc', 'anthroweight1lb','anthroweight2lb', 'anthroweight3lb','ehi_y_ss_scoreb')
control.variables[cols.num2] <- sapply(control.variables[cols.num2],as.numeric)
rm(cols.num2)
control.variables$weight <- rowMeans(subset(control.variables, select = c('anthroweight1lb','anthroweight2lb', 'anthroweight3lb')), na.rm = TRUE)
#BMI = (weight in pounds x 703) / (height in inches x height in inches)
control.variables$BMI <- (control.variables$weight  * 703) / (control.variables$anthroheightcalc*control.variables$anthroheightcalc)
control.variables <- inner_join(x=control.variables, y=dem_onlyrace, by='src_subject_id')
control.variables <- inner_join(x=control.variables, y=dem, by='src_subject_id')
control.variables[, 3:11][control.variables[, 3:11] == 999] <- NA
control.variables[, 3:11][control.variables[, 3:11] == 777] <- NA

#set outliers reflecting data-entry errors for weight and height to NA
outliers_weight <- subset(control.variables, weight < 5 | weight > 300) #these are definitely due to data error
control.variables$weight <- ifelse(control.variables$weight < 5 | control.variables$weight > 300, NA, control.variables$weight)
lower_bound <- median(control.variables$anthroheightcalc, na.rm = TRUE) - 5 * sd(control.variables$anthroheightcalc, na.rm = TRUE)
upper_bound <- median(control.variables$anthroheightcalc, na.rm = TRUE) + 5 * sd(control.variables$anthroheightcalc, na.rm = TRUE)
outliers_height <- subset(control.variables, anthroheightcalc < lower_bound | anthroheightcalc > upper_bound)
control.variables$anthroheightcalc <- ifelse(control.variables$anthroheightcalc < lower_bound | control.variables$anthroheightcalc > upper_bound, NA, control.variables$anthroheightcalc)
control.variables$BMI <- (control.variables$weight  * 703) / (control.variables$anthroheightcalc*control.variables$anthroheightcalc) #recalculate BMI

#################################
#create final covariate dataframe 
#################################
control.variables$demo_race_adapted <- as.factor(control.variables$demo_race_adapted)
control.variables$demo_comb_income_v2 <- as.factor(control.variables$demo_comb_income_v2)
control.variables$site_id_l <- as.factor(control.variables$site_id_l)
control.variables$ehi_y_ss_scoreb <- as.factor(control.variables$ehi_y_ss_scoreb)


################################################################################################
################################################################################################
#Control Group Selection
################################################################################################
################################################################################################

#https://cran.r-project.org/web/packages/MatchIt/vignettes/matching-methods.html
#https://datascienceplus.com/how-to-use-r-for-matching-samples-propensity-score/:

#For controls, consider all datasets not part of the TBI group and who have never been in one of the TBI-risk situations 
control_data_l <- anti_join(x = Baseline, y = allTBI_nd, by = "src_subject_id")
control_data_l <- subset(control_data_l , tbi_1 == 0 & tbi_2 == 0 & tbi_3 == 0 & tbi_4 == 0 & tbi_5 == 0 & tbi_6o == 0 & tbi_7a == 0) 
control_data_l$group <- as.factor(0)

#################################
#For TDC controls, do not consider those treated for broken bones (OI, medhx_6a=0)
#################################
medhist <- read.csv2("abcd_mx01.csv")
TDC_control_l <- inner_join(x = medhist, y = control_data_l, by = "src_subject_id") 
TDC_control_l <- subset(TDC_control_l, medhx_6a == "0" & medhx_2c == "0" & medhx_2e == "0" & medhx_2f == "0" & medhx_2h == "0" & medhx_2m == "0"& medhx_6i == "0" & medhx_6j == "0"& medhx_6p == "0") #Those who have NOT been treated by a doctor for broken bones / fractures
sampleTDC_l <- data.frame(TDC_control_l$src_subject_id)
colnames(sampleTDC_l) <- c("src_subject_id")
TDC_control_l <- inner_join(x = Baseline, y = sampleTDC_l, by = "src_subject_id")
TDC_control_l <- inner_join(x=TDC_control_l, y = dem, by='src_subject_id')
TDC_control_l$group <- as.factor(0)

otbi_TBI <- inner_join(x=Baseline, y=sampleTBI, by="src_subject_id")
otbi_TBI <- inner_join(x=otbi_TBI, y=dem, by="src_subject_id")
otbi_TBI$group <- as.factor(1) 
all <- rbind(otbi_TBI, TDC_control_l) %>% drop_na(demo_comb_income_v2) %>% drop_na(demo_race_adapted) #all includes the children with TBI before Baseline and those who are eligible for the TDC control group
all$demo_comb_income_v2 <- as.factor(all$demo_comb_income_v2)

#set seed for reproducible results:
set.seed(1234)
match.it <- matchit(group ~ interview_age + sex + demo_comb_income_v2+site_id_l+demo_race_adapted, data = all, method="optimal", ratio=2)
sample_inc_TDC <- match.data(match.it, subclass = "subclass")
sampleTDC_l <- data.frame(subset(sample_inc_TDC, sample_inc_TDC$group == 0)) %>% select("src_subject_id", "group", "subclass")
levels(sample_inc_TDC$group) <- c("mTBI", "TDC")

#################################
#For OI controls use the ones with a history of "broken bones" (medhx_6a=1)
#################################
OI_control_l <- inner_join(x = medhist, y = control_data_l, by = "src_subject_id") #also excluding those with any LOC
OI_control_l <- subset(OI_control_l , medhx_6a == "1" & medhx_2c == "0" & medhx_2e == "0" & medhx_2f == "0" & medhx_2h == "0" & medhx_2m == "0"& medhx_6i == "0" & medhx_6j == "0"& medhx_6p == "0") #Those who have been treated by a doctor for broken bones / fractures
sampleOI_l <- data.frame(OI_control_l$src_subject_id)
colnames(sampleOI_l) <- c("src_subject_id")
OI_control_l <- inner_join(x = Baseline, y = sampleOI_l, by = "src_subject_id")
OI_control_l <- inner_join(x=OI_control_l, y = dem, by='src_subject_id')
OI_control_l$group <- as.factor(0)

allOI <- rbind(otbi_TBI, OI_control_l)%>% drop_na(demo_comb_income_v2) %>% drop_na(demo_race_adapted)

set.seed(1234)
match.itOI <- matchit(group ~ interview_age + sex + demo_comb_income_v2+site_id_l+demo_race_adapted, data = allOI, method="optimal", ratio=2)
sample_inc_OI <- match.data(match.itOI, subclass = "subclass")
sampleOI_l <- data.frame(subset(sample_inc_OI, sample_inc_OI$group == 0)) %>% select("src_subject_id", "group", "subclass")
sampleOI_l$group <- as.factor(0)
colnames(sampleOI_l) <- c("src_subject_id", 'group')
levels(sample_inc_OI$group) <- c("mTBI", "OI")

rm(all, allOI, match.it, match.itOI)
