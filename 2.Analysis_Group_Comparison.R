######################################################################
#Outcome Variables:
######################################################################
library(mosaic)#for standardizing variables

barkley <- read.csv2('barkley_exec_func01.csv')
cols <- c("bdefs_calm_down_p",	"bdefs_consequences_p",	"bdefs_distract_upset_p",	"bdefs_explain_idea_p",	"bdefs_explain_pt_p",	"bdefs_explain_seq_p",	"bdefs_impulsive_action_p",	"bdefs_inconsistant_p",	"bdefs_lazy_p", "bdefs_process_info_p",	"bdefs_rechannel_p",	"bdefs_sense_time_p",	"bdefs_shortcuts_p", "bdefs_stop_think_p")
barkley[cols] <- sapply(barkley[cols],as.numeric)
barkley[, 10:26][barkley[, 10:26] == 999] <- NA
barkley[, 10:26][barkley[, 10:26] == 777] <- NA
barkley$Sum <- barkley$bdefs_calm_down_p + barkley$bdefs_consequences_p + barkley$bdefs_distract_upset_p + barkley$bdefs_explain_idea_p + barkley$bdefs_explain_pt_p + barkley$bdefs_explain_seq_p + barkley$bdefs_impulsive_action_p + barkley$bdefs_inconsistant_p + barkley$bdefs_lazy_p + barkley$bdefs_process_info_p + barkley$bdefs_rechannel_p + barkley$bdefs_sense_time_p + barkley$bdefs_shortcuts_p + barkley$bdefs_stop_think_p
barkley<-barkley %>% drop_na(Sum) %>% mutate(Sum = zscore(Sum))

NIH <- read.csv2("abcd_tbss01.csv")
cols <- c("nihtbx_picvocab_agecorrected", "nihtbx_flanker_agecorrected", "nihtbx_list_agecorrected", "nihtbx_cardsort_agecorrected", "nihtbx_pattern_agecorrected", "nihtbx_picture_agecorrected", "nihtbx_reading_agecorrected", "nihtbx_fluidcomp_agecorrected", "nihtbx_cryst_agecorrected", "nihtbx_totalcomp_agecorrected")
NIH[cols] <- sapply(NIH[cols],as.numeric)
NIH_Flanker <- NIH %>% drop_na(nihtbx_flanker_agecorrected)
NIH_Flanker_BL <- subset(NIH_Flanker, eventname == 'baseline_year_1_arm_1') %>% mutate(nihtbx_flanker_agecorrected = zscore(nihtbx_flanker_agecorrected))
NIH_Flanker_2y <- subset(NIH_Flanker, eventname == '2_year_follow_up_y_arm_1') %>% mutate(nihtbx_flanker_agecorrected = zscore(nihtbx_flanker_agecorrected))

UPPS <- read.csv2(file="abcd_upps01.csv")
cols <- c("upps6_y",	"upps7_y",	"upps11_y",	"upps12_y",	"upps15_y",	"upps16_y",	"upps17_y",	"upps18_y",	"upps19_y",	"upps20_y",	"upps21_y",	"upps22_y",	"upps23_y",	"upps24_y",	"upps27_y",	"upps28_y",	"upps35_y",	"upps36_y",	"upps37_y",	"upps39_y")
UPPS[cols] <- sapply(UPPS[cols],as.numeric)
UPPS$Sum <- UPPS$upps6_y	+ UPPS$upps7_y	+ UPPS$upps11_y	+ UPPS$upps12_y	+ UPPS$upps15_y	+ UPPS$upps16_y	+ UPPS$upps17_y	+ UPPS$upps18_y	+ UPPS$upps19_y	+ UPPS$upps20_y	+ UPPS$upps21_y	+ UPPS$upps22_y	+ UPPS$upps23_y	+ UPPS$upps24_y	+ UPPS$upps27_y	+ UPPS$upps28_y	+ UPPS$upps35_y	+ UPPS$upps36_y	+ UPPS$upps37_y	+ UPPS$upps39_y
UPPS<-UPPS %>% drop_na(Sum)
UPPS_BL <- subset(UPPS, eventname == 'baseline_year_1_arm_1') %>% mutate(Sum = zscore(Sum))
UPPS_2y <- subset(UPPS, eventname == '2_year_follow_up_y_arm_1') %>% mutate(Sum = zscore(Sum))

BISBAS <- read.csv2("abcd_bisbas01.csv") #Item "I do not become fearful or nervous, even when something bad happens to me." never answered! "bisbas5r_y" as replacement item
cols <- c("bisbas1_y",	"bisbas2_y",	"bisbas3_y",	"bisbas4_y",	"bisbas5_y",	"bisbas6_y",	"bisbas7_y",	"bisbas8_y",	"bisbas10_y",	"bisbas9_y",	"bisbas11_y", "bisbas12_y",	"bisbas13_y",	"bisbas14_y",	"bisbas15_y",	"bisbas16_y",	"bisbas17_y",	"bisbas18_y",	"bisbas19_y",	"bisbas20_y",	"bisbas5r_y")
BISBAS[cols] <- sapply(BISBAS[cols],as.numeric)
BISBAS$BIS_Sum <- BISBAS$bisbas1_y + BISBAS$bisbas2_y + BISBAS$bisbas3_y + BISBAS$bisbas4_y + (3-BISBAS$bisbas5r_y) + BISBAS$bisbas6_y + BISBAS$bisbas7_y
BISBAS$Reward_Resp_Sum <- BISBAS$bisbas8_y + BISBAS$bisbas9_y + BISBAS$bisbas10_y + BISBAS$bisbas11_y + BISBAS$bisbas12_y
BISBAS$Drive_Sum <- BISBAS$bisbas13_y +BISBAS$bisbas14_y +BISBAS$bisbas15_y +BISBAS$bisbas16_y
BISBAS$Fun_Seeking_Sum <- BISBAS$bisbas17_y + BISBAS$bisbas18_y + BISBAS$bisbas19_y + BISBAS$bisbas20_y
BISBAS$Total <- BISBAS$bisbas1_y + BISBAS$bisbas2_y + BISBAS$bisbas3_y + BISBAS$bisbas4_y + BISBAS$bisbas5r_y + BISBAS$bisbas6_y + BISBAS$bisbas7_y +BISBAS$bisbas8_y + BISBAS$bisbas9_y + BISBAS$bisbas10_y + BISBAS$bisbas11_y + BISBAS$bisbas12_y + BISBAS$bisbas13_y +BISBAS$bisbas14_y +BISBAS$bisbas15_y +BISBAS$bisbas16_y +BISBAS$bisbas17_y + BISBAS$bisbas18_y + BISBAS$bisbas19_y + BISBAS$bisbas20_y
BISBAS <- BISBAS %>% drop_na(Fun_Seeking_Sum)
BISBAS_BL <- subset(BISBAS, eventname == 'baseline_year_1_arm_1') %>% mutate(Fun_Seeking_Sum = zscore(Fun_Seeking_Sum))
BISBAS_2y <- subset(BISBAS, eventname == '2_year_follow_up_y_arm_1') %>% mutate(Fun_Seeking_Sum = zscore(Fun_Seeking_Sum))

DERS_P <- read.csv2("diff_emotion_reg_p01.csv") #insg. 29 Items
cols <- c("ders_attn_awareness_p",	"ders_clear_feelings_p",	"ders_emotion_overwhelm_p",	"ders_feelings_attentive_p",	"ders_feelings_care_p",	"ders_feelings_know_p",	"ders_upset_ack_p",	"ders_upset_angry_p",	"ders_upset_ashamed_p",	"ders_upset_behavior_control_p",	"ders_upset_behavior_p",	"ders_upset_better_p",	"ders_upset_concentrate_p",	"ders_upset_control_p", "ders_upset_depressed_p", "ders_upset_difficulty_p",	"ders_upset_embarrassed_p",	"ders_upset_emotion_overwhelm_p",	"ders_upset_esteem_p",	"ders_upset_feel_better_p",	"ders_upset_fixation_p",	"ders_upset_focus_p",	"ders_upset_guilty_p",	"ders_upset_irritation_p",	"ders_upset_long_time_better_p",	"ders_upset_lose_control_p",	"ders_upset_out_control_p",	"ders_upset_time_p",	"ders_upset_weak_p")
DERS_P[cols] <- sapply(DERS_P[cols],as.numeric)
DERS_P[, cols][DERS_P[, cols] == 777] <- NA
DERS_P$Fac1_Sum <- DERS_P$ders_emotion_overwhelm_p +DERS_P$ders_upset_control_p	+DERS_P$ders_upset_depressed_p +DERS_P$ders_upset_out_control_p +DERS_P$ders_upset_better_p +DERS_P$ders_upset_behavior_control_p +DERS_P$ders_upset_behavior_p +DERS_P$ders_upset_feel_better_p +DERS_P$ders_upset_lose_control_p +DERS_P$ders_upset_long_time_better_p +DERS_P$ders_upset_emotion_overwhelm_p
DERS_P$Fac2_Sum <- DERS_P$ders_upset_angry_p +DERS_P$ders_upset_embarrassed_p +DERS_P$ders_upset_ashamed_p +DERS_P$ders_upset_weak_p +DERS_P$ders_upset_guilty_p +DERS_P$ders_upset_irritation_p +DERS_P$ders_upset_esteem_p
DERS_P$Fac3_Sum <- DERS_P$ders_attn_awareness_p	+DERS_P$ders_clear_feelings_p +DERS_P$ders_feelings_attentive_p	+DERS_P$ders_feelings_care_p	+DERS_P$ders_feelings_know_p
DERS_P$Fac4_Sum <- DERS_P$ders_upset_difficulty_p +DERS_P$ders_upset_focus_p +DERS_P$ders_upset_concentrate_p +DERS_P$ders_upset_fixation_p
DERS_P$Total_Sum <- DERS_P$ders_attn_awareness_p	+DERS_P$ders_clear_feelings_p	+DERS_P$ders_emotion_overwhelm_p	+DERS_P$ders_feelings_attentive_p	+DERS_P$ders_feelings_care_p	+DERS_P$ders_feelings_know_p +DERS_P$ders_upset_ack_p	+DERS_P$ders_upset_angry_p	+DERS_P$ders_upset_ashamed_p	+DERS_P$ders_upset_behavior_control_p	+DERS_P$ders_upset_behavior_p	+DERS_P$ders_upset_better_p	+DERS_P$ders_upset_concentrate_p	+DERS_P$ders_upset_control_p	+DERS_P$ders_upset_depressed_p +DERS_P$ders_upset_difficulty_p	+DERS_P$ders_upset_embarrassed_p	+DERS_P$ders_upset_emotion_overwhelm_p	+DERS_P$ders_upset_esteem_p	+DERS_P$ders_upset_feel_better_p	+DERS_P$ders_upset_fixation_p	+DERS_P$ders_upset_focus_p	+DERS_P$ders_upset_guilty_p	+DERS_P$ders_upset_irritation_p	+DERS_P$ders_upset_long_time_better_p	+DERS_P$ders_upset_lose_control_p	+DERS_P$ders_upset_out_control_p	+DERS_P$ders_upset_time_p	+DERS_P$ders_upset_weak_p
DERS_P <- DERS_P %>% drop_na(Total_Sum) %>% mutate(Fac1_Sum = zscore(Fac1_Sum)) %>% mutate(Fac4_Sum = zscore(Fac4_Sum))


######################################################################
#Cognitive/Behavioral Outcome Measures
######################################################################

Measures <- control.variables
BL_age <- select(Baseline, c("interview_age","sex", "src_subject_id"))

Measures <- inner_join(x=Measures, y=BL_age, by="src_subject_id")
barkley <- select(barkley, c("Sum", "src_subject_id"))
colnames(barkley) <- c("Sum_BDEFS", 'src_subject_id')
Measures <- left_join(Measures, barkley, by='src_subject_id')
NIH_Flanker_2y <- select(NIH_Flanker_2y, c("nihtbx_flanker_agecorrected", "src_subject_id"))
colnames(NIH_Flanker_2y) <- c("NIH_Flanker_2y", 'src_subject_id')
Measures <- left_join(Measures, NIH_Flanker_2y, by='src_subject_id')
UPPS_2y <- select(UPPS_2y, c("Sum", "src_subject_id"))
colnames(UPPS_2y) <- c("Sum_UPPS_2y", 'src_subject_id')
Measures <- left_join(Measures, UPPS_2y, by='src_subject_id')
BISBAS_2y <- select(BISBAS_2y, c("Fun_Seeking_Sum","src_subject_id"))
colnames(BISBAS_2y) <- c("BISBAS_2y_Fun_Seeking_Sum", "src_subject_id")
Measures <- left_join(Measures, BISBAS_2y, by='src_subject_id')
DERS_P <- select(DERS_P, c("Fac1_Sum", "Fac4_Sum", "src_subject_id"))
colnames(DERS_P) <- c("DERS_P_Fac1_Sum", "DERS_P_Fac4_Sum", 'src_subject_id')
Measures <- left_join(Measures, DERS_P, by='src_subject_id')

#Convert to factors
Measures$site_id_l <- as.factor(Measures$site_id_l)
Measures$ehi_y_ss_scoreb <- as.factor(Measures$ehi_y_ss_scoreb)
Measures$demo_race_adapted <- as.factor(Measures$demo_race_adapted)
Measures$demo_comb_income_v2 <- as.factor(Measures$demo_comb_income_v2)
Measures$interview_age <- as.numeric(Measures$interview_age)
Measures$sex <- as.factor(Measures$sex)

sample_TDCTBI <- select(sample_inc_TDC, c("src_subject_id", "group", "subclass"))
sample_OITBI <- select(sample_inc_OI, c("src_subject_id", "group", "subclass"))

Measures_TDCTBI <- inner_join(x=Measures, y=sample_TDCTBI, by="src_subject_id")
Measures_OITBI <- inner_join(x=Measures, y=sample_OITBI, by="src_subject_id")
Measures_TDCTBI$group <- as.factor(Measures_TDCTBI$group)
Measures_OITBI$group <- as.factor(Measures_OITBI$group)


######################################################################
#Prepare Diffusion MRI Data
######################################################################
library(naniar)
abcd_harmonized <- read.csv("ABCD-DiffusionMeasure-data_NDA.csv") %>%
  select("subjectkey", "eventname", "sex", "interview_age", contains("CB_"), contains("CC"), contains("SLF_II"))%>%
  select("subjectkey", "eventname", "sex", "interview_age", contains("_Ten1_FA"), contains("_Ten1_MD")) %>% 
  replace_with_na_all(condition = ~.x == -1) %>% 
  mutate_at(vars(contains("_MD")), ~ . * 1000)%>%
  mutate(across(CB_left_Ten1_FA:SLF_II_right_Ten1_MD, ~zscore(., na.rm = getOption("na.rm", TRUE))))%>%
  rename(src_subject_id=subjectkey)

abcd_harmonized_TDC <- inner_join(x=abcd_harmonized, y=sample_TDCTBI, by ="src_subject_id")
abcd_harmonized_TDC <- inner_join(x=abcd_harmonized_TDC, y=control.variables, by ="src_subject_id")
abcd_harmonized_OI <- inner_join(x=abcd_harmonized, y=sample_OITBI, by ="src_subject_id")
abcd_harmonized_OI <- inner_join(x=abcd_harmonized_OI, y=control.variables, by ="src_subject_id")


######################################################################
#Descriptive Statistics
######################################################################
Measures_TDCTBI <- Measures_TDCTBI %>% drop_na(demo_comb_income_v2) %>% drop_na(demo_race_adapted) 
Measures_OITBI<- Measures_OITBI %>% drop_na(demo_comb_income_v2) %>% drop_na(demo_race_adapted) 

sample_TDCTBI <- select(Measures_TDCTBI, c("src_subject_id", "group"))
sample_OITBI <- select(Measures_OITBI, c("src_subject_id", "group"))

control.variables.desc <- inner_join(x=control.variables, y= sample_TDCTBI, by="src_subject_id")
control.variables.desc2 <- inner_join(x=control.variables, y= sample_OITBI, by="src_subject_id")

age1 <- inner_join(x=Baseline, y= sample_TDCTBI, by="src_subject_id")
describeBy(as.numeric(age1$interview_age), group=age1$group)
t.test(as.numeric(interview_age)~group, data=age1)

age2 <- inner_join(x=Baseline, y= sample_OITBI, by="src_subject_id")
describeBy(as.numeric(age2$interview_age), group=age2$group)
t.test(as.numeric(interview_age)~group, data=age2)

with(age1, table(group, sex)) %>% prop.table((margin = 1))
chisq.test(age1$sex, age1$group)

with(age2, table(group, sex)) %>% prop.table((margin = 1))
chisq.test(age2$sex, age2$group)

with(control.variables.desc, table(group, ehi_y_ss_scoreb)) %>% prop.table((margin = 1))
chisq.test(control.variables.desc$ehi_y_ss_scoreb, control.variables.desc$group)

with(control.variables.desc2, table(group, ehi_y_ss_scoreb)) %>% prop.table((margin = 1))
chisq.test(control.variables.desc2$ehi_y_ss_scoreb, control.variables.desc2$group)

describeBy(control.variables.desc$BMI, group=control.variables.desc$group)
t.test(as.numeric(BMI)~group, data=control.variables.desc)

describeBy(control.variables.desc2$BMI, group=control.variables.desc2$group)
t.test(as.numeric(BMI)~group, data=control.variables.desc2)


with(control.variables.desc, table(group, demo_race_adapted)) 
with(control.variables.desc, table(group, demo_race_adapted)) %>% prop.table((margin = 1))
fisher.test(control.variables.desc$group, control.variables.desc$demo_race_adapted)

with(control.variables.desc2, table(group, demo_race_adapted)) 
with(control.variables.desc2, table(group, demo_race_adapted)) %>% prop.table((margin = 1))
fisher.test(control.variables.desc2$group, control.variables.desc2$demo_race_adapted)


with(control.variables.desc, table(group, demo_comb_income_v2)) 
with(control.variables.desc, table(group, demo_comb_income_v2)) %>% prop.table((margin = 1))
fisher.test(control.variables.desc$group, control.variables.desc$demo_comb_income_v2, simulate.p.value=TRUE)

with(control.variables.desc2, table(group, demo_comb_income_v2)) 
with(control.variables.desc2, table(group, demo_comb_income_v2)) %>% prop.table((margin = 1))
chisq.test(control.variables.desc2$group, control.variables.desc2$demo_comb_income_v2)

fisher.test(control.variables.desc$group, control.variables.desc$site_id_l, simulate.p.value=TRUE)
fisher.test(control.variables.desc2$group, control.variables.desc2$site_id_l, simulate.p.value=TRUE)


abcd_harmonized_OI %>%
  group_by(group) %>%
  summarise(total_non_na = sum(!is.na(CB_left_Ten1_FA) & !is.na(BMI)& !is.na(ehi_y_ss_scoreb)))#example for determining the respective sample 

abcd_harmonized_TDC %>%
  group_by(group) %>%
  summarise(total_non_na = sum(!is.na(CB_left_Ten1_FA) & !is.na(BMI)& !is.na(ehi_y_ss_scoreb)))#example for determining the respective sample 

Measures_TDCTBI %>%
  group_by(group) %>%
  summarise(total_non_na = sum(!is.na(DERS_P_Fac4_Sum)))#example for determining the respective sample 


################################################################################################
################################################################################################
#Data Analysis
################################################################################################
################################################################################################

######################################################################
#Linear Regression Models for Cognition
######################################################################

library(rstatix)
joint_model <- rbind(Measures_TDCTBI, Measures_OITBI[Measures_OITBI$group=="OI", ])

#combined regression model, also accounting for baseline age
cogtest_lm_joint <- lapply(14:19, function(i) broom::tidy(lm(joint_model[,i] ~ joint_model[,"group"]+joint_model[,"interview_age"]+joint_model[,"sex"]+joint_model[,"demo_comb_income_v2"]+joint_model[,"site_id_l"]+joint_model[,"demo_race_adapted"])))
names(cogtest_lm_joint) <- names(joint_model[14:19])
lm_information_joint <-
  map2_df(cogtest_lm_joint,
          names(cogtest_lm_joint),
          ~ mutate(.x, which_dependent = .y)) 
cogn_joint <- lm_information_joint%>%filter(str_detect(term,"group"))
cogn_joint$lower <- cogn_joint$estimate - 1.96*cogn_joint$std.error
cogn_joint$upper <- cogn_joint$estimate + 1.96*cogn_joint$std.error
cogn_joint$p.adjusted <- p.adjust(cogn_joint$p.value,method = "fdr")

#####################################################################################


lmBDEFS <- lm(Sum_BDEFS ~ group + interview_age + sex + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= joint_model)
partial_f2(lmBDEFS, covariates = c("groupTDC", "groupOI"))
BDEFS_stats <- ggcoefstats(lmBDEFS, output="tidy") %>% mutate(variable="BDEFS")%>%filter(str_detect(term,"group"))

lmFlanker <- lm(NIH_Flanker_2y ~ group + interview_age + sex + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= joint_model)
partial_f2(lmFlanker, covariates = c("groupTDC", "groupOI"))
Flanker_stats <- ggcoefstats(lmFlanker, output="tidy") %>% mutate(variable="Flanker")%>%filter(str_detect(term,"group"))

lmUPPS <- lm(Sum_UPPS_2y ~ group + interview_age + sex + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= joint_model)
partial_f2(lmUPPS, covariates = c("groupTDC", "groupOI"))
UPPS_stats <- ggcoefstats(lmUPPS, output="tidy") %>% mutate(variable="UPPS-P")%>%filter(str_detect(term,"group"))

lmBISBAS <- lm(BISBAS_2y_Fun_Seeking_Sum ~ group + interview_age + sex + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= joint_model)
partial_f2(lmBISBAS, covariates = c("groupTDC", "groupOI"))
BISBAS_stats <- ggcoefstats(lmBISBAS, output="tidy") %>% mutate(variable="BISBAS")%>%filter(str_detect(term,"group"))

lmDERS1 <- lm(DERS_P_Fac1_Sum ~ group + interview_age + sex + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= joint_model)
partial_f2(lmDERS1, covariates = c("groupTDC", "groupOI"))
DERS1_stats <- ggcoefstats(lmDERS1, output="tidy") %>% mutate(variable="DERS1")%>%filter(str_detect(term,"group"))

lmDERS4 <- lm(DERS_P_Fac4_Sum ~ group + interview_age + sex + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= joint_model)
partial_f2(lmDERS4, covariates = c("groupTDC", "groupOI"))
DERS4_stats <- ggcoefstats(lmDERS4, output="tidy") %>% mutate(variable="DERS4")%>%filter(str_detect(term,"group"))

statsMerged <- do.call("rbind", list(BDEFS_stats, Flanker_stats, UPPS_stats, BISBAS_stats, DERS1_stats, DERS4_stats)) %>% mutate(p.adjusted=NA)
statsMerged$p.adjusted <- p.adjust(statsMerged$p.value,method = "fdr")
statsMerged <- statsMerged %>% relocate(any_of(c("variable", "term","df.error", "statistic", "estimate", "conf.low", "conf.high", "p.adjusted"))) %>% mutate_at(4:7, round, 2) %>% mutate_at(8, round, 3) %>% select(c("variable", "term", "df.error", "statistic", "estimate", "conf.low", "conf.high", "p.adjusted"))
names(statsMerged) <- c("Term", "control","Df", "Statistic", "B", "CI_lower", "CI_upper", "p-value")


######################################################################
#Linear Regression Models for Diffusion
######################################################################

abcd_harmonized_joint <- rbind(abcd_harmonized_TDC, abcd_harmonized_OI[abcd_harmonized_OI$group=="OI", ])

#Regression for group differences in FA
FA_lm_cov <- lapply(5:17, function(i) broom::tidy(lm(abcd_harmonized_joint[,i] ~ abcd_harmonized_joint[,"group"]+abcd_harmonized_joint[,"interview_age"]+abcd_harmonized_joint[,"sex"]+abcd_harmonized_joint[,"demo_comb_income_v2"]+abcd_harmonized_joint[,"demo_race_adapted"]+ abcd_harmonized_joint[,"BMI"] +abcd_harmonized_joint[,"site_id_l"]+abcd_harmonized_joint[,"ehi_y_ss_scoreb"])))
names(FA_lm_cov) <- names(abcd_harmonized_joint[5:17])
FA_cov <-
  map2_df(FA_lm_cov,
          names(FA_lm_cov),
          ~ mutate(.x, which_dependent = .y)) %>%
  select(which_dependent, everything()) %>%
  filter(str_detect(term,"group"))
FA_cov$p.adjusted <- p.adjust(FA_cov$p.value,method = "fdr")

CB_l <- lm(CB_left_Ten1_FA ~ group+ interview_age +sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CB left")
partial_f2(lm(CB_left_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CB_r <- lm(CB_right_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CB right")
partial_f2(lm(CB_right_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

SLFII_l <- lm(SLF_II_left_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="SLF II left")
partial_f2(lm(SLF_II_left_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

SLFII_r <- lm(SLF_II_right_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="SLF II right")
partial_f2(lm(SLF_II_right_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

SLFIII_l <- lm(SLF_III_left_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="SLF III left")
partial_f2(lm(SLF_III_left_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

SLFIII_r <- lm(SLF_III_right_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="SLF III right")
partial_f2(lm(SLF_III_right_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC1 <- lm(CC1_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 1")
partial_f2(lm(CC1_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC2 <- lm(CC2_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 2")
partial_f2(lm(CC2_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC3 <- lm(CC3_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 3")
partial_f2(lm(CC3_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC4 <- lm(CC4_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 4")
partial_f2(lm(CC4_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC5 <- lm(CC5_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 5")
partial_f2(lm(CC5_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC6 <- lm(CC6_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 6")
partial_f2(lm(CC6_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC7 <- lm(CC7_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 7")
partial_f2(lm(CC7_Ten1_FA ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

FAMerged <- do.call("rbind", list(CB_l, CB_r, SLFII_l, SLFII_r, SLFIII_l, SLFIII_r, CC1, CC2, CC3, CC4, CC5, CC6, CC7)) %>% mutate(p.adjusted=NA)
FAMerged$p.adjusted <- p.adjust(FAMerged$p.value,method = "fdr")
levels(FAMerged$term) <- list(TDC  = "groupTDC", OI = "groupOI") 
FAMerged <- rename(FAMerged,control="term")

FAMerged <- FAMerged %>% relocate(any_of(c("variable", "control", "df.error", "statistic", "estimate", "conf.low", "conf.high", "p.adjusted"))) %>% mutate_at(4, round, 2) %>% mutate_at(5, round, 4) %>% mutate_at(6:7, round, 3) %>% mutate_at(8, round, 3) %>% select(c("variable", "control", "df.error", "statistic", "estimate", "conf.low", "conf.high", "p.adjusted"))
names(FAMerged) <- c("Term", "control","Df", "Statistic", "B", "CI_lower", "CI_upper", "p")
FAMerged$B <- format(FAMerged$B, scientific = FALSE)
FAMerged$CI1 <- "["
FAMerged$CI2 <- ";"
FAMerged$CI3 <- "]"
FAMerged<-unite(FAMerged, col='95% CI', c('CI1', 'CI_lower', "CI2", "CI_upper","CI3"), sep='')

##########################################################  
#Regression for group differences in MD 

MD_lm_cov <- lapply(17:29, function(i) broom::tidy(lm(abcd_harmonized_joint[,i] ~ abcd_harmonized_joint[,"group"]+abcd_harmonized_joint[,"interview_age"]+abcd_harmonized_joint[,"sex"]+abcd_harmonized_joint[,"demo_comb_income_v2"]+abcd_harmonized_joint[,"demo_race_adapted"]+ abcd_harmonized_joint[,"BMI"] +abcd_harmonized_joint[,"site_id_l"]+abcd_harmonized_joint[,"ehi_y_ss_scoreb"])))
names(MD_lm_cov) <- names(abcd_harmonized_joint[17:29])
MD_cov <-
  map2_df(MD_lm_cov,
          names(MD_lm_cov),
          ~ mutate(.x, which_dependent = .y)) %>%
  select(which_dependent, everything()) %>%
  filter(str_detect(term,"group"))
MD_cov$p.adjusted <- p.adjust(MD_cov$p.value,method = "fdr")


CB_l <- lm(CB_left_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CB left")
partial_f2(lm(CB_left_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CB_r <- lm(CB_right_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CB right")
partial_f2(lm(CB_right_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

SLFII_l <- lm(SLF_II_left_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="SLF II left")
partial_f2(lm(SLF_II_left_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

SLFII_r <- lm(SLF_II_right_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="SLF II right")
partial_f2(lm(SLF_II_right_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

SLFIII_l <- lm(SLF_III_left_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="SLF III left")
partial_f2(lm(SLF_III_left_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

SLFIII_r <- lm(SLF_III_right_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="SLF III right")
partial_f2(lm(SLF_III_right_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC1 <- lm(CC1_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 1")
partial_f2(lm(CC1_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC2 <- lm(CC2_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 2")
partial_f2(lm(CC2_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC3 <- lm(CC3_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 3")
partial_f2(lm(CC3_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC4 <- lm(CC4_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 4")
partial_f2(lm(CC4_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC5 <- lm(CC5_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 5")
partial_f2(lm(CC5_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC6 <- lm(CC6_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 6")
partial_f2(lm(CC6_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

CC7 <- lm(CC7_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"group"))%>% mutate(variable="CC 7")
partial_f2(lm(CC7_Ten1_MD ~ group+ interview_age+ sex+ demo_comb_income_v2 + demo_race_adapted + BMI + site_id_l+ehi_y_ss_scoreb, data= abcd_harmonized_joint), covariates = c("groupTDC", "groupOI"))

MDMerged <- do.call("rbind", list(CB_l, CB_r, SLFII_l, SLFII_r, SLFIII_l, SLFIII_r, CC1, CC2, CC3, CC4, CC5, CC6, CC7)) %>% mutate(p.adjusted=NA)
MDMerged$p.adjusted <- p.adjust(MDMerged$p.value,method = "fdr")
levels(MDMerged$term) <- list(TDC  = "groupTDC", OI = "groupOI") 
MDMerged <- rename(MDMerged,control="term")
MDMerged <- MDMerged %>% relocate(any_of(c("variable", "control", "df.error", "statistic", "estimate", "conf.low", "conf.high", "p.adjusted"))) %>% mutate_at(4, round, 2) %>% mutate_at(5, round, 4) %>% mutate_at(6:7, round, 3) %>% mutate_at(8, round, 3) %>% select(c("variable", "control", "df.error", "statistic", "estimate", "conf.low", "conf.high", "p.adjusted"))
names(MDMerged) <- c("Term", "control","Df", "Statistic", "B", "CI_lower", "CI_upper", "p")

MDMerged$B <- format(MDMerged$B, scientific = FALSE)
MDMerged$CI1 <- "["
MDMerged$CI2 <- ";"
MDMerged$CI3 <- "]"
MDMerged<-unite(MDMerged, col='95% CI', c('CI1', 'CI_lower', "CI2", "CI_upper","CI3"), sep='')
