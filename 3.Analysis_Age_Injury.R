######################################################################
######################################################################
#Age at TBI
######################################################################
######################################################################

Measures_agedummy <- left_join(x=Measures_TDCTBI, y=typeofTBIs, by="src_subject_id")
Measures_agedummy$age_cat <- factor(Measures_agedummy$age_cat, levels = levels(addNA(Measures_agedummy$age_cat)), labels = c(levels(Measures_agedummy$age_cat), "-1"), exclude = NULL)
Measures_agedummy<-Measures_agedummy[!(Measures_agedummy$subclass=="41" | Measures_agedummy$subclass=="54"),]

table(Measures_agedummy$age_cat)
table(Measures_agedummy$group.x)


######################################################################
#Divide in three age groups and combine with matched controls for direct comparison
######################################################################
controls <- subset(Measures_agedummy, age_cat=="-1")
youngest <- subset(Measures_agedummy, age_cat=="0-3") 
subclass_youngest <- data.frame(youngest$subclass) %>% rename(subclass = "youngest.subclass")
youngest <- inner_join(controls, subclass_youngest) %>% rbind(youngest)
middle <- subset(Measures_agedummy, age_cat=="4-7") 
subclass_middle <- data.frame(middle$subclass) %>% rename(subclass = "middle.subclass")
middle <- inner_join(controls, subclass_middle) %>% rbind(middle)
oldest <- subset(Measures_agedummy, age_cat=="8-10") 
subclass_oldest <- data.frame(oldest$subclass) %>% rename(subclass = "oldest.subclass")
oldest <- inner_join(controls, subclass_oldest) %>% rbind(oldest)

######################################################################
#Cognition and Behavior
######################################################################
cogtest_agedummy_y <- lapply(14:19, function(i) broom::tidy(lm(youngest[,i] ~ youngest[,"group.x"]+youngest[,"interview_age.x"]+youngest[,"sex.x"]+youngest[,"demo_comb_income_v2"] + youngest[,"site_id_l"]+ youngest[,"demo_race_adapted"])))
names(cogtest_agedummy_y) <- names(youngest[14:19])
cog_agedummy_y <-
  map2_df(cogtest_agedummy_y,
          names(cogtest_agedummy_y),
          ~ mutate(.x, which_dependent = .y)) %>%
  select(which_dependent, everything())%>%
  filter(str_detect(term,"group.x"))%>%
  mutate(comparison = "0-3")


cogtest_agedummy_m <- lapply(14:19, function(i) broom::tidy(lm(middle[,i] ~ middle[,"group.x"]+middle[,"interview_age.x"]+middle[,"sex.x"]+middle[,"demo_comb_income_v2"] + middle[,"site_id_l"]+ middle[,"demo_race_adapted"])))
names(cogtest_agedummy_m) <- names(middle[14:19])
cog_agedummy_m <-
  map2_df(cogtest_agedummy_m,
          names(cogtest_agedummy_m),
          ~ mutate(.x, which_dependent = .y)) %>%
  select(which_dependent, everything())%>%
  filter(str_detect(term,"group.x"))%>%
  mutate(comparison = "4-7")


cogtest_agedummy_o <- lapply(14:19, function(i) broom::tidy(lm(oldest[,i] ~ oldest[,"group.x"]+oldest[,"interview_age.x"]+oldest[,"sex.x"]+oldest[,"demo_comb_income_v2"] + oldest[,"site_id_l"]+ oldest[,"demo_race_adapted"])))
names(cogtest_agedummy_o) <- names(oldest[14:19])
cog_agedummy_o <-
  map2_df(cogtest_agedummy_o,
          names(cogtest_agedummy_o),
          ~ mutate(.x, which_dependent = .y)) %>%
  select(which_dependent, everything())%>%
  filter(str_detect(term,"group.x"))%>%
  mutate(comparison = "8-10")

cog_agedummy_all <- rbind(cog_agedummy_y, cog_agedummy_m, cog_agedummy_o)
cog_agedummy_all$p.adjusted <- p.adjust(cog_agedummy_all$p.value,method = "fdr")

######################################################################

age_BDEFSy <- lm(Sum_BDEFS ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= youngest)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="BDEFS")%>% mutate(term="0-3")
partial_f2(lm(Sum_BDEFS ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= youngest))

age_BDEFSm <- lm(Sum_BDEFS ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= middle)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="BDEFS")%>% mutate(term="4-7")
partial_f2(lm(Sum_BDEFS ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= middle))

age_BDEFSo <- lm(Sum_BDEFS ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= oldest)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="BDEFS")%>% mutate(term="8-10")
partial_f2(lm(Sum_BDEFS ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= oldest))

age_Flankery <- lm(NIH_Flanker_2y ~ relevel(age_cat, ref = "-1")+ interview_age.x  + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= youngest)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="Flanker")%>% mutate(term="0-3")
partial_f2(lm(NIH_Flanker_2y ~ relevel(age_cat, ref = "-1")+ interview_age.x  + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= youngest))

age_Flankerm <- lm(NIH_Flanker_2y ~ relevel(age_cat, ref = "-1")+ interview_age.x  + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= middle)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="Flanker")%>% mutate(term="4-7")
partial_f2(lm(NIH_Flanker_2y ~ relevel(age_cat, ref = "-1")+ interview_age.x  + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= middle))

age_Flankero <- lm(NIH_Flanker_2y ~ relevel(age_cat, ref = "-1")+ interview_age.x  + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= oldest)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="Flanker")%>% mutate(term="8-10")
partial_f2(lm(NIH_Flanker_2y ~ relevel(age_cat, ref = "-1")+ interview_age.x  + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= oldest))

age_UPPSy <- lm(Sum_UPPS_2y ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= youngest)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="UPPS")%>% mutate(term="0-3")
partial_f2(lm(Sum_UPPS_2y ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= youngest))

age_UPPSm <- lm(Sum_UPPS_2y ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= middle)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="UPPS")%>% mutate(term="4-7")
partial_f2(lm(Sum_UPPS_2y ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= middle))

age_UPPSo <- lm(Sum_UPPS_2y ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= oldest)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="UPPS")%>% mutate(term="8-10")
partial_f2(lm(Sum_UPPS_2y ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= oldest))

age_BISBASy <- lm(BISBAS_2y_Fun_Seeking_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= youngest)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="BISBAS")%>% mutate(term="0-3")
partial_f2(lm(BISBAS_2y_Fun_Seeking_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= youngest))

age_BISBASm <- lm(BISBAS_2y_Fun_Seeking_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= middle)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="BISBAS")%>% mutate(term="4-7")
partial_f2(lm(BISBAS_2y_Fun_Seeking_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= middle))

age_BISBASo <- lm(BISBAS_2y_Fun_Seeking_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= oldest)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="BISBAS")%>% mutate(term="8-10")
partial_f2(lm(BISBAS_2y_Fun_Seeking_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= oldest))

age_DERS1y <- lm(DERS_P_Fac1_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= youngest)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="DERS1")%>% mutate(term="0-3")
partial_f2(lm(DERS_P_Fac1_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= youngest))

age_DERS1m <- lm(DERS_P_Fac1_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= middle)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="DERS1")%>% mutate(term="4-7")
partial_f2(lm(DERS_P_Fac1_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= middle))

age_DERS1o <- lm(DERS_P_Fac1_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= oldest)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="DERS1")%>% mutate(term="8-10")
partial_f2(lm(DERS_P_Fac1_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= oldest))

age_DERS4y <- lm(DERS_P_Fac4_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= youngest)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="DERS4")%>% mutate(term="0-3")
partial_f2(lm(DERS_P_Fac4_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= youngest))

age_DERS4m <- lm(DERS_P_Fac4_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= middle)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="DERS4")%>% mutate(term="4-7")
partial_f2(lm(DERS_P_Fac4_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= middle))

age_DERS4o <- lm(DERS_P_Fac4_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= oldest)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="DERS4")%>% mutate(term="8-10")
partial_f2(lm(DERS_P_Fac4_Sum ~ relevel(age_cat, ref = "-1") + interview_age.x + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l, data= oldest))

Age_Cog_Merged <- do.call("rbind", list(age_BDEFSy,age_BDEFSm,age_BDEFSo, age_Flankery, age_Flankerm,age_Flankero,age_UPPSy, age_UPPSm,age_UPPSo,age_BISBASy, age_BISBASm,age_BISBASo,age_DERS1y, age_DERS1m,age_DERS1o,age_DERS4y,age_DERS4m,age_DERS4o)) %>% mutate(p.adjusted=NA)
Age_Cog_Merged$p.adjusted <- p.adjust(Age_Cog_Merged$p.value,method = "fdr")
#Age_Cog_Merged$term<- sapply(strsplit(as.character(Age_Cog_Merged$term), split=')', fixed=TRUE), function(x) (x[2]))
Age_Cog_Merged <- Age_Cog_Merged %>% relocate(any_of(c("variable", "term", "df.error", "statistic", "estimate", "conf.low", "conf.high", "p.adjusted"))) %>% mutate_at(4, round, 2) %>% mutate_at(5, round, 2) %>% mutate_at(6:7, round, 2) %>% mutate_at(8, round, 3) %>% select(c("variable", "term", "df.error", "statistic", "estimate", "conf.low", "conf.high", "p.adjusted"))
names(Age_Cog_Merged) <- c("Term", "control","Df", "Statistic", "B", "CI_lower", "CI_upper", "p")

Measures_ageTBI <- inner_join(x=Measures_TDCTBI, y=typeofTBIs, by="src_subject_id")
Measures_ageTBI$timesince <- Measures_ageTBI$interview_age.x - (Measures_ageTBI$agefirst * 12) #maximum time since injury in months
describe(Measures_ageTBI$timesince)

#########################################################################  
#Fractional Anisotropy
#########################################################################  
dti_TBI_agedummy <- inner_join(x=abcd_harmonized, y=sample_TDCTBI, by="src_subject_id")

dti_TBI_agedummy_y <- inner_join(x=dti_TBI_agedummy, y=youngest, by="src_subject_id")
dti_TBI_agedummy_m <- inner_join(x=dti_TBI_agedummy, y=middle, by="src_subject_id")
dti_TBI_agedummy_o <- inner_join(x=dti_TBI_agedummy, y=oldest, by="src_subject_id")

FA_lm_agedummy_y <- lapply(5:17, function(i) broom::tidy(lm(dti_TBI_agedummy_y[,i] ~ relevel(dti_TBI_agedummy_y[,"age_cat"], ref = "-1")+ dti_TBI_agedummy_y[,"interview_age"]+ dti_TBI_agedummy_y[,"sex.x"]+ dti_TBI_agedummy_y[,"BMI"] +dti_TBI_agedummy_y[,"site_id_l"]+dti_TBI_agedummy_y[,"ehi_y_ss_scoreb"]+dti_TBI_agedummy_y[,"demo_race_adapted"]+dti_TBI_agedummy_y[,"demo_comb_income_v2"])))
names(FA_lm_agedummy_y) <- names(dti_TBI_agedummy_y[5:17])
FA_agedummy_y <-
  map2_df(FA_lm_agedummy_y,
          names(FA_lm_agedummy_y),
          ~ mutate(.x, which_dependent = .y)) %>%
  select(which_dependent, everything()) %>% filter(str_detect(term,"age_cat"))

FA_lm_agedummy_m <- lapply(5:17, function(i) broom::tidy(lm(dti_TBI_agedummy_m[,i] ~ relevel(dti_TBI_agedummy_m[,"age_cat"], ref = "-1")+ dti_TBI_agedummy_m[,"interview_age"]+ dti_TBI_agedummy_m[,"sex.x"]+ dti_TBI_agedummy_m[,"BMI"] +dti_TBI_agedummy_m[,"site_id_l"]+dti_TBI_agedummy_m[,"ehi_y_ss_scoreb"]+dti_TBI_agedummy_m[,"demo_race_adapted"]+dti_TBI_agedummy_m[,"demo_comb_income_v2"])))
names(FA_lm_agedummy_m) <- names(dti_TBI_agedummy_m[5:17])
FA_agedummy_m <-
  map2_df(FA_lm_agedummy_m,
          names(FA_lm_agedummy_m),
          ~ mutate(.x, which_dependent = .y)) %>%
  select(which_dependent, everything()) %>% filter(str_detect(term,"age_cat"))

FA_lm_agedummy_o <- lapply(5:17, function(i) broom::tidy(lm(dti_TBI_agedummy_o[,i] ~ relevel(dti_TBI_agedummy_o[,"age_cat"], ref = "-1")+ dti_TBI_agedummy_o[,"interview_age"]+ dti_TBI_agedummy_o[,"sex.x"]+ dti_TBI_agedummy_o[,"BMI"] +dti_TBI_agedummy_o[,"site_id_l"]+dti_TBI_agedummy_o[,"ehi_y_ss_scoreb"]+dti_TBI_agedummy_o[,"demo_race_adapted"]+dti_TBI_agedummy_o[,"demo_comb_income_v2"])))
names(FA_lm_agedummy_o) <- names(dti_TBI_agedummy_o[5:17])
FA_agedummy_o <-
  map2_df(FA_lm_agedummy_o,
          names(FA_lm_agedummy_o),
          ~ mutate(.x, which_dependent = .y)) %>%
  select(which_dependent, everything()) %>% filter(str_detect(term,"age_cat"))


FA_agedummy_all <- rbind(FA_agedummy_y, FA_agedummy_m, FA_agedummy_o)
FA_agedummy_all$p.adjusted <- p.adjust(FA_agedummy_all$p.value,method = "fdr")  


#youngest:
age_CB_l_y <- lm(CB_left_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CB left", age="0-3")
partial_f2(lm(CB_left_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CB_r_y <- lm(CB_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CB right", age="0-3")
partial_f2(lm(CB_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_SLFII_l_y <- lm(SLF_II_left_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF II left", age="0-3")
partial_f2(lm(SLF_II_left_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_SLFII_r_y <- lm(SLF_II_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF II right", age="0-3")
partial_f2(lm(SLF_II_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_SLFIII_l_y <- lm(SLF_III_left_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF III left", age="0-3")
partial_f2(lm(SLF_III_left_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_SLFIII_r_y <- lm(SLF_III_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF III right", age="0-3")
partial_f2(lm(SLF_III_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC1_y <- lm(CC1_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 1", age="0-3")
partial_f2(lm(CC1_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC2_y <- lm(CC2_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 2", age="0-3")
partial_f2(lm(CC2_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC3_y <- lm(CC3_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 3", age="0-3")
partial_f2(lm(CC3_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC4_y <- lm(CC4_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 4", age="0-3")
partial_f2(lm(CC4_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC5_y <- lm(CC5_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 5", age="0-3")
partial_f2(lm(CC5_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC6_y <- lm(CC6_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 6", age="0-3")
partial_f2(lm(CC6_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC7_y <- lm(CC7_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 7", age="0-3")
partial_f2(lm(CC7_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

#middle:
age_CB_l_m <- lm(CB_left_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CB left", age="4-7")
partial_f2(lm(CB_left_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CB_r_m <- lm(CB_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CB right", age="4-7")
partial_f2(lm(CB_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_SLFII_l_m <- lm(SLF_II_left_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF II left", age="4-7")
partial_f2(lm(SLF_II_left_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_SLFII_r_m <- lm(SLF_II_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF II right", age="4-7")
partial_f2(lm(SLF_II_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_SLFIII_l_m <- lm(SLF_III_left_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF III left", age="4-7")
partial_f2(lm(SLF_III_left_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_SLFIII_r_m <- lm(SLF_III_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF III right", age="4-7")
partial_f2(lm(SLF_III_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC1_m <- lm(CC1_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 1", age="4-7")
partial_f2(lm(CC1_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC2_m <- lm(CC2_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 2", age="4-7")
partial_f2(lm(CC2_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC3_m <- lm(CC3_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 3", age="4-7")
partial_f2(lm(CC3_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC4_m <- lm(CC4_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 4", age="4-7")
partial_f2(lm(CC4_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC5_m <- lm(CC5_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 5", age="4-7")
partial_f2(lm(CC5_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC6_m <- lm(CC6_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 6", age="4-7")
partial_f2(lm(CC6_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC7_m <- lm(CC7_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 7", age="4-7")
partial_f2(lm(CC7_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

#oldest:
age_CB_l_o <- lm(CB_left_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CB left", age="8-10")
partial_f2(lm(CB_left_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CB_r_o <- lm(CB_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CB right", age="8-10")
partial_f2(lm(CB_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_SLFII_l_o <- lm(SLF_II_left_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF II left", age="8-10")
partial_f2(lm(SLF_II_left_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_SLFII_r_o <- lm(SLF_II_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF II right", age="8-10")
partial_f2(lm(SLF_II_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_SLFIII_l_o <- lm(SLF_III_left_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF III left", age="8-10")
partial_f2(lm(SLF_III_left_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_SLFIII_r_o <- lm(SLF_III_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF III right", age="8-10")
partial_f2(lm(SLF_III_right_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC1_o <- lm(CC1_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 1", age="8-10")
partial_f2(lm(CC1_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC2_o <- lm(CC2_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 2", age="8-10")
partial_f2(lm(CC2_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC3_o <- lm(CC3_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 3", age="8-10")
partial_f2(lm(CC3_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC4_o <- lm(CC4_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 4", age="8-10")
partial_f2(lm(CC4_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC5_o <- lm(CC5_Ten1_FA ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 5", age="8-10")
partial_f2(lm(CC5_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC6_o <- lm(CC6_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 6", age="8-10")
partial_f2(lm(CC6_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC7_o <- lm(CC7_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 7", age="8-10")
partial_f2(lm(CC7_Ten1_FA ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

#all
FA_age_Merged <- do.call("rbind", list(age_CB_l_y, age_CB_l_m,age_CB_l_o,age_CB_r_y,age_CB_r_m,age_CB_r_o, age_SLFII_l_y, age_SLFII_l_m,age_SLFII_l_o,age_SLFII_r_y,age_SLFII_r_m,age_SLFII_r_o, age_SLFIII_l_y,age_SLFIII_l_m,age_SLFIII_l_o, age_SLFIII_r_y,age_SLFIII_r_m,age_SLFIII_r_o, age_CC1_y,age_CC1_m, age_CC1_o, age_CC2_y, age_CC2_m, age_CC2_o, age_CC3_y,age_CC3_m,age_CC3_o, age_CC4_y,age_CC4_m, age_CC4_o, age_CC5_y, age_CC5_m, age_CC5_o, age_CC6_y,age_CC6_m, age_CC6_o, age_CC7_y, age_CC7_m, age_CC7_o)) %>% mutate(p.adjusted=NA)
FA_age_Merged$p.adjusted <- p.adjust(FA_age_Merged$p.value,method = "fdr")  
FA_age_Merged <- FA_age_Merged %>% relocate(any_of(c("variable", "age", "df.error", "statistic", "estimate", "conf.low", "conf.high", "p.adjusted"))) %>% mutate_at(4, round, 2) %>% mutate_at(5, round, 4) %>% mutate_at(6:7, round, 3) %>% mutate_at(8, round, 3) %>% select(c("variable", "age", "df.error", "statistic", "estimate", "conf.low", "conf.high", "p.adjusted"))
names(FA_age_Merged) <- c("Term", "age","Df", "Statistic", "B", "CI_lower", "CI_upper", "p")

FA_age_Merged$B <- format(FA_age_Merged$B, scientific = FALSE)
FA_age_Merged$CI1 <- "["
FA_age_Merged$CI2 <- ";"
FA_age_Merged$CI3 <- "]"
FA_age_Merged<-unite(FA_age_Merged, col='95% CI', c('CI1', 'CI_lower', "CI2", "CI_upper","CI3"), sep='')


#########################################################################  
#Mean Diffusivity
#########################################################################  

MD_lm_agedummy_y <- lapply(18:30, function(i) broom::tidy(lm(dti_TBI_agedummy_y[,i] ~ relevel(dti_TBI_agedummy_y[,"age_cat"], ref = "-1")+ dti_TBI_agedummy_y[,"interview_age"]+ dti_TBI_agedummy_y[,"sex.x"]+ dti_TBI_agedummy_y[,"BMI"] +dti_TBI_agedummy_y[,"site_id_l"]+dti_TBI_agedummy_y[,"ehi_y_ss_scoreb"]+dti_TBI_agedummy_y[,"demo_race_adapted"]+dti_TBI_agedummy_y[,"demo_comb_income_v2"])))
names(MD_lm_agedummy_y) <- names(dti_TBI_agedummy_y[18:30])
MD_agedummy_y <-
  map2_df(MD_lm_agedummy_y,
          names(MD_lm_agedummy_y),
          ~ mutate(.x, which_dependent = .y)) %>%
  select(which_dependent, everything()) %>% filter(str_detect(term,"age_cat"))

MD_lm_agedummy_m <- lapply(18:30, function(i) broom::tidy(lm(dti_TBI_agedummy_m[,i] ~ relevel(dti_TBI_agedummy_m[,"age_cat"], ref = "-1")+ dti_TBI_agedummy_m[,"interview_age"]+ dti_TBI_agedummy_m[,"sex.x"]+ dti_TBI_agedummy_m[,"BMI"] +dti_TBI_agedummy_m[,"site_id_l"]+dti_TBI_agedummy_m[,"ehi_y_ss_scoreb"]+dti_TBI_agedummy_m[,"demo_race_adapted"]+dti_TBI_agedummy_m[,"demo_comb_income_v2"])))
names(MD_lm_agedummy_m) <- names(dti_TBI_agedummy_m[18:30])
MD_agedummy_m <-
  map2_df(MD_lm_agedummy_m,
          names(MD_lm_agedummy_m),
          ~ mutate(.x, which_dependent = .y)) %>%
  select(which_dependent, everything()) %>% filter(str_detect(term,"age_cat"))

MD_lm_agedummy_o <- lapply(18:30, function(i) broom::tidy(lm(dti_TBI_agedummy_o[,i] ~ relevel(dti_TBI_agedummy_o[,"age_cat"], ref = "-1")+ dti_TBI_agedummy_o[,"interview_age"]+ dti_TBI_agedummy_o[,"sex.x"]+ dti_TBI_agedummy_o[,"BMI"] +dti_TBI_agedummy_o[,"site_id_l"]+dti_TBI_agedummy_o[,"ehi_y_ss_scoreb"]+dti_TBI_agedummy_o[,"demo_race_adapted"]+dti_TBI_agedummy_o[,"demo_comb_income_v2"])))
names(MD_lm_agedummy_o) <- names(dti_TBI_agedummy_o[18:30])
MD_agedummy_o <-
  map2_df(MD_lm_agedummy_o,
          names(MD_lm_agedummy_o),
          ~ mutate(.x, which_dependent = .y)) %>%
  select(which_dependent, everything()) %>% filter(str_detect(term,"age_cat"))


MD_agedummy_all <- rbind(MD_agedummy_y, MD_agedummy_m, MD_agedummy_o)
MD_agedummy_all$p.adjusted <- p.adjust(MD_agedummy_all$p.value,method = "fdr")  

#####

#youngest:
age_CB_l_y <- lm(CB_left_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CB left", age="0-3")
partial_f2(lm(CB_left_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CB_r_y <- lm(CB_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CB right", age="0-3")
partial_f2(lm(CB_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_SLFII_l_y <- lm(SLF_II_left_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF II left", age="0-3")
partial_f2(lm(SLF_II_left_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_SLFII_r_y <- lm(SLF_II_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF II right", age="0-3")
partial_f2(lm(SLF_II_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_SLFIII_l_y <- lm(SLF_III_left_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF III left", age="0-3")
partial_f2(lm(SLF_III_left_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_SLFIII_r_y <- lm(SLF_III_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF III right", age="0-3")
partial_f2(lm(SLF_III_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC1_y <- lm(CC1_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 1", age="0-3")
partial_f2(lm(CC1_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC2_y <- lm(CC2_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 2", age="0-3")
partial_f2(lm(CC2_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC3_y <- lm(CC3_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 3", age="0-3")
partial_f2(lm(CC3_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC4_y <- lm(CC4_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 4", age="0-3")
partial_f2(lm(CC4_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC5_y <- lm(CC5_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 5", age="0-3")
partial_f2(lm(CC5_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC6_y <- lm(CC6_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 6", age="0-3")
partial_f2(lm(CC6_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

age_CC7_y <- lm(CC7_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 7", age="0-3")
partial_f2(lm(CC7_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_y))

#middle:
age_CB_l_m <- lm(CB_left_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CB left", age="4-7")
partial_f2(lm(CB_left_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CB_r_m <- lm(CB_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CB right", age="4-7")
partial_f2(lm(CB_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_SLFII_l_m <- lm(SLF_II_left_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF II left", age="4-7")
partial_f2(lm(SLF_II_left_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_SLFII_r_m <- lm(SLF_II_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF II right", age="4-7")
partial_f2(lm(SLF_II_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_SLFIII_l_m <- lm(SLF_III_left_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF III left", age="4-7")
partial_f2(lm(SLF_III_left_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_SLFIII_r_m <- lm(SLF_III_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF III right", age="4-7")
partial_f2(lm(SLF_III_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC1_m <- lm(CC1_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 1", age="4-7")
partial_f2(lm(CC1_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC2_m <- lm(CC2_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 2", age="4-7")
partial_f2(lm(CC2_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC3_m <- lm(CC3_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 3", age="4-7")
partial_f2(lm(CC3_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC4_m <- lm(CC4_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 4", age="4-7")
partial_f2(lm(CC4_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC5_m <- lm(CC5_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 5", age="4-7")
partial_f2(lm(CC5_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC6_m <- lm(CC6_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 6", age="4-7")
partial_f2(lm(CC6_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

age_CC7_m <- lm(CC7_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 7", age="4-7")
partial_f2(lm(CC7_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_m))

#oldest:
age_CB_l_o <- lm(CB_left_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CB left", age="8-10")
partial_f2(lm(CB_left_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CB_r_o <- lm(CB_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CB right", age="8-10")
partial_f2(lm(CB_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_SLFII_l_o <- lm(SLF_II_left_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF II left", age="8-10")
partial_f2(lm(SLF_II_left_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_SLFII_r_o <- lm(SLF_II_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF II right", age="8-10")
partial_f2(lm(SLF_II_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_SLFIII_l_o <- lm(SLF_III_left_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF III left", age="8-10")
partial_f2(lm(SLF_III_left_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_SLFIII_r_o <- lm(SLF_III_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="SLF III right", age="8-10")
partial_f2(lm(SLF_III_right_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC1_o <- lm(CC1_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 1", age="8-10")
partial_f2(lm(CC1_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC2_o <- lm(CC2_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 2", age="8-10")
partial_f2(lm(CC2_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC3_o <- lm(CC3_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 3", age="8-10")
partial_f2(lm(CC3_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC4_o <- lm(CC4_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 4", age="8-10")
partial_f2(lm(CC4_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC5_o <- lm(CC5_Ten1_MD ~ relevel(age_cat, ref = "-1")+ interview_age + sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 5", age="8-10")
partial_f2(lm(CC5_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC6_o <- lm(CC6_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 6", age="8-10")
partial_f2(lm(CC6_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

age_CC7_o <- lm(CC7_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l + BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o)%>% 
  ggcoefstats(output="tidy")%>%filter(str_detect(term,"relevel"))%>% mutate(variable="CC 7", age="8-10")
partial_f2(lm(CC7_Ten1_MD ~ relevel(age_cat, ref = "-1") + interview_age+ sex.x + demo_comb_income_v2 + demo_race_adapted + site_id_l+ BMI +ehi_y_ss_scoreb, data= dti_TBI_agedummy_o))

#all
MD_age_Merged <- do.call("rbind", list(age_CB_l_y, age_CB_l_m,age_CB_l_o,age_CB_r_y,age_CB_r_m,age_CB_r_o, age_SLFII_l_y, age_SLFII_l_m,age_SLFII_l_o,age_SLFII_r_y,age_SLFII_r_m,age_SLFII_r_o, age_SLFIII_l_y,age_SLFIII_l_m,age_SLFIII_l_o, age_SLFIII_r_y,age_SLFIII_r_m,age_SLFIII_r_o, age_CC1_y,age_CC1_m, age_CC1_o, age_CC2_y, age_CC2_m, age_CC2_o, age_CC3_y,age_CC3_m,age_CC3_o, age_CC4_y,age_CC4_m, age_CC4_o, age_CC5_y, age_CC5_m, age_CC5_o, age_CC6_y,age_CC6_m, age_CC6_o, age_CC7_y, age_CC7_m, age_CC7_o)) %>% mutate(p.adjusted=NA)
MD_age_Merged$p.adjusted <- p.adjust(MD_age_Merged$p.value,method = "fdr")  
MD_age_Merged <- MD_age_Merged %>% relocate(any_of(c("variable", "age", "df.error", "statistic", "estimate", "conf.low", "conf.high", "p.adjusted"))) %>% mutate_at(4, round, 2) %>% mutate_at(5, round, 4) %>% mutate_at(6:7, round, 3) %>% mutate_at(8, round, 3) %>% select(c("variable", "age", "df.error", "statistic", "estimate", "conf.low", "conf.high", "p.adjusted"))
names(MD_age_Merged) <- c("Term", "age","Df", "Statistic", "B", "CI_lower", "CI_upper", "p")

MD_age_Merged$B <- format(MD_age_Merged$B, scientific = FALSE)
MD_age_Merged$CI1 <- "["
MD_age_Merged$CI2 <- ";"
MD_age_Merged$CI3 <- "]"
MD_age_Merged<-unite(MD_age_Merged, col='95% CI', c('CI1', 'CI_lower', "CI2", "CI_upper","CI3"), sep='')
