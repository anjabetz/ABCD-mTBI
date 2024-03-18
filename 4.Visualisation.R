################################################################################################
################################################################################################
#Plots
################################################################################################
################################################################################################
cogn_joint$term <- c("TDC", "OI", "TDC", "OI","TDC", "OI","TDC", "OI","TDC", "OI","TDC", "OI")
cogn_joint$domain <- factor(ifelse(cogn_joint$which_dependent == "Sum_BDEFS" | cogn_joint$which_dependent == "NIH_Flanker_2y", cogn_joint$domain <- "Executive Functioning", 
                                   ifelse(cogn_joint$which_dependent == "Sum_UPPS_2y" | cogn_joint$which_dependent == "BISBAS_2y_Fun_Seeking_Sum", cogn_joint$domain <- "Impulsivity", "Emotion Regulation")),
                            levels = c("Executive Functioning", "Impulsivity", "Emotion Regulation"))
cogn_joint$comparison <- paste(cogn_joint$domain,cogn_joint$term)
cogn_joint$comparison <- factor(cogn_joint$comparison, levels = c("Executive Functioning TDC", "Executive Functioning OI", "Impulsivity TDC","Impulsivity OI", "Emotion Regulation TDC", "Emotion Regulation OI"))

comp_cols <- c("darkgoldenrod2","gold", "palegreen4", "palegreen","steelblue3", "steelblue1")

ggplot(cogn_joint, aes(x = estimate, y =factor(which_dependent, level = c('DERS_P_Fac4_Sum', 'DERS_P_Fac1_Sum', 'BISBAS_2y_Fun_Seeking_Sum','Sum_UPPS_2y','NIH_Flanker_2y','Sum_BDEFS')), xmin = estimate-1.96*std.error, xmax = estimate+1.96*std.error, linetype = factor(term, level = c("TDC", "OI")), color=comparison)) +
  theme(text = element_text(size = 25)) +
  labs(linetype='Comparison Group')+ 
  labs(color='Domain')+ 
  scale_linetype_manual(values=c("solid", "dashed"))+
  geom_point(position=position_dodge(width=-0.3), size =3) +
  geom_errorbarh(position = position_dodge(-0.3),height=.2, size =1)+
  scale_color_manual(values=comp_cols) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept = 0, color = "gray")+
  geom_vline(xintercept = -0.25, linetype="dotted", color = "lightgray")+
  geom_vline(xintercept = -0.5, linetype="dotted", color = "lightgray")+
  geom_vline(xintercept = 0.1, linetype="dotted", color = "lightgray")+
  scale_x_continuous(breaks=c(-0.25,-0.5))


#Age

cog_agedummy_all2 <- cog_agedummy_all %>% mutate(Sign = ifelse(p.adjusted < 0.05, "sign", "ns")) %>%   mutate(Sign = factor(Sign))
ggplot(cog_agedummy_all2, aes(x = estimate, y =comparison, xmin = estimate-1.96*std.error, xmax = estimate+1.96*std.error, color=factor(which_dependent, level = c('Sum_BDEFS', 'NIH_Flanker_2y', 'Sum_UPPS_2y','BISBAS_2y_Fun_Seeking_Sum','DERS_P_Fac1_Sum', 'DERS_P_Fac4_Sum')))) +
  theme(text = element_text(size = 20)) +
  labs(linetype='Comparison Group')+ 
  labs(color='Domain')+ 
  geom_point(position=position_dodge(width=-0.3), size =3) +
  geom_errorbarh(position = position_dodge(-0.3),height=.2, size =1)+
  scale_color_manual(values=comp_cols) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept = 0, color = "gray")+
  geom_vline(xintercept = -0.25, linetype="dotted", color = "lightgray")+
  geom_vline(xintercept = -0.5, linetype="dotted", color = "lightgray")+
  geom_vline(xintercept = 0.1, linetype="dotted", color = "lightgray")

