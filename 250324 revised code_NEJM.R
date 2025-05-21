library(dplyr)
library(DescTools)
library(lmtest)
library(forcats)
library(ggplot2)
library(patchwork)
library(emmeans)

setwd("C:\\Users\\pumpk\\OneDrive - University of Pittsburgh\\XiaLab\\PASC\\20241220 data/")
#setwd("~/OneDrive - University of Pittsburgh/XiaLab/PASC/20241220 data/")
DATA=read.csv("pasc_rawintegrated_03022023.csv")
demo=read.csv("demographics.csv")
ms=read.csv("MSRD.csv")
initial=read.csv("covid_initial.csv")
vacnew=read.csv("vaccine.csv")

###### Table1 #######
data=merge(demo, ms,by="id_participant" )
data=merge(data, initial, by="id_participant")
data=merge( data, vacnew[!duplicated(vacnew$id_participant),1:3],by="id_participant")
data=data %>% filter(neuro_type !="Unknown") %>% filter(is.na(covid)==F)
data$id_participant[which(data$covid==1 & is.na(data$covid_wave)==T)]
data=data %>% filter(id_participant !="UPMC251")
data$ms_diagnosis[data$ms_diagnosis=="Unknown"]=NA
data$dmt_efficacy[data$dmt_efficacy=="Unknown"]="None"
data$covid_severity[data$covid_severity=="Unknown"]=NA
data=data%>% mutate(diag_pcr=factor(diag_pcr,levels = c(0,1),labels = c("N","Y")),
                    diag_lab_antigen=factor(diag_lab_antigen,levels = c(0,1),labels = c("N","Y")),
                    diag_home_antigen=factor(diag_home_antigen,levels = c(0,1),labels = c("N","Y")),
                    diag_other=factor(diag_other,levels = c(0,1),labels = c("N","Y")))

surveytime=DATA[,c(2,3)]
data=left_join(data, surveytime, by="id_participant")
data=data %>% mutate(survey_to_covid= as.numeric(as.Date(postacute_sequelae_of_sarscov2_timestamp, format = "%m/%d/%y")-as.Date(covid_date_initial)),
                     survey_to_covid= survey_to_covid/30.25,
                     survey_to_covid= ifelse(survey_to_covid<=1, 1, survey_to_covid ),
                     covid.source=ifelse(diag_pcr=="Y","PCR",ifelse(diag_lab_antigen =="Y"| diag_home_antigen =="Y", "Antigen",ifelse(covid=="No","No COVID","Other"))))

table(DATA$employment_pandemic, useNA = "ifany")
data=left_join(data, DATA[,c("id_participant","employment_pandemic")], by="id_participant")
table(data$employment_pandemic, useNA = "ifany")
data=data %>% mutate(employment_pandemic = ifelse(is.na(employment_pandemic)==T | employment_pandemic %in% c(3,4,5,8,9,10),0,1))
table(data$employment_pandemic, useNA = "ifany")
table(data$covid_number, useNA = "ifany")
data=data %>% mutate(covid_multiple = ifelse(covid_number >=2, "Multiple", ifelse(covid_number==1, "One","Never")))
table(data$covid_multiple, useNA = "ifany")
table(DATA$dmt, useNA = "ifany")
data=left_join(data, DATA[,c("id_participant","dmt")], by="id_participant")
data= data %>% mutate(dmt=ifelse (is.na(dmt)==T | dmt ==99, "None", 
                                  ifelse(dmt %in% c(21,13,16,23),"B cell",
                                         ifelse(dmt %in% c(6,18,20,25),"S1P","Other"))))
table(data$neuro_diagnosis, data$dmt, useNA = "ifany")
table(DATA$pdds_precovid, useNA = "ifany")
table(DATA$pdds_current, useNA = "ifany")
data=left_join(data, DATA[,c("id_participant","pdds_precovid","pdds_current")], by="id_participant")
data.base=data %>% filter(covid_multiple != "Never")
################single symptom ##################
sys= read.csv("allsymptom.csv")
sys= sys %>% mutate (type=ifelse(is.na(type)==T, 0, type),
                     duration=ifelse(duration>=3, 1, 0),
                     symptom=ifelse(symptom=="menstrual changes","Menstrual changes",symptom),
                     symptom=ifelse(symptom=="New allergies","Allergies",symptom),
                     symptom=ifelse(symptom=="New anaphylaxis","Anaphylaxis",symptom),
                     symptom= ifelse(symptom == "Headaches","Sexual capacity", 
                    ifelse(symptom == "Sore throat","Thirsty",symptom)),
                    symptom= ifelse( symptom == "breathing_difficulty","Breathing difficulty", symptom),
                    symptom= ifelse( symptom == "chills","Chills", symptom),
                    symptom= ifelse( symptom == "elevated_temp","Elevated temp", symptom),
                    symptom= ifelse( symptom == "exercise_intolerance","Exercise intolerance", symptom),
                    symptom= ifelse( symptom == "fatigue","Fatigue", symptom),
                    symptom= ifelse( symptom == "fever","Fever", symptom),
                    symptom= ifelse( symptom == "low_temp","Low temp", symptom),
                    symptom= ifelse( symptom == "require supplemental oxygen","Require supplemental oxygen", symptom),
                    symptom= ifelse( symptom == "sneezing","Sneezing", symptom),
                    symptom= ifelse( symptom == "wheezing","Wheezing", symptom) )
data.base2=data.base %>%
  mutate(neuro_type=factor(neuro_type, levels=c("Control","MSRD")),
         sex=ifelse(sex=="Other", "F", sex),
         sex=factor(sex, levels=c("M","F")),
         race_eth=factor(race_eth, levels=c("Non-Hispanic White","Other")),
         CCI_cata= ifelse(CCI_cata  %in% c("Moderate", "Severe"),"Mod/Severe", CCI_cata),
         CCI_cata = factor(CCI_cata, levels=c("None","Mild","Mod/Severe")),
         
         employment_pandemic=factor(employment_pandemic, levels=c("1","0")),
         covid.source=factor(covid.source,levels=c("Antigen","PCR","Other")),
         covid_severity=ifelse(covid_severity %in% c("Severe","Very Severe"),"Severe/Critical",covid_severity),
         covid_severity=factor(covid_severity, levels=c("Mild","No symptoms","Moderate","Severe/Critical")),
         vaccine_doses=ifelse(is.na(vaccine_doses)==T | vaccine_doses<3,"Not fully","Fully"),
         vaccine_doses=factor(vaccine_doses, levels=c("Fully","Not fully")),
         covid_wave= as.factor(covid_wave),
         covid_multiple = factor(covid_multiple, levels = c("One","Multiple")))


a=names(table(sys$symptom))
######new symptom######
result_new=data.frame(symptom = a, n_msrd = NA, n_crtl= NA, OR= NA, low=NA, high= NA, p=NA)
for (j in 21: length(a)){
  tmp=sys %>% filter(symptom == a[j]) %>% select(id_participant,symptom, type, duration)%>% 
    mutate(type_new=ifelse(type==1 & duration ==1,1,0),  type_worse=ifelse(type==2 & duration==1,1,0) )
  tmp=merge(tmp, data.base2, by="id_participant")
  
  result_new$n_msrd[j]=table(tmp$type_new, tmp$neuro_type)["1","MSRD"]
  result_new$n_crtl[j]=table(tmp$type_new, tmp$neuro_type)["1","Control"]
  m_new=glm( type_new ~neuro_type+age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+survey_to_covid+
               covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple, family=binomial(),data=tmp)
  
  result_new[j,4:6] =  exp(cbind(coef(m_new)[2], confint(m_new)[2,1],confint(m_new)[2,2]))  
  result_new[j,7] =summary(m_new)$coef[2,4]
}

levels_sys=unique(sys$symptom)
levels_organ= c("Systemic","Respiratory","Gastrointestinal","Reproductive / Genitourinary",
                        "Cardiovascular","Dermatology","Musculoskeletal","Immunology","HEENT","Neuropsychiatry: Cognition",
                        "Neuropsychiatry: Sensorimotor","Neuropsychiatry: Other")
  
result_new2= result_new %>% 
  mutate(n_all=n_msrd+n_crtl,
         p_all=(n_all/1227)*100,
         p_msrd=(n_msrd/613)*100,
         p_crtl=(n_crtl/614)*100)%>%
  mutate(symptom=factor(symptom, levels=levels_sys, labels=levels_sys)) %>% 
  arrange(symptom) %>% 
  mutate(`Organ system`=c(rep("Systemic",6),
                          rep("Respiratory",8),
                          rep("Gastrointestinal",7),
                          rep("Reproductive / Genitourinary",2),
                          rep("Cardiovascular",5),
                          rep("Dermatology",8),
                          rep("Musculoskeletal",5),
                          rep("Immunology",2),
                          rep("HEENT",8),
                          rep("Neuropsychiatry: Cognition",7),
                          rep("Neuropsychiatry: Sensorimotor",6),
                          rep("Neuropsychiatry: Other",7)),
         `Organ system`= factor(`Organ system`, levels= levels_organ, labels=levels_organ ) )%>%
  
  select(`Organ system`,symptom,n_all, p_all, n_msrd, p_msrd,n_crtl, p_crtl, OR, low, high, p)%>%
  arrange(`Organ system`, symptom) 

result_new2[10,3:8]=0
summary(result_new2$OR)
result_new2[which(is.na(result_new2$OR)==T | result_new2$OR>15),]
result_new2[5,9:12 ]=c(1,1,1,NA)
result_new2[10,9:12 ]=c(1,1,1,NA)
result_new2[43,9:12 ]=c(1,1,1,NA)
result_new2[67,9:12 ]=c(1,1,1,NA)
summary(result_new2$low)
summary(result_new2$high)
result_new2[which(is.na(result_new2$high)==T | result_new2$high>15),]

result_new2= result_new2 %>% mutate(
         high2 = ifelse(high > 15, 15, high),
         p_sig=ifelse(is.na(p)==T | p> 0.05/71, "","*"),
         est= paste0( round(OR, 2), "(",round(low,2),"-", round(high,2),")",p_sig))

#write.csv(result_new2, "../2024 Final/Annals of Neuroloogy/Revision_NEJM/or_new.csv", row.names = F)
#result_new2=read.csv( "../2024 Final/Annals of Neuroloogy/Revision_NEJM/or_new.csv")
ggplot(result_new2, aes(y=fct_rev(symptom), x= p_all, fill=`Organ system`))+
  geom_bar(stat="identity")+
  labs(y="",x="% of new symptoms in all participants")+
  geom_text(aes(x=10, label = sprintf("%.1f%%", round(p_all,2))), 
            size = 2.8, hjust = 0) +  # Adjust position and size
  theme_light()+
  theme(#plot.title = element_text(hjust = 0, size = 9),
        axis.text.y = element_text( size = 8),
        axis.text.x = element_text( size = 8),
        axis.title=element_text( size = 9),
        legend.position = "none")
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM/s1A_new sysmptom_overall.png", dpi = 300, width = 12, height = 8, units = "in")
ggplot(result_new2, aes(y=fct_rev(symptom), x= p_msrd, fill=`Organ system`))+
  geom_bar(stat="identity")+
  labs(y="",x="% of new symptoms in pwMSRD")+
  geom_text(aes(x=15, label = sprintf("%.1f%%", round(p_msrd,2))), 
            hjust = 0, size = 2.8) +  # Adjust position and size
  theme_light()+
  theme(#plot.title = element_text(hjust = 0, size = 9),
    axis.text.y = element_text( size = 8),
    axis.text.x = element_text( size = 8),
    axis.title=element_text( size = 9),
    legend.position = "none")
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM//s1B_new sysmptom_pwMSRD.png", dpi = 300, width = 12, height = 8, units = "in")
  
ggplot(result_new2, aes(y=fct_rev(symptom), x= p_crtl, fill=`Organ system`))+
  geom_bar(stat="identity")+
  labs(y="",x="% of new symptoms in controls")+
  geom_text(aes(x=10, label = sprintf("%.1f%%", round(p_crtl,2))), 
            hjust = 0, size = 3) +  # Adjust position and size
  theme_light()+
  theme(#plot.title = element_text(hjust = 0, size = 9),
    axis.text.y = element_text( size = 8),
    axis.text.x = element_text( size = 8),
    axis.title=element_text( size = 9),
    legend.position = "none")
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM//s1C_new sysmptom_crtl.png", dpi = 300, width = 12, height = 8, units = "in")

ggplot(data=result_new2, aes(y=fct_rev(symptom),x=OR,fill=`Organ system`))+
  geom_errorbarh(aes(xmin=low, xmax=high2),height=0.2, size=1,col="gray") +
  geom_point(aes(col=`Organ system`),size=1)+
  geom_vline(xintercept=1, linetype='longdash') +
  xlim(0, 18)+
  labs(y="",x="aOR of new symptoms between pwMSRD and controls")+
  geom_text(aes(x=15, label = est), 
            hjust = 0, size = 2.8,
            color = ifelse(result_new2$p_sig =="*", "red", "black") )+
  theme_light()+
  theme(
    axis.text.y = element_text(colour = rev(ifelse(result_new2$p_sig=="*", "red","black")), size=8),
    axis.text.x = element_text( size = 8),
    axis.title=element_text( size = 9),
    legend.position = "none")
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM//s1D_new sysmptom_OR.png", dpi = 300, width = 12, height = 8, units = "in")

ggplot(result_new2, aes(y=fct_rev(symptom), x= p_crtl, fill=`Organ system`))+
  geom_bar(stat="identity")+
  labs(y="",x="% of new symptoms in controls")+
  geom_text(aes(x=20, label = sprintf("%.1f%%", round(p_crtl,2))), 
            hjust = 0, size = 3) +  # Adjust position and size
  theme(#plot.title = element_text(hjust = 0, size = 9),
    axis.text.y = element_text( size = 8),
    axis.text.x = element_text( size = 8),
    axis.title=element_text( size = 9),
    legend.position = "bottom")
ggsave("../2024 Final/Annals of Neuroloogy/Revision/s1E_legends.png", dpi = 300, width = 12, height = 8, units = "in")

###### worse symptom ###########
result_worse=data.frame(symptom = a, n_msrd = NA, n_crtl= NA, OR= NA, low=NA, high= NA, p=NA)
for (j in 33: length(a)){
  tmp=sys %>% filter(symptom == a[j]) %>% select(id_participant,symptom, type,duration)%>% 
    mutate(type_new=ifelse(type==1 & duration ==1,1,0),  type_worse=ifelse(type==2 & duration ==1,1,0) )
  tmp=merge(tmp, data.base2, by="id_participant")
  
  result_worse$n_msrd[j]=table(tmp$type_worse, tmp$neuro_type)["1","MSRD"]
  result_worse$n_crtl[j]=table(tmp$type_worse, tmp$neuro_type)["1","Control"]
  m_worse=glm( type_worse ~neuro_type+age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+survey_to_covid+
               covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple, family=binomial(),data=tmp)
  
  result_worse[j,4:6] =  exp(cbind(coef(m_worse)[2], confint(m_worse)[2,1],confint(m_worse)[2,2]))  
  result_worse[j,7] =summary(m_worse)$coef[2,4]
}


result_worse2= result_worse %>% 
  mutate(n_all=n_msrd+n_crtl,
         p_all=(n_all/1227)*100,
         p_msrd=(n_msrd/613)*100,
         p_crtl=(n_crtl/614)*100)%>%
  mutate(symptom=factor(symptom, levels=levels_sys, labels=levels_sys)) %>% 
  arrange(symptom) %>% 
  mutate(`Organ system`=c(rep("Systemic",6),
                          rep("Respiratory",8),
                          rep("Gastrointestinal",7),
                          rep("Reproductive / Genitourinary",2),
                          rep("Cardiovascular",5),
                          rep("Dermatology",8),
                          rep("Musculoskeletal",5),
                          rep("Immunology",2),
                          rep("HEENT",8),
                          rep("Neuropsychiatry: Cognition",7),
                          rep("Neuropsychiatry: Sensorimotor",6),
                          rep("Neuropsychiatry: Other",7)),
         `Organ system`= factor(`Organ system`, levels= levels_organ, labels=levels_organ ) )%>%
  
  select(`Organ system`,symptom,n_all, p_all, n_msrd, p_msrd,n_crtl, p_crtl, OR, low, high, p)%>%
  arrange(`Organ system`, symptom) 
result_worse2[5,3:8]=0
result_worse2[10,3:8]=0
result_worse2[43,3:8]=0
summary(result_worse2$OR)
result_worse2[which(result_worse2$OR==0 | result_worse2$OR > 20 | is.na(result_worse2$OR)==T),]



result_worse2$OR[which(result_worse2$OR==0 | result_worse2$OR > 20 | is.na(result_worse2$OR)==T) ]=1
result_worse2$low[which(result_worse2$OR==0 | result_worse2$OR > 20 | is.na(result_worse2$OR)==T) ]=1
result_worse2$high[which(result_worse2$OR==0 | result_worse2$OR > 20 | is.na(result_worse2$OR)==T) ]=1
result_worse2$p[which(result_worse2$OR==0 | result_worse2$OR > 20 | is.na(result_worse2$OR)==T) ]=NA



summary(result_worse2$low)
result_worse2[which(result_worse2$low==0 |is.na(result_worse2$low)==T),]
result_worse2$low[which(result_worse2$low==0 |is.na(result_worse2$low)==T)]=1
result_worse2$high[which(result_worse2$low==0 |is.na(result_worse2$low)==T)]=1
result_worse2$p[which(result_worse2$low==0 |is.na(result_worse2$low)==T)]=1

summary(result_worse2$high)
result_worse2[which(is.na(result_worse2$high)==T | result_worse2$high>20 | is.finite(result_worse2$high)==F),]
result_worse2$low[which(result_worse2$OR==1)]=1
result_worse2$high[which(result_worse2$OR==1)]=1
result_worse2$low[which(result_worse2$OR==1)]=1
result_worse2$p[which(result_worse2$OR==1)]=NA





result_worse2= result_worse2 %>% mutate(
  high2 = ifelse(high > 25, 25, high),
  p_sig=ifelse(is.na(p)==T | p> 0.05/71, "","*"),
  est= paste0( round(OR, 2), "(",round(low,2),"-", round(high,2),")",p_sig))

#write.csv(result_new2, "../2024 Final/Annals of Neuroloogy/Revision_NEJM/or_new.csv", row.names = F)
#result_new2=read.csv( "../2024 Final/Annals of Neuroloogy/Revision_NEJM/or_new.csv")
ggplot(result_worse2, aes(y=fct_rev(symptom), x= p_all, fill=`Organ system`))+
  geom_bar(stat="identity")+
  labs(y="",x="% of worsening symptoms in all participants")+
  geom_text(aes(x=18, label = sprintf("%.1f%%", round(p_all,2))), 
            size = 2.8, hjust = 0) +  # Adjust position and size
  theme_light()+
  theme(#plot.title = element_text(hjust = 0, size = 9),
    axis.text.y = element_text( size = 8),
    axis.text.x = element_text( size = 8),
    axis.title=element_text( size = 9),
    legend.position = "none")
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM//s2A_worse sysmptom_overall.png", dpi = 300, width = 12, height = 8, units = "in")
ggplot(result_worse2, aes(y=fct_rev(symptom), x= p_msrd, fill=`Organ system`))+
  geom_bar(stat="identity")+
  labs(y="",x="% of worsening symptoms in pwMSRD")+
  geom_text(aes(x=30, label = sprintf("%.1f%%", round(p_msrd,2))), 
            hjust = 0, size = 2.8) +  # Adjust position and size
  theme_light()+
  theme(#plot.title = element_text(hjust = 0, size = 9),
    axis.text.y = element_text( size = 8),
    axis.text.x = element_text( size = 8),
    axis.title=element_text( size = 9),
    legend.position = "none")
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM/s2B_worse sysmptom_pwMSRD.png", dpi = 300, width = 12, height = 8, units = "in")

ggplot(result_worse2, aes(y=fct_rev(symptom), x= p_crtl, fill=`Organ system`))+
  geom_bar(stat="identity")+
  labs(y="",x="% of worsening symptoms in controls")+
  geom_text(aes(x=7.5, label = sprintf("%.1f%%", round(p_crtl,2))), 
            hjust = 0, size = 3) +  # Adjust position and size
  theme_light()+
  theme(#plot.title = element_text(hjust = 0, size = 9),
    axis.text.y = element_text( size = 8),
    axis.text.x = element_text( size = 8),
    axis.title=element_text( size = 9),
    legend.position = "none")
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM//s2C_worse sysmptom_crtl.png", dpi = 300, width = 12, height = 8, units = "in")

ggplot(data=result_worse2, aes(y=fct_rev(symptom),x=OR,fill=`Organ system`))+
  geom_errorbarh(aes(xmin=low, xmax=high2),height=0.2, size=1,col="gray") +
  geom_point(aes(col=`Organ system`),size=1)+
  geom_vline(xintercept=1, linetype='longdash') +
  xlim(0, 27)+
  labs(y="",x="aOR of worsening symptoms between pwMSRD and controls")+
  geom_text(aes(x=25, label = est), 
            hjust = 0, size = 2.8,
            color = ifelse(result_worse2$p_sig =="*", "red", "black") )+
  theme_light()+
  theme(
    axis.text.y = element_text(colour = rev(ifelse(result_worse2$p_sig=="*", "red","black")), size=8),
    axis.text.x = element_text( size = 8),
    axis.title=element_text( size = 9),
    legend.position = "none")
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM//s2D_worse sysmptom_OR.png", dpi = 300, width = 12, height = 8, units = "in")









############## NEJM definition of PASC ##################
sys2=sys %>% mutate(pasc= ifelse(duration==1 & type !=0, 1, 0),
                    pasc_new= ifelse(duration==1 & type ==1, 1, 0),
                    pasc_worse= ifelse(duration==1 & type ==2, 1, 0))
sys2= sys2 %>% group_by(id_participant) %>% 
  summarise(pasc = ifelse(sum(pasc)>0,1,0),
         pasc_new = ifelse(sum(pasc_new)>0,1,0),
         pasc_worse = ifelse(sum(pasc_worse)>0,1,0))
data2=merge(sys2[,c("id_participant","pasc","pasc_new","pasc_worse")], data.base, by="id_participant")

table(data2$neuro_type, data2$pasc,useNA = "ifany"); chisq.test(table(data2$neuro_type, data2$pasc,useNA = "ifany"))
table(data2$neuro_type, data2$pasc_new,useNA = "ifany")
table(data2$neuro_type, data2$pasc_worse,useNA = "ifany")

tmp = data2%>% group_by(neuro_type) %>% summarise(n = n(), pos=sum(pasc==1), p=round(pos/n*100,1))
p1=ggplot(tmp, aes(x=neuro_type, y= p, fill = neuro_type))+
  geom_bar(stat="identity")+
  labs(y="",x="")+
  ylim(0,55)+
  ggtitle("A. Prevalence of NASEM-defined long COVID \n(Overall)")+
  geom_text(aes(x=neuro_type, label =  sprintf("%.1f%%", p), y= p), 
            hjust = 0.5, size = 4, vjust=-0.5) +  # Adjust position and size
  theme_light()+
  theme(#plot.title = element_text(hjust = 0, size = 9),
    legend.position = "none",
    axis.text.y = element_text( size = 8),
    axis.text.x = element_text( size = 9, face = "bold"),
    title=element_text( size = 8, vjust = 0))+
   scale_fill_brewer(palette="Blues")
tmp = data2%>% group_by(neuro_type) %>% summarise(n = n(), pos=sum(pasc_new==1), p=round(pos/n*100,1))
p2=ggplot(tmp, aes(x=neuro_type, y= p, fill=neuro_type))+
  geom_bar(stat="identity")+
  labs(y="",x="")+
  ylim(0,55)+
  ggtitle("B. Prevalence of NASEM-defined long COVID \n(New symptoms)")+
  geom_text(aes(x=neuro_type, label =  sprintf("%.1f%%", p), y= p), 
            hjust = 0.5, size = 4, vjust=-0.5) +  # Adjust position and size
  theme_light()+
  theme(#plot.title = element_text(hjust = 0, size = 9),
    legend.position = "none",
    axis.text.y = element_text( size = 8),
    axis.text.x = element_text( size = 9, face = "bold"),
    title=element_text( size = 8, vjust = 0))+
  scale_fill_brewer(palette="Blues")
tmp = data2%>% group_by(neuro_type) %>% summarise(n = n(), pos=sum(pasc_worse==1), p=round(pos/n*100,1))
p3=ggplot(tmp, aes(x=neuro_type, y= p, fill=neuro_type))+
  geom_bar(stat="identity")+
  labs(y="",x="")+
  ylim(0,55)+
  ggtitle("C. Prevalence of NASEM-defined long COVID \n(Worsening symptoms)")+
  geom_text(aes(x=neuro_type, label =  sprintf("%.1f%%", p), y= p), 
            hjust = 0.5, size = 4, vjust=-0.5) +  # Adjust position and size
  theme_light()+
  theme(#plot.title = element_text(hjust = 0, size = 9),
    legend.position = "none",
    axis.text.y = element_text( size = 8),
    axis.text.x = element_text( size = 9, face = "bold"),
    title=element_text( size = 8, vjust = 0))+
  scale_fill_brewer(palette="Blues")
p1+p2+p3
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM//Fig2A_PASC_Prev.png", dpi = 300, width = 12, height = 8, units = "in")


# age, sex, race and ethnicity, BMI, and CCI category, employment status before March 1, 2020, 
#COVID-19 detection methods, and initial infection severity, vaccine, covid wave, multiple infections, 
# and time from infection to survey completion, 
data2=data2 %>%
  mutate(neuro_type=factor(neuro_type, levels=c("Control","MSRD")),
         sex=ifelse(sex=="Other", "F", sex),
         sex=factor(sex, levels=c("M","F")),
         race_eth=factor(race_eth, levels=c("Non-Hispanic White","Other")),
         CCI_cata= ifelse(CCI_cata  %in% c("Moderate", "Severe"),"Mod/Severe", CCI_cata),
         CCI_cata = factor(CCI_cata, levels=c("None","Mild","Mod/Severe")),
         
         employment_pandemic=factor(employment_pandemic, levels=c("1","0")),
         covid.source=factor(covid.source,levels=c("Antigen","PCR","Other")),
         covid_severity=ifelse(covid_severity %in% c("Moderate", "Severe","Very Severe"),"Mod/Severe/Critical",covid_severity),
         covid_severity=factor(covid_severity, levels=c("No symptoms","Mild","Mod/Severe/Critical")),
         vaccine_doses=ifelse(is.na(vaccine_doses)==T | vaccine_doses<3,"Not fully","Fully"),
         vaccine_doses=factor(vaccine_doses, levels=c("Fully","Not fully")),
         covid_wave= as.factor(covid_wave),
         covid_multiple = factor(covid_multiple, levels = c("One","Multiple")))

m=glm( pasc ~ neuro_type, family=binomial(),data=data2); summary(m)
m=glm( pasc ~neuro_type+age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+survey_to_covid+
          covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple, family=binomial(),data=data2)
OR= exp(coef(m))
low=   exp( confint(m)[,1])
high = exp(confint(m)[,2])
p=summary(m)$coef[,4]
var=c("Intercept","pwMSRD \n[ref = Controls]",
      "Age at survey completion (Years)",
      "Female \n[ref = Male]",
      "Race/ethnicity: Other \n[ref = Non-Hispanic White]",
      "BMI (kg/m2)",
      "Comorbidity: Mild \n[ref = No comorbidity]",
      "Comorbidity: Moderate to severe \n[ref = No comorbidity]",
      "Pre-pandemic unemployed \n[ref = Employed]",
      "Time from survey completion \nto the first infection (months)",
      "COVID-19 diagnosis source: PCR \n[ref = Antigen]",
      "COVID-19 diagnosis source: Other \n[ref = Antigen]",
      "Time of initial infection: pre-Omicron \n[ref = Omicron era]",
      "Acute infection severity: Mild \n[ref = Asymptomatic]",
      "Acute infection severity: Moderate to critical \n[ref = Asymptomatic]",
      "Not fully vaccinated \n[ref = Fully vaccinated]",
      "Multiple infections \n[ref = Single infection]")
result=data.frame( var=var, OR= OR, low=low, high = high, pval=p )[-1,]
#result[14,2:5]= c(3.07, 1.53, 6.15, 0.002)
#result[15,2:4]= c(3.98, 1.64, 9.59)
result = result %>% mutate(var=factor(var, levels=var),
                           stimate_lab = paste0(round(OR,2),"(", round(low,2),"-",round(high, 2),")"),
                           bold=ifelse(pval< 0.05, 1 , 0),
                           pval=ifelse(pval< 0.001, "<0.001", round(as.numeric(pval),3)))
                           
                      
                           
p <- 
  result %>% 
  ggplot(aes(y = fct_rev(var))) + 
  xlim(c(0.53, 7))+
  theme_classic() +
  geom_point(aes(x=OR), shape=15, size=1.5) +
  
  geom_linerange(aes(xmin=low, xmax=high), linewidth = 1)  +
  labs(title = "NASEM-defined long COVID (Overall)") +
  geom_vline(xintercept = 1, linetype="dashed") +
  labs(x="aOR", y="")+
  theme(
    axis.text.y = element_text( size=12),
    axis.text.x = element_text( size = 8,face = "bold"),
    title =element_text( size = 10, face="bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Centered title
   
  )
p_mid <- 
  result %>% 
  ggplot(aes(y = fct_rev(var))) + 
  geom_text(aes(x = 10, label = stimate_lab), hjust = 0, 
            fontface = ifelse(result$bold == 1, "bold", "plain"))+
  theme_void() 
  
p_right <-
  result  |>
  ggplot() +
  geom_text(
    aes(x = 0, y = fct_rev(var), label = pval ),
    hjust = 0,
    fontface = ifelse(result$bold == 1, "bold", "plain")) +
  theme_void() 

layout <- c(
  area(t = 0, l = 0, b = 30, r = 10), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 1, l = 0, b = 30, r = 20), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  area(t = 0, l = 10, b = 30, r = 18) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)

# final plot arrangement
p1=p + p_mid + p_right + plot_layout(design = layout)
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM//Fig2B_PASC_Overall.png", dpi = 300, width = 12, height = 8, units = "in")


m=glm( pasc_new ~neuro_type+age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+survey_to_covid+
          covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple, family=binomial(),data=data2)
summary(m)### in pwMS, what factors is associated with  having new or worsening symptoms##
OR= exp(coef(m))
low=   exp( confint(m)[,1])
high = exp(confint(m)[,2])
p=summary(m)$coef[,4]
var=c("Intercept","pwMSRD \n[ref = Controls]",
      "Age at survey completion (Years)",
      "Female \n[ref = Male]",
      "Race/ethnicity: Other \n[ref = Non-Hispanic White]",
      "BMI (kg/m2)",
      "Comorbidity: Mild \n[ref = No comorbidity]",
      "Comorbidity: Moderate to severe \n[ref = No comorbidity]",
      "Pre-pandemic unemployed \n[ref = Employed]",
      "Time from survey completion \nto the first infection (months)",
      "COVID-19 diagnosis source: PCR \n[ref = Antigen]",
      "COVID-19 diagnosis source: Other \n[ref = Antigen]",
      "Time of initial infection: pre-Omicron \n[ref = Omicron era]",
      "Acute infection severity: Mild \n[ref = Asymptomatic]",
      "Acute infection severity: Moderate to critical \n[ref = Asymptomatic]",
      "Not fully vaccinated \n[ref = Fully vaccinated]",
      "Multiple infections \n[ref = Single infection]")
result=data.frame( var=var, OR= OR, low=low, high = high, pval=p )[-1,]

#result[1,2:5]= c(1.42, 0.98, 2.06, 0.06617835)
#result[14,2:5]= c(5.81, 3.12, 10.82, 2.819959e-08)

result = result %>% mutate(var=factor(var, levels=var),
                           stimate_lab = paste0(round(OR,2),"(", round(low,2),"-",round(high, 2),")"),
                           bold=ifelse(pval< 0.05, 1 , 0),
                           pval=ifelse(pval< 0.001, "<0.001", round(as.numeric(pval),3)))


p <- 
  result %>% 
  ggplot(aes(y = fct_rev(var))) + 
  xlim(c(0.40, 5))+
  theme_classic() +
  geom_point(aes(x=OR), shape=15, size=1.5) +
  
  geom_linerange(aes(xmin=low, xmax=high), linewidth = 1)  +
  labs(title = "NASEM-defined long COVID (New symptoms)") +
  geom_vline(xintercept = 1, linetype="dashed") +
  labs(x="aOR", y="")+
  theme(
    axis.text.y = element_text( size=12),
    axis.text.x = element_text( size = 8,face = "bold"),
    title =element_text( size = 10, face="bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Centered title
    
  )
p_mid <- 
  result %>% 
  ggplot(aes(y = fct_rev(var))) + 
  geom_text(aes(x = 10, label = stimate_lab), hjust = 0, 
            fontface = ifelse(result$bold == 1, "bold", "plain"))+
  theme_void() 

p_right <-
  result  |>
  ggplot() +
  geom_text(
    aes(x = 0, y = fct_rev(var), label = pval ),
    hjust = 0,
    fontface = ifelse(result$bold == 1, "bold", "plain")) +
  theme_void() 

layout <- c(
  area(t = 0, l = 0, b = 30, r = 10), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 1, l = 0, b = 30, r = 20), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  area(t = 0, l = 10, b = 30, r = 18) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)
# final plot arrangement
p2=p + p_mid + p_right + plot_layout(design = layout)
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM//Fig2B_PASC_New.png", dpi = 300, width = 12, height = 8, units = "in")





m=glm( pasc_worse ~neuro_type+age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+survey_to_covid+
         covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple, family=binomial(),data=data2)

summary(m)
OR= exp(coef(m))
low=   exp( confint(m)[,1])
high = exp(confint(m)[,2])
p=summary(m)$coef[,4]
var=c("Intercept","pwMSRD \n[ref = Controls]",
      "Age at survey completion (Years)",
      "Female \n[ref = Male]",
      "Race/ethnicity: Other \n[ref = Non-Hispanic White]",
      "BMI (kg/m2)",
      "Comorbidity: Mild \n[ref = No comorbidity]",
      "Comorbidity: Moderate to severe \n[ref = No comorbidity]",
      "Pre-pandemic unemployed \n[ref = Employed]",
      "Time from survey completion \nto the first infection (months)",
      "COVID-19 diagnosis source: PCR \n[ref = Antigen]",
      "COVID-19 diagnosis source: Other \n[ref = Antigen]",
      "Time of initial infection: pre-Omicron \n[ref = Omicron era]",
      "Acute infection severity: Mild \n[ref = Asymptomatic]",
      "Acute infection severity: Moderate to critical \n[ref = Asymptomatic]",
      "Not fully vaccinated \n[ref = Fully vaccinated]",
      "Multiple infections \n[ref = Single infection]")
result=data.frame( var=var, OR= OR, low=low, high = high, pval=p )[-1,]
#result[13,2:5]= c(1.63, 0.57, 4.70, 0.36)
#result[14,2:5]= c(6.69, 3.43, 13.02, 2.293887e-08)

result = result %>% mutate(var=factor(var, levels=var),
                           stimate_lab = paste0(round(OR,2),"(", round(low,2),"-",round(high, 2),")"),
                           bold=ifelse(pval< 0.05, 1 , 0),
                           pval=ifelse(pval< 0.001, "<0.001", round(as.numeric(pval),3)))


p <- 
  result %>% 
  ggplot(aes(y = fct_rev(var))) + 
  xlim(c(0.6, 13))+
  theme_classic() +
  geom_point(aes(x=OR), shape=15, size=1.5) +
  
  geom_linerange(aes(xmin=low, xmax=high), linewidth = 1)  +
  labs(title = "NASEM-defined long COVID (Worsening symptoms)") +
  geom_vline(xintercept = 1, linetype="dashed") +
  labs(x="aOR", y="")+
  theme(
    axis.text.y = element_text( size=12),
    axis.text.x = element_text( size = 8,face = "bold"),
    title =element_text( size = 10, face="bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Centered title
    
  )
p_mid <- 
  result %>% 
  ggplot(aes(y = fct_rev(var))) + 
  geom_text(aes(x = 10, label = stimate_lab), hjust = 0, 
            fontface = ifelse(result$bold == 1, "bold", "plain"))+
  theme_void() 

p_right <-
  result  |>
  ggplot() +
  geom_text(
    aes(x = 0, y = fct_rev(var), label = pval ),
    hjust = 0,
    fontface = ifelse(result$bold == 1, "bold", "plain")) +
  theme_void() 

layout <- c(
  area(t = 0, l = 0, b = 30, r = 10), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 1, l = 0, b = 30, r = 20), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  area(t = 0, l = 10, b = 30, r = 18) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)
# final plot arrangement
p3=p + p_mid + p_right + plot_layout(design = layout)
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM//Fig2B_PASC_worsening.png", dpi = 300, width = 12, height = 8, units = "in")
p1/
  p2/
  p3
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM//Fig2B_ALL.png", dpi = 300, width = 12, height = 20, units = "in")

############## new/worsening PASC analysis in MS ##################
data.ms=data2 %>% filter(neuro_diagnosis == "MS" & is.na(ms_diagnosis)==F & is.na(pdds_precovid)==F)   # 561 pwMS who had COVID, and have pre-covid PDDS, MS subtype and DMT data
data.ms= data.ms %>% mutate(ms_diagnosis=ifelse(ms_diagnosis %in% c("PPMS","SPMS"),"PMS","RRMS"),
                            ms_diagnosis=factor(ms_diagnosis, levels=c("RRMS","PMS")),
                           dmt=factor(dmt, levels=c("None","B cell","S1P","Other")) )
table(data.ms$ms_diagnosis, useNA="ifany")
table(data.ms$dmt, useNA="ifany")
m_full=glm( pasc ~age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+ms_diagnosis+dmt+pdds_precovid+
          survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple, family=binomial(),data=data.ms)
summary(m_full)
var=c("Intercept",
      "Age at survey completion (Years)",
      "Female [ref = Male]",
      "Race/ethnicity: Other [ref = Non-Hispanic White]",
      "BMI (kg/m2)",
      "Comorbidity: Mild [ref = No comorbidity]",
      "Comorbidity: Moderate to severe [ref = No comorbidity]",
      "Pre-pandemic unemployed [ref = Employed]",
      "MS subtype: Progressive MS [ref = RRMS]",
      "DMT use: Anti-CD20s [ref = None] ",
      "DMT use: S1P modulators [ref = None]",
      "DMT use: Other [ref = None]",
      "Baseline PDDS scale",
      "Time from survey completion to the first infection (months)",
      "COVID-19 diagnosis source: PCR [ref = Antigen]",
      "COVID-19 diagnosis source: Other [ref = Antigen]",
      "Time of initial infection: pre-Omicron [ref = Omicron era]",
      "Acute infection severity: Mild [ref = Asymptomatic]",
      "Acute infection severity: Moderate to critical[ref = Asymptomatic]",
      "Not fully vaccinated [ref = Fully vaccinated]",
      "Multiple infections [ref = Single infection]")
OR= exp(coef(m_full))
low=   exp( confint(m_full)[,1])
high = exp(confint(m_full)[,2])
p=summary(m_full)$coef[,4]
est=paste0(round(OR,2),"(", round(low,2),"-",round(high, 2),")")

result=data.frame( var=var, est=est, pval=NA )[-1,]
result$pval[c(1,2,3,4,5,7,8,9,12, 13, 14, 16,17, 19, 20 )]=
  c(lrtest(m_full, . ~ . - age_survey)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - sex)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - race_eth)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - bmi)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - CCI_cata)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - employment_pandemic)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - ms_diagnosis)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - dmt)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - pdds_precovid)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - survey_to_covid)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - covid.source)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - covid_wave)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - covid_severity)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - vaccine_doses)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - covid_multiple)$`Pr(>Chisq)`[2])

result = result %>% mutate(pval=ifelse(pval< 0.001, "<0.001", round(as.numeric(pval),3)))
result_overall=result


m_full=glm( pasc_new ~age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+ms_diagnosis+dmt+pdds_precovid+
              survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple, family=binomial(),data=data.ms)
summary(m_full)
OR= exp(coef(m_full))
low=   exp( confint(m_full)[,1])
high = exp(confint(m_full)[,2])
p=summary(m_full)$coef[,4]
est=paste0(round(OR,2),"(", round(low,2),"-",round(high, 2),")")

result=data.frame( var=var, est=est, pval=NA )[-1,]
result$pval[c(1,2,3,4,5,7,8,9,12, 13, 14, 16,17, 19, 20 )]=
  c(lrtest(m_full, . ~ . - age_survey)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - sex)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - race_eth)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - bmi)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - CCI_cata)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - employment_pandemic)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - ms_diagnosis)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - dmt)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - pdds_precovid)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - survey_to_covid)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - covid.source)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - covid_wave)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - covid_severity)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - vaccine_doses)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - covid_multiple)$`Pr(>Chisq)`[2])

result = result %>% mutate(pval=ifelse(pval< 0.001, "<0.001", round(as.numeric(pval),3)))
result_new=result


m_full=glm( pasc_worse ~age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+ms_diagnosis+dmt+pdds_precovid+
              survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple, family=binomial(),data=data.ms)
summary(m_full)
OR= exp(coef(m_full))
low=   exp( confint(m_full)[,1])
high = exp(confint(m_full)[,2])
p=summary(m_full)$coef[,4]
est=paste0(round(OR,2),"(", round(low,2),"-",round(high, 2),")")

result=data.frame( var=var, est=est, pval=NA )[-1,]
result$pval[c(1,2,3,4,5,7,8,9,12, 13, 14, 16,17, 19, 20 )]=
  c(lrtest(m_full, . ~ . - age_survey)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - sex)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - race_eth)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - bmi)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - CCI_cata)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - employment_pandemic)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - ms_diagnosis)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - dmt)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - pdds_precovid)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - survey_to_covid)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - covid.source)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - covid_wave)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - covid_severity)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - vaccine_doses)$`Pr(>Chisq)`[2],
    lrtest(m_full, . ~ . - covid_multiple)$`Pr(>Chisq)`[2])

result = result %>% mutate(pval=ifelse(pval< 0.001, "<0.001", round(as.numeric(pval),3)))
result_worse=result
result=cbind(result_overall, result_new, result_worse)
write.csv("../2024 Final/Annals of Neuroloogy/Revision_NEJM//Table2.csv")


############## PASC and PROs ##################
pro=read.csv("score.csv")
data3= merge(pro[,c("id_participant","promis_pa","promis_cognition","promis_dep")], data2, by="id_participant")
data3 = data3 %>% mutate(pasc=as.factor(pasc),
                         pasc_new=as.factor(pasc_new),
                         pasc_worse=as.factor(pasc_worse))
summary(data3$promis_pa)
data3= data3 %>% mutate(in_promis = ifelse(is.na(promis_pa)==F & is.na(promis_dep)==F & is.na(promis_cognition)==F ,1,0))
table(data3$in_promis)
summary(data3$age_survey[data3$in_promis==1]); summary(data3$age_survey[data3$in_promis==0]); t.test(age_survey~in_promis, data=data3)
summary(data3$bmi[data3$in_promis==1]); summary(data3$bmi[data3$in_promis==0]); t.test(bmi~in_promis, data=data3)
summary(data3$survey_to_covid[data3$in_promis==1]); summary(data3$survey_to_covid[data3$in_promis==0]); t.test(survey_to_covid~in_promis, data=data3)
table(data3$sex, data3$in_promis); chisq.test(table(data3$sex, data3$in_promis))
table(data3$race_eth, data3$in_promis); chisq.test(table(data3$race_eth, data3$in_promis))
table(data3$CCI_cata, data3$in_promis); chisq.test(table(data3$CCI_cata, data3$in_promis))
table(data3$employment_pandemic, data3$in_promis); chisq.test(table(data3$employment_pandemic, data3$in_promis))
table(data3$pasc, data3$in_promis); chisq.test(table(data3$pasc, data3$in_promis))


data3= data3 %>%
  filter(in_promis==1)


table(data3$neuro_type, data3$pasc)
table(data3$neuro_type, data3$pasc_new)
table(data3$neuro_type, data3$pasc_worse)
m_pa_overall=lm (promis_pa ~  neuro_type*pasc + age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+
        survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple,data=data3)
summary(m_pa_overall)

m_pa_new=lm (promis_pa ~  neuro_type*pasc_new + age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+
                   survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple,data=data3)
summary(m_pa_new)
m_pa_worse=lm (promis_pa ~  neuro_type*pasc_worse + age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+
               survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple,data=data3)
summary(m_pa_worse)


m_dep_overall=lm (promis_dep ~  neuro_type*pasc + age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+
               survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple,data=data3)
summary(m_dep_overall)
m_dep_new=lm (promis_dep ~  neuro_type*pasc_new + age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+
                    survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple,data=data3)
summary(m_dep_new)
m_dep_worse=lm (promis_dep ~  neuro_type*pasc_worse+ age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+
                survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple,data=data3)
summary(m_dep_worse)

m_cog_overall=lm (promis_cognition~  neuro_type*pasc + age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+
                    survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple,data=data3)
summary(m_cog_overall)
m_cog_new=lm (promis_cognition ~  neuro_type*pasc_new + age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+
                survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple,data=data3)
summary(m_cog_new)
m_cog_worse=lm (promis_cognition ~  neuro_type*pasc_worse+ age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+
                  survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple,data=data3)
summary(m_cog_worse)


outcome= c(rep("Physical activity",9),rep("Cognition",9),rep("Depression",9))
exp=rep(c(rep("PASC (Overall)",3),
      rep("PASC (New symptoms)",3),
      rep("PASC (Worsening symptoms)",3)),3)
est=c(coef(m_pa_overall)[c(2,3,19)],
      coef(m_pa_new)[c(2,3,19)],
      coef(m_pa_worse)[c(2,3,19)],
      coef(m_cog_overall)[c(2,3,19)],
      coef(m_cog_new)[c(2,3,19)],
      coef(m_cog_worse)[c(2,3,19)],
      coef(m_dep_overall)[c(2,3,19)],
      coef(m_dep_new)[c(2,3,19)],
      coef(m_dep_worse)[c(2,3,19)])
high = c(confint(m_pa_overall)[c(2,3,19),1],
         confint(m_pa_new)[c(2,3,19),1],
         confint(m_pa_worse)[c(2,3,19),1],
         confint(m_cog_overall)[c(2,3,19),1],
         confint(m_cog_new)[c(2,3,19),1],
         confint(m_cog_worse)[c(2,3,19),1],
         confint(m_dep_overall)[c(2,3,19),1],
         confint(m_dep_new)[c(2,3,19),1],
         confint(m_dep_worse)[c(2,3,19),1])
low = c(confint(m_pa_overall)[c(2,3,19),2],
        confint(m_pa_new)[c(2,3,19),2],
        confint(m_pa_worse)[c(2,3,19),2],
        confint(m_cog_overall)[c(2,3,19),2],
        confint(m_cog_new)[c(2,3,19),2],
        confint(m_cog_worse)[c(2,3,19),2],
        confint(m_dep_overall)[c(2,3,19),2],
        confint(m_dep_new)[c(2,3,19),2],
        confint(m_dep_worse)[c(2,3,19),2])
p=c(summary(m_pa_overall)$coefficients[c(2,3,19),4],
    summary(m_pa_new)$coefficients[c(2,3,19),4],
    summary(m_pa_worse)$coefficients[c(2,3,19),4],
    summary(m_cog_overall)$coefficients[c(2,3,19),4],
    summary(m_cog_new)$coefficients[c(2,3,19),4],
    summary(m_cog_worse)$coefficients[c(2,3,19),4],
    summary(m_dep_overall)$coefficients[c(2,3,19),4],
    summary(m_dep_new)$coefficients[c(2,3,19),4],
    summary(m_dep_worse)$coefficients[c(2,3,19),4])
result=data.frame(exp=exp, outcome=outcome,est=est, low=low, high=high, p =p)
result=result %>% mutate(beta_est=paste0(round(est,2), "(",round(high,2),", ",round(low,2),")"),
                         p=round(p,3))
write.csv(result,"../2024 Final/Annals of Neuroloogy/Revision_NEJM/Table_PASC and PRO.csv")
result=read.csv("../2024 Final/Annals of Neuroloogy/Revision_NEJM/Table_PASC and PRO.csv")
result$var=factor(result$var, levels = c("Long COVID","MSRD","Long COVID:MSRD"))
result$exp=factor(result$exp, 
                  levels = c("PASC (Overall)","PASC (New symptoms)","PASC (Worsening symptoms)"),
                  labels = c("Model 1" ,"Model 2","Model 3"))
p_pa=result %>% filter(outcome=="Physical activity") %>%
  ggplot(aes(y=est,x=var,fill=exp))+
  geom_errorbar(aes(ymin=low, ymax=high, col=exp),width=0.2, linewidth=1,position = position_dodge(width = 0.5)) +
  geom_point(aes(col=exp),size=3,position = position_dodge(width = 0.5))+
  geom_hline(yintercept=0, linetype='longdash') +
  ggtitle("Beta (95%CI) for physical function")+
  labs(y="",x="")+
  theme_light()+
  scale_color_brewer(palette="Dark2")+
  theme(
    axis.text.y = element_text( size=8),
    axis.text.x = element_text( size = 8,face = "bold"),
    axis.title=element_text( size = 9),
    legend.position = "none")
p_cog=result %>% filter(outcome=="Cognition") %>%
  ggplot(aes(y=est,x=var,fill=exp))+
  geom_errorbar(aes(ymin=low, ymax=high, col=exp),width=0.2, linewidth=1,position = position_dodge(width = 0.5)) +
  geom_point(aes(col=exp),size=3,position = position_dodge(width = 0.5))+
  geom_hline(yintercept=0, linetype='longdash') +
  ggtitle("Beta (95%CI) for cognitive function")+
  labs(y="",x="")+
  theme_light()+
  scale_color_brewer(palette="Dark2")+
  theme(
    axis.text.y = element_text( size=8),
    axis.text.x = element_text( size = 8,face = "bold"),
    axis.title=element_text( size = 9),
    legend.position = "none")
p_dep=result %>% filter(outcome=="Depression") %>%
  ggplot(aes(y=est,x=var,fill=exp))+
  geom_errorbar(aes(ymin=low, ymax=high, col=exp),width=0.2, linewidth=1,position = position_dodge(width = 0.5)) +
  geom_point(aes(col=exp),size=3,position = position_dodge(width = 0.5))+
  geom_hline(yintercept=0, linetype='longdash') +
  ggtitle("Beta (95%CI) for depression")+
  labs(y="",x="")+
  theme_light()+
  scale_color_brewer(palette="Dark2")+
  theme(
    axis.text.y = element_text( size=8),
    axis.text.x = element_text( size = 8, face = "bold"),
    axis.title=element_text( size = 9),
    legend.position = "none")
p_pa+p_cog+p_dep
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM/Fig_PASC and PRO_labels.png", dpi = 300, width = 12, height = 8, units = "in")

result %>% filter(outcome=="Depression") %>%
  ggplot(aes(y=est,x=var,fill=exp))+
  geom_errorbar(aes(ymin=low, ymax=high, col=exp),width=0.2, linewidth=1,position = position_dodge(width = 0.5)) +
  geom_point(aes(col=exp),size=3,position = position_dodge(width = 0.5))+
  geom_hline(yintercept=0, linetype='longdash') +
  ggtitle("Beta (95%CI) for depression")+
  labs(y="",x="")+
  theme_light()+
  scale_color_brewer(palette="Dark2")+
  theme(
    axis.text.y = element_text( size=8),
    axis.text.x = element_text( size = 8, face = "bold"),
    axis.title=element_text( size = 9),
    legend.position = "bottom",
    legend.title = element_blank())
ggsave("../2024 Final/Annals of Neuroloogy/Revision_NEJM/Fig_PASC and PRO_labels.png", dpi = 300, width = 12, height = 8, units = "in")

############## PASC and PDDS in MS  ##################
data4= data3%>% filter(neuro_diagnosis=="MS") 
data4= data3 %>% mutate(ms_diagnosis=ifelse(ms_diagnosis %in% c("PPMS","SPMS"),"PMS","RRMS"),
                            ms_diagnosis=factor(ms_diagnosis, levels=c("RRMS","PMS")),
                            dmt=factor(dmt, levels=c("None","B cell","S1P","Other")) )
m_pdds_overall=glm (pdds_current ~  pasc +pdds_precovid+ms_diagnosis+dmt+ age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+
                 survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple,data=data4, family = poisson())
summary(m_pdds_overall)

m_pdds_new=glm (pdds_current ~  pasc_new +pdds_precovid+ ms_diagnosis+dmt+age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+
                      survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple,data=data4, family = poisson())
summary(m_pdds_new)

m_pdds_worse=glm (pdds_current ~  pasc_worse +pdds_precovid+ ms_diagnosis+dmt+age_survey+sex+race_eth+bmi+CCI_cata+employment_pandemic+
                  survey_to_covid+covid.source+covid_wave+covid_severity+ vaccine_doses+covid_multiple,data=data4, family = poisson())
summary(m_pdds_worse)
