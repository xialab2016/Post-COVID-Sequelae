library(dplyr)
library(reshape)
library(stringr)
library(table1)
library(tableone)
library(pheatmap)
library(dendextend)
library(RColorBrewer)
library(ggplot2)
library(VennDiagram)
library(multcomp)
DATA=read.csv("~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/pasc_rawintegrated_03022023.csv")
length(unique(DATA$id_participant)); dim(DATA) ##2156

#### demographics ####
demo=DATA[,2:34]
sum(is.na(demo$postacute_sequelae_of_sarscov2_timestamp))
demo=demo %>% mutate(sex=ifelse(gender==1,"M",ifelse(gender==2,"F","Other")),
                     sex=factor(sex, levels = c("F","M","Other")),
                     age_survey=2022-yob,
                     race=ifelse(race==1,"Black or African American",ifelse(race==2,"American Indian or Alaska Native",ifelse(race==3,"Asian",ifelse(race==4,"White",ifelse(race==5,"Multi-racial",ifelse(race==6,"Native Hawaiian or Other Pacific Islander","Not sure/Prefer not to answer" )))))),
                     ethnicity=ifelse(ethnicity==1,"Hispanic",ifelse(ethnicity==2,"Non-Hispanic","Not sure/Prefer not to answer")),
                     race_eth=ifelse(race=="White" & ethnicity=="Non-Hispanic","Non-Hispanic White","Other"),
                     race_eth=factor(race_eth,levels = c("Non-Hispanic White","Other")),
                     bmi=(weight/(height_feet*12)^2)*703,
                     bmi_cata=ifelse(bmi< 18.5,"Underweight",ifelse(bmi<25, "Normal",ifelse(bmi<40,"Overweight","Obese"))),
                     bmi_cata=factor(bmi_cata,levels = c("Underweight","Normal","Overweight","Obese")))
demo<-demo %>% mutate(CCI=cci___1+cci___2+cci___3+cci___4+cci___5+cci___6+cci___7+cci___8+cci___9+3*cci___17+cci___10+2*cci___11+
                        2*(cci___12+cci___13+cci___14+cci___15+cci___16)+6*cci___18+6*cci___19+
                        (age_survey>=50 &age_survey<60 )+2*(age_survey>=60 &age_survey<70)+3*(age_survey>=60 &age_survey<70)+4*(age_survey>=80),
                      CCI_cata=ifelse(CCI==0,"None",ifelse(CCI<=2,"Mild",ifelse(CCI<=5,"Moderate","Severe"))))


table(demo$CCI,useNA = "ifany")
table(demo$CCI_cata,useNA = "ifany")
demo=demo %>% dplyr::select(id_participant,age_survey,sex, race_eth, bmi, bmi_cata, CCI,CCI_cata)
write.csv(demo, "~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/demographics.csv",row.names = F)
#### vaccine ####
vac=DATA[,c(2,35:46)]
table(vac$vaccine,vac$vaccine_doses,useNA = "ifany")
id_unknown=vac$id_participant[is.na(vac$vaccine_doses)==T & (vac$vaccine !=0  | is.na(vac$vaccine)==T )] #74 unknown vaccine status
id_nonevac=vac$id_participant[which(vac$vaccine==0)]#124 none vaccine
vactmp=vac %>% filter(id_participant %in% c(id_unknown,id_nonevac) == F) %>% select(id_participant, vaccine, vaccine_doses,vaccine_date1,vaccine_date2,vaccine_date3,vaccine_date4,vaccine_date5)
long=melt(vactmp,id.vars = c("id_participant","vaccine","vaccine_doses"))
colnames(long)[5]="vac_date"
long=long %>% arrange(id_participant,variable) %>% group_by(id_participant) %>%mutate(dose=1:n())
long=long %>% filter(dose<=vaccine_doses)
long=long %>% select(id_participant, vaccine,vaccine_doses,dose,vac_date)
long2=data.frame(id_participant=id_unknown,vaccine=NA, vaccine_doses=NA,dose=NA, vac_date=NA)
long3=data.frame(id_participant=id_nonevac,vaccine=0, vaccine_doses=0,dose=NA, vac_date=NA)
vacnew=rbind(long,long2,long3)
vacnew=vacnew %>% arrange(id_participant) 
write.csv(vacnew, "~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/vaccine.csv",row.names = F)

#### COVID infection ####
id_unknown=DATA$id_participant[which(DATA$covid==101 | is.na(DATA$covid)==T)] ##117 unknown infection status
id_nocov=DATA$id_participant[which(DATA$covid==2)] ##757 no infection
id_cov=DATA$id_participant[which(DATA$covid==1)] ; length(id_cov)  #1282 had >=1 infection##
infect_date=DATA[,c("id_participant","covid","covid_number",colnames(DATA)[str_starts(colnames(DATA),"covid_date")])]
infect_date_long=infect_date %>% filter(id_participant %in% id_cov) %>%
  melt(id.vars = c("id_participant","covid","covid_number")) %>%
  arrange(id_participant, variable) %>% 
  group_by(id_participant) %>%
  mutate(covid_n=1:n())%>%
  filter(covid_n<=covid_number) %>% 
  select(id_participant, covid, covid_number,covid_n,value)
colnames(infect_date_long)[5]="covid_date"
infect_date_long2=data.frame(id_participant=id_unknown,covid=NA, covid_number=NA,covid_n=NA, covid_date=NA)
infect_date_long3=data.frame(id_participant=id_nocov,covid=0, covid_number=0,covid_n=NA, covid_date=NA)
infect_date_long_new=rbind(infect_date_long,infect_date_long2,infect_date_long3)
infect_date_long_new=infect_date_long_new %>% arrange(id_participant) 
length(unique(infect_date_long_new$id_participant))


infect_diag=DATA[which(DATA$id_participant %in% id_cov),c("id_participant","covid","covid_number",colnames(DATA)[str_starts(colnames(DATA),"covid_diagnosis1")])]
infect_diag$covid_n=1
infect_diag=infect_diag %>% mutate(diag_other=ifelse(covid_diagnosis1___4==1 | covid_diagnosis1___5==1 | covid_diagnosis1___6==1,1,0))
infect_diag=infect_diag[,c(1,2,3,10,4,5,6,11)]
colnames(infect_diag)[5:7]=c("diag_pcr","diag_lab_antigen","diag_home_antigen")
infect_diag_all=infect_diag
infect_diag=DATA[which(DATA$id_participant %in% id_cov),c("id_participant","covid","covid_number",colnames(DATA)[str_starts(colnames(DATA),"covid_diagnosis2")])]
infect_diag$covid_n=2
infect_diag=infect_diag %>% mutate(diag_other=ifelse(covid_diagnosis2___4==1 | covid_diagnosis2___5==1 | covid_diagnosis2___6==1,1,0))
infect_diag=infect_diag[,c(1,2,3,10,4,5,6,11)]
colnames(infect_diag)[5:7]=c("diag_pcr","diag_lab_antigen","diag_home_antigen")
infect_diag_all=rbind(infect_diag_all,infect_diag)
infect_diag=DATA[which(DATA$id_participant %in% id_cov),c("id_participant","covid","covid_number",colnames(DATA)[str_starts(colnames(DATA),"covid_diagnosis3")])]
infect_diag$covid_n=3
infect_diag=infect_diag %>% mutate(diag_other=ifelse(covid_diagnosis3___4==1 | covid_diagnosis3___5==1 | covid_diagnosis3___6==1,1,0))
infect_diag=infect_diag[,c(1,2,3,10,4,5,6,11)]
colnames(infect_diag)[5:7]=c("diag_pcr","diag_lab_antigen","diag_home_antigen")
infect_diag_all=rbind(infect_diag_all,infect_diag)
infect_diag=DATA[which(DATA$id_participant %in% id_cov),c("id_participant","covid","covid_number",colnames(DATA)[str_starts(colnames(DATA),"covid_diagnosis4")])]
infect_diag$covid_n=4
infect_diag=infect_diag %>% mutate(diag_other=ifelse(covid_diagnosis4___1==1 | covid_diagnosis4___5==1 | covid_diagnosis4___6==1,1,0))
infect_diag=infect_diag[,c(1,2,3,10,4,5,6,11)]
colnames(infect_diag)[5:7]=c("diag_pcr","diag_lab_antigen","diag_home_antigen")
infect_diag_all=rbind(infect_diag_all,infect_diag)
infect_diag_all=infect_diag_all %>% filter(covid_n<=covid_number) %>% arrange(id_participant,covid_n)
infect_diag_all2=data.frame(id_participant=id_unknown,covid=NA, covid_number=NA,covid_n=NA, diag_pcr=NA,diag_lab_antigen=NA,diag_home_antigen=NA,diag_other=NA)
infect_diag_all3=data.frame(id_participant=id_nocov,covid=0, covid_number=0,covid_n=NA, diag_pcr=NA,diag_lab_antigen=NA,diag_home_antigen=NA,diag_other=NA)
infect_diag_all=rbind(infect_diag_all,infect_diag_all2,infect_diag_all3)
infect_diag_all=infect_diag_all %>% arrange(id_participant) 
length(unique(infect_diag_all$id_participant))


infect_sev=DATA[,c("id_participant","covid","covid_number",colnames(DATA)[str_starts(colnames(DATA),"covid_severity")])]
infect_sev=infect_sev %>% mutate(covid_severity1=ifelse(is.na(covid_severity1)==T,0,covid_severity1),
                                 covid_severity2=ifelse(is.na(covid_severity2)==T,0,covid_severity2),
                                 covid_severity3=ifelse(is.na(covid_severity3)==T,0,covid_severity3),
                                 covid_severity4=ifelse(is.na(covid_severity4)==T,0,covid_severity4))
infect_sev_long=infect_sev %>% filter(id_participant %in% id_cov) %>%
  melt(id.vars = c("id_participant","covid","covid_number")) %>%
  arrange(id_participant, variable) %>% 
  group_by(id_participant) %>%
  mutate(covid_n=1:n())%>%
  filter(covid_n<=covid_number) %>% 
  select(id_participant, covid, covid_number,covid_n,value)
colnames(infect_sev_long)[5]="covid_severity"
infect_sev_long2=data.frame(id_participant=id_unknown,covid=NA, covid_number=NA,covid_n=NA, covid_severity=NA)
infect_sev_long3=data.frame(id_participant=id_nocov,covid=0, covid_number=0,covid_n=NA, covid_severity=NA)
infect_sev_long_new=rbind(infect_sev_long,infect_sev_long2,infect_sev_long3)
infect_sev_long_new=infect_sev_long_new %>% arrange(id_participant)  %>% 
  mutate(covid_severity=factor(covid_severity,levels =c(1,2,3,4,5,0),labels = c("No symptoms","Mild","Moderate","Severe","Very Severe","Unknown") ))
length(unique(infect_sev_long_new$id_participant))

infect=infect_date_long_new %>% left_join(infect_sev_long_new, by=c("id_participant","covid","covid_number","covid_n")) %>%
  left_join(infect_diag_all, by=c("id_participant","covid","covid_number","covid_n"))
infect$covid_date=as.Date(infect$covid_date,"%m/%d/%y")
write.csv(infect, "~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/covid.csv",row.names = F)
latest=infect %>% group_by(id_participant) %>% mutate(covid_date_latest=max(as.Date(covid_date),na.rm = T))
latest$covid_date_latest[which(latest$covid_date_latest==-Inf)]=NA
latest=latest %>% filter(covid_date==covid_date_latest | is.na(covid_date_latest)==T)
latest=latest %>% group_by(id_participant) %>% arrange(covid_n) %>% filter(row_number()==n())
latest<- latest %>% mutate(prev_covid_n=covid_n-1,
                           covid_wave=ifelse(covid_date_latest<"2022-01-01","Before Jan 1, 2022","After Jan 1, 2022")) 


initial=infect %>% group_by(id_participant) %>% mutate(covid_date_initial=min(as.Date(covid_date),na.rm = T))
initial$covid_date_initial[which(initial$covid_date_initial==Inf)]=NA
initial=initial %>% filter(covid_date==covid_date_initial | is.na(covid_date_initial)==T)
initial=initial %>% group_by(id_participant) %>% arrange(covid_n) %>% filter(row_number()==n())
initial<- initial %>% mutate( covid_wave=ifelse(covid_date_initial<"2022-01-01","Before Jan 1, 2022","After Jan 1, 2022")) 
write.csv(latest, "~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/covid_latest.csv",row.names = F)
write.csv(initial, "~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/covid_initial.csv",row.names = F)

### MS and related ####
ms=DATA[,c(2,143:147,160)]
ms=ms %>% mutate(neuro_diagnosis=ifelse((is.na(neuro_diagnosis)==T | neuro_diagnosis==101), "Unknown",
                                        ifelse(neuro_diagnosis==1,"MS",
                                               ifelse(neuro_diagnosis==2,"NMOSD",
                                                      ifelse(neuro_diagnosis==3, "MOGAD",
                                                             ifelse(neuro_diagnosis==4,"NID","Control"))))),
                 neuro_type=ifelse(neuro_diagnosis=="Unknown","Unknown",
                                   ifelse(neuro_diagnosis =="Control","Control","MSRD")),
                 ms_diagnosis=ifelse(neuro_diagnosis!="MS",NA,
                                     ifelse(ms_diagnosis==1,"CIS",ifelse(ms_diagnosis==2,"RIS", ifelse(ms_diagnosis==3,"RRMS",ifelse(ms_diagnosis==4,"SPMS",ifelse(ms_diagnosis==5,"PPMS","Unknown")))))),
                 dmt_efficacy=ifelse(is.na(dmt)==T,"Unknown",
                                     ifelse(dmt==99,"None",
                                            ifelse(dmt==11,"Other",
                                                   ifelse(dmt%in% c(1,2,26,3,4,5,6,18,14,25,8,9,19,20,15),"Standard","High")))))
ms=ms%>% select(id_participant,neuro_diagnosis,neuro_type,ms_diagnosis,dmt_efficacy )
write.csv(ms, "~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/MSRD.csv",row.names = F)


##########################analysis#######################
DATA=read.csv("~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/pasc_rawintegrated_03022023.csv")
demo=read.csv("~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/demographics.csv")
ms=read.csv("~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/MSRD.csv")
initial=read.csv("~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/covid_initial.csv")
vacnew=read.csv("~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/vaccine.csv")
### Table1 ###
data=merge(demo, ms,by="id_participant" )
data=merge(data, initial, by="id_participant")
data=merge( data, vacnew[!duplicated(vacnew$id_participant),1:3],by="id_participant")
data=data %>% filter(neuro_type !="Unknown") %>% filter(is.na(covid)==F)
data$id_participant[which(data$covid==1 & is.na(data$covid_wave)==T)]
data=data %>% filter(id_participant !="UPMC251")
data$ms_diagnosis[data$ms_diagnosis=="Unknown"]=NA
data$ms_diagnosis=factor(data$ms_diagnosis,levels = c("CIS","RIS","RRMS","SPMS","PPMS"))
data$dmt_efficacy[data$dmt_efficacy=="Unknown"]="None"
data$dmt_efficacy=factor(data$dmt_efficacy,levels = c("None","Standard","High","Other"))
data$covid=factor(data$covid,labels = c("No","Yes"),levels = c(0,1))
data$covid_severity[data$covid_severity=="Unknown"]=NA
data$vaccine=factor(data$vaccine,labels = c("No","Yes"),levels = c(0,1))
data$vaccine_doses=as.factor(data$vaccine_doses)
data$CCI_cata=factor(data$CCI_cata,levels = c("None","Mild","Moderate","Severe"))
data=data%>% mutate(diag_pcr=factor(diag_pcr,levels = c(0,1),labels = c("N","Y")),
                    diag_lab_antigen=factor(diag_lab_antigen,levels = c(0,1),labels = c("N","Y")),
                    diag_home_antigen=factor(diag_home_antigen,levels = c(0,1),labels = c("N","Y")),
                    diag_other=factor(diag_other,levels = c(0,1),labels = c("N","Y")))
table1 (~ age_survey+sex+race_eth+bmi+bmi_cata+CCI+CCI_cata+neuro_diagnosis+ms_diagnosis+dmt_efficacy+covid+covid_number+covid_severity+covid_wave+diag_pcr+diag_lab_antigen+diag_home_antigen+diag_other+vaccine+vaccine_doses|neuro_type ,data=data)

##### each symptom yes/no, new/worse, last months
dta=DATA [,c(2, 161:385)]
dta=merge(data,dta,by="id_participant")
dta=dta %>% filter(covid=="Yes")
names=paste0(gsub("_duration","",colnames(dta)[which(grepl("_duration",colnames(dta))==T)]),"___1")
type=paste0(gsub("_duration","",colnames(dta)[which(grepl("_duration",colnames(dta))==T)]),"_type")
who=paste0(gsub("_duration","",colnames(dta)[which(grepl("_duration",colnames(dta))==T)]),"_who")
exist=paste0(gsub("_duration","",colnames(dta)[which(grepl("_duration",colnames(dta))==T)]),"_exist")
for (i in 1:71){
  dta$tmp=ifelse(dta[,names[i]]==0, 0, ifelse(dta[,type[i]]==1,1,0))
  colnames(dta)[which(colnames(dta)=="tmp")]=who[i]
  dta$tmp=ifelse(dta[,names[i]]==0, 0, ifelse(dta[,type[i]]==2,1,0))
  colnames(dta)[which(colnames(dta)=="tmp")]=exist[i]
  
}

dta$any_symp_who=ifelse(rowSums(dta[,who],na.rm = T)==0,0,1)
dta$any_symp_exist=ifelse(rowSums(dta[,exist],na.rm = T)==0,0,1)
dta$symp_n=rowSums(dta[,exist],na.rm = T)+rowSums(dta[,who],na.rm = T)



set1=dta$id_participant[which(dta$any_symp_who==1 )]
set2=dta$id_participant[which(dta$any_symp_exist==1 )]
set3=dta$id_participant[which(dta$any_symp_who==0 & dta$any_symp_exist==0  )]

dta2=dta %>% filter(sex!="Other")
dta2$covid_severity=ifelse(dta2$covid_severity=="Very Severe","Severe",dta2$covid_severity)
dta2$covid_severity=factor(dta2$covid_severity,levels = c("No symptoms","Mild","Moderate","Severe"))
dta2$covid_severity2=ifelse(dta2$covid_severity %in% c("Moderate","Severe"),1,0)

m_new=glm(any_symp_who~ neuro_type+age_survey+sex+race_eth+CCI_cata, family=binomial(),data=dta2); summary(m_new)
m_worse=glm(any_symp_exist~ neuro_type+age_survey+sex+race_eth+CCI_cata, family=binomial(),data=dta2); summary(m_worse)
dta2=dta2 %>% mutate(any_symp=ifelse(any_symp_who==1 | any_symp_exist==1,1,0))
m_newworse=glm(any_symp~ neuro_type+age_survey+sex+race_eth+CCI_cata, family=binomial(),data=dta2); summary(m_newworse)


#####Figure 2######
venn=data.frame(group=rep(c("Overall","MSRD","Control"),2),
                type=c(rep(">=1 PCS",3),rep(">=1 worsening baseline symptom",3)),
                n=c(669,371,298,537,359,178),
                prop=c("669 (54.5%)","371 (60.5%)","298 (48.5%)","537 (43.8%)","359 (58.6%)","178 (29.0%)"))
venn$group=factor(venn$group,levels = c("Overall", "MSRD","Control"),
                  labels = c("Overall (n=1227)","MSRD (n=613)","Control (n=614)"))
ggplot(data=venn, aes(x=type,y=n, fill=type))+
  geom_bar(stat="identity")+
  labs(y="N",x="")+
  facet_grid(~group)+
  scale_fill_brewer(palette="Blues")+
  geom_text(aes(label=prop), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_text(size=8,face="bold"),
        axis.text.x = element_text(size=8,face="bold") ,
        strip.text.x = element_text(size = 10, face = "bold"))

####### baseline disease characteritics with post-covid condition #######
score=read.csv("~/OneDrive - University of Pittsburgh/XiaLab/PASC/data/score.csv")
dta_all=merge(dta2,score,by=c("id_participant","covid","neuro_type") )
dta_ms=dta_all %>% filter(neuro_diagnosis=="MS" & sex !="Other")
dta_ms=dta_ms %>% mutate(ms_diagnosis=ifelse(ms_diagnosis %in% c("CIS","RIS","RRMS"),"RRMS","PMS"))



m=glm(any_symp_who ~ sex+covid_severity2+pdds_precovid+ms_diagnosis+dmt_efficacy, data=dta_ms, family = binomial())
m=glm(any_symp_exist ~ sex+covid_severity2+pdds_precovid+ms_diagnosis+dmt_efficacy, data=dta_ms, family = binomial())
m=glm(any_symp_who ~ sex+pdds_precovid+ms_diagnosis+dmt_efficacy, data=dta_ms, family = binomial())
dta_ms <- dta_ms %>% mutate(any_symp_overall=ifelse(any_symp_who==0 & any_symp_exist==0,"No PCS",
                                                    ifelse(any_symp_who==1 & any_symp_exist==0,"New PCS only",
                                                           ifelse(any_symp_who==0 & any_symp_exist==1,"Worsening PCS only","Both"))))

dta_ms$any_symp_overall=factor(dta_ms$any_symp_overall, levels = c("No PCS","New PCS only","Worsening PCS only","Both"))


#m=lm(msrsr ~ any_symp_who+age_survey+sex+race_eth+CCI_cata+covid_severity2, data=dta_ms); summary (m); confint(m)[2,]
#m=lm(msrsr ~ any_symp_exist+age_survey+sex+race_eth+CCI_cata+covid_severity2, data=dta_ms); summary (m);confint(m)[2,]
m=lm(msrsr ~ any_symp_who*any_symp_exist+age_survey+sex+race_eth+CCI_cata+covid_severity2, data=dta_ms); summary (m);confint(m)[11,]
m=lm(msrsr ~ any_symp_overall+age_survey+sex+race_eth+CCI_cata+covid_severity2, data=dta_ms); summary (m);confint(m)[2,]
cc=matrix(c(1,0,rep(0,9), 1,1,rep(0,9), 1,0,1, rep(0,8),1,0,0,1, rep(0,7)),byrow =T, nrow = 4)
ccc=matrix(c(0,1,rep(0,9),
             0,0,1,rep(0,8),
             0,0,0,1,rep(0,7),
             0,-1,1,rep(0,8),
             0,-1,0,1,rep(0,7),
             0,0,-1,1,rep(0,7)),byrow = T, nrow=6)
confint(glht(m,cc ))
summary(confint(glht(m,ccc )))
m=lm(msrsr ~ symp_n+age_survey+sex+race_eth+CCI_cata+covid_severity2, data=dta_ms); summary (m);confint(m)[2,]
m=lm(pdds_current ~ symp_n+age_survey+sex+race_eth+CCI_cata+covid_severity2, data=dta_ms); summary (m);confint(m)[2,]




m=lm(pdds_current ~ any_symp_who*any_symp_exist+age_survey+sex+race_eth+CCI_cata+covid_severity2, data=dta_ms); summary (m);confint(m)[11,]
m=lm(pdds_current ~ any_symp_overall+age_survey+sex+race_eth+CCI_cata+covid_severity2, data=dta_ms); summary (m);confint(m)[2,]
tmp=dta_ms %>%filter(is.na(pdds_current)==F) %>%  mutate(pred=predict(m))
tmp=tmp %>% group_by(any_symp_overall)%>% summarise(m=mean(pred),sd=sd(pred),n=n())


pro=readxl::read_xlsx("~/OneDrive - University of Pittsburgh/XiaLab/PASC/Tables_Figures/promis.xlsx")
pro$symptoms2=factor(pro$symptoms2, levels = c("No symptoms","New symptoms only","Worsening symptoms only","New and worsening symptoms"))
pro_tmp=pro %>% filter(Outcome=="Cognitive function" & neuro_type=="Control")
p1=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,80))+
  labs(x="",y="Cognitive function T-score")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.ticks.x=element_blank(),
    axis.text.y = element_text(size=8, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.text.x = element_blank()
    )

pro_tmp=pro %>% filter(Outcome=="Physical function" & neuro_type=="Control")
p2=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,80))+
  labs(x="",y="Physical function T-score")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.ticks.x=element_blank(),
    axis.text.y = element_text(size=8, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.text.x = element_blank()
  )

pro_tmp=pro %>% filter(Outcome=="Depression" & neuro_type=="Control")
p3=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,80))+
  labs(x="",y="Depression T-score")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.ticks.x=element_blank(),
    axis.text.y = element_text(size=8, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.text.x = element_blank()
  )

pro_tmp=pro %>% filter(Outcome=="Cognitive function" & neuro_type=="MSRD")
p4=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,80))+
  labs(x="",y="Cognitive function T-score")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.ticks.x=element_blank(),
    axis.text.y = element_text(size=8, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.text.x = element_blank()
  )

pro_tmp=pro %>% filter(Outcome=="Physical function" & neuro_type=="MSRD")
p5=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,80))+
  labs(x="",y="Physical function T-score")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.ticks.x=element_blank(),
    axis.text.y = element_text(size=8, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.text.x = element_blank()
  )
pro_tmp=pro %>% filter(Outcome=="Depression" & neuro_type=="MSRD")
p6=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,80))+
  labs(x="",y="Depression T-score")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.ticks.x=element_blank(),
    axis.text.y = element_text(size=8, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.text.x = element_blank()
  )

pro_tmp=pro %>% filter(Outcome=="MSRS-R" & neuro_type=="MS")
p7=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,14))+
  labs(x="",y="MSRS-R")+
  theme_bw()+
  theme(
    legend.text = element_text( size=11,face="bold"), 
    axis.ticks.x=element_blank(),
    axis.text.y = element_text(size=8, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.text.x = element_blank()
  )

p1+p2+p3+plot_spacer()+p4+p5+p6+p7+
  plot_layout(ncol=4)


dis<-read.csv("~/OneDrive - University of Pittsburgh/XiaLab/PASC/Tables_Figures/PDDS_MSRSR.csv")
dis$Group=factor(dis$Group,levels = c("Overall","Not fully vaccinated","Fully vaccinated","Pre-Omicron","Omicron"))
dis=dis %>% filter(outcome=="MSRS-R")
colnames(dis)[3]="Post-COVID condition"
dis$`Post-COVID condition`[1:5]=">=1 PCS"
dis$`Post-COVID condition`[6:10]=">=1 worsening from baseline symptom"

p1=dis %>% 
  ggplot(aes(x=Group,y=est,fill=Group))+
  geom_errorbar(aes(ymin=low, ymax=high), width=.2) +
  geom_point(aes(x=Group,y=est))+
  geom_hline(yintercept = 0,col=2,linetype="dashed")+
  facet_grid(~ `Post-COVID condition`)+
  labs(x="",y="Beta")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_text(size=8, face="bold"),
        axis.title.y = element_text(size=9, face="bold"),
        axis.title.x = element_text(size=9,face="bold" ) ,
        axis.text.x = element_text(size=8,face="bold" ) ,
        strip.text = element_text(size = 10, face="bold")
        )




cond<-read.csv("~/OneDrive - University of Pittsburgh/XiaLab/PASC/Tables_Figures/OR_condition.csv")
cond$Group=factor(cond$Group,levels = c("Overall","Not fully vaccinated","Fully vaccinated","Pre-Omicron","Omicron"))
colnames(cond)[2]="Post-COVID condition"
cond$`Post-COVID condition`[1:5]=">=1 PCS"
cond$`Post-COVID condition`[6:10]=">=1 worsening from baseline symptom"

ggplot(data=cond, aes(x=Group,y=OR,fill=Group))+
  geom_errorbar(aes(ymin=low, ymax=high), width=.2) +
  geom_point(aes(x=Group,y=OR))+
  geom_hline(yintercept = 1,col=2,linetype="dashed")+
  facet_grid(~ `Post-COVID condition`)+
  labs(x="",y="OR")+
  ylim(c(0.8,9.2))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_text(size=8, face="bold"),
        axis.title.y = element_text(size=9, face="bold"),
        axis.title.x = element_text(size=9,face="bold" ) ,
        axis.text.x = element_text(size=8,face="bold" ) ,
        strip.text = element_text(size = 10, face="bold")
  )

####### post-COVID condition and promis scores #######
dta_all=dta_all %>% filter(sex!="Other")
dd=dta_all %>%  mutate(covid="Yes") %>% 
  select(id_participant, neuro_type, any_symp_who, any_symp_exist, age_survey,sex,race_eth,CCI_cata, covid_severity2,covid, promis_pa, promis_cognition,promis_dep,vaccine_doses,covid_wave) 
dd2=data %>% filter(covid=="No") %>% mutate(covid_severity2=covid_severity, any_symp_who=0, any_symp_exist=0) %>% 
  select(id_participant,neuro_type, any_symp_who, any_symp_exist, age_survey,sex,race_eth,CCI_cata, covid_severity2,covid,vaccine_doses,covid_wave) 
dd2=merge(dd2, score[,c(1,7,8,9)],by="id_participant")
dd_all=rbind(dd,dd2)
dd_all=dd_all %>% mutate(covid_stat1=ifelse(covid=="No","Not infected",ifelse(any_symp_who==1,"Having new post-COVID condition", "Infected but without new post-COVID condition")),
                         covid_stat1=factor(covid_stat1, levels = c("Not infected","Infected but without new post-COVID condition","Having new post-COVID condition")),
                         covid_stat2=ifelse(covid=="No","Not infected",ifelse(any_symp_exist==1,"Having worsening post-COVID condition", "Infected but without worsening post-COVID condition")),
                         covid_stat2=factor(covid_stat2, levels = c("Not infected","Infected but without worsening post-COVID condition","Having worsening post-COVID condition")),
                         covid_stat3=ifelse(covid_stat1=="Not infected","Not infected",
                                            ifelse(covid_stat1=="Infected but without new post-COVID condition" & covid_stat2=="Infected but without worsening post-COVID condition","No post-COVID condition",
                                                   ifelse(covid_stat1=="Infected but without new post-COVID condition" & covid_stat2=="Having worsening post-COVID condition","Worsening post-COVID condition",
                                                          ifelse(covid_stat1=="Having new post-COVID condition" & covid_stat2=="Infected but without worsening post-COVID condition","New post-COVID condition","New and worsening post-COVID condition")))),
                         covid_stat3=factor(covid_stat3, levels = c("Not infected","No post-COVID condition","New post-COVID condition","Worsening post-COVID condition","New and worsening post-COVID condition")))
dd_all=dd_all %>% mutate(missing=ifelse(is.na(promis_pa)==T,1,0))
table1(~neuro_type+covid+age_survey+sex+race_eth+CCI_cata | missing,dd_all )
tab=CreateTableOne(vars = c("neuro_type","covid","age_survey","sex","race_eth","CCI_cata"), 
                   strata = "missing" , 
                   data = dd_all,
                   addOverall = T,
                   factorVars =c("neuro_type","covid","sex","race_eth","CCI_cata"),
                   argsApprox = list(correct = F), 
                   argsNormal = list(var.equal = T) )
tab=print(tab,
          exact=c("sex","race_eth","CCI_cata","covid","neuro_type"),  #exact fisher test
          nonnormal="age_survey",
          catDigits = 2,
          contDigits = 2,
          pDigits = 3,
          formatOptions = list(big.mark = ","),
          showAllLevels = T)


dd_all2=dd_all %>% filter(covid_stat3!="Not infected")
m1=lm(promis_cognition ~ covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all2[dd_all2$neuro_type=="Control",]);summary(m)
cc=matrix(c(1,0,rep(0,9), 1,1,rep(0,9), 1,0,1, rep(0,8),1,0,0,1, rep(0,7)),byrow =T, nrow = 4)
ccc=matrix(c(0,1,rep(0,9),
             0,0,1,rep(0,8),
             0,0,0,1,rep(0,7),
             0,-1,1,rep(0,8),
             0,-1,0,1,rep(0,7),
             0,0,-1,1,rep(0,7)),byrow = T, nrow=6)
confint(glht(m1,cc ))
summary(confint(glht(m1,ccc )))
m2=lm(promis_cognition ~ covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all2[dd_all2$neuro_type=="MSRD",]);summary(m)
confint(glht(m2,cc ))
summary(confint(glht(m2,ccc )))

m3=lm(promis_pa ~ covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all2[dd_all2$neuro_type=="Control",]);summary(m)
summary(confint(glht(m3,ccc )))
m4=lm(promis_cognition ~ covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all2[dd_all2$neuro_type=="MSRD",]);summary(m)
summary(confint(glht(m4,ccc )))
m5=lm(promis_dep ~ covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all2[dd_all2$neuro_type=="Control",]);summary(m)
summary(confint(glht(m5,ccc )))
m6=lm(promis_dep ~ covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all2[dd_all2$neuro_type=="MSRD",]);summary(m)
confint(glht(m6,cc ))
summary(confint(glht(m6,ccc )))

newdata_control=c(m1$fitted.values[dd_all2$neuro_type=="Control"],
                  m3$fitted.values[dd_all2$neuro_type=="Control"],
                  m5$fitted.values[dd_all2$neuro_type=="Control"])
newdata_msrd=c(m1$fitted.values[dd_all2$neuro_type=="MSRD"],
               m3$fitted.values[dd_all2$neuro_type=="MSRD"],
               m5$fitted.values[dd_all2$neuro_type=="MSRD"])

newdata=data.frame(score=c(newdata_control,newdata_msrd),
                   neuro_type=c(rep("Control",length(newdata_control)), rep("MSRD",length(newdata_msrd))),
                   outcome=c(rep("Cognitive function",612), rep("Physical function",612),rep("Depression",612),
                             rep("Cognitive function",611), rep("Physical function",611),rep("Depression",611)),
                   postCOVID=c(rep(dd_all2$covid_stat3[dd_all2$neuro_type=="Control"],3),
                               rep(dd_all2$covid_stat3[dd_all2$neuro_type=="MSRD"],3)))


pro=read.csv("~/OneDrive - University of Pittsburgh/XiaLab/PASC/Tables_Figures/promis.csv")
pro1=pro %>% filter(neuro_type=="Control")
colnames(pro1)[2]="Post-COVID condition in Controls"
pro1$`Post-COVID condition in Controls`=factor(pro1$`Post-COVID condition in Controls`,levels = c("No condition (n=255)","New condition only (n=152)","Worsening condition only (n=35)","Both (n=134)"))
pro2=pro %>% filter(neuro_type=="MSRD")
colnames(pro2)[2]="Post-COVID condition in MSRDs"
pro2$`Post-COVID condition in MSRDs`=factor(pro2$`Post-COVID condition in MSRDs`,levels = c(  "No condition (n=148)","New condition only (n=72)","Worsening condition only (n=60)","Both (n=277)"))
pro1$Outcome=factor(pro1$Outcome,levels = c("Physical function","Cognitive function","Depression"))
pro2$Outcome=factor(pro2$Outcome,levels = c("Physical function","Cognitive function","Depression"))


p1=ggplot(data=pro1,aes(x=`Post-COVID condition in Controls`,y=est,fill=`Post-COVID condition in Controls`))+
  geom_bar( aes(x=`Post-COVID condition in Controls`, y=est), stat="identity", alpha=0.7) +
  geom_errorbar(aes(ymin=low, ymax=up,color=`Post-COVID condition in Controls`), width=.3) +
  facet_grid(~ Outcome)+
  ylim(c(40,75))+
  labs(x="",y="")+
  theme_bw()+
  theme(
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=8) ,
    legend.text = element_text(size = 8),
    legend.title =element_text(size = 9) ,
    axis.text.x = element_blank())
p2=
  ggplot(data=pro2,aes(x=`Post-COVID condition in MSRDs`,y=est,fill=`Post-COVID condition in MSRDs`))+
  geom_bar( aes(x=`Post-COVID condition in MSRDs`, y=est), stat="identity", alpha=0.7) +
  geom_errorbar(aes(ymin=low, ymax=up,color=`Post-COVID condition in MSRDs`), width=.3) +
  facet_grid(~ Outcome)+
  ylim(c(0,70))+
  labs(x="",y="")+
  theme_bw()+
  theme(
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=8) ,
    legend.text = element_text(size = 8),
    legend.title =element_text(size = 9) ,
    axis.text.x = element_blank())
p1 /
  p2
newdata1=newdata %>% mutate(fit=pred$fit, low=fit-1.96*pred$se.fit, high=fit+1.96*pred$se.fit,
                            fit2=c( 57.42677-4.41790 ,57.42677-4.41790  -2.00131 , 57.42677-5.83134-4.41790 , 57.42677-8.90042-4.41790,
                                    57.42677 ,57.42677  -2.00131 , 57.42677-5.83134 , 57.42677-8.90042))


m=lm(promis_pa ~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected",]);summary(m)
newdata=data.frame(neuro_type=c(rep("MSRD",4),rep("Control",4)),
                   covid_stat3=rep(c("No post-COVID condition","New post-COVID condition","Worsening post-COVID condition","New and worsening post-COVID condition"),2),
                   age_survey=0, sex="F",race_eth="Non-Hispanic White",CCI_cata="None",covid_severity2="0")

pred=predict(m , newdata = newdata,se=T)
newdata2=newdata %>% mutate(fit=pred$fit, low=fit-1.96*pred$se.fit, high=fit+1.96*pred$se.fit,
                            fit2=c(67.53998 -7.10099 ,67.53998 -7.10099-0.75261 ,67.53998 -7.10099-5.22158 ,67.53998 -7.10099 -5.70893  ,
                                   67.53998  ,67.53998 -0.75261 ,67.53998 -5.22158 ,67.53998  -5.70893))


m=lm(promis_dep ~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected",]);summary(m)
newdata=data.frame(neuro_type=c(rep("MSRD",4),rep("Control",4)),
                   covid_stat3=rep(c("No post-COVID condition","New post-COVID condition","Worsening post-COVID condition","New and worsening post-COVID condition"),2),
                   age_survey=0, sex="F",race_eth="Non-Hispanic White",CCI_cata="None",covid_severity2="0")

pred=predict(m , newdata = newdata,se=T)
newdata3=newdata %>% mutate(fit=pred$fit, low=fit-1.96*pred$se.fit, high=fit+1.96*pred$se.fit,
                            fit2=c(52.13228+0.22858,52.13228+0.22858+ 0.24935 ,52.13228+0.22858+4.22988 ,52.13228+0.22858+6.03206 ,
                                   52.13228,52.13228+ 0.24935 ,52.13228+4.22988 ,52.13228+6.03206 ))

newdata=rbind(newdata1,newdata2,newdata3)
newdata$outcome=c(rep("Cognition",8),rep("Physical function",8), rep("Depression",8))
newdata$outcome=factor(newdata$outcome, levels = c("Physical function","Cognition","Depression"))
newdata$covid_stat3=factor(newdata$covid_stat3, levels = c("No post-COVID condition","New post-COVID condition","Worsening post-COVID condition","New and worsening post-COVID condition"))
newdata %>% filter(neuro_type=="MSRD") %>%
  ggplot( aes(y=covid_stat3,x=fit2, fill=outcome))+
  geom_bar(aes(color=outcome),stat="identity")+
  facet_wrap( ~ outcome)+
  labs(x="",y="")+
  theme_light()+
  theme(
    axis.text = element_text(size=12, face="bold") ,     
    legend.position = "none",
    strip.text.x = element_text(size = 13, face="bold"))+
  xlim(0,70)


newdata %>% filter(neuro_type=="MSRD") %>%
  ggplot( aes(y=fit,x=covid_stat3, fill=covid_stat3))+
  
  geom_errorbar(aes(color=covid_stat3,y=fit, ymin=low, ymax=high), width=0.2, size=1.5)+
  geom_point(aes(y=fit,color=covid_stat3),size=2.5)+
  facet_wrap( ~ outcome)+
  labs(x="",y="")+
  theme_light()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 13, face="bold"))



m=lm(promis_cognition ~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected" & dd_all$vaccine_doses %in% c("0","1","2"),]);summary(m)
m=lm(promis_cognition ~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected" & dd_all$vaccine_doses %in% c("3","4","5"),]);summary(m)
m=lm(promis_cognition ~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected" & dd_all$vaccine_doses %in% c("3","4","5"),]);summary(m)

m=lm(promis_pa ~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected" & dd_all$vaccine_doses %in% c("0","1","2"),]);summary(m)
m=lm(promis_pa~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected" & dd_all$vaccine_doses %in% c("3","4","5"),]);summary(m)
m=lm(promis_dep ~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected" & dd_all$vaccine_doses %in% c("0","1","2"),]);summary(m)
m=lm(promis_dep~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected" & dd_all$vaccine_doses %in% c("3","4","5"),]);summary(m)
m=lm(promis_cognition ~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected" & dd_all$covid_wave=="Before Jan 1, 2022",]);summary(m)
m=lm(promis_cognition ~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected" & dd_all$covid_wave=="After Jan 1, 2022",]);summary(m)
m=lm(promis_pa ~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected"  & dd_all$covid_wave=="Before Jan 1, 2022",]);summary(m)
m=lm(promis_pa~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected"  & dd_all$covid_wave=="After Jan 1, 2022",]);summary(m)
m=lm(promis_dep ~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected" &   dd_all$covid_wave=="Before Jan 1, 2022",]);summary(m)
m=lm(promis_dep~ neuro_type+covid_stat3+age_survey+sex+race_eth+CCI_cata+covid_severity2,data=dd_all[dd_all$covid_stat3!="Not infected" &   dd_all$covid_wave=="After Jan 1, 2022",]);summary(m)

