library(pak)
library(ragg)
library(ggplot2)
library(maps)
library(dplyr)


####Figure1 ####
us_map <- map_data("state")
base_map <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  theme_classic() +
  coord_fixed(1.3, xlim = c(-125, -66), ylim = c(24, 49))
cities <- data.frame(
  city = c("Pittsburgh", "Buffalo", "Philadelphia", "New York", "New Haven"),
  lat = c(40.4406, 42.8802, 39.9526, 40.7128, 41.3082),
  long = c(-79.9959, -78.8784, -75.1652, -74.0060, -72.9279)
)

map_with_cities <- base_map +
  geom_point(data = cities, aes(x = long, y = lat, label = city), color = "red", size = 3)
png("Fig1_map.png", units="in", width=7, height=7, res=300)
map_with_cities
dev.off()

#### Figure 2A ####
venn=data.frame(group=rep(c("Overall","MSRD","Control"),2),
                type=c(rep("≥1 new symptom",3),rep("≥1 worsening symptom",3)),
                n=c(669,371,298,537,359,178),
                prop=c("669 (54.5%)","371 (60.5%)","298 (48.5%)","537 (43.8%)","359 (58.6%)","178 (29.0%)"))
venn$group=factor(venn$group,levels = c("Overall", "MSRD","Control"),
                  labels = c("Overall (n=1227)","MSRD (n=613)","Control (n=614)"))

tiff("Fig2A.tiff", units="in", width=12, height=7, res=300)
ggplot(data=venn, aes(x=type,y=n, fill=type))+
  geom_bar(stat="identity")+
  labs(y="N",x="")+
  facet_grid(~group)+
  scale_fill_brewer(palette="Blues")+
  geom_text(aes(label=prop), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3)+
  theme_bw()+
  theme(
        legend.position = "none",
        axis.text.y = element_text(size=10,face="bold"),
        axis.title.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(size=10,face="bold") ,
        strip.text.x = element_text(size = 11, face = "bold"))
dev.off()
###### Figure 2B ######
library(readxl)
ORnew <- read_excel("Tables_Figures/ORs.xlsx",  sheet = "New")
ORnew$Domain=factor(ORnew$Domain, levels = rev(unique(ORnew$Domain)))
ORnew =ORnew %>% arrange(Domain,OR)
ORnew$Symptom=factor(ORnew$Symptom, levels = ORnew$Symptom)
z=abs(qnorm(ORnew$p.value))
se=abs(log(ORnew$OR))/z
ORnew$low=exp(log(ORnew$OR)-1.96*se)
ORnew$high=exp(log(ORnew$OR)+1.96*se)
ORnew$high[which(ORnew$high>30)]=30
color=ifelse(ORnew$p.value<=0.0007,"red","black")

tiff("JAMA Neurology/Figures/20231112/Fig2B_new.tiff", units="in", width=6, height=12, res=300)
ggplot(data=ORnew, aes(x=OR,y=Symptom,fill=Domain))+
  geom_errorbarh(aes(xmin=low, xmax=high), height=.3,col="gray") +
  geom_point(aes(col=Domain),size=1)+
  geom_vline(xintercept=1, linetype='longdash') +
  labs(x="aOR for having ≥1 new symptom",y="")+
  theme_light()+
  xlim(c(0,30))+
  theme_classic()+
  theme(
        legend.position = "none",
        axis.text.y = element_text(colour = color,size=10, face="bold"),
        axis.text.x = element_text(size=10, face='bold'),
        axis.title.x = element_text(size=10, face="bold")  )
dev.off()

ORworse <- read_excel("Tables_Figures/ORs.xlsx",  sheet = "Worsening")
ORworse$Domain=factor(ORworse$Domain, levels = rev(unique(ORworse$Domain)))
ORworse =ORworse%>% arrange(Domain,OR)
ORworse$Symptom=factor(ORworse$Symptom, levels = ORworse$Symptom)
z=abs(qnorm(ORworse$p.value))
se=abs(log(ORworse$OR))/z
ORworse$low=exp(log(ORworse$OR)-1.96*se)
ORworse$high=exp(log(ORworse$OR)+1.96*se)
ORworse$high[which(ORworse$high>60)]=60
color=ifelse(ORworse$p.value<=0.0007,"red","black")

tiff("JAMA Neurology/Figures/20231112/Fig2B_worse.tiff", units="in", width=6, height=12, res=300)
ggplot(data=ORworse, aes(x=OR,y=Symptom,fill=Domain))+
  
  geom_errorbarh(aes(xmin=low, xmax=high), height=.3,col="gray") +
  geom_point(aes(col=Domain),size=1)+
  geom_vline(xintercept=1, linetype='longdash') +
  labs(x="aOR for having ≥1 worsening symptom",y="")+
  theme_light()+
  xlim(c(0,60))+
  theme_classic()+
  theme(
    legend.position = "none",
    axis.text.y = element_text(colour = color,size=10, face="bold"),
    axis.text.x = element_text(size=10, face='bold'),
    axis.title.x = element_text(size=10, face="bold")  )
dev.off()

#### figure 3 #####
pro=readxl::read_xlsx("promis.xlsx")
pro$symptoms2=factor(pro$symptoms2, levels = c("No symptoms","≥1 new symptom only","≥1 worsening symptom only","≥1 new and ≥1 worsening symptoms"))
pro_tmp=pro %>% filter(Outcome=="Cognitive function" & neuro_type=="Control")

tiff("JAMA Neurology/Figures/20231112/Fig3.tiff", units="in", width=8, height=12, res=300)
p1=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,80))+
  labs(x="",y="Cognitive function T-score")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=11, face="bold"),
    axis.text.x = element_blank(),
    axis.ticks.x=element_blank()
  
  )
#dev.off()

#tiff("JAMA Neurology/Figures/20231112/Fig3_contro_dep.tiff", units="in", width=6, height=6, res=300)
pro_tmp=pro %>% filter(Outcome=="Depression" & neuro_type=="Control")
p2=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,80))+
  labs(x="",y="Depression T-score")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=11, face="bold"),
    axis.text.x = element_blank(),
    axis.ticks.x=element_blank()
    
  )
#dev.off()

#tiff("JAMA Neurology/Figures/20231112/Fig3.tiff", units="in", width=6, height=6, res=300)
pro_tmp=pro %>% filter(Outcome=="Physical function" & neuro_type=="Control")
p3=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,80))+
  labs(x="",y="Physical function T-score")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=11, face="bold"),
    axis.text.x = element_blank(),
    axis.ticks.x=element_blank()
    
  )
pro_tmp=pro %>% filter(Outcome=="Cognitive function" & neuro_type=="MSRD")
p4=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,70))+
  labs(x="",y="Cognitive function T-score")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=11, face="bold"),
    axis.text.x = element_blank(),
    axis.ticks.x=element_blank()
    
  )

pro_tmp=pro %>% filter(Outcome=="Physical function" & neuro_type=="MSRD")
p5=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,70))+
  labs(x="",y="Physical function T-score")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=11, face="bold"),
    axis.text.x = element_blank(),
    axis.ticks.x=element_blank()
    
  )
pro_tmp=pro %>% filter(Outcome=="Depression" & neuro_type=="MSRD")
p6=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,70))+
  labs(x="",y="Depression T-score")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=11, face="bold"),
    axis.text.x = element_blank(),
    axis.ticks.x=element_blank()
    
  )

pro_tmp=pro %>% filter(Outcome=="MSRS-R" & neuro_type=="MS")
p7=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,12))+
  labs(x="",y="MSRS-R")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=11, face="bold"),
    axis.text.x = element_blank(),
    axis.ticks.x=element_blank()
    
  )


pro_tmp=pro %>% filter(Outcome=="PDDS" & neuro_type=="MS")
p8=ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,3))+
  labs(x="",y="PDDS")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=11, face="bold"),
    axis.text.x = element_blank(),
    axis.ticks.x=element_blank()
    
  )

   
p1+p2+p3+p4+p6+p5+p7+p8+
  plot_layout(ncol=3)

dev.off()

tiff("JAMA Neurology/Figures/20231112/label.tiff", units="in", width=4, height=4, res=300)
ggplot(data=pro_tmp,aes(x=`symptoms2`,y=est,fill=`symptoms2`))+
  geom_bar( aes(x=`symptoms2`, y=est), stat="identity", alpha=0.7)+
  geom_errorbar(aes(ymin=low, ymax=up,color=`symptoms2`), width=.3) +
  ylim(c(0,3))+
  labs(x="",y="PDDS")+
  theme_bw()+
  theme(
    legend.text = element_text(size=10, face="bold"),
    axis.text.y = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=11, face="bold"),
    axis.text.x = element_blank(),
    axis.ticks.x=element_blank()
    
  )
dev.off()

##### sFigure 1####
dis<-read.csv("PDDS_MSRSR.csv")
dis$Group=factor(dis$Group,levels = c("Overall","Not fully vaccinated","Fully vaccinated","Pre-Omicron","Omicron"))
dis=dis %>% filter(outcome=="MSRS-R")
colnames(dis)[3]="Post-COVID condition"
dis$`Post-COVID condition`[1:5]="≥1 new symptom"
dis$`Post-COVID condition`[6:10]="≥1 worsening symptom"
tiff("R_sFigure2.tiff", units="in", width=8, height=8, res=300)
dis %>% 
  ggplot(aes(x=Group,y=est,fill=Group))+
  geom_errorbar(aes(ymin=low, ymax=high), width=.2) +
  geom_point(aes(x=Group,y=est))+
  geom_hline(yintercept = 0,col=2,linetype="dashed")+
  facet_wrap(~ `Post-COVID condition`,ncol = 1)+
  labs(x="",y="Beta")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_text(size=10, face="bold"),
        axis.title.y = element_text(size=11, face="bold"),
        axis.title.x = element_text(size=9,face="bold" ) ,
        axis.text.x = element_text(size=10,face="bold" ) ,
        strip.text = element_text(size = 11, face="bold")
  )
dev.off()

##### sFigure 2####
cond<-read.csv("OR_condition.csv")
cond$Group=factor(cond$Group,levels = c("Overall","Not fully vaccinated","Fully vaccinated","Pre-Omicron","Omicron"))
colnames(cond)[2]="Post-COVID condition"
cond$`Post-COVID condition`[1:5]="≥1 new symptom"
cond$`Post-COVID condition`[6:10]="≥1 worsening symptom"

tiff("R_sFigure1.tiff", units="in", width=8, height=8, res=300)
ggplot(data=cond, aes(x=Group,y=OR,fill=Group))+
  geom_errorbar(aes(ymin=low, ymax=high), width=.2) +
  geom_point(aes(x=Group,y=OR))+
  geom_hline(yintercept = 1,col=2,linetype="dashed")+
  facet_wrap(~ `Post-COVID condition`,ncol = 1)+
  labs(x="",y="OR")+
  ylim(c(0.8,9.2))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_text(size=10, face="bold"),
        axis.title.y = element_text(size=11, face="bold"),
        axis.title.x = element_text(size=9,face="bold" ) ,
        axis.text.x = element_text(size=10,face="bold" ) ,
        strip.text = element_text(size = 11, face="bold")
  )
dev.off()

