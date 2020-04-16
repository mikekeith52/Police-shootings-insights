### ANALYSIS PERFORMED 2018-09-12

# Reset
rm(list=ls())

# Set seed
set.seed(123)

# Read data - from WaPo GitHub
D<-read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")
D<-D[D$date<='2019-09-12', ] # Added in 04/2020 to be able to recreate reported results

## Create functions
# Create a function to show Information Criteria for comparing models
ic<-function(name="model",model){
  return(data.frame(Model=name,
         LL=logLik(model),
         AIC=AIC(model),
         BIC=BIC(model)))
}

# Create a function which summarizes a model with the info I want
model_sum<-function(model,sig.level=.1){
  df<-data.frame(summary(model)[[12]])
  coefs<-data.frame(var=row.names(df),
    coefs=df[[1]],
    effect_perc=exp(df[[1]])-1,
    p_value=df[[4]])
    coefs$sig<-ifelse(coefs$p_value<=sig.level,T,F)
    coefs<-coefs[order(coefs$p_value),]
  return(list(summary(model)[[1]],coefs))
}

# Make a function to return a 0/1 var for if a kw is in a col
pdum<-function(kw,col,type=c(1,0)){ # can also be c(T,F)
  t<-as.data.frame(grep(as.character(kw),col,ignore.case=T))
  t$one<-type[1]
  colnames(t)<-c("col1","dummy") 
  t2<-as.data.frame(grep(as.character(kw),col,ignore.case=T,
    invert=T))
  t2$zero<-type[2]
  colnames(t2)<-c("col1","dummy")
  t3<-rbind(t,t2)
  t3<-t3[order(t3$col1),]
  return(t3$dummy)
}

# Create a function to derive summary stats in df format
sum.stats<-function(df){
  ret<-data.frame(mean=apply(df,2,mean),
    median=apply(df,2,median),
    sd=apply(df,2,sd),
    min=apply(df,2,min),
    max=apply(df,2,max))
}

# Create a data frame with state name and abbreviation (including DC)
state_info<-data.frame(name=c(state.name,"District of Columbia"),
  abb=c(state.abb,"DC"))

# Scrape 2010 census data for perc of whites per state
library(rvest)
library(stringi)
url <- "http://www.censusscope.org/us/rank_race_nonhispaniclatino_white.html"
df1 <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/table') %>%
    html_table(fill=T)
df1<-data.frame(df1)

# Data is funky, so subset cols and rows
state_race<-df1[c(11:61),c(2,5)]
# Rename cols
colnames(state_race)<-c("state","percent.white")
# Change percents (strings) to numeric and make decimals
state_race$percent.white.num<-as.numeric(substr(state_race$percent.white,
  1,5))/100
# View transformation
head(state_race)

# Match back to state_info
state_info$perc.white<-state_race[[3]][
  match(state_info$name,state_race[[1]])]
# See if it worked
head(state_info) ; tail(state_info)

# check NA values in each col
table(is.na(D))
na<-list()
for (i in 1:ncol(D)){
  na[[i]]<-table(is.na(D[[i]]))
}
na

# 146 NA values in age column
# no other NA values

# Change blank race to U (unknown)
D$race<-ifelse(D$race=="","U",as.character(D$race))

# Drop NAs & unknown race data
D_clean<-na.omit(D)
D_clean<-D_clean[D_clean$race!="U",]

# Key assumption - unknown races distributed randomly

# How many obs did we lose
nrow(D)-nrow(D_clean)
# How many are we left with
nrow(D_clean)

# Control for demographic differences between states
# by matching back census data
D_clean$perc.state.white<-state_info$perc.white[
  match(D_clean$state,state_info$abb)]

# Make age numeric (instead of factor)
D_clean$age<-as.numeric(D_clean$age)

# Clone the data - to save for another operation with this same set of vars
D_reduced<-D_clean

# View transformation
head(D_clean)

# Create some extra vars - for time series
D_clean$CityState<-paste0(D_clean$caity,", ",D_clean$state)
D_clean$month<-substr(D_clean$date,6,7)
D_clean$year<-substr(D_clean$date,1,4)
D_clean$mo_yr<-paste0(D_clean$month,"-",01,"-",D_clean$year)
D_clean$mo_yr<-as.Date(D_clean$mo_yr,format="%m-%d-%Y")

# Create race dummies
library(dummies)
D_clean$rac<-D_clean$race
D_clean<-dummy.data.frame(D_clean,names="rac",sep="e_")

# Aggregate data by date
library(plyr)
time<-ddply(D_clean,~mo_yr,summarize,
            race_A=sum(race_A),
            race_B=sum(race_B),
            race_H=sum(race_H),
            race_N=sum(race_N),
            race_O=sum(race_O),
            race_W=sum(race_W)
)

# Drop last row
time2<-time[c(1:nrow(time)-1),]
summary(time2)

# Create a set of colors for graphing
library(RColorBrewer)
colors<-colorRampPalette(brewer.pal(6,"Set1"))(6)

# Set graph parameters
par(mfrow=c(1,1))

# Graph
plot(x=time2$mo_yr,y=time2$race_W,type='l',ylim=c(0,90),col=colors[1],
  xlab="",ylab="Number of Fatal Shootings",
  main="Total Number of People\nKilled by Police Over Time")
lines(x=time2$mo_yr,y=time2$race_B,col=colors[2])
lines(x=time2$mo_yr,y=time2$race_H,col=colors[3])
lines(x=time2$mo_yr,y=time2$race_A,col=colors[4])
lines(x=time2$mo_yr,y=time2$race_N,col=colors[5])
lines(x=time2$mo_yr,y=time2$race_O,col=colors[6])
legend(x=min(time2$mo_yr),y=90,legend=c("White","Black","Hispanic","Asian",
  "Native Amer.","Other"),col=colors,fill=colors,
  title="Race of Individual",ncol=2,title.adj=.1)

# Get monthly averages
apply(time2[,c(c(2:ncol(time2)))],2,mean)

# Yearly breakdown
yearly<-ddply(D_clean,~year,summarize,
            race_A=sum(race_A),
            race_B=sum(race_B),
            race_H=sum(race_H),
            race_N=sum(race_N),
            race_O=sum(race_O),
            race_W=sum(race_W)
)
apply(yearly[,c(c(2:ncol(time2)))],2,mean)
apply(yearly[yearly$year<2018,c(c(2:ncol(time2)))],2,mean)

##### Preliminary modeling
# Subset with white/non-white as dep var
model_set_white<-subset(D_clean,select=c(manner_of_death,armed,age,
  gender,state, signs_of_mental_illness,threat_level,flee,perc.state.white,
  body_camera,race_W))

# invert the 0/1 so it's easier to interpret
model_set_white$race_NW<-ifelse(model_set_white$race_W==1,0,1)

#Drop race_W
model_set_white<-model_set_white[,c(c(1:(ncol(model_set_white)-2)),
  ncol(model_set_white))]

# Subset with black as dep var
model_set_black<-subset(D_clean,select=c(manner_of_death,armed,age,
  gender,state,perc.state.white,
  signs_of_mental_illness,threat_level,flee,body_camera,race_B))

# Model one - int: % more likely to be non-white given killed
glm.out_white<-glm(race_NW~.,data=model_set_white,
  family=binomial(link = "logit"))
model_sum(glm.out_white)

# Model 2 - int: % more likely to be black given killed
glm.out_black<-glm(race_B~.,data=model_set_black,
  family=binomial(link = "logit"))
model_sum(glm.out_black)

### See in general how much more likely different races
### are to be killed by police
races<-data.frame(table(D_reduced$race))
races$perc<-races$Freq/nrow(D_reduced)
# Manually fill averages from census data - easier than scraping
# src: https://en.wikipedia.org/wiki/Race_and_ethnicity_in_the_United_States
races$nat.avg<-c(0.048,0.126,0.163,0.009,0.062+0.029+0.002,0.724)

# Divide by natl avg. to obtain ratio - how much more/less
# one race is being killed given their proportional population
races$ratio<-races$perc/races$nat.avg
colnames(races)<-c("Race","Number_times_in_dataset","Percent_of_Total_in_Data",
  "Natl_Avg","Ratio:Perc_in_Data_to_Natl_Avg")
races

# Other is like a minority race in spite of the fact that its ratio is .11
# Small sample size, to say it is more like WA htan BHN is faulty

#### Return to D_clean
# After seeing racial breakdown, make groups B, H, N, and O vs W, A
D_clean$race_BHNO<-D_clean$race_B+
  D_clean$race_H+D_clean$race_N+D_clean$race_O

## Start process of changing vars to all numeric
# View what we will be working with
tables<-list()
for (i in c(4,5,11,12,13,14)){
  tables[[i]]<-data.frame(table(D_clean[[i]]))
  tables[[i]]$type<-names(D)[i]
}
tables

# Shot/tasered
D_clean$shot_tasered<-ifelse(D_clean[[4]]=="shot",0,1)

# Ranged weapon
D_clean$weapon_Ranged<-pdum("gun",D_clean[[5]])+
  pdum("bow",D_clean[[5]])+pdum("incendiary device",D_clean[[5]])+
  pdum("taser",D_clean[[5]])+pdum("pepper spray",D_clean[[5]])
table(D_clean$armed,D_clean$weapon_Ranged)

# Car weapon
D_clean$weapon_car<-pdum("vehicle",D_clean[[5]])+pdum("car",D_clean[[5]])-
  pdum("carjack",D_clean[[5]])
table(D_clean$armed,D_clean$weapon_car)

# Unarmed
D_clean$weapon_none<-pdum("unarmed",D_clean[[5]])+
  pdum("claimed to",D_clean[[5]])
table(D_clean[[5]],D_clean$weapon_none)

# Unclear if weapon was used
D_clean$weapon_unclear<-ifelse(D_clean[[5]]=="",1,0)+
  pdum("unknown",D_clean[[5]])+pdum("undetermined",D_clean[[5]])
table(D_clean[[5]],D_clean$weapon_unclear)

# unranged weapon
D_clean$weapon_NonRanged<-rep(1,nrow(D_clean))-
  D_clean$weapon_Ranged-
  D_clean$weapon_car-
  D_clean$weapon_none-
  D_clean$weapon_unclear
D_clean$weapon_NonRanged<-ifelse(D_clean$weapon_NonRanged==-1,0,D_clean$weapon_NonRanged)
table(D_clean[[5]],D_clean$weapon_NonRanged)

# toy weapon
D_clean$weapon_toy<-pdum("toy",D_clean[[5]])

# mental_illness
D_clean$MI_signs<-ifelse(D_clean[[11]]=="True",1,0)

# threat_level: pdum
D_clean$threat_level_unknown<-pdum("undetermined",D_clean$threat_level)
D_clean$attacking<-pdum("attack",D_clean$threat_level)

# flee: fleeing / not fleeing / unknown = mean
D_clean$fleeing<-ifelse(D_clean$flee=="Not fleeing",0,1)
D_clean$fleeing<-ifelse(D_clean$flee=="",
  1-mean(pdum("Not",D_clean$flee)),
  D_clean$fleeing)

# Body camera
D_clean$BodyCamera<-ifelse(D_clean[[14]]=="True",1,0)

# Gender
D_clean$Male<-ifelse(D_clean$gender=="M",1,0)

# State var
library(dummies)
D_clean2<-D_clean
D_clean2$state_<-D_clean2$state
D_clean<-dummy.data.frame(D_clean2,names="state_",sep="")
names(D_clean)

# Subset down to just numeric columns
Dfset<-D_clean[,c(6,15,c(26:30),c(32:ncol(D_clean)))]
nostates<-ncol(Dfset)-51
Dfset_nostates<-Dfset[,c(c(1:nostates))]

# Pearson correlation (exclude states - they are not colinear anyway)
library(corrplot)
library(tidyverse)
Dfset_nostates %>%
 cor() %>%
 corrplot()

# Logit
glm.out.logit<-glm(race_BHNO~.,data=Dfset,
  family=binomial(link = "logit"))

# Probit
glm.out.probit<-glm(race_BHNO~.,data=Dfset,
  family=binomial(link = "probit"))

# One more
glm.out.logit.nostates<-glm(race_BHNO~.,data=Dfset_nostates,
  family=binomial(link = "logit"))

# Check the IC
rbind(ic(name="logit",glm.out.logit),ic(name="probit",glm.out.probit),
  ic(name="nostates",glm.out.logit.nostates))

# Choose the best model
model_sum(glm.out.logit.nostates,sig.level=.05)
best.model<-model_sum(glm.out.logit.nostates,sig.level=.05)[[2]]
# interpret the sig variables
blank.reps<-nrow(best.model)-7
ints<-c(
  "As the percentage of non-white people increases per state, the proportion of blacks, Hispanics, and Native Americans killed by police increases almost exactly proportionally",
  "The younger one is when fatally shot by police, the more likely that individual is to be black, Hispanic, or Native American all else constant",  
  "The percentage of blacks, Hispanics, and Native Americans killed by police is disproportiantely high for reasons not necessarily controlled for or explained in the model",
  "If the suspect killed by police has exhibited signs of mental illness, he or she is 49% more likely to have been white or Asian",
  "If the officer is wearing an active body camera, he/she is 38% more likely to have killed a black, Hispanic, or Native American suspect",
  "If the suspect killed was male, he was 50% more likely to have been black, Hispanic, or Native American",
  "If the suspect was brandishing a toy weapon, he/she was 36% more likely to have been white or Asian",
  rep("",blank.reps))
best.model$interpretations<-ints

# Create a confusion matrix
p<-glm.out.logit.nostates$fitted
pRound<-ifelse(p>=.5,1,0)
table(pRound,D_clean$race_BHNO)

# Turn into class
p_class<-as.factor(ifelse(pRound==1,"BHNO","WA"))
a_class<-as.factor(ifelse(D_clean$race_BHNO==1,"BHNO","WA"))

# Make some visuals/derive model accuracy
library(caret)
library(e1071)
library(caTools)
confusionMatrix(p_class,a_class)
colAUC(p,D_clean$race_BHNO,plotROC=TRUE)

# Box plots - age
D_clean$race<-ordered(D_clean$race, levels = c("W", "B", "H", "A", "N", "O"))
ggplot(D_clean, aes(x=ordered(race, levels = c("O", "N", "A", "H", "B", "W"),
    labels=c("Other", "Native Amer.", "Asian", "Hispanic", "Black", "White")),
    y=age, group=race, col=race, fill=factor(race_BHNO)))+
  geom_boxplot()+
  ggtitle("Average Ages of Suspects Killed Varies by Race")+
  labs(y="Age of Suspect", x="Race of Suspect")+
  theme(plot.title = element_text(size = 14, face = "bold",hjust=.5),
    axis.title = element_text(size=10, face = "bold"),
    legend.position="none")+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=c("grey80","white"))+
  coord_flip()

# View the other model
model_sum(glm.out.logit,sig.level=.05)

# Summary statistics of final data
sum_stats<-sum.stats(Dfset_nostates)

description<-c("Age of individual killed",
  "Percent of individuals identifying as white in the state which killing occured (according to 2010 census)",
  "1 if suspect was black, Hispanic, Native American, or other, 0 otherwise",
  "1 if suspect was shot and tasered, 0 if just shot",
  "1 if the suspect was armed with a ranged weapon (gun, etc.), 0 otherwise",
  "1 if the suspect was using car as a weapon, 0 otherwise",
  "1 if the suspect was not wielding a weapon, 0 otherwise",
  "1 if the suspect was wielding a non-ranged weapon (knife, etc.), 0 otherwise",
  "1 if the suspect was armed with a toy, 0 otherwise",
  "1 if the suspect showed signs of mental illness, 0 otherwise",
  "1 if the threat level of suspect was unknown, 0 otherwise",
  "1 if the suspect was attacking the officer, 0 otherwise",
  "1 if the suspect was fleeing (by foot, car, or otherwise), 0 otherwise",
  "1 if the officer had an active body camera at the time of the shooting, 0 otherwise",
  "1 if the suspect was male, 0 if female")

sum_stats$description<-description

# Summary stats per race
sum_stats_race<-list()
race_vars<-c(20:25)
race_names<-c("Asian","Black","Hispanic.","Native Amer.","Other","White")
for (i in race_vars){
  sum_stats_race[[i-19]]<-sum.stats(Dfset_nostates[D_clean[[i]]==1,])
  sum_stats_race[[i-19]]$race<-race_names[i-19]
  sum_stats_race[[i-19]]$total_number_in_data<-nrow(Dfset_nostates[D_clean[[i]]==1,])
}

# Combine into one dataframe
sum_stats_race_combined<-sum_stats_race[[1]]
for (i in 2:length(sum_stats_race)){
  sum_stats_race_combined<-rbind(sum_stats_race_combined,sum_stats_race[[i]])
}

# For fun - weapons
weapons<-data.frame(table(D$armed))
weapons<-weapons[order(weapons$Freq,decreasing=T),]
colnames(weapons)<-c("Weapon","Number_Times_Used")
weapons_write<-weapons[weapons$Weapon!="",]

