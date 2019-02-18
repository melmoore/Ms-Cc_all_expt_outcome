#Making outcome data set for all Ms+Cc experiments
  ##will need to be updatedd as data collection is finished

#load libraries

library(readr)
library(ggplot2)
library(tidyr)
library(Rmisc)
library(dplyr)
library(viridis)
library(cowplot)
library(extrafont)

#------------------

#set working directory
setwd("~/Manduca expts")

#load data sets

#FIELD SUBLETHAL
slhs <- read_csv("Spring+Summer+Fall 2018/Field_Sublethal_heat_shock/data files/Field_SLHS_clean_wdead.csv",
                 col_types = cols(test.temp = col_factor(levels = c("35", "40", "42", "43", "44")), 
                                  treat.hs = col_factor(levels = c("control", "shock")), 
                                  treat.para = col_factor(levels = c("np", "p"))))

View(slhs)


#LAB EARLY HEAT SHOCK

ehs <- read_csv("~/Manduca expts/Spring+Summer+Fall 2018/Early Ms+Cc heat shock/Ms-Cc-EHS-analysis/data/Ms+Cc_EHS_incomplete_clean.csv", 
                col_types = cols(hs.num = col_factor(levels = c("0","1", "2", "3", "4")), 
                                 hs.temp = col_factor(levels = c("0", "40", "42"))))
View(ehs)


#LAB REPEATED HEAT SHOCK

rhs <- read_csv("~/Manduca expts/Spring+Summer+Fall 2018/Ms-Cc_repeated_heatshock/data files/Ms+Cc_RHS_incomplete_clean.csv", 
                col_types = cols(shock.stage = col_factor(levels = c("control","early", "mid", "late"))))
View(rhs)



#LAB LATE HEAT SHOCK

lhs <- read_csv("~/Manduca expts/Spring+Summer+Fall 2018/Late_Ms-Cc_heat-shock/Ms+Cc_late-heatshock_semi-cl.csv", 
                col_types = cols(hs.treat = col_factor(levels = c("0", "hs.4", "hs.5")), 
                                 temp.var = col_factor(levels = c("0", "10"))))
View(lhs)



#LAB TEMP VAR EXPT

tv <- read_csv("~/Manduca expts/Summer+Fall 2017/Ms-Cc_25-28-30_temp-var/data files/25-28-30_tv-final_clean.csv", 
               col_types = cols(temp.avg = col_factor(levels = c("25","28", "30")), 
                                temp.var = col_factor(levels = c("0", "5", "10")), 
                                treatment = col_factor(levels = c("control", "para"))))
View(tv)



#LAB CONSTANT TEMP CONSUMPTION EXPT

cpt <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt gr wide.csv", 
                        col_types = cols(temp = col_factor(levels = c("20", "25", "30")), 
                                         treatment = col_factor(levels = c("control", "para"))))
View(cpt)


#-------------------------

#Make outcome data frames for all expts


#FIELD SUBLEHTAL HEAT SHOCK

#remove dead individuals
slhs.cl<-subset(slhs, died.toss=="0")

#calculating proportion of each "outcome" for each temp and para treatment

slhs.cl$mass.wand[is.na(slhs.cl$mass.wand)]<-0
slhs.cl$wander<-ifelse(slhs.cl$mass.wand>0, 1, 0)

slhs_outcome<-slhs.cl %>% group_by(test.temp, treat.para) %>% tally(wander)
View(slhs_outcome)


#find total number in each treatment
slhs_tot.n<-slhs.cl %>% count(test.temp, treat.para)

#add total number to dataframe with number of outcomers
slhs_outcome$tot.n<-slhs_tot.n$n  

#rename outcome$n to outcome$wander to avoid confusion
slhs_outcome<-rename(slhs_outcome, wander=n)

#calculate proportion of outcomeers
slhs_outcome$prop.wand<-slhs_outcome$wander/slhs_outcome$tot.n

#calculate the number in each treatment that had wasp emergence
slhs.cl$date.em.j[is.na(slhs.cl$date.em.j)]<-0
slhs.cl$emergence<-ifelse(slhs.cl$date.em.j>0, 1, 0)

slhs_emerge<-slhs.cl %>% group_by(test.temp, treat.para) %>% tally(emergence)
View(slhs_emerge)

#add emergence data to outcome dataframe
slhs_outcome$emerge<-slhs_emerge$n

#calculate proportion emergence
slhs_outcome$prop.emerge<-slhs_outcome$emerge/slhs_outcome$tot.n


#Make a mongo sorting column specifically for mongos that were not wanderers
slhs.cl$mongo.calc<-ifelse(slhs.cl$mongo==1 & slhs.cl$mass.wand==0, 1, 0)


#calculate the number in each treatment that were culled (assumed mongos)
slhs_mongo<-slhs.cl %>% group_by(test.temp, treat.para) %>% tally(mongo.calc)
View(slhs_mongo)

#add mongo data to outcome dataframe
slhs_outcome$mongo<-slhs_mongo$n

#calculate the proportion of mongos in each treatment
slhs_outcome$prop.mongo<-slhs_outcome$mongo/slhs_outcome$tot.n

#make a column with experiment name, for combination with other experiment outcome data
slhs_outcome$expt<-"f_slhs"

#make a column with the year the experiment took place
slhs_outcome$year<-2018

#add columns with rearing temp avg and temp var data and population, for later combination with other datasets
  ##also add dummy hs.num column and shock.stage column
slhs_outcome$rear.temp.avg<-25
slhs_outcome$rear.temp.var<-10
slhs_outcome$hs.num<-0
slhs_outcome$shock.stage<-"early"
slhs_outcome$pop<-"field"

#rename test.temp column to be hs.temp for combination
slhs_outcome<-rename(slhs_outcome, hs.temp=test.temp)

#making hs.temp numeric for binding
slhs_outcome$hs.temp<-as.numeric(slhs_outcome$hs.temp)

#-------------------------------

#LAB EARLY HEAT SHOCK

#(rewriting code so that it mirrors how I did it for FSLHS--hopefully will make it easier to format them
#for merging)

#create table of outcomes--count the number of individuals in each heat shock treatment and class
ehs_outcome<-ehs %>% count (hs.temp, hs.num, class)

#put into wide format
ehs_outcome<-ehs_outcome %>% spread(class, n, fill=0)

#count total in each treatment
ehs_tot.n<-ehs %>% count(hs.temp, hs.num)

#add column to ehs_outcome with total sample size
ehs_outcome$tot.n<-ehs_tot.n$n

#calculate the proportion of each class
ehs_outcome$prop.wand<-ehs_outcome$wand/ehs_outcome$tot.n
ehs_outcome$prop.emerge<-ehs_outcome$em/ehs_outcome$tot.n
ehs_outcome$prop.mongo<-ehs_outcome$mongo/ehs_outcome$tot.n

#renaming N emerge column to match other datasets
ehs_outcome<-rename(ehs_outcome, emerge=em, wander=wand)

#create expt name column and year column
ehs_outcome$expt<-"l_ehs"
ehs_outcome$year<-2018


#creating rear.temp.avg and rear.temp.var columns for combination, also shock.stage and treat.para
ehs_outcome$rear.temp.avg<-25
ehs_outcome$rear.temp.var<-10
ehs_outcome$shock.stage<-"early"
ehs_outcome$treat.para<-"p"
ehs_outcome$pop<-"lab"


#make ehs hs.num and hs.temp numeric instead of factor
ehs_outcome$hs.num<-as.numeric(ehs_outcome$hs.num)
ehs_outcome$hs.temp<-as.numeric(ehs_outcome$hs.temp)

#------------------------------------------

#LAB REPEATED HEAT SHOCK

#remove dead individuals
rhs.cl<-subset(rhs, died.bf5==0)

#removing control individual that molted poorly and died, but was recorded as cull instead of dead
rhs.cl<-subset(rhs.cl, id!="232")

rhs.cl$date.em.j[is.na(rhs.cl$date.em.j)]<-0
rhs.cl$date.cull.j[is.na(rhs.cl$date.cull.j)]<-0
rhs.cl$date.wand.j[is.na(rhs.cl$date.wand.j)]<-0
rhs.cl$num.em[is.na(rhs.cl$num.em)]<-0

rhs.cl$class<-ifelse(rhs.cl$date.em.j>0 | rhs.cl$num.em>0, "em",
                     ifelse(rhs.cl$date.cull.j>0, "mongo",
                            ifelse(rhs.cl$date.wand.j>0, "wander", "unk")))

check<-rhs.cl[,c("id","shock.stage", "date.em.j", "date.cull.j", "date.wand.j", "class")]
View(check)

#removing the "unk" class--probably errors with missing data
rhs.cl<-subset(rhs.cl, class!="unk")

#fix some typos where some "late" shock stages did not have hs.temp or hs.num data input
  ##need to fix in data sheet
which(rhs.cl$shock.stage=="late" & rhs.cl$hs.temp==0)

rhs.cl[117, 5]<-42
rhs.cl[117, 6]<-3
rhs.cl[122, 5]<-42
rhs.cl[122, 6]<-3
rhs.cl[133, 5]<-42
rhs.cl[133, 6]<-3


#fix some typos where some "control" shock stages have hs.temp and hs.num that do not == 0
  ##need to fix in data sheet

which(rhs.cl$shock.stage=="control" & rhs.cl$hs.num=="3")

rhs.cl[17, 5]<-0
rhs.cl[17, 6]<-0
rhs.cl[52, 5]<-0
rhs.cl[52, 6]<-0
rhs.cl[57, 5]<-0
rhs.cl[57, 6]<-0
rhs.cl[62, 5]<-0
rhs.cl[62, 6]<-0
rhs.cl[67, 5]<-0
rhs.cl[67, 6]<-0
rhs.cl[82, 5]<-0
rhs.cl[82, 6]<-0
rhs.cl[86, 5]<-0
rhs.cl[86, 6]<-0
rhs.cl[97, 5]<-0
rhs.cl[97, 6]<-0
rhs.cl[100, 5]<-0
rhs.cl[100, 6]<-0


#create table of outcomes--count the number of individuals in each heat shock treatment and class
rhs_outcome<-rhs.cl %>% count (shock.stage, hs.temp, hs.num, class)
View(rhs_outcome)

#put into wide format
rhs_outcome<-rhs_outcome %>% spread(class, n, fill=0)

#count total in each treatment
rhs_tot.n<-rhs.cl %>% count(shock.stage)

#add column to ehs_outcome with total sample size
rhs_outcome$tot.n<-rhs_tot.n$n

#calculate the proportion of each class
rhs_outcome$prop.wand<-rhs_outcome$wander/rhs_outcome$tot.n
rhs_outcome$prop.emerge<-rhs_outcome$em/rhs_outcome$tot.n
rhs_outcome$prop.mongo<-rhs_outcome$mongo/rhs_outcome$tot.n


#renaming N emerge column to match other datasets
rhs_outcome<-rename(rhs_outcome, emerge=em)

#create expt name column and year column
rhs_outcome$expt<-"l_rhs"
rhs_outcome$year<-2018

#add columns for rear.temp.avg and rear.temp.var and treat.para for combination 
rhs_outcome$rear.temp.avg<-25
rhs_outcome$rear.temp.var<-10
rhs_outcome$treat.para<-"p"
rhs_outcome$pop<-"lab"

#making shock.stage character instead of factor
rhs_outcome$shock.stage<-as.character(rhs_outcome$shock.stage)

#-------------------------------

#LAB LATE HEAT SHOCK

#find rows with NA in date.em.j

lhs$date.em.j[is.na(lhs$date.em.j)]<-0
which(lhs$date.em.j==0)

check<-lhs[c(12, 16, 27, 43, 63, 64, 65, 81, 105, 113, 116, 138, 144, 196, 231),]
View(check)

#discard individuals with missing data
lhs<-subset(lhs, id!=97 & id!=98 & id!=99 & id!=124 & id!=167 & id!=195)

#fill in missing wander data for specific individuals (wander data in notes, not in binary column)
which(lhs$id==25)
lhs[12, 97]<-1

which(lhs$id==30)
lhs[16, 97]<-1

which(lhs$id==170)
lhs[111, 97]<-1

which(lhs$id==269)
lhs[191, 97]<-1


#Create "class" column (wand, emerge, mongo)

lhs$wand[is.na(lhs$wand)]<-0
lhs$date.em.j[is.na(lhs$date.em.j)]<-0

lhs$class<-ifelse(lhs$wand==1, "wander",
                  ifelse(lhs$date.em.j>0, "emerge", "mongo"))


#count number of each class per treatment
lhs_outcome<-lhs %>% count(temp.var, hs.treat, class)
View(lhs_outcome)

#put into wide format
lhs_outcome<-spread(lhs_outcome, class, n, fill=0)


#find total number in each treatment
lhs_tot.n<-lhs %>% count(temp.var, hs.treat)

#add total number to dataframe with number of outcomeers
lhs_outcome$tot.n<-lhs_tot.n$n  

#create column for # of mongos (all 0s)
lhs_outcome$mongo<-0

#calculate proportion for each class
lhs_outcome$prop.wand<-lhs_outcome$wander/lhs_outcome$tot.n
lhs_outcome$prop.emerge<-lhs_outcome$emerge/lhs_outcome$tot.n
lhs_outcome$prop.mongo<-lhs_outcome$mongo/lhs_outcome$tot.n


#add columns for experiment name and year
lhs_outcome$expt<-"l_lhs"
lhs_outcome$year<-2018

#create a rear.temp.avg and treat.para column for combination
lhs_outcome$rear.temp.avg<-28
lhs_outcome$treat.para<-"p"
lhs_outcome$pop<-"lab"
lhs_outcome$hs.temp<-45

#rename columns for combination
lhs_outcome<-rename(lhs_outcome, rear.temp.var=temp.var, 
                    shock.stage=hs.treat)

#create column of hs.num based on shock stage data (control=0, shocks=1)
lhs_outcome$hs.num<-ifelse(lhs_outcome$shock.stage==0, 0, 1)

#Make rear.temp.var numeric
lhs_outcome$rear.temp.var<-as.numeric(lhs_outcome$rear.temp.var)

#converting factor to numeric makes values 1, 2--replacing with actual temperature values
lhs_outcome$rear.temp.var<-ifelse(lhs_outcome$rear.temp.var==1, 0, 10)

#----------------------------

#LAB TEMPERATURE VARIATION EXPERIMENT
  ##(NOT including field individuals)

tv<-subset(tv, pop!="field")

#create a class column
tv$date.wand.j[is.na(tv$date.wand.j)]<-0
tv$date.em.j[is.na(tv$date.em.j)]<-0

tv$class<-ifelse(tv$date.wand.j>0 | tv$wander==1, "wander",
                 ifelse(tv$date.em.j>0, "emerge", "mongo"))

check<-tv[, c("temp.avg", "temp.var", "date.wand.j", "date.em.j", "class")]


#count the number of each class in each treatment
tv_outcome<-tv %>% count(temp.avg, temp.var, treatment, class)
View(tv_outcome)

#put into wide format
tv_outcome<-tv_outcome %>% spread(class, n, fill=0)

#count the total sample size in each treatment
tv_tot.n<-tv %>% count(temp.avg, temp.var, treatment)
View(tv_tot.n)

#add total sample size to outcome data frame
tv_outcome$tot.n<-tv_tot.n$n


#calculate the proportion of emergence, wanderers and mongos
tv_outcome$prop.wand<-tv_outcome$wander/tv_outcome$tot.n
tv_outcome$prop.emerge<-tv_outcome$emerge/tv_outcome$tot.n
tv_outcome$prop.mongo<-tv_outcome$mongo/tv_outcome$tot.n


#create experiment name and year columns, and hs.temp and hs.num columns
tv_outcome$expt<-"l_tv"
tv_outcome$year<-2017
tv_outcome$hs.temp<-0
tv_outcome$hs.num<-0
tv_outcome$pop<-"lab"
tv_outcome$shock.stage<-"none"

#rename columns to match other data sets
tv_outcome<-rename(tv_outcome, treat.para=treatment,
                   rear.temp.avg=temp.avg,
                   rear.temp.var=temp.var)

#rename treat.para factor levels to match other data sets
tv_outcome<-tv_outcome %>% mutate(treat.para = factor(treat.para, labels=c("np", "p")))

#make rear.temp.avg and rear.temp.var numeric instead of factor
tv_outcome$rear.temp.avg<-as.numeric(tv_outcome$rear.temp.avg)
tv_outcome$rear.temp.var<-as.numeric(tv_outcome$rear.temp.var)

#converting from factors to numeric converts values to 1, 2, 3--make correct temp values
tv_outcome$rear.temp.avg<-ifelse(tv_outcome$rear.temp.avg==1, 25,
                                 ifelse(tv_outcome$rear.temp.avg==2, 28, 30))

tv_outcome$rear.temp.var<-ifelse(tv_outcome$rear.temp.var==1, 0,
                                 ifelse(tv_outcome$rear.temp.var==2, 5, 10))


#----------------------------

#LAB CONSTANT CONSUMPTION EXPERIMENT

#remove dead individuals
cpt.cl<-subset(cpt, died==0)

#create a class column

cpt.cl$suc.ovp[is.na(cpt.cl$suc.ovp)]<-0
cpt.cl$load[is.na(cpt.cl$load)]<-0
cpt.cl$date.wander[is.na(cpt.cl$date.wander)]<-0
cpt.cl$date.em[is.na(cpt.cl$date.em)]<-0

cpt.cl$class<-ifelse(cpt.cl$date.wander>0, "wander",
                     ifelse(cpt.cl$date.em>0, "emerge",
                            ifelse(cpt.cl$load==0, "mongo", "unk")))


#count the number of each class in each treatment
cpt_outcome<-cpt.cl %>% count(temp, treatment, class)
View(cpt_outcome)


#put into wide format
cpt_outcome<-cpt_outcome %>% spread(class, n, fill=0)

#count the total sample size in each treatment
cpt_tot.n<-cpt.cl %>% count(temp, treatment)
View(cpt_tot.n)

#add total sample size to outcome data frame
cpt_outcome$tot.n<-cpt_tot.n$n


#calculate the proportion of emergence, wanderers and mongos
cpt_outcome$prop.wand<-cpt_outcome$wander/cpt_outcome$tot.n
cpt_outcome$prop.emerge<-cpt_outcome$emerge/cpt_outcome$tot.n
cpt_outcome$prop.mongo<-cpt_outcome$mongo/cpt_outcome$tot.n


#create experiment name and year columns, and hs.temp and hs.num columns
cpt_outcome$expt<-"l_cpt"
cpt_outcome$year<-2016
cpt_outcome$hs.temp<-0
cpt_outcome$hs.num<-0
cpt_outcome$rear.temp.var<-0
cpt_outcome$pop<-"lab"
cpt_outcome$shock.stage<-"none"


#rename temp and treatment columns to temp.avg and treat.para
cpt_outcome<-rename(cpt_outcome, rear.temp.avg=temp, treat.para=treatment)

#rename treat.para factor levels to np and p to match other data sets
cpt_outcome<- cpt_outcome %>% mutate(treat.para = factor(treat.para, labels=c("np", "p")))

#make rear.temp.avg numeric instead of factor
cpt_outcome$rear.temp.avg<-as.numeric(cpt_outcome$rear.temp.avg)

#transforming to factor makes values 1, 2, 3--convert back to temperature values
cpt_outcome$rear.temp.avg<-ifelse(cpt_outcome$rear.temp.avg==1, 20,
                                  ifelse(cpt_outcome$rear.temp.avg==2, 25, 30))

#-----------------------------

#combind outcome dataframes into one data set with outcomes from all experiments

oc_bind<-bind_rows(slhs_outcome, ehs_outcome, rhs_outcome, lhs_outcome, tv_outcome, cpt_outcome)

#write csv

write.csv(oc_bind, "Ms-Cc_all-expt_outcome_data_2016-2018.csv",row.names = FALSE)



