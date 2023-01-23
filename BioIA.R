#PLAN
#1. need to check how many data points you have available to use
  #since FMR is the independent variable and cholesterol is dependent, we want
#to merge dataset on cholesterol, but first have each independent data set for 
#you to view (FMR, cholesterol, risk of diabetes)

#2. remember need to find correlation between trunk/leg and cholesterol
  #first make regression of cholesterol (Y) and FMR (X)
    #need to look up online how to do this, shouldn't be too bad 
    #need to adjust for age, sex, smoking, etc. 
  #then do logistic regression of diabetes (Y) and FMR (X)
    #again look up online
  #after you have both of these, create a google doc for bio IA and just
#focus on raw and processed data and graphs for now and analysis/conclusion


#if not enough try to slightly broaden the research question
#once you decide you have enough data, please do the graphs
#with the graphs do the analysis
#make a conclusion, answer research question 
#_______________________________________________________________________________
library(haven)
library(tibble)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(tableone)

#need to first combine DXA with cholesterol to see how many data points you 
#actually have, then incorporate the other stuff for ethnicity and sex. 

#Demographics (includes sex and ethnicity)
DEMO_J = read_xpt('C:/Users/sarth/Downloads/DEMO_J.xpt')
demo1718 <- DEMO_J[,1:10]
demo1718$RIAGENDR[demo1718$RIAGENDR == 1] <- 0
demo1718$RIAGENDR[demo1718$RIAGENDR == 2] <- 1

#Fat mass ratio stuff
DXA1718 = read_xpt('C:/Users/sarth/Downloads/DXX_J.xpt')
dfDXA <- select(DXA1718, c('SEQN','DXXLLFAT','DXDLLTOT','DXXRLFAT',
                         'DXDRLTOT','DXDTRPF'))
names(dfDXA) <- c('SEQN','LLegFat','LLegTotal','RLegFat','RLegTotal',
                'TrunkPercent')

fat = 100*(dfDXA['LLegFat']+dfDXA['RLegFat'])/(dfDXA['LLegTotal']+dfDXA['RLegTotal'])
dfDXA['TrunkLeg'] = dfDXA['TrunkPercent']/fat

#Cholesterol 
#TCHOL = read_xpt('C:/Users/sarth/Downloads/TCHOL_D.xpt')
TCHOL1718 = read_xpt('C:/Users/sarth/Downloads/TCHOL_J.xpt')
TCHOLnew <- na.omit(TCHOL1718)
names(TCHOLnew) <- c('SEQN','Chol1','Chol2')

#Smoking
SMQ_J = read_xpt('C:/Users/sarth/Downloads/SMQ_J.xpt')
smoke <- select(SMQ_J, c('SEQN','SMQ020'))
smoke$SMQ020[smoke$SMQ020 == 2] <- 0

df1 <- merge(x=dfDXA,y=TCHOLnew,by='SEQN',all.x=TRUE)

df2 <- merge(x=df1,y=demo1718,by='SEQN',all.x=TRUE)

df3 <- merge(x=df2,y=smoke,by='SEQN',all.x=TRUE)

print(CreateTableOne(data=df3))

#add in diabetes stuff later i guess, add in smoking also 

#Diabetes 
#DIQ = read_xpt('C:/Users/sarth/Downloads/DIQ_D.xpt')
#diq <- select(DIQ, c('SEQN','DIQ010'))

#may want to add in more data from previous years to increase sample size 
#since even though this table has 5114 values there are a lot of N/A's that don't
#overlap between the cholesterol and fat mass ratio columns 

a <- lm(Chol2 ~ TrunkLeg, data=df3)
summary(a)

ggplot(df3,aes(x=TrunkLeg,y=Chol2)) +
  geom_point() +
  geom_smooth(method='lm')

# ggplot(df3,aes(x=TrunkLeg,y=Chol2)) +
#   geom_point() +
#   geom_abline(slope=1.75662,intercept=3.04834)

b <- lm(Chol2 ~ TrunkLeg + RIDAGEYR + RIAGENDR + SMQ020 +
          factor(RIDRETH3), data=df3)
summary(b)

divide by ethnicity and sex 

could be dependent on age only, so need to built regression so what 
- after adjusting for age, and other stuff 

could prefer 0 and 1 for male and female respectively
- can look up stuff, change the column into male and female 
- this would give you a coefficient for female but should be zero for male? 
confounding (term) --> could be caused by sex instead of the actual thing you 
think it would be 
one hot encoding 

read about how to plot a best fit line YOU made 
have a control experiment first 
use control first: make linear regression of cholesterol vs FMR unadjusted 
- look at coefficients, 
make linear regression, control for confounders 
- 
- second one is adjusted cholesterol vs FMR 

make a dummy data table with 4 columns, with each covariate 
- artificially set age to avg age
- set smoking to avg smoking 

- first one is the control

can see how the coefficients change 

confirm that ggplot is same between geomsmooth and geomab where geomab is the 
manually inputed values for slope and intercept

----------------------------------------------------
- second one is adjusted
adjustment: including other covariates in your model 
- coefficient is differnet in second model because that's after you control 
for the other '
if age ends up being the correlation, this is called confoundment 
when you include covariates in regression, 

adjustment for regression 

after the adjusted model, report the coefficients in a table 
  model 1: unadjusted 
  model 2: adjusted 
  
report effect sizes and p values 
------------------
raw data: tableone 
processed: adjusted linear regressions, show coefficients and p values from
adjusted and unadjusted models 
----------------------------------------------------
----------------------------------------------------
  
  
second step: predict stuff, 
  predict.lm function in R 


  

#Need to make sure you merge correctly
#need to 






  # Questions i dont know how to fucking answer
  # - which function to merge on

#____________________Checking distribution of FMR in 2017-2018__________________
library(patchwork)

demo1718 = read_xpt('C:/Users/sarth/Downloads/DEMO_J.xpt')
DXA1718 = read_xpt('C:/Users/sarth/Downloads/DXX_J.xpt')
df0 <- select(DXA1718, c('SEQN','DXXLLFAT','DXDLLTOT','DXXRLFAT',
                         'DXDRLTOT','DXDTRPF'))
names(df0) <- c('SEQN','LLegFat','LLegTotal','RLegFat','RLegTotal',
                'TrunkPercent')

df1 <- merge(x=df0,y=demo1718,by='SEQN',all.x=TRUE)
fat = 100*(df1['LLegFat']+df1['RLegFat'])/(df1['LLegTotal']+df1['RLegTotal'])
TrunkLeg = df1['TrunkPercent']/fat
df1 = add_column(df1, TrunkLeg, .after=6)
colnames(df1)[7] <- 'TrunkLeg'

dff1 <- df1 %>% filter(RIDRETH1 == 1)
dff2 <- df1 %>% filter(RIDRETH1 == 2)
dff3 <- df1 %>% filter(RIDRETH1 == 3)
dff4 <- df1 %>% filter(RIDRETH1 == 4)
dff5 <- df1 %>% filter(RIDRETH1 == 5)

pT1 <- ggplot(dff1,aes(x=TrunkLeg, fill=factor(RIAGENDR)))+
  geom_histogram(bins=75, position='identity', alpha=0.4)+ggtitle('TrunkLeg MexAmer')+
  scale_fill_discrete(name='Sex',labels=c('M','F'))+xlim(0,2.5)+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())
#      axis.ticks.x=element_blank())
pT2 <- ggplot(dff2,aes(x=TrunkLeg, fill=factor(RIAGENDR)))+
  geom_histogram(bins=75, position='identity', alpha=0.4)+ggtitle('TrunkLeg Hisp')+
  scale_fill_discrete(name='Sex',labels=c('M','F'))+xlim(0,2.5)+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())
pT3 <- ggplot(dff3,aes(x=TrunkLeg, fill=factor(RIAGENDR)))+
  geom_histogram(bins=75, position='identity', alpha=0.4)+ggtitle('TrunkLeg White')+
  scale_fill_discrete(name='Sex',labels=c('M','F'))+xlim(0,2.5)+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())
pT4 <- ggplot(dff4,aes(x=TrunkLeg, fill=factor(RIAGENDR)))+
  geom_histogram(bins=75, position='identity', alpha=0.4)+ggtitle('TrunkLeg Black')+
  scale_fill_discrete(name='Sex',labels=c('M','F'))+xlim(0,2.5)+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())
pT5 <- ggplot(dff5,aes(x=TrunkLeg, fill=factor(RIAGENDR)))+
  geom_histogram(bins=75, position='identity', alpha=0.4)+ggtitle('TrunkLeg Other')+
  scale_fill_discrete(name='Sex',labels=c('M','F'))+xlim(0,2.5)

pT1/pT2/pT3/pT4/pT5






#demo[25<'RIDAGEYR'<30]
#demosub <- subset(demo, RIDAGEYR > 25 & RIDAGEYR < 30)

#need to do merge function to see how many people have both
#cholesterol data, risk of diabetes, and fat mass ratio stuff 