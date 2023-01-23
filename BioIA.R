library(haven)
library(tibble)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(tableone)

#Demographics (includes sex, age, and ethnicity)
DEMO_J = read_xpt('C:/Users/sarth/Downloads/DEMO_J.xpt')
demo1718 <- DEMO_J[,1:10]
demo1718$RIAGENDR[demo1718$RIAGENDR == 1] <- 0
demo1718$RIAGENDR[demo1718$RIAGENDR == 2] <- 1 

#FMR relevant data, selecting relevant columns
DXA1718 = read_xpt('C:/Users/sarth/Downloads/DXX_J.xpt')
dfDXA <- select(DXA1718, c('SEQN','DXXLLFAT','DXDLLTOT','DXXRLFAT',
                         'DXDRLTOT','DXDTRPF'))
#Renaming columns 
names(dfDXA) <- c('SEQN','LLegFat','LLegTotal','RLegFat','RLegTotal',
                'TrunkPercent')

#Adding in calculated FMR 
fat = 100*(dfDXA['LLegFat']+dfDXA['RLegFat'])/(dfDXA['LLegTotal']+dfDXA['RLegTotal'])
dfDXA['TrunkLeg'] = dfDXA['TrunkPercent']/fat

#Cholesterol levels
TCHOL1718 = read_xpt('C:/Users/sarth/Downloads/TCHOL_J.xpt')
TCHOLnew <- na.omit(TCHOL1718)
names(TCHOLnew) <- c('SEQN','Chol1','Chol2') #Chol1 is cholesterol levels in mg/dL, Chol2 is levels in mmol/L

#Smoking
SMQ_J = read_xpt('C:/Users/sarth/Downloads/SMQ_J.xpt')
smoke <- select(SMQ_J, c('SEQN','SMQ020'))
smoke$SMQ020[smoke$SMQ020 == 2] <- 0

#Combining FMR data with cholesterol
df1 <- merge(x=dfDXA,y=TCHOLnew,by='SEQN',all.x=TRUE)

#Combining resulting data with demographics
df2 <- merge(x=df1,y=demo1718,by='SEQN',all.x=TRUE)

#Combining resulting data with smoking 
df3 <- merge(x=df2,y=smoke,by='SEQN',all.x=TRUE)

#Creating a summary of data with means and standard deviations of variables
print(CreateTableOne(data=df3))

#Creating a linear regression unadjusted for confounding variables such as age, sex, ethnicity, and smoking 
a <- lm(Chol2 ~ TrunkLeg, data=df3)
summary(a)
ggplot(df3,aes(x=TrunkLeg,y=Chol2)) +
  geom_point() +
  geom_smooth(method='lm')

#Adjusted linear regression
b <- lm(Chol2 ~ TrunkLeg + RIDAGEYR + RIAGENDR + SMQ020 +
          factor(RIDRETH3), data=df3)
summary(b)
