########### Cutlets 1 ##############################################

# Two Sample Two Tail T Test

cutlets=read.csv('E:\\Sree-Official\\Sree-Personal\\EXCELR\\Data Science\\Assignments\\Hypothesis Testing\\Cutlets.csv')

# Define - 
# Null Hypothesis (Ho) - Mean unit A = Mean unit B
# Alternate Hypothesis (Ha) - Mean unit A != Mean unit B

t.test(cutlets$Unit.A,cutlets$Unit.B,alternative = 'two.sided')

# Pvalue 0.4723, which is greater than alpha value
# Conclusion: There is no significant difference in the diameter of the cutlet between two units. 

########### LabTAT 2 ##############################################

# ANNOVA

LabTAT=read.csv('E:\\Sree-Official\\Sree-Personal\\EXCELR\\Data Science\\Assignments\\Hypothesis Testing\\LabTAT.csv')
stack_LabTAT<- stack(LabTAT)
View(stack_LabTAT)

shapiro.test(LabTAT$Laboratory.1)
shapiro.test(LabTAT$Laboratory.2)
shapiro.test(LabTAT$Laboratory.3)
shapiro.test(LabTAT$Laboratory.4)
summary(LabTAT)

# Data is normally distributed
library(car)

# Test for vaiance
leveneTest(values ~ ind, data = stack_LabTAT)
?leveneTest

Anova_results <- aov(values~ind,data = stack_LabTAT)
summary(Anova_results)
print(Anova_results)

View(stack_LabTAT)
plot(values ~ ind,data = stack_LabTAT)
results=aov(values ~ ind,data = stack_LabTAT)
summary(results)
print(results)

# Conclusion : P Value is less than alpha value, Hence there is a significant difference in average TAT among the different laboratories.


########### BuyerR 3 ##############################################

# Chisq Test

# Define - 
# Null Hypothesis (Ho) - Male & Female all regions proportion are equal
# Alternate Hypothesis (Ha) - Male & Female all regions proportion are not equal

BuyerR=read.csv('E:\\Sree-Official\\Sree-Personal\\EXCELR\\Data Science\\Assignments\\Hypothesis Testing\\BuyerRatio.csv')
View(BuyerR)
attach(BuyerR)
table(East,West,North,South)
chisq.test(table(table(East,West,North,South)))

# P value 0.0027, which is less than alpha value 0.05
# Conclusion: Male Female buyer rations not all proportions are equal across regions. 

########### CustomerOrderForms 4 ##############################################

# Chisq Test

# Define - 
# Null Hypothesis (Ho) – defective % varies by centre
# Alternate Hypothesis (Ha) - defective % not varies by centre

custordrforms=read.csv('E:\\Sree-Official\\Sree-Personal\\EXCELR\\Data Science\\Assignments\\Hypothesis Testing\\Costomer+OrderForm.csv')
View(custordrforms)
colnames(custordrforms)
attach(custordrforms)
table(Phillippines,Indonesia,Malta,India)
chisq.test(table(table(Phillippines,Indonesia,Malta,India)))

# Pvalue 0.1035, which is greater than alpha value
# Conclusion: Defective % varies by centre.



########### Fantaloons 5 ##############################################

# Two proportion T Test

# Define - 
# Null Hypothesis (Ho) - %males Versus %females walking in to the store are equal
# Alternate Hypothesis (Ha) - %males Versus %females walking in to the store are not equal

fantaloons=read.csv('E:\\Sree-Official\\Sree-Personal\\EXCELR\\Data Science\\Assignments\\Hypothesis Testing\\Faltoons.csv')
View(fantaloons)
attach(fantaloons)
table1=table(Weekend,Weekdays)
table1

prop.test(x=c(167,66),n=c(287,113),conf.level = 0.95,correct = FALSE,alternative = 'two.sided')

# Pvalue 0.9681, which is greater than alpha value
# Conclusion: %males Versus %females walking in to the store are equal. 
