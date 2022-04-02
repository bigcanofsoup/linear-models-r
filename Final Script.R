#Linear Models Final Script

#SETTING UP/LOADING IN DATA
load('nameofdata.RData')
attach(nameofdata)

#to remove NA type:
nameofdata = na.omit(nameofdata)

#to remove a certain data frame column:
new_dataanme <- dataname[-c(column number), (row number) ]

#to point out a certain point on a scatterplot:
points(columnName[index], rowName[index], col = 'color', pch = 16)

#PLOT DATA
plot(x,y)

#CREATE A LINEAR MODEL
lm.fit.dataname = lm(y~x1 + x2 + x1:x2 + I(x1^2) + I(x2^2), data = nameofdata)
summary(lm.fit.dataname)

#CONFIDENCE AND PREDICTION INTERVALS
#For E(y)
predict(lm.fit.dataname, dataframevar1 = c(value), dataframevar2 = c(value), interval = 'confidence')
predict(lm.fit.dataname, dataframevar1 = c(value), dataframevar2 = c(value), interval = 'prediction')
#For individual Bk
confint(lm.fit.dataname)
confint(lm.fit)

#NESTED MODEL TEST
lm.fit.reduced = lm(y~x1+x2+x1:x2)
lm.fit.complete = lm(y~x1+x1+x1:x1 + I(x1^2) + I(x2^2))

anova(lm.fit.reduced, lm.fit.complete)

#Formula: [SSE(R) - SSE(C)]/[MSE(C)]
#Degrees of Freedom: Numerator = number of new terms, Denominator = n - (k+1)
#For output: SSR = SSE

#CODED VALUES FOR U
DataName$u = (Var1 - mean(VarA))/sd(VarA)
DataName$u2 = (DataName$u)^2

DataName$VarA2 = (VarA)^2

attach(DataName)

corr(VarA, VarA2)
corr(u, u2)

summary(u)

qt(0.025, 30, lower.tail = FALSE)


2*pt(8.556, 30, lower.tail = FALSE)

qt(0.1, 194, lower.tail = FALSE)

pt(0.52, 194, lower.tail = FALSE)

pt(-0.55, 194)

2*pf(7.247, 4, 194, lower.tail = FALSE)
load("STREETVN.Rdata")
attach(STREETVN)


load('BODYFT.Rdata')
