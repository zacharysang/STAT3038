rm(list=ls())

################################################
# Activity 1. 

#### Reading data ####
NFLdata = read.table(file="NFL12_14.txt", header=TRUE)
dimnames(NFLdata) # Shows ID of 96 rows and the variable names of 60 columns
print(NFLdata[c(1:3),])

Y = NFLdata[,"Wins"]
X = NFLdata[,"PassPct"]
n = length(Y)

plot(X,Y,xlim=c(53,71),ylim=c(0.5,13.9),xlab="Percentage of completions per pass attempts (%)",ylab="Regular season wins")

################################################
# Activity 2. 

xbar = mean(X)
ybar = mean(Y)

print(X)
print(Y)
print(X * Y)

XtimesY = X * Y

sumXtimesY = sum(XtimesY)

SSxy = sumXtimesY - n * xbar * ybar

Xsquared = X^2
print(X)
print(Xsquared)

sumXsquared = sum(Xsquared)

SSxx = sumXsquared - n * xbar^2

beta1hat = SSxy / SSxx

beta0hat = ybar - beta1hat * xbar

################################################
# Activity 3. 

print(beta1hat)
print(X)
print(beta1hat * X)

Yhat = beta0hat + beta1hat * X
print(Yhat)

epsilon_hat = Y - Yhat

SSE = sum(epsilon_hat^2)
MSE = SSE/(n-2)

s = sqrt( MSE )

CV = 100 * s / ybar
print(CV)

################################################
# Activity 4. 

tstat = beta1hat / ( s/sqrt(SSxx) )
DF = n - 2

pvalue = 2 * pt(tstat,df=DF,lower.tail=F)

################################################
# Activity 5. 

c(-1,1) * 10
1 + c(-1,1) * 10

SEbeta1 = s / sqrt(SSxx)

t1 = qt(0.025,df=DF,lower.tail=F)  
CI1 = beta1hat + c(-1,1) * t1 * SEbeta1
print(CI1)

t2 = qt(0.005,df=DF,lower.tail=F)  
CI2 = beta1hat + c(-1,1) * t2 * SEbeta1
print(CI2)

################################################
# Activity 6.
 
SSyy = sum(Y^2) - n * ybar^2

R2 = ( SSyy - SSE ) / SSyy

################################################
# Activity 7.

LM = lm(Y ~ X)

aov(LM)

summary(LM)



################################################
# Activity 8. 

#####
ImportedData = read.table(file="BOXING2.txt", header=TRUE)
ImportedData
dim(ImportedData)
Lactate = ImportedData[,1]
Recovery = ImportedData[,2]
plot(Recovery,Lactate)

n = length(Lactate)
sd(Recovery)
summary(Lactate)

#####
ImportedData = read.table(file="HEAT.txt", header=TRUE)
ImportedData
dim(ImportedData)
apply(ImportedData,2,mean)
apply(ImportedData,2,median)
apply(ImportedData,2,min)
apply(ImportedData,2,max)

#####
ImportedData = read.table(file="TAMPALMS.txt", header=TRUE)
ImportedData
dim(ImportedData)
MarketVal = ImportedData[,2]
SalePrice = ImportedData[,3]
plot(MarketVal,SalePrice)
summary(MarketVal)
summary(SalePrice)

#####
ImportedData = read.table(file="FHWABRIDGE.txt", header=TRUE)
ImportedData
dim(ImportedData)
apply(ImportedData[,-1],2,mean)
apply(ImportedData[,2:3],2,mean)
apply(ImportedData[,-1],2,sd)


################################################
# Extra activities
NFLdata = read.table(file="NFL12_14.txt", header=TRUE)

# Extra activity 1
NFLdata[,1:3]
NFLdata[,1] 
NFLdata[,1] == "CincinnatiBengals"
which(NFLdata[,1] == "CincinnatiBengals")
id_Bengals = which(NFLdata[,1] == "CincinnatiBengals")
id_Bengals
NFLdata[id_Bengals,1]
Data_Bengals = NFLdata[id_Bengals,]
Data_Bengals
Data_Bengals[,c(1,2,4)]

# Extra activity 2
id_2012 = which(NFLdata[,2] == 2012)
Data_2012 = NFLdata[id_2012,]
Data_2012[,1:3]

# Extra activity 3
par(mfrow=c(1,2))
hist(NFLdata[,c("PassYds_Att")], breaks=30)
hist(NFLdata[,c("RushAvg")], breaks=30)

# Extra activity 4
AscOrder = order(Data_2012[,3])
Data_2012[AscOrder,1:3]
DesOrder = order(Data_2012[,3],decreasing=TRUE)
Data_2012[DesOrder,1:3]



## Code to make figure embedded in lecture slide ##

png(file="Fig_Rush_Pass.png",width=800,height=700,pointsize=20)
par(mfrow=c(1,1),family="serif",mgp = c(1.5, 0.5, 0)) # mai=c(0.4,0.4,0.2,0.2),
hist(NFLdata$RatioRush_Pass, xlab="Rush attempts / Passing attempts")
dev.off()
