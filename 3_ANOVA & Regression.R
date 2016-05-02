




#5th week pre lab

film <- FilmData

film$Rank[film$Film == "Titanic"]

temp <- film[film$Studio == "Uni.", ]
temp[order(temp$Rank),]

?sort
?order

filmsByRank <- film[order(film$Rank),]

head(filmsByRank,  10)

min(head(filmsByRank,  10)$IMDB)



?head

str(film)

#5th week prelab

# Show how many films are in each group
table(film$Rating)
str(film)
# Question 1

# Calculate avg film budget of each group
aggregate(Budget~Rating,film,mean)

# Calculate sd of film budget within each group
aggregate(Budget~Rating,film,sd)

# Visualize the group means and variability
boxplot(film$Budget~film$Rating, main= "Film Budgets by Rating",
        ylab= "Budget", xlab= "MPAA Rating")

# Run ANOVA
modelbud <- aov(film$Budget~film$Rating)
summary(modelbud)

# Run post-hoc test if F statistic is significant
TukeyHSD(modelbud)

# Question 2

# Calculate avg IMDB score of each group
aggregate(IMDB~Rating,film,mean)

#Calculate sd of IMDB scores within each group
aggregate(IMDB~Rating,film,sd)

# Visualize the group means and variability
boxplot(film$IMDB~film$Rating, main= "IMDB Scores by Rating",
        ylab= "IMDB Score", xlab= "MPAA Rating")

# Run ANOVA
modelscore <- aov(film$IMDB~film$Rating)
summary(modelscore)
modelscore
# Run post-hod text if F statistic is significant
TukeyHSD(modelscore)


table(film$Studio)

#5th week lab

# Show how many films are in each group
table(film$Studio)
str(film)
# Question 1

# Calculate avg days in theaters by studio 
aggregate(Days~Studio,film,mean)

# Calculate sd of days in theaters by studio 
aggregate(Days~Studio,film,sd)

# Visualize the group means and variability
boxplot(film$Days~film$Studio, main= "Days by Studio",
        ylab= "Days", xlab= "Studio")

# Run ANOVA
modelDays <- aov(film$Days~film$Studio)
summary(modelDays)

# Run post-hoc test if F statistic is significant
TukeyHSD(modelDays)

# Domestic Earnings question


# Calculate avg domestic earnings by studio 
aggregate(Pct.Dom~Studio,film,mean)

# Calculate sd of days in theaters by studio 
aggregate(Pct.Dom~Studio,film,sd)

# Visualize the group means and variability
boxplot(film$Pct.Dom~film$Studio, main= "Dom earnings by Studio",
        ylab= "Dom earnings", xlab= "Studio")

# Run ANOVA
modelDomEarnings <- aov(film$Pct.Dom~film$Studio)
summary(modelDomEarnings)

# Run post-hoc test if F statistic is significant
TukeyHSD(modelDays)


#5th week questions

attach(film)
film$budCat[Budget < 100] <- "low-budget"
film$budCat[Budget >= 100 & Budget < 150 ] <- "medium-budget"
film$budCat[Budget >= 150] <- "high-budget"
detach(film)

table(film$budCat)

# Domestic Earnings question


# Calculate avg domestic earnings by budget Category
aggregate(Pct.Dom~budCat,film,mean)

# Calculate sd of days in theaters by studio 
aggregate(Pct.Dom~budCat,film,sd)

# Visualize the group means and variability
boxplot(film$Pct.Dom~film$budCat, main= "Dom earnings by Studio",
        ylab= "Dom earnings", xlab= "BudCat")

# Run ANOVA
modelDomEarnings <- aov(film$Pct.Dom~film$budCat)
summary(modelDomEarnings)

# Run post-hoc test if F statistic is significant
TukeyHSD(modelDomEarnings)

qf(.95, df1=42, df2=2) 

qf(.95, df1=2, df2=42) 


SSW <- 5949.1 - 2387.7
SSW
(2387.7/2)/(3561.4/42)

qf(.95, df1=2, df2=15) 

391/((2147 - 782)/34)

0.05/3

#6th week prelab

library(SDSFoundations)

res <- TempskiResilience

#Subset into the Clinical Sciences
clin <- res[res$Group == "Clinical Sciences",]

# Question One
#Intial Correlations
vars <- c("QoL", "BDI")
cor(clin[,vars])

#RQ1 Model
ov_mod <- lm(QoL ~ BDI, data=clin)
summary(ov_mod)
confint(ov_mod)

#Diagnostics
plot(ov_mod, which=1)
cutoff <- 4/(ov_mod$df) 
plot(ov_mod, which=4, cook.levels=cutoff)

# Question Two
#Initial correlations
vars <- c("MS.QoL", "DREEM.S.SP", "DREEM.A.SP", "Resilience", "BDI", "Age")
cor(clin[,vars], use="pairwise.complete.obs")

#Test the initial correlations
library(psych)
corr.test(clin[,vars], use="pairwise.complete.obs")

#RQ2 Model
ms_mod <- lm(MS.QoL ~ DREEM.S.SP + DREEM.A.SP + Resilience + BDI + Age, data=clin)
summary(ms_mod)
confint(ms_mod)

#Diagnostics
library(car)
vif(ms_mod)
plot(ms_mod, which=1)
cutoff <- 4/(ms_mod$df) 
plot(ms_mod, which=4, cook.levels=cutoff)

#Put model into context
lmBeta(ms_mod) 
round(pCorr(ms_mod), 4) 
        

#6th week lab

res <- TempskiResilience

#Subset into the Clinical Sciences
basi <- res[res$Group == "Basic Sciences",]

str(basi)

# Question One
#Intial Correlations
vars <- c("MS.QoL","WHOQOL.PH", "WHOQOL.PSY","WHOQOL.SOC", "WHOQOL.ENV")
cor(basi[,vars])

#RQ1 Model
msqol_mod <- lm(MS.QoL ~ WHOQOL.PH + WHOQOL.PSY + WHOQOL.SOC + WHOQOL.ENV, data=basi)
summary(msqol_mod)
confint(msqol_mod)

#Diagnostics
plot(ov_mod, which=1)
cutoff <- 4/(ov_mod$df) 
plot(ov_mod, which=4, cook.levels=cutoff)

round(pCorr(msqol_mod), 4) 
lmBeta(msqol_mod) 

# Question Two
#Initial correlations
vars <- c("MS.QoL", "DREEM.S.SP", "DREEM.A.SP", "Resilience", "BDI", "Age")
cor(clin[,vars], use="pairwise.complete.obs")

#Test the initial correlations
library(psych)
corr.test(clin[,vars], use="pairwise.complete.obs")

#RQ2 Model
ms_mod <- lm(MS.QoL ~ DREEM.S.SP + DREEM.A.SP + Resilience + BDI + Age, data=clin)
summary(ms_mod)
confint(ms_mod)

#Diagnostics
library(car)
vif(ms_mod)
plot(ms_mod, which=1)
cutoff <- 4/(ms_mod$df) 
plot(ms_mod, which=4, cook.levels=cutoff)

#Put model into context
lmBeta(ms_mod) 
round(pCorr(ms_mod), 4) 


#6th week questions

# question # 1

res <- TempskiResilience

#Subset into the Clinical Sciences
clin <- res[res$Group == "Clinical Sciences",]

str(clin)

# Question One

#Intial Correlations
vars <- c("MS.QoL","WHOQOL.PH", "WHOQOL.PSY","WHOQOL.SOC", "WHOQOL.ENV")
cor(basi[,vars])

#RQ1 Model
msqol_mod <- lm(BDI ~ Age + Female + State.Anxiety +Trait.anxiety , data=clin)
summary(msqol_mod)
confint(msqol_mod)

#Diagnostics
plot(ov_mod, which=1)
cutoff <- 4/(ov_mod$df) 
plot(ov_mod, which=4, cook.levels=cutoff)

round(pCorr(msqol_mod), 4) 
lmBeta(msqol_mod) 


1848.76/69.22 * 18

23.4325/12.74
8.32 * 0.1528

1-  480.75/1848.76


qf(.95, df1=4, df2=88) 

3526.4  + 90.22*12 + 1.269*10 + 23.406 * 15


mean(c(29.4,29.0,28.4,28.8,28.9,29.3,28.5,28.2))


0.4257347/sqrt(8)

(28.8125 - 28.5)/(0.4257347/sqrt(8))

t.test(c(29.4,29.0,28.4,28.8,28.9,29.3,28.5,28.2), mu = 28.5)


qt(p = 0.05, df = 7)

(93.6 - 91)/(7.8/5)


qt(0.05, 11)

42.6 + (5.3/sqrt(12))*1.796
