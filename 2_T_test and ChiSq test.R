qnorm(0.05)

?qnorm
(8.12 - 8)/(0.72/9)

(2000 - 1891)/(251/5)

qt(0.025, 24)

(861 - 900)/(59/sqrt(7))

qt(0.05, 6)



library(SDSFoundations)


#bull <- subset(BullRiders, BullRiders$Events12 = 

age <- 2012 - bull$YearBorn

age

hist(age)

t.test(age, mu = 30)


#2nd week pre lab
bull <- BullRiders
str(bull)

bull$YearBorn

#Select bull riders from the US
USA <-bull[bull$Country=="USA",]

# Summarize the bull rider weights
mean(USA$Weight)
sd(USA$Weight)

# Visualize the weight distribution
hist(USA$Weight, main='Histogram of US Bull Rider Weights',xlab='Weight (lbs)')

# Run the single sample t-test
t.test(USA$Weight, mu=190)


lab# 2nd week lab

bull <- BullRiders
str(bull)

bullAtLeast5 <- bull[bull$Events14 >= 5,]

mean(bullAtLeast5$RidePer14)

sd(bullAtLeast5$Rides14)

bull$YearBorn

#Select bull riders from the US
USA <-bull[bull$Country=="USA",]

# Summarize the bull rider weights
mean(USA$Weight)
sd(USA$Weight)

# Visualize the weight distribution
hist(USA$Weight, main='Histogram of US Bull Rider Weights',xlab='Weight (lbs)')

# Run the single sample t-test
t.test(bullAtLeast5$Rides14 - bullAtLeast5$BuckOuts14, mu=50)



1/0.707

pre <- c(79, 95, 85, 82)

post <- c(80,
          94,
          87,
          84)

t.test(pre, post, paired = T)

qt(0.05, 9)
library(SDSFoundations)

post <- PostSurvey

str(post)

# 3rd week Lab Question 1

# Make a vector of happiness scores for each sample
underclass_happy <- post$happy[post$classification=='Freshman'|post$classification=='Sophomore']
upperclass_happy <- post$happy[post$classification=='Junior'|post$classification=='Senior']

# Check the normality assumption
hist(underclass_happy, xlab='Underclassman Happiness', main='Percent of Time Happy')
hist(upperclass_happy, xlab='Upperclassman Happiness', main='Percent of Time Happy')

# Run independent t-test
t.test(underclass_happy, upperclass_happy)

#Lab Question 2

# Make a vector of difference scores
post$diff_happy <- post$happy - post$post_happy
str(post$diff_happy)

hist(post$happy)
hist(post$post_happy)
# Check the normality assumption
hist(post$diff_happy, xlab= 'Difference in Happiness over the Semester', main = 'Happy-Post Happy')

# Run dependent t-test
t.test(post$happy, post$post_happy, paired=T)

mean(post$happy) - mean(post$post_happy)


#week 4

qchisq(0.95, 2)

16/20 + 16/28

#week 4 pre lab

acl <- AustinCityLimits

str(acl)

acl$Year[acl$Artist == "Allen Toussaint"]
acl$Age[acl$Artist == "Allen Toussaint"]


acl[acl$Artist == "Allen Toussaint",]
str(acl[acl$Artist == "Allen Toussaint",])

summary(acl[acl$Artist == "Allen Toussaint",])




# Question 1 (Goodness of Fit)
# Create a table of counts for Gender
gender_tab <-table(acl$Gender)
gender_tab

# Create vector of expected proportions
ExpGender <- c(.50, .50)

# Check expected counts assumption
chisq.test(gender_tab, p=ExpGender)$expected

# Run goodness of fit
chisq.test(gender_tab, p=ExpGender)


# Question 2 (Test of Independence)
# Create two-way table
gender_top10 <-table(acl$Gender, acl$BB.wk.top10)
gender_top10


# Generate expected counts
chisq.test(gender_top10, correct=FALSE)$expected

# Run test of independence
chisq.test(gender_top10, correct=FALSE)


117/99

60/96

19/0.625

1-((1-0.05)^6)

