library(SDSFoundations)

BikeData$BikeData[8,7]

test <- BikeData[,7]
typeof(test)

#1st week
13 / sqrt(36)

(77 - 74) / (13 / sqrt(36))

1 - pnorm(1.384615)

?pnorm

qnorm(0.95, 77, 2.166667)

qnorm(0.95, 77, 2.166667, lower.tail = F)

77 + 1.96 * (13 / 6)
77 - 1.96 * (13 / 6)

48.5 / sqrt(14)

(
qnorm(
        p = 0.95, mean = 157.1428571, sd = 12.96217, lower.tail = T
) -
        qnorm(
                p = 0.95, mean = 157.1428571, sd = 12.96217, lower.tail = F
        )
) / 2

(157.1428571 + 1.96 * (48.5 / sqrt(14))) - 157.1428571

(157.1428571 - 1.96 * (48.5 / sqrt(14)))

survey <- StudentSurvey

rm(ls =)

mean(survey$age)
sd(survey$age)

hist(survey$age)


sample(survey$age, size = 30)


# create an empty vector, called say, myXbar, which will eventually hold the




# means of all the samples that we draw from the population
# we will then write a for loop (with 1000 loops).
# In each run, we draw a sample and populate the next element of the empty mean vector

myXbar <- rep(NA, 1000)
myXbar

for(i in 1:1000){
        currSample <- sample(survey$age, size = 30)
        myXbar[i] <- mean(currSample)
        
}

#now for verifying the central limit theorem, or at least check what our means
#list looks like

#we first do a historgram of the means list, expecting to see a normal distribution

hist(myXbar)

#next we find out the mean of our sample means, expecting it to equal the population mean
mean(myXbar)

#next we find the SD of our sampleing distribution, expecting it to equal "popn SD/sqrt(sample size)"
sd(myXbar)

sd(survey$age)/sqrt(30)

View((survey))

str(survey)

#prelab
# Calculate the population parameters
hist(survey$name_letters)
fivenum(survey$name_letters)

mean(survey$name_letters)
sd(survey$name_letters)


# Draw 1,000 samples of n=5 and find the mean of each sample.
xbar5 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$name_letters, size =5)
xbar5[i] <-  mean(x)}


# Graph the histogram of 1,000 sample means.
hist(xbar5,xlim=c(2,10))


# Calculate the mean and sd of the sampling distribution.
mean(xbar5)
sd(xbar5)

# Compare to the std dev predicted by the CTL.
sd(survey$name_letters)/sqrt(5)


#Repeat for samples of size n=15
xbar15 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$name_letters, size =15)
xbar15[i] <- mean(x)}
hist(xbar15,xlim=c(2,10))
mean(xbar15)
sd(xbar15)
sd(survey$name_letters)/sqrt(15)


#Repeat for samples of size n=25
xbar25 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$name_letters, size =25)
xbar25[i] <- mean(x)}
hist(xbar25,xlim=c(2,10))
mean(xbar25)
sd(xbar25)
sd(survey$name_letters)/sqrt(25)


#Lab
# Calculate the population parameters
hist(survey$happy)
fivenum(survey$happy)

mean(survey$happy)
sd(survey$happy)


# Draw 1,000 samples of n=5 and find the mean of each sample.
xbar5 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$happy, size =5)
xbar5[i] <-  mean(x)}


# Graph the histogram of 1,000 sample means.
hist(xbar5,xlim=c(2,10))


# Calculate the mean and sd of the sampling distribution.
mean(xbar5)
sd(xbar5)

# Compare to the std dev predicted by the CTL.
sd(survey$happy)/sqrt(5)


#Repeat for samples of size n=15
xbar15 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$happy, size =15)
xbar15[i] <- mean(x)}
hist(xbar15,xlim=c(2,10))
mean(xbar15)
sd(xbar15)
sd(survey$happy)/sqrt(15)


#Repeat for samples of size n=25
xbar25 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$happy, size =25)
xbar25[i] <- mean(x)}
hist(xbar25,xlim=c(2,10))
mean(xbar25)
sd(xbar25)
sd(survey$happy)/sqrt(25)

mean(78.1372,77.90393,78.03772  )


#problem set 1

# Calculate the population parameters
hist(survey$austin)
fivenum(survey$austin)

mean(survey$austin)
sd(survey$austin)


# Draw 1,000 samples of n=5 and find the mean of each sample.
xbar5 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$austin, size =10)
xbar5[i] <-  mean(x)}


# Graph the histogram of 1,000 sample means.
hist(xbar5,xlim=c(2,10))


# Calculate the mean and sd of the sampling distribution.
mean(xbar5)
sd(xbar5)

# Compare to the std dev predicted by the CTL.
sd(survey$austin)/sqrt(10)


#Repeat for samples of size n=15
xbar15 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$austin, size =15)
xbar15[i] <- mean(x)}
hist(xbar15,xlim=c(2,10))
mean(xbar15)
sd(xbar15)
sd(survey$austin)/sqrt(15)


#Repeat for samples of size n=25
xbar25 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$austin, size =25)
xbar25[i] <- mean(x)}
hist(xbar25,xlim=c(2,10))
mean(xbar25)
sd(xbar25)
sd(survey$austin)/sqrt(25)

mean(78.1372,77.90393,78.03772  )


pnorm(3.2, 3.08, 0.4, lower.tail = F)

0.4/5

pnorm(2.9, 3.08, 0.08, lower.tail = F) - pnorm(3.2, 3.08, 0.08, lower.tail = F)



(35.1-28)/(11/sqrt(23))

pnorm(35.1, 28, (11/sqrt(23)), lower.tail = F)

#dixie queen

471.46+1.96*(1.5/sqrt(15))
471.46-1.96*(1.5/sqrt(15))

pnorm(471.46, 473.20, (1.5/sqrt(15)))



