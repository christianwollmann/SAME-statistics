#load packages and set working directory
library(car)
library(ggplot2)
library(pastecs)
library(psych)
library(Rcmdr)
setwd("C:\\Users\\chris\\Documents\\Uni\\SAME")

#1. Visual inspection


#read in dataframe
dlf <- read.delim("DownloadFestival(NoOutlier).dat")

#plot histogram of day1
hist.day1 <- ggplot(dlf, aes(day1)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x= "Hygiene score of day 1", y = "Density")
hist.day1 #visual inspection

#adding another layer to determine normal distribution
hist.day1 + stat_function(fun = dnorm, args = list(mean = mean(dlf$day1, na.rm =
 TRUE), sd = sd(dlf$day1, na.rm = TRUE)), colour = "black", size = 1)

#doint the same for day2 and day3
#day2:
hist.day2 <- ggplot(dlf, aes(day2)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x= "Hygiene score of day 2", y = "Density") + #plotting
  stat_function(fun = dnorm, args = list(mean = mean(dlf$day2, na.rm =TRUE), sd = sd(dlf$day2, na.rm = TRUE)), colour = "black", size = 1) #adding a normal distribution curve
hist.day2

#day3:
hist.day3 <- ggplot(dlf, aes(day3)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x= "Hygiene score of day 3", y = "Density") + #plotting
  stat_function(fun = dnorm, args = list(mean = mean(dlf$day3, na.rm =TRUE), sd = sd(dlf$day3, na.rm = TRUE)), colour = "black", size = 1) #adding a normal distribution curve
hist.day3

#Plotting a QQ-Plot
# plots the cumulative values we have in our data against the cumulative probability of a particular distribution; normal if straight line
qq_day1 <- ggplot(data = dlf, aes(sample = day1)) + geom_qq()
qq_day1 #more or less normal
qq_day2 <- ggplot(data = dlf, aes(sample = day2)) + geom_qq()
qq_day2 # heavy rails (kurtosis)
qq_day3 <- ggplot(data = dlf, aes(sample = day3)) + geom_qq()
qq_day3 #no normal distribution


#2. Quantifiying with numbers
describe(dlf$day1) #psych package required


stat.desc(dlf$day1, basic = FALSE, norm = TRUE) #pastecs package required
#Note that we have specified the variable day1 in the dlf dataframe, asked not to see the basic statistics (basic = FALSE) but asked to see the normality statistics (norm = TRUE). 

#using cbind() to look at several distributions:
describe(cbind(dlf$day1, dlf$day2, dlf$day3))
stat.desc(cbind(dlf$day1, dlf$day2, dlf$day3), basic = FALSE, norm = TRUE)
# Positive values of skew indicate a pile-up of scores on the left of the distribution, whereas negative values indicate a pile-up on the right. Positive values of kurtosis indicate a pointy and heavy-tailed distribution, whereas negative values indicate a flat and light-tailed distribution.

#IMPORTANT: look at standardized values of skew and kurtosis when comparing samples!
#significant when z > 2

#3. Exploring groups of data
rexam <- read.delim("C:\\Users\\chris\\Documents\\Uni\\SAME\\RExam.dat",header=T)

#using Uni as a factor to divide df
rexam$uni<-factor(rexam$uni, levels = c(0:1), labels = c("Duncetown University",
                                                     "Sussex University")) #creating a two level factor and assigning labels to both groups

#3.1 Verify distribution without grouping

#create histogram for exam scores
hist_uni1 <- ggplot(rexam, aes(exam)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x="first year exam scores", y = "density") #bimodal distribution

hist_uni1+ stat_function(fun = dnorm, args = list(mean = mean(rexam$exam, na.rm = TRUE), sd = sd(rexam$exam, na.rm = TRUE)), colour = "black", size = 1)

#quantify distribution with numbers
stat.desc(rexam, basic = FALSE, norm = TRUE) #significant when z > 2


#3.2 Verify distribution for different groups using by()

#quantifiying distribution with numbers:

#use by() to obtain seperate discriptive statistics for different factors
by(data = rexam$exam, INDICES = rexam$uni, FUN = describe)

by(rexam$exam, rexam$uni, stat.desc, basic = FALSE, norm = TRUE)


#obtain statistics for multiple variables, then we can use cbind() within by()
by(cbind(data=rexam$exam,data=rexam$numeracy), rexam$uni, describe)

#visual inspection of group distributions:
#creating different datasets using the subset() function:
dunceData<-subset(rexam, rexam$uni=="Duncetown University")
sussexData<-subset(rexam, rexam$uni=="Sussex University")

#numeracy distribution for duncetown
hist.numeracy.duncetown <- ggplot(dunceData, aes(numeracy)) + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Numeracy Score", y = "Density") + #plotting histogram
  stat_function(fun=dnorm, args=list(mean = mean(dunceData$numeracy, na.rm = TRUE), sd = sd(dunceData$numeracy, na.rm = TRUE)), colour = "blue", size=1)#fitting normal distribution 
hist.numeracy.duncetown 


#numeracy distribution for sussex
hist.numeracy.sussex <- ggplot(sussexData, aes(numeracy)) + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Numeracy Score", y = "Density") + #plotting histogram
  stat_function(fun=dnorm, args=list(mean = mean(sussexData$numeracy, na.rm = TRUE), sd = sd(sussexData$numeracy, na.rm = TRUE)), colour = "blue", size=1)#fitting normal distribution 
hist.numeracy.sussex 

#4. Doing the Shapiro-Wilk-Test

#for the global distribution
shapiro.test(rexam$exam) #significant
shapiro.test(rexam$numeracy) #significant

#for the subsets
by(rexam$exam, rexam$uni, shapiro.test) #no deviation from normal distribution
by(rexam$numeracy, rexam$uni, shapiro.test) #deviation from normal distribution

#QQ-Plots do define if data is distributed normal
qq_exam <- ggplot(data = rexam, aes(sample = exam)) + geom_qq()
qq_exam 

qq_numeracy <- ggplot(data=rexam, aes(sample=numeracy)) + geom_qq()
qq_numeracy


#5. Testing for Variance homogenecity
#using leveneTest() from car package
#first variable=outcome, second variable=factor
leveneTest(rexam$exam, rexam$uni) #default - centering the median
leveneTest(rexam$exam, rexam$uni, center = mean) #centering the mean

#Outcome: Pr(>F) !>0.5 -> no significant difference in variance to variance homogenecity

#6.Transformation of data

#log transformation for variable day1 of dlf
dlf$logday1 <- log(dlf$day1 + 1) #adding 1 because in day2 there is a value for 0
dlf$logday2 <- log(dlf$day2 + 1)
dlf$logday3 <- log(dlf$day3 + 1)

#plotting histograms for the transformed variables
hist_logday1 <- ggplot(dlf, aes(logday1)) + geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + labs(x = "Hygien Score day1", y = "Density")
hist_logday1


hist_logday2 <- ggplot(dlf, aes(logday2)) + geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + labs(x = "Hygien Score day2", y = "Density")
hist_logday2

hist_logday3 <- ggplot(dlf, aes(logday3)) + geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + labs(x = "Hygien Score day3", y = "Density")
hist_logday3
