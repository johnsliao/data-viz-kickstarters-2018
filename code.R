# John Liao
# CS 544 - FALL 2018
# U47-74-9883
# Final Project

# -- Preparing the data
data <- read.csv('/home/john/workspace/cs544/kickstarter_data.csv', stringsAsFactors=FALSE)

# Only interested in US data
data <- subset(data, country == 'US')

# Convert string dates column values to date objects
data$launched <- as.Date(data$launched, '%Y-%m-%d %H:%M:%S')  
data$deadline <- as.Date(data$deadline, '%Y-%m-%d')

# Interested in data that had money $1000+ pledged to it; backers > 25; state is canceled, failed, successful
data <- subset(data, usd.pledged > 1000)
data <- subset(data, backers > 25)
data <- subset(data, state != 'live')
data <- subset(data, state != 'suspended')

# Note: Sample #1/#2 are gathered using the same methods. 
# Simply uncomment the lines in corresponding code to calculate.

# SAMPLE #1: REGULAR KICKSTARTERS
# <$1M pledged, >$1K pledged
data_sample <- subset(data, usd.pledged > 1000)
data_sample <- subset(data_sample, usd.pledged < 1000000)

# SAMPLE #2: UNICORN KICKSTARTERS
# $1M+ Pledged
# data_sample <- subset(data, usd.pledged > 1000000)
# data_sample

# -- Analyzing the data

# Analysis of at least one categorical variable
d <- table(data_sample['category'])*100/sum(table(data_sample['category']))
df <- data.frame(d)

nrow(df)  # Total Number of unique categories
nrow(data_sample['category'])  # Total Number of samples
head(df[with(df, order(-Freq)),], n=10)  # Percentage breakdown of categories

barplot(table(data_sample['main_category']), las=2, main='Histogram of Main Categories', ylab='Frequency', xlab='Category')
pie(table(data_sample['state']), main="Pie Chart of States")

# Analysis of at least one numerical variable
hist(log(data_sample[['usd.pledged']]), xlab='log($ Pledged) (USD)', main='Histogram of $ Pledged (USD)')

# Analysis for at least one set of two or more variables
addmargins(table(data_sample[['state']], data_sample[['main_category']]))
format(prop.table(table(data_sample[['state']], data_sample[['main_category']])) * 100, scientific = FALSE)

mosaicplot(table(data_sample[['state']], data_sample[['main_category']]), las=1, main='Mosaic plot of Category and Final State', color=c('red','cyan'))
barplot(table(data_sample[['state']], data_sample[['main_category']]), beside=TRUE, legend.text=TRUE, col=rainbow(3), main='Histogram of Final State')

# Pick one variable with numerical data and examine the distribution of the data
fivenum(data_sample[['backers']])
hist(log(data_sample[['backers']]), main='Histogram of Backers', xlab='log(# of backers)')

boxplot(log(data_sample[['backers']]), horizontal=TRUE, xaxt="n", main='Boxplot of backers', xlab='log(# of backers)')
axis(side=1, at=round(fivenum(log(data_sample[['backers']])), 1), labels=TRUE, las=2)

# Draw various random samples of the data and show applicability of the Central Limit Theorem for this Variable
mean_population <- mean(log(data_sample[['backers']]))
sd_population <- sd(log(data_sample[['backers']]))

draw_samples <- function(number_samples) {
  x <- log(data_sample[['backers']])
  sample_size <- 10
  xbar <- numeric(length(samples))
  
  for (i in 1:samples) {
    xbar[i] <- mean(sample(x, size=sample_size, replace=FALSE))
  }
  return(xbar)
}

mean_5_samples <- draw_samples(5)
mean_50_samples <- draw_samples(50)
mean_500_samples <- draw_samples(500)

sample_mean_5 <- mean(mean_5_samples)
sample_sd_5 <- sd(mean_5_samples)

sample_mean_50 <- mean(mean_5_samples)
sample_sd_50 <- sd(mean_5_samples)

sample_mean_500 <- mean(mean_500_samples)
sample_sd_500 <- sd(mean_500_samples)

hist(mean_5_samples, prob=TRUE, main='Histogram of densities of sample mean of # backers\nSAMPLES=5, SAMPLE SIZE=10', xlab='log(# backers)')
hist(mean_50_samples, prob=TRUE, main='Histogram of densities of sample mean of # backers\nSAMPLES=50, SAMPLE SIZE=10', xlab='log(# backers)')
hist(mean_500_samples, prob=TRUE, main='Histogram of densities of sample mean of # backers\nSAMPLES=500, SAMPLE SIZE=10', xlab='log(# backers)')

# Drawing conclusions if these samples are used instead of the whole dataset
# The population data is skewed to the left. Using central limit theorem shows a normal distribution around the population mean.
# As sample size goes up, the mean of sample means gets closer to the true mean and the sd of sample means decreases
# Increase in the sample sizes result in the distribution becoming less skewed and slowly approaching the shape of a normal distribution

# Show how various sampling methods can be used on your data

# Sampling method #1: SRSWOR
library('sampling')

s <- srswor(100, nrow(data_sample['backers']))
sample.1 <- data_sample[s != 0,]
mean(log(sample.1[['backers']]))
fivenum(log(sample.1[['backers']]))

boxplot(log(sample.1[['backers']]), horizontal=TRUE, xaxt="n", main='Sampling Method #1: SRSWOR For # Backers', xlab='log(backers)')
axis(side=1, at=round(fivenum(log(sample.1[['backers']]))), 1, labels=TRUE, las=2)

# Sampling method #2: Systematic Sampling
data_sample$index <- seq.int(nrow(data_sample))  # Add index row for systematic sampling
N <- nrow(data_sample['backers']); N
n <- 100

# items in each group
k <- ceiling(N/n)
r <- sample(k, 1)

# select every kth item
sequence <- seq(r, by=k, length=n); sequence
sample.2 <- subset(data_sample, index %in% sequence)
mean(log(sample.2[['backers']]))
fivenum(log(sample.2[['backers']]))

boxplot(log(sample.2[['backers']]), horizontal=TRUE, xaxt="n", main='Sampling Method #2 For # Backers: Systematic Sampling',  xlab='log(backers)')
axis(side=1, at=round(fivenum(log(sample.1[['backers']]))), 2, labels=TRUE, las=2)

# -- Implementing of any feature(s) not mentioned in the specification
# Item #1: Graphing amount pledged vs goal
pledged <- data_sample[['pledged']]
goal <- data_sample[['goal']]
plot(log(pledged), log(goal), pch='.', main='Scatterplot of pledges vs goal')

# Item #2: Deadline vs launched
plot(data_sample[['launched']], data_sample[['deadline']], pch='.', main='Scatterplot launched date vs deadline', xlab='Launched (year)', ylab='Deadline (year)')
launched_after_deadline <- data_sample[['launched']] > data_sample[['deadline']]  # Add col Launched after deadline
table(launched_after_deadline)['TRUE']  #  Number that launched after deadline

# Item #2a: Calculate number of days
delta_from_launch <- data_sample[['deadline']] - data_sample[['launched']]
delta_from_launch  # Time differences in days
hist(as.vector(delta_from_launch), main='Histogram of how many days kicker starters launched before deadline', xlab='Days')


