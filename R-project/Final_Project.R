# Final Project

# Description:
# ==============================================================================================================
# Task 1. Pick two separate corporations and from www.nasdaq.com,
#         pick the “historical quote” option and randomly select 30 opening stock prices for the last two years.
# Note: this step is already done in Excel.

# Task 2.
# Determine a claim based on two population means/samples. Claim "Mean_1 is equal Mean_2" is chosen.

# Task 3.
# Calculate the descriptive statistics for the two samples.
# The distribution shape should be analyzed to determine the appropriate descriptive statistics to use
# (mean/standard deviation versus median/IQR).
# The graph should be built. Find the appropriate descriptive statistics
# noting any outliers or any irregularities discovered.
# ==============================================================================================================

# Task 1: Download data
global_data_frame <- read.csv(file = "d:/sample_prices.csv")

fb_median <- median(global_data_frame$FB)
apple_median <- median(global_data_frame$AAPL)
fb_iqr <- IQR(global_data_frame$FB)
apple_iqr <- IQR(global_data_frame$AAPL)

output_stats <- matrix(nrow = 2, ncol = 2)
output_stats[1,1]<-fb_median
output_stats[1,2]<-fb_iqr
output_stats[2,1]<-apple_median
output_stats[2,2]<-apple_iqr
colnames(output_stats) <- c('median', 'IQR')
rownames(output_stats) <- c('Facebook', 'Apple')
output_stats <- as.data.frame(output_stats)
output_stats

hist(global_data_frame$FB, xlab = 'Price', ylab = 'Frequency', main = 'FB price distribution', col = c('red', 'blue', 'yellow','cyan','magenta','green'))
hist(global_data_frame$AAPL, xlab = 'Price', ylab = 'Frequency', main = 'AAPL price distribution', col = c('red', 'blue', 'yellow','cyan','magenta','green'))
boxplot(global_data_frame$FB, main = 'FB price distribution')
boxplot(global_data_frame$AAPL, main = 'AAPL price distribution')

test_results <- t.test(global_data_frame$FB, global_data_frame$AAPL, alternative = "two.sided")
p_value <-test_results$p.value
t_value <-test_results$statistic

output <- matrix(nrow = 1, ncol = 2)
output[1,1]<-p_value
output[1,2]<-t_value
colnames(output) <- c('p.value', 't.value')
rownames(output) <- 't.test results'
output <- as.data.frame(output)
output

