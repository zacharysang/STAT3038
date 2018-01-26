# A = 3
# B = 7
# A + B
# A - B
# A / B
# A * B
# C = A / B
# C

# Working with data
NFLdata = read.table(file='NFL12_14.txt', header=TRUE)
Y = NFLdata[,'RatioRush_Pass']

# can use this command to output an image

png(filename='test2.png')
hist(Y, breaks=10, main='Your title', xlab='x-axis')

dev.off() #close the io stream and save the image file

# Normal distribution stuff
pnorm(q=0) # where q is the z-score (standardized value) and output is the probability (0.5)
qnorm(p=0.025) # where p is the area. output is the corresponding z-score
# ^ in above command, can use lower.tail to specify if this is on the upper or lower tail?

# Estimating population mean
ybar = mean(Y)
s = sd(Y)
n = length(Y)

alpha = 0.05
z_alpha_5 = qnorm(p=(alpha/2),lower.tail=FALSE)
LowerBound_5 = ybar - z_alpha_5 * s / sqrt(n)
UpperBound_5 = ybar + z_alpha_5 * s / sqrt(n)
interval_5 = c(LowerBound_5, UpperBound_5)

alpha = 0.01
z_alpha_1 = qnorm(p=(alpha/2),lower.tail=FALSE)
LowerBound_1 = ybar - z_alpha_1 * s / sqrt(n)
UpperBound_1 = ybar + z_alpha_1 * s / sqrt(n)
interval_1 = c(LowerBound_1, UpperBound_1)

# t-distribution
n1 = 10
t_alpha_5
