data = read.csv('GASTURBINE.txt', sep='\t')

alpha = 0.05
n = length(data$HEATRATE)

mu = 10000

x_bar = mean(data$HEATRATE)
s = sd(data$HEATRATE)

z = (x_bar - mu) / (s/sqrt(n))

p_val = pnorm(z, lower.tail=FALSE)

# true if we should reject the null hypothesis (pval is less than alpha)
conclusion = p_val < alpha
