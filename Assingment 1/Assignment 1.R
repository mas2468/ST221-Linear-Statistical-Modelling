dia <- read.csv("dia.csv")

# Question 1

# a)

slr.dia <- lm(dia$price~dia$width)
plot(dia$width,dia$price, ylab="price of diamond (in US dollars)",
     xlab="width of diamond (in mm)",
     main="Scatterplot of price against width", pch=20)
abline(slr.dia,col="red", lwd=2)

# b)

plot(slr.dia,1)

# the variance of the residuals is clearly not constant as for larger fitted values the residuals are further away from the smoother so the model assumption of heteroscedasticity is violated

# c)

# if you log transformed price as scatterplot looks like an exponential curve

log_price=log(dia$price)

slr.logdia <- lm(log_price~dia$width)
plot(slr.logdia,1)

# I would say the residuals plot is now acceptable as the smoother is close to a horiztonal line at 0 and the variance of the residuals seems to be constant across all the fitted values

# d)

coef(slr.logdia)

# so as the slope is 1 for each 20% increase in width, the average price of a diamond is expected to multiply by e^0.2 so increase by about 22 percent

# Question 2

# a)

dia$size <- ifelse(dia$weight<0.5, "small",
                   ifelse(dia$weight<1, "medium", "large"))
dia$size <- factor(dia$size, levels=c("small", "medium", "large"))

slr.dia2 = lm(log_price~dia$weight)
plot(slr.dia2,1)

# I don't think there are any features that make this an unacceptable residual plot as the smoother is relatively close to a horizontal line at 0 and the residuals are generally evenly spread around the smoother across all values

# b)

# paper

# c)

slr.dia3=lm(log_price~dia$weight+dia$size+dia$weight:dia$size)
plot(slr.dia3,1)

# d)

coef(slr.dia3)


# e)

cubed_weight=(dia$weight)^(1/3)
slr.dia4=lm(log_price~cubed_weight)
plot(slr.dia4,1)

# f)

# penis penis

plot(dia$weight,dia$price)
plot(dia$weight,log_price)
abline(coef(slr.dia3)[1],coef(slr.dia3)[2])
abline(coef(slr.dia3)[1]+coef(slr.dia3)[3],coef(slr.dia3)[2]+coef(slr.dia3)[5])
abline(coef(slr.dia3)[1]+coef(slr.dia3)[4],coef(slr.dia3)[2]+coef(slr.dia3)[6])
