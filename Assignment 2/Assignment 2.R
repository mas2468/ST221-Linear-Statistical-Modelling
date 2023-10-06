houses <- read.csv("houses.csv")
summary(houses)

# Question 1

# a)

log2livingArea = log2(houses$livingArea)
slr.houses1 = lm(price ~ log2livingArea, data=houses)
summary(slr.houses1)

# b)

coef(slr.houses1)[2]

# d)

houses$fuel = factor(houses$fuel)
cols <- c("darkgreen", "darkviolet", "blue")
index.type <- as.numeric(dta$Type)
plot(houses$price, ylab="House prices (in 1000 usd)", main="Scatterplot of houses prices identified with fuel used",pch=index.type, col=cols[index.type],xlim=c(0,900))
legend("bottomright", legend = levels(houses$fuel),
       col=cols, pch=c(1,2,3),cex=0.8,title="Fuel")

# e)

slr.houses2 = lm(price ~ 0 + factor(fuel), data = houses)
coef(slr.houses2)

# f)

M0 = lm(price ~ bathrooms, data=houses)
M1 = lm(price ~ 0+ factor(bathrooms), data=houses)
x = seq(1,2.5,0.5)
y0 = 1000*predict(M0, newdata = list(bathrooms=x))
y1 = 1000*predict(M1, newdata = list(bathrooms=x))
plot(x,y0,ylab="Predicted Price of House (in US dollars)",
     xlab="Number of Bathrooms",
     main="Scatterplot of Predicted Price against Bathrooms", pch=19, col="red",
     ylim=c(160000,225000))
points(x,y1,pch=17,col="blue")
legend("bottomright", pch=c(19,17),col=c("red","blue"),legend=c("M0","M1"),title="Model")

# g)

# not too sure and too long for now

# Question 2

# e)

slr.houses3 = lm(price ~ fuel + bathrooms + log2livingArea, data=houses)
which.max((cooks.distance(slr.houses3)))
cooks.distance(slr.houses3)[455]
max(cooks.distance(slr.houses3))
plot(slr.houses3,5)

# f)

LM2 = lm(price~log2livingArea+bathrooms+fuel,data=houses)
anova(LM2)
LM1 = lm(price ~ log2livingArea+bathrooms,data=houses)
null = lm(price ~ rep(mean(price),length(price)),data = houses)
anova(null,LM1)

# g)

anova(LM1,LM2)

# Question 3

# a)

boxplot(houses$price~houses$bedrooms,ylab="Price of House (in 1000 US dollars)",xlab="Number of Bedrooms",main="Boxplots of Predicted Price against bedrooms")

# b)

slr.houses4 = lm(price ~ fuel + bathrooms + log2livingArea + bedrooms, data = houses)
summary(slr.houses4)

# c)

VIFlm1 = lm(bedrooms ~ bathrooms + log2livingArea, data=houses)
VIFlm2 = lm(log2livingArea ~ bathrooms + bedrooms, data=houses)
CoD1 = summary(VIFlm1)$r.squared
CoD2= summary(VIFlm2)$r.squared
VIF1 = 1/(1-CoD1)
VIF1
VIF2 = 1/(1-CoD2)
VIF2
