df <- read.csv(file.choose()) #nominate data
head(df)
table(df$party_code)
df$party_code[df$party_code == 100] <- 1
df$party_code[df$party_code == 200] <- 0
df$party_code[df$party_code == 328] <- NA
## 1 = Dem, 0 = Rep
names(df$party_code)[1] <- "Dem"
names(df$party_code)[0] <- "Rep"
table(df$party_code)
table(df$chamber)
summary(df$nominate_dim1); boxplot(df$nominate_dim1)
df$age <- 2018-df$born
summary(df$age); boxplot(df$age)

##YOUR TURN
model <- lm(nominate_dim1~party_code+chamber+age, data=df)
summary(model)
#when party control switches from republican to democrat, the ideology score decreases by 0.86 points, only when chamber is house.
#when chamber switches house to senate, ideology score increases by 0.03 points, only when party code is republican.
#for every one year increase in age, ideology score decreases by 0.002 of a point, other variables constant.
#when age is at 0 units, republicans are in control and the chamber is house, ideology score is predicted to be 0.63.

model2 <- lm(nominate_dim1~age+party_code*chamber, data=df)
summary(model2)
library(interplot)
interplot(m=model2, var1="party_code", var2="chamber")
library(effects)
effect("party_code*chamber", model2) #predicted values for not-possible combinations
0.6263196-0.0023476*mean(df$age, na.rm=T) #party code=0 and senate=1
0.6263196+0.0143781-0.0023476*mean(df$age, na.rm=T) #party code=0 and senate=1
#non-significant interaction since interplot overlaps a lot
#ideology scores get weaker as the chamber goes from house to senate

model3 <- lm(nominate_dim1~party_code+age*chamber, data=df)
summary(model3)
interplot(m=model3, var1="chamber", var2="age")
interplot(m=model3, var1="age", var2="chamber")
effect("age*chamber", model3)
#senate is more conservative than the house when age is less and as age increases there is not much of a difference
#as age goes up by one year, ideology scores are 0.002 more liberal when the chamber is house

df$age.s <- scale(df$age) #age scaled
model4 <- lm(nominate_dim1~party_code+age.s*chamber, data=df)
summary(model4)
#how much more conservative is the senate relative to the house when members are at their mean age
#one standard deviation increase in age leads to a predicted decrease of 0.0235

#Why we should use continous DVs
library(car)
bad.model <- lm(as.numeric(cyl)~mpg+wt, data=mtcars)
summary(bad.model)
crPlots(bad.model)
residualPlots(bad.model)
qqPlot(bad.model$residuals)

#Polytomous Qualitative Variables
df$party_code[is.na(df$party_code)] <- 2
table(df$party_code)
is.factor(df$party_code)
model5 <- lm(nominate_dim1~party_code+chamber+age, data=df)
summary(model5)
#party_code: slope coefficient is the same whether we are going from 0 to 1 or 1 to 2
which(is.na(df$nominate_dim1)) #what observations are being dropped
df$bioname[1]; df$bioname[320]
df$party_code <- as.factor(df$party_code)
model6 <- lm(nominate_dim1~party_code+chamber+age, data=df)
summary(model6) #reference category is republican (first factor is 0 to 1 and second is 1 to 2 coded as 0 to 1)
df.na <- subset(df, !is.na(df$nominate_dim1)) #drop 2 observations
with(df.na, tapply(nominate_dim1, party_code, mean))
library(effects)
effect("party_code", model6) #party_code needs to be a factor variable, gives adjusted means
library(car)
linearHypothesis(model6, "party_code1=party_code2") #tests whether coefficients are significantly different from a certain value or one another
#cannot reject null that coefficients are equal to one another
df$party_code <- relevel(df$party_code, ref = "2") #make independence the reference category
summary(lm(nominate_dim1~party_code+chamber+age, data=df))
#democrats not significantly different from independents

model7 <- lm(mpg~wt+cyl+am, data = mtcars)
summary(model7)
table(mtcars$cyl)
mtcars$cyl.factor <- as.factor(mtcars$cyl)
model7 <- lm(mpg~wt+cyl.factor+am, data = mtcars)
summary(model7) #comparing 6 to 4 and 8 to 4, coefficient for 8 should be double of 6 cylinder coefficient

