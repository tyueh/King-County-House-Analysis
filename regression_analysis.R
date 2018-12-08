library(corrplot)
house_data = read.csv(file.choose(), header=T)

dim(house_data)
colnames(house_data)
mat = house_data[,3:21]

#### correlation matrix
cor(mat) 
corrplot(cor(mat), method='circle')
mat = mat[-4] ## remove sqft_living
corrplot(cor(mat), method='circle')
mat = mat[-10] ## remove sqft_above 
corrplot(cor(mat), method='circle')
mat = mat[-17] ## remove sqft_lot15 
corrplot(cor(mat), method='circle')
mat = mat[-13] ## remove zipcode 

## checking whether there are any missing values
sum(is.na(house_data))

## multiple regression by backward method
fit_back = step(lm(price~., data=mat), direction='backward')
summary(fit_back)

## visualize the importance of selected variables
library(caret)
Imp = varImp(fit_back, scale = FALSE)
a = unlist(Imp, use.names=F)
names(a) = names(selected_beta)
barplot(a, las=2, ylab='Importance', main='Selected variables')

## study their relationship with price
plot(mat[c('lat', 'price')], main='Latitude vs Price',xlab='latitude\nSouth                                                                                       North')
abline(lm(mat$price~mat$lat), col="red", lwd=3) 
plot(mat[c('waterfront', 'price')], main='Waterfront vs Price')
abline(lm(mat$price~mat$waterfront), col="red", lwd=3)
