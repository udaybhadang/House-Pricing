setwd('C:/Users/Uday.Bhadang/Documents/R/DataSet/Kaggle/House Pricing')

HSTrain <- read.csv('train.csv')
HSTest <- read.csv('test.csv')
HSTrain_1 <- HSTrain[,1:80]
Combined <- rbind(HSTrain_1, HSTest)
str(HSTrain)


summary(HSTrain)
summary(HSTest)
install.packages('mice')
library(mice)
ImputationData <- Combined[,c('MasVnrArea', 'LotFrontage', 'GarageYrBlt', 'GarageCars', 'GarageArea','BsmtHalfBath', 'BsmtFullBath', 'TotalBsmtSF', 'BsmtUnfSF', 'BsmtFinSF2', 'BsmtFinSF1')]
summary(ImputationData)
imputed <- complete(mice(ImputationData))
summary(imputed)
Combined[,c('MasVnrArea', 'LotFrontage', 'GarageYrBlt', 'GarageCars', 'GarageArea','BsmtHalfBath', 'BsmtFullBath', 'TotalBsmtSF', 'BsmtUnfSF', 'BsmtFinSF2', 'BsmtFinSF1')] <- imputed[,c('MasVnrArea', 'LotFrontage', 'GarageYrBlt', 'GarageCars', 'GarageArea','BsmtHalfBath', 'BsmtFullBath', 'TotalBsmtSF', 'BsmtUnfSF', 'BsmtFinSF2', 'BsmtFinSF1')]
HSTrainImp <- Combined[1:1460, ]
HSTest <- Combined[1461:2919,]
HSTrainImp[,81] <- HSTrain[,81]
colnames(HSTrainImp)[81] <- 'SalePrice'
str(HSTrainImp)

lm1 <- lm(SalePrice ~  MSSubClass +	 LotFrontage+	 LotArea+	 OverallQual+	 OverallCond+	 YearBuilt+	 YearRemodAdd +	 MasVnrArea +	 BsmtFinSF1 +	 BsmtFinSF2 +	 BsmtUnfSF+	 TotalBsmtSF+	 X1stFlrSF+	 X2ndFlrSF+	 LowQualFinSF +	 GrLivArea+	 BsmtFullBath +	 BsmtHalfBath +	 FullBath +	 HalfBath +	 BedroomAbvGr +	 KitchenAbvGr +	 TotRmsAbvGrd +	 Fireplaces +	 GarageYrBlt+	 GarageCars +	 GarageArea +	 WoodDeckSF +	 OpenPorchSF+	 EnclosedPorch+	 X3SsnPorch +	 ScreenPorch+	 PoolArea +	 MiscVal+	 MoSold +	 YrSold, data = HSTrainImp)
Steplm1<-step(lm1)

summary(lm1)
summary(Steplm1)
predictlm1 <- predict(Steplm1, newdata = HSTest)
submittion <- cbind(HSTest[,1],predictlm1)
write.csv(submittion,'Submittion.csv' )
HSTrainImpInt <- HSTrainImp[,c('MSSubClass',	'LotFrontage',	'LotArea',	'OverallQual',	'OverallCond',	'YearBuilt',	'YearRemodAdd',	'MasVnrArea',	'BsmtFinSF1',	'BsmtFinSF2',	'BsmtUnfSF',	'TotalBsmtSF',	'X1stFlrSF',	'X2ndFlrSF',	'LowQualFinSF',	'GrLivArea',	'BsmtFullBath',	'BsmtHalfBath',	'FullBath',	'HalfBath',	'BedroomAbvGr',	'KitchenAbvGr',	'TotRmsAbvGrd',	'Fireplaces',	'GarageYrBlt',	'GarageCars',	'GarageArea',	'WoodDeckSF',	'OpenPorchSF',	'EnclosedPorch',	'X3SsnPorch',	'ScreenPorch',	'PoolArea',	'MiscVal',	'MoSold',	'YrSold',	'SalePrice')]
CorHSTrain<- cor(HSTrainImpInt)
write.csv(CorHSTrain, 'Co-relation.csv')

CorHSTrain <- cor(HSTrainImpInt)

install.packages('corrplot')
library(corrplot)
corrplot(CorHSTrain>0.8)


