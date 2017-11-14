HSTrainImpInt1 <- HSTrainImpInt
HSTrainImpInt1$GarageAreaByCars <- (HSTrainImpInt$GarageArea/HSTrainImpInt$GarageCars)


lm3 <- lm(SalePrice ~  GarageArea+MSSubClass +	 LotFrontage+	 LotArea+	 OverallQual+	 OverallCond+	 YearBuilt+	 YearRemodAdd +	 MasVnrArea +	 BsmtFinSF1 +	 BsmtFinSF2 +	 BsmtUnfSF+	X1stFlrSF+	 X2ndFlrSF+	 LowQualFinSF +	 	 BsmtHalfBath +	 FullBath +	 	 BedroomAbvGr +	 KitchenAbvGr +	 Fireplaces +	WoodDeckSF +	 OpenPorchSF+	 EnclosedPorch+	 X3SsnPorch +	 ScreenPorch+	 PoolArea +	 MiscVal+	 MoSold +	 YrSold, data = HSTrainImpInt1)
summary(lm3)
summary(lm2)
Steplm3<- step(lm3)
Steplm2<- step(lm2)
summary(Steplm2)

corrplot(cor(HSTrainImpInt1[,c('GarageArea',	'MSSubClass',	'LotFrontage',	'LotArea',	'OverallQual',	'OverallCond',	'YearBuilt',	'YearRemodAdd',	'MasVnrArea',	'BsmtFinSF1',	'BsmtFinSF2',	'BsmtUnfSF',	'X1stFlrSF',	'X2ndFlrSF',	'LowQualFinSF',	'TotRmsAbvGrd',	'BsmtFullBath',	'BsmtHalfBath',	'FullBath',	'HalfBath',	'BedroomAbvGr',	'KitchenAbvGr',	'Fireplaces',	'WoodDeckSF',	'OpenPorchSF',	'EnclosedPorch',	'X3SsnPorch',	'ScreenPorch',	'PoolArea',	'MiscVal',	'MoSold',	'YrSold')])>0.6)

Prediction<-predict(lm2, newdata = HSTest)
Submittion2 <- cbind(HSTest$Id, Prediction)
write.csv(Submittion2, 'Submittion2.csv')

Prediction<-predict(Steplm2, newdata = HSTest)
Submittion3 <- cbind(HSTest$Id, Prediction)
write.csv(Submittion3, 'Submittion3.csv')
