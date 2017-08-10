library(glmnet)

data<- read.csv('MTV_Data.csv')
rundate <- as.Date(format(Sys.time(), "%Y-%m-%d")) + 3

data$Age <- 2017 - data$Year.Built
data$Lot.Size[which(is.na(data$Lot.Size))] <- median(data$Lot.Size,na.rm=T)

data$Last.Sale.Date <- as.Date(data$Last.Sale.Date,format='%m/%d/%Y')
data$Days_Diff <- rundate - data$Last.Sale.Date



data <- subset(data,Zip==94043 & IsCondo==0)

data <- data[,6:ncol(data)]



train <- data[1:(nrow(data)-8),]

validation <- data[(nrow(data)-7):(nrow(data)-2),]

test <- data[(nrow(data)-1):nrow(data),]

formula = {as.formula(Last.Sale.Price 
                      ~Beds
                      +Baths
                      +Sqft
                      +Lot.Size
                      +HOA_Due
                      +ES_Score
                      +MS_Score
                      +HS_Score
                      +Age
                      +Days_Diff
                      +Walk_Score
                      +Transit_Score
                      +Views
                      +Favorites
                      +Cross_Outs
)}

x <- model.matrix(object = formula, data=train)
y <- train$Last.Sale.Price





#model <- lm(formula=formula,data=train)
#predict(model,test)

#model2 <- glmnet(x,y,alpha=0.05,family='gaussian',lambda.min.ratio = 0.02015)
model2 <- glmnet(x,y,alpha=0.05,lambda=148896,family='gaussian',lambda.min.ratio = 0.02015)


new_validation <- validation
validx <- model.matrix(object=formula,data=new_validation)
predict(model2, newx=validx)

new_test <- test
new_test$Last.Sale.Price <- 0
predx <- model.matrix(object=formula,data=new_test)
predict(model2, newx=predx)

model$coefficients
mean(model2$lambda[seq(60,66)])
model2$lambda[65]


