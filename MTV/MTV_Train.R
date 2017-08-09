library(glmnet)

data<- read.csv('MTV_Data.csv')
rundate <- as.Date(format(Sys.time(), "%Y-%m-%d"))

data$Age <- 2017 - data$Year.Built
data$Lot.Size[which(is.na(data$Lot.Size))] <- median(data$Lot.Size,na.rm=T)

data$Last.Sale.Date <- as.Date(data$Last.Sale.Date,format='%m/%d/%Y')
data$Days_Diff <- rundate - data$Last.Sale.Date

data <- data[,8:ncol(data)]

train <- data[1:(nrow(data)-2),]
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
                      +IsCondo
)}

x <- model.matrix(object = formula, data=train)
y <- train$Last.Sale.Price





model <- lm(formula=formula,data=train)
#model2 <- glmnet(x,y,alpha=0.5,nlambda=50,family='gaussian',lambda.min.ratio = 0.01)
model2 <- glmnet(x,y,alpha=0.5,lambda=46846.52,family='gaussian')

predict(model,test)

new_test <- test
new_test$Last.Sale.Price <- 0
predx <- model.matrix(object=formula,data=new_test)
predict(model2, newx=predx)

model$coefficients
model2$lambda





