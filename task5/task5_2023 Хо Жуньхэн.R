library(MASS)
library(aod)
library(lmtest)
library(broom)
library(pROC)
library(caret)
library(ROSE)
library(tidyverse)
library(boot)
library(fbroc)
df<-read.csv("D:\\CARS.csv")
df$Invoice<- as.numeric(gsub("[,\\$]", "", df$Invoice))
df<-na.omit(df)
#task1
#обработка данных
df1<-df
df1$Expensive[df1$Invoice>35000] <- 1
df1$Expensive[df1$Invoice<=35000] <- 0
df1$Cylinders<- ifelse(df1$Cylinders <= mean(df1$Cylinders), 0, 1)
# основной предиктор-Cylinders,стратифицирующий предиктор-MPG_City
Y<-df1$Expensive
X1<-df1$Cylinders
df1 <- subset(df1, df1$DriveTrain != "All")
chist_table <- table(Y, X1)
chisq.test(chist_table,correct = FALSE)
df1$Cylinders<- ifelse(df1$Cylinders <= mean(df1$Cylinders), 0, 1)
df1$MPG_City<- ifelse(df1$MPG_City<= mean(df1$MPG_City), 0, 1)
mantelhaen.test(xtabs(~Expensive+Cylinders+MPG_City,data=df1))
#task2
#обработка данных
df2<-df
df2$Cheap<-df2$Invoice>20000
glm_step<-glm(Cheap~1, data = na.omit(df2), family = binomial(link="logit"))
steps<-list()
AUCs<-numeric(0)
plot.new()
max_auc<-0
#variables_to_add <- c("EngineSize", "Length", "Weight", "Cylinders", "Wheelbase", "MPG_Highway")
# step
for(i in 1:10){
  glm_step<-step(glm_step,direction="forward",scope=~EngineSize + Type + Origin + Horsepower + Length + Weight + 
                   Cylinders + Wheelbase + MPG_City + MPG_Highway,k=0.000001,steps=1,trace=0)
  roc_glm <- roc(glm_step$y ~ glm_step$fitted.values)
  AUCs[i]<-roc_glm$auc
  if(roc_glm$auc>max_auc)
  {
    final_glm<-glm_step
    max_auc<-roc_glm$auc
  }
  if (i == 1) {
    plot(roc_glm, col=i)
  } else {
    plot(roc_glm, add = TRUE, col=i)
  }
  print(roc_glm$auc)
  control <- trainControl(method = "cv", number = 5, summaryFunction = twoClassSummary, classProbs = TRUE)
  df2_proved<-df
  df2_proved$Cheap <- ifelse(df2_proved$Invoice < 20000, "C", "NC")
  Trainning<-train(glm_step$formula,data = df2_proved,method = "glm",family = "binomial",trControl = control)
  print(Trainning)
}
print(AUCs)
#print(format(max_auc, nsmall=10))
num_variables <- 1:10
results <- data.frame(Number_of_Variables = num_variables, AUC = AUCs)
#график зависимости оценки качества (CV ROC AUC) от сложности (количества предикторов) модели 
ggplot(data = results, aes(x = Number_of_Variables, y = AUC)) +
  geom_line() + 
  geom_point() +  
  xlab("Number of Variables") +
  ylab("AUC") +
  ggtitle("AUC vs Number of Variables")
summary(final_glm)
#task3
#обработка данных
df3<-df
df3$Cheap<-df3$Invoice>20000
df3$Cheap<-ifelse(df3$Invoice<=20000, 0, 1)
df3<-na.omit(df3)
final_formula<-deparse(final_glm$formula)
final_formula<-paste0(final_formula[1],final_formula[2])
final_formula
#model_glm
model1<-glm(final_formula, data = df3, family = binomial(link="logit"))
majority_count <- max(table(df3$Cheap))
oversampled_df3 <- ovun.sample(Cheap ~ ., data = df3, method = "over", N = 2 * majority_count)$data
model2<-glm(final_formula, data = oversampled_df3, family = binomial(link="logit"))
#get the predictions
predictions1 <- predict(model1, df3, type = "response")
predictions2 <- predict(model2, oversampled_df3, type = "response")
#boot.roc
boot<- boot.roc(predictions1,df3$Cheap==1, n.boot = 1000)
boot_oversampled <- boot.roc(predictions2,oversampled_df3$Cheap==1, n.boot = 1000)
#ответ
plot(boot)
plot(boot_oversampled)
print(paste("model_AUC: ", boot$auc))
print(paste("model_oversampled_AUC: ", boot_oversampled$auc))

