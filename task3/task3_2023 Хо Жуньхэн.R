library(R6)
library(pls)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(olsrr)
library(readr)
library(stats)
MyModelClass <- R6Class("MyModelClass",
                        public = list(
                          model = NULL,
                          step=NULL,
                          train_data=NULL,
                          model1=NULL,
                          aic_values=NULL,
                          initialize = function(data) {
                            self$model1<- lm(MPG_Highway~., data = data)
                            self$model<- lm(MPG_Highway~., data = data)
                            self$step<-1
                          },
                          
                          fit=function(train_data,step){
                            train_data<-train_data
                            if (step == 1) {
                              self$model <- lm(MPG_Highway ~ Length+Weight+Wheelbase+Horsepower+Invoice+EngineSize+Cylinders+Origin+Type, data = train_data)
                              self$step<-1
                            } else if (step == 2) {
                              self$model <- lm(MPG_Highway ~ Length+Weight+Wheelbase+Horsepower+Invoice+EngineSize+Cylinders+Origin+Type, data = train_data)
                              self$step<-2
                              }
                            else if(step==3){
                              self$train_data<-train_data
                              self$model<-lm(MPG_Highway ~ Length+Weight+Wheelbase+Horsepower+Invoice+EngineSize+Cylinders+Origin+Type, data=train_data)
                              self$step<-3
                            }
                            else if(step==4)
                            {
                              k<-self$model
                              xs<-subset(k,aic==min(k$aic))$predictors
                              xs_vector<- strsplit(xs, split = " ")
                              xs_vector<-unlist(xs_vector)
                              formul<- paste(xs_vector, collapse = " + ")
                              formul<- paste("MPG_Highway", "~", formul)
                              self$model<-lm(formul, data=train_data)
                            }
                            else if(step==5)
                            {
                              names<-c("MPG_Highway","Length","Weight","Wheelbase","Horsepower","Invoice","EngineSize","Cylinders","Origin","Type")
                              train_data <- train_data[c("MPG_Highway","Length","Weight","Wheelbase","Horsepower","Invoice","EngineSize","Cylinders","Origin","Type")]
                              model_pcr<-pcr(MPG_Highway ~ Length+Weight+Wheelbase+Horsepower+Invoice+EngineSize+Cylinders+Origin+Type,data=train_data,scale=TRUE,valiation="CV")
                              aic_values <- numeric()
                              for (i in 1:model_pcr$ncomp) {
                                pcr_model <- pcr(MPG_Highway ~ ., ncomp = i, scale = TRUE, data = train_data)
                                scores <- pcr_model$scores[,1:i]
                                pcr_data <- data.frame(scores, MPG_Highway = train_data$MPG_Highway)
                                lm_model <- lm(MPG_Highway ~ ., data = pcr_data)
                                aic_values[i] <- AIC(lm_model)
                              }
                              self$aic_values<-aic_values
                              index<-which.min(aic_values)
                              names<-names[2:index]
                              formul<- paste(names, collapse = "+")
                              formul<-paste("MPG_Highway","~",formul)
                              self$model<- lm(formul, data =train_data)
                              self$step<-5
                            }
                          },
                          
                          summary = function() {
                            if (self$step == 1) {
                              info<-summary(self$model)
                              r.squared<-info$r.squared
                              adj.r.squared<-info$adj.r.squared
                              aic<-AIC(self$model)
                              p_values <- info$coefficients[, "Pr(>|t|)"]
                              p_values<- p_values[-1]
                              least<-rownames(info$coefficients)[which.max(p_values)+1]
                              most<-rownames(info$coefficients)[which.min(p_values)+1]
                              cat("r.squared:",r.squared,"\n","aic: ",aic,"\n","adj.r.squared: ",adj.r.squared,"\n","the most important var: ",most,"\n","the least important var: ",least,"\n")
                            } 
                            else if (self$step == 2) {
                              info<-summary(self$model)
                              r.squared<-info$r.squared
                              adj.r.squared<-info$adj.r.squared
                              aic<-AIC(self$model)
                              p_values <- info$coefficients[, "Pr(>|t|)"]
                              p_values<- p_values[-1]
                              least<-rownames(info$coefficients)[which.max(p_values)+1]
                              most<-rownames(info$coefficients)[which.min(p_values)+1]
                              cat("r.squared:",r.squared,"\n","aic: ",aic,"\n","adj.r.squared: ",adj.r.squared,"\n","the most important var: ",most,"\n","the least important var: ",least,"\n")
                            }
                            else if(self$step==3){
                              info<-summary(self$model)
                              r.squared<-info$r.squared
                              adj.r.squared<-info$adj.r.squared
                              aic<-AIC(self$model)
                              p_values <- info$coefficients[, "Pr(>|t|)"]
                              p_values<- p_values[-1]
                              least<-rownames(info$coefficients)[which.max(p_values)+1]
                              most<-rownames(info$coefficients)[which.min(p_values)+1]
                              cat("r.squared:",r.squared,"\n","aic: ",aic,"\n","adj.r.squared: ",adj.r.squared,"\n","the most important var: ",most,"\n","the least important var: ",least,"\n")
                            }
                            else if(self$step==4)
                            {
                              info<-summary(self$model)
                              r.squared<-info$r.squared
                              adj.r.squared<-info$adj.r.squared
                              aic<-AIC(self$model)
                              p_values <- info$coefficients[, "Pr(>|t|)"]
                              p_values<- p_values[-1]
                              least<-rownames(info$coefficients)[which.max(p_values)+1]
                              most<-rownames(info$coefficients)[which.min(p_values)+1]
                              cat("r.squared:",r.squared,"\n","aic: ",aic,"\n","adj.r.squared: ",adj.r.squared,"\n","the most important var: ",most,"\n","the least important var: ",least,"\n")
                            }
                            else if(self$step==5)
                            {
                              info<-summary(self$model)
                              r.squared<-info$r.squared
                              adj.r.squared<-info$adj.r.squared
                              aic<-AIC(self$model)
                              p_values <- info$coefficients[, "Pr(>|t|)"]
                              p_values<- p_values[-1]
                              least<-rownames(info$coefficients)[which.max(p_values)+1]
                              most<-rownames(info$coefficients)[which.min(p_values)+1]
                              cat("r.squared:",r.squared,"\n","aic: ",aic,"\n","adj.r.squared: ",adj.r.squared,"\n","the most important var: ",most,"\n","the least important var: ",least,"\n")
                              
                            }
                          },
                          plot = function() {
                            if (self$step == 1) {
                              fit<-self$model
                              res<-residuals(fit)
                              plot(fit,which=1)
                              residplot <- function(fit, nbreaks=10) { 
                                z <- rstudent(fit) 
                                hist(z, breaks=nbreaks, freq=FALSE, 
                                     xlab="Residual", 
                                     main="Distribution of Errors") 
                                rug(jitter(z), col="brown") 
                                curve(dnorm(x, mean=mean(z), sd=sd(z)), 
                                      add=TRUE, col="blue", lwd=2) 
                                lines(density(z)$x, density(z)$y, 
                                      col="red", lwd=2, lty=2) 
                                legend("topright", 
                                       legend = c( "Normal Curve", "Kernel Density Curve"), 
                                       lty=1:2, col=c("blue","red"), cex=.7) 
                              } 
                              residplot(fit)
                            } else if (self$step == 2) {
                              fit<-self$model
                              res<-residuals(fit)
                              plot(fit,which=1)
                              residplot <- function(fit, nbreaks=10) { 
                                z <- rstudent(fit) 
                                hist(z, breaks=nbreaks, freq=FALSE, 
                                     xlab="Residual", 
                                     main="Distribution of Errors") 
                                rug(jitter(z), col="brown") 
                                curve(dnorm(x, mean=mean(z), sd=sd(z)), 
                                      add=TRUE, col="blue", lwd=2) 
                                lines(density(z)$x, density(z)$y, 
                                      col="red", lwd=2, lty=2) 
                                legend("topright", 
                                       legend = c( "Normal Curve", "Kernel Density Curve"), 
                                       lty=1:2, col=c("blue","red"), cex=.7) 
                              } 
                              residplot(fit)
                            }
                            else if(self$step==3){
                              fit<-self$model
                              res<-residuals(fit)
                              plot(fit,which=1)
                              residplot <- function(fit, nbreaks=10) { 
                                z <- rstudent(fit) 
                                hist(z, breaks=nbreaks, freq=FALSE, 
                                     xlab="Residual", 
                                     main="Distribution of Errors") 
                                rug(jitter(z), col="brown") 
                                curve(dnorm(x, mean=mean(z), sd=sd(z)), 
                                      add=TRUE, col="blue", lwd=2) 
                                lines(density(z)$x, density(z)$y, 
                                      col="red", lwd=2, lty=2) 
                                legend("topright", 
                                       legend = c( "Normal Curve", "Kernel Density Curve"), 
                                       lty=1:2, col=c("blue","red"), cex=.7) 
                              } 
                              residplot(fit)
                            }
                            else if(self$step==4)
                            {
                              fit<-self$model
                              res<-residuals(fit)
                              plot(fit,which=1)
                              residplot <- function(fit, nbreaks=10) { 
                                z <- rstudent(fit) 
                                hist(z, breaks=nbreaks, freq=FALSE, 
                                     xlab="Residual", 
                                     main="Distribution of Errors") 
                                rug(jitter(z), col="brown") 
                                curve(dnorm(x, mean=mean(z), sd=sd(z)), 
                                      add=TRUE, col="blue", lwd=2) 
                                lines(density(z)$x, density(z)$y, 
                                      col="red", lwd=2, lty=2) 
                                legend("topright", 
                                       legend = c( "Normal Curve", "Kernel Density Curve"), 
                                       lty=1:2, col=c("blue","red"), cex=.7) 
                              } 
                              residplot(fit)
                              k<-model
                            }
                            else if(self$step==5)
                            {
                              fit<-self$model
                              res<-residuals(fit)
                              plot(fit,which=1)
                              residplot <- function(fit, nbreaks=10) { 
                                z <- rstudent(fit) 
                                hist(z, breaks=nbreaks, freq=FALSE, 
                                     xlab="Residual", 
                                     main="Distribution of Errors") 
                                rug(jitter(z), col="brown") 
                                curve(dnorm(x, mean=mean(z), sd=sd(z)), 
                                      add=TRUE, col="blue", lwd=2) 
                                lines(density(z)$x, density(z)$y, 
                                      col="red", lwd=2, lty=2) 
                                legend("topright", 
                                       legend = c( "Normal Curve", "Kernel Density Curve"), 
                                       lty=1:2, col=c("blue","red"), cex=.7) 
                              } 
                              residplot(fit)
                              ncomp<-1:10
                              plot(ncomp,self$aic_values)
                            }
                          }
                        )
)
df<-read.csv("D:\\CARS.csv")
df$Invoice<- as.numeric(gsub("[,\\$]", "", df$Invoice))
df<-na.omit(df)
my_model <- MyModelClass$new(df)

my_model$fit(df,step=1)
my_model$summary()
my_model$plot()

n <- nrow(df)
p <- length(coef(df)) 
threshold <- 4 / (n - p - 1)
cutoff <- 4/threshold 
cooks_d <- cooks.distance(my_model$model)
cooks_d_indices <- order(cooks_d, decreasing = TRUE)
outliers_to_remove <- cooks_d_indices[1:3]
cooks_d <- cooks.distance(my_model$model)
plot(cooks_d,pch=19)
colors <- c('red', 'red', 'red')
points(outliers_to_remove, cooks_d[outliers_to_remove], col = colors, pch = 19)
for (i in 1:3) {
  text(outliers_to_remove[i],cooks_d[outliers_to_remove[i]], labels = as.character(outliers_to_remove[i]), pos = 3)
}
df <- df[-outliers_to_remove, ]
my_model$fit(df,step=2)
my_model$summary()
my_model$plot()


p_lever=0.01
pairwise_t_test <- pairwise.t.test(df$MPG_Highway, df$Origin, p.adjust.method = "none")
insignificant_groups <- pairwise_t_test$p.value > 0.01
groups_to_merge <- list()
for (i in 1:ncol(insignificant_groups)) {
  for (j in 1:nrow(insignificant_groups)) {
    if (!is.na(insignificant_groups[i, j]) && insignificant_groups[i, j] == TRUE) {
      groups_to_merge[[length(groups_to_merge) + 1]] <- c(colnames(insignificant_groups)[j], rownames(insignificant_groups)[i])
    }
  }
}
for (groups in groups_to_merge) {
  group_names <- unlist(groups)
  new_group_name <- paste0(unlist(groups), collapse = "_")
  df$Origin[df$Origin %in% group_names] <- new_group_name
}
pairwise_t_test <- pairwise.t.test(df$MPG_Highway, df$Type, p.adjust.method = "none")
insignificant_groups <- pairwise_t_test$p.value > 0.01
groups_to_merge <- list()
for (i in 1:ncol(insignificant_groups)) {
  for (j in 1:nrow(insignificant_groups)) {
    if (!is.na(insignificant_groups[i, j]) && insignificant_groups[i, j] == TRUE) {
      groups_to_merge[[length(groups_to_merge) + 1]] <- c(colnames(insignificant_groups)[j], rownames(insignificant_groups)[i])
    }
  }
}
for (groups in groups_to_merge) {
  group_names <- unlist(groups)
  new_group_name <- paste0(unlist(groups), collapse = "_")
  df$Type[df$Type %in% group_names] <- new_group_name
}
my_model$fit(df,step=3)
my_model$summary()
my_model$plot()

train_data<-my_model$train_data
k<-ols_step_best_subset(my_model$model,metric="AIC")
plot(k$aic)
my_model$model<-k
my_model$fit(df,step=4)
my_model$summary()
my_model$plot()

my_model$fit(df,step=5)
my_model$summary()
my_model$plot()

