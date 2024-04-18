library(dplyr)
library(ggplot2)
library(caret)
library(olsrr)
library(forecast)
library(MASS)
library(R6)
library(plotly)
MySolve <- R6Class("MySolve",
                        public = list(
                          model = NULL,
                          data=NULL,
                          train_data=NULL,
                          test_data=NULL,
                          lambda=NULL,
                          predictions=NULL,
                          xs=NULL,
                          initialize = function(df) {
                            self$data<-df
                          },
                          fit=function(train_data){
                            #Преобразование входных переменных
                            boxcox_result <- boxcox(MPG_Highway ~ Length+Weight+ Wheelbase + Horsepower + Invoice + EngineSize + Cylinders + Origin + Type, data = train_data)
                            lambda_best <- boxcox_result$x[which.max(boxcox_result$y)]
                            self$lambda<-lambda_best
                            train_data$MPG_Highway  <- ((train_data$MPG_Highway ^lambda_best) - 1) / lambda_best
                            model <- lm(MPG_Highway ~ Length+Weight+ Wheelbase + Horsepower + Invoice + EngineSize + Cylinders + Origin + Type, data = train_data)
                            #Отбор значимых переменных с помощью пошаговых методов
                            k<-ols_step_best_subset(model,metric="AIC")
                            xs<-subset(k,aic==min(k$aic))$predictors
                            xs_vector<- unlist(strsplit(xs, split = " "))
                            
                            #добавление полиномиальных членов в уравнение
                            for(i in 1:length(xs_vector))
                            {
                              if(xs_vector[i]=="Length"||xs_vector[i]=="Weight"||xs_vector[i]=="Horsepower"||xs_vector[i]=="Cylinders")
                              {
                                s<-paste0(xs_vector[i], " + I(", xs_vector[i], "^2) + I(", xs_vector[i], "^3)")
                                xs_vector[i]<-s
                              }
                            }
                            self$xs<-xs_vector
                            formul<- paste(xs_vector, collapse = " + ")
                            formul<- paste("MPG_Highway", "~", formul)
                            model<-lm(formul, data=train_data)
                            #model <- lm(MPG_Highway ~ Length+I(Length^2)+ I(Length^3)+Weight+I(Weight^2)+ I(Weight^3) + Horsepower+I(Horsepower^2)+I(Horsepower^3) + Cylinders+I(Cylinders^2)+I(Cylinders^3) + Type, data = train_data)
                            self$model<-model
                            return(model)
                          },
                          predict= function(model,test_data) {
                            predictions <- predict(model,test_data)
                            predictions = I((predictions*self$lambda+1)^(1/self$lambda))
                            self$predictions<-predictions
                            mape <- mean(abs((test_data$MPG_Highway - predictions) / test_data$MPG_Highway))
                            print(mape)
                          },
                          plot = function(model) {
                            all<-self$xs
                            formul<- paste(all, collapse = " + ")
                            formul<- paste("MPG_Highway", "~", formul)
                            l<- lm(formul, data = train_data)
                            #
                            len<-length(self$xs)
                            coefficients<-coef(l)[2:(1+len)]
                            important_vars <- names(sort(abs(coefficients), decreasing = TRUE))[1:2]
                            var1<- self$data[[important_vars[1]]]
                            var2<- self$data[[important_vars[2]]]
                            x<- seq(from = min(var1), to = max(var1), length.out = 20)
                            y<- seq(from = min(var2), to = max(var2), length.out = 20)
                            all<-c("Length","Weight","Wheelbase","Horsepower","Invoice","EngineSize","Cylinders","Origin","Type")
                            all<- all[all!= "Origin" & all!= "Type"&all!= important_vars[1] & all!= important_vars[2]]
                            type<-unique(self$data$Type)
                            origin<-unique(self$data$Origin)
                            origin_type<-c()
                            freqs<-numeric(0)
                            for(i in origin)
                            {
                              for(j in type)
                              {
                                origin_type<-c(origin_type,i)
                                origin_type<-c(origin_type,j)
                                ndata <- subset(self$data, Origin == i & Type == j)
                                freq<-nrow(ndata)/nrow(self$data)
                                freqs<-c(freqs,freq)
                              }
                            }
                            matrix_z<- matrix(0, nrow = 20, ncol = 20)
                            values<-numeric(0)
                            for(i in x)
                            {
                              for(j in y)
                              {
                                var <- setNames(data.frame(i,j), c(important_vars[1], important_vars[2]))
                                for(p in all)
                                {
                                  var[[p]]<-mean(self$data[[p]])
                                }
                                sum<-0
                                #Возьмите средневзвешенное значение результатов различных Type и Origin
                                for(t in 1:length(freqs))
                                {
                                  test_var<-var
                                  test_var["Origin"]<-origin_type[t*2-1]
                                  test_var["Type"]<-origin_type[t*2]
                                  pr<-predict(model,test_var)
                                  value<-as.numeric(pr[1])
                                  value<-I((value*self$lambda+1)^(1/self$lambda))
                                  sum<-sum+value*freqs[t]
                                }
                                values<-c(values,sum)
                              }
                            }
                            matrix_z<-matrix(values, nrow=20, ncol=20, byrow=TRUE)
                            #print(matrix_z)
                            plot_ly(x = ~x, y = ~y, z = ~matrix_z, type = "surface")
                          }
                        )
)
df<-read.csv("D:\\CARS.csv")
df$Invoice<- as.numeric(gsub("[,\\$]", "", df$Invoice))
df<-na.omit(df)
pair<- pairwise.t.test(df$MPG_City, df$Origin, p.adjust.method = "none")

#Преобразования категориальных переменных
p_values <- pair$p.value
groups_to_merge <- list()
for (i in 1:ncol(p_values)) {
  for (j in 1:nrow(p_values)) {
    if (!is.na(p_values[i, j]) && p_values[i, j] >0.01) {
      groups_to_merge[[length(groups_to_merge) + 1]] <- c(colnames(p_values)[j], rownames(p_values)[i])
    }
  }
}
for (group in groups_to_merge) {
  group_names <- unlist(group)
  newname <- paste0(unlist(group), collapse = "_")
  df$Origin[df$Origin %in% group_names] <- newname
}
pair<- pairwise.t.test(df$MPG_City, df$Type, p.adjust.method = "none")
p_values <- pair$p.value
groups_to_merge <- list()
for (i in 1:ncol(p_values)) {
  for (j in 1:nrow(p_values)) {
    if (!is.na(p_values[i, j]) && p_values[i, j] >0.01) {
      groups_to_merge[[length(groups_to_merge) + 1]] <- c(colnames(p_values)[j], rownames(p_values)[i])
    }
  }
}
for (group in groups_to_merge) {
  group_names <- unlist(group)
  newname <- paste0(unlist(group), collapse = "_")
  df$Type[df$Type %in% group_names] <- newname
}

#много раз

# for(i in 1:20)
# {
# s<-paste0(as.character(i),"st test:\n")
# cat(s)
# training_samples <- df$MPG_Highway %>%
#   createDataPartition(p = 0.8, list = FALSE)
# train_data <- df[training_samples, ]
# test_data <- df[-training_samples, ]
# my_solve <- MySolve$new(df)
# model<-my_solve$fit(train_data)
# my_solve$predict(model,test_data)
# my_solve$plot(model)
# }

#один раз
set.seed(321)#example
training_samples <- df$MPG_Highway %>%
  createDataPartition(p = 0.8, list = FALSE)
train_data <- df[training_samples, ]
test_data <- df[-training_samples, ]
my_solve <- MySolve$new(df)
model<-my_solve$fit(train_data)
my_solve$predict(model,test_data)
my_solve$plot(model)

