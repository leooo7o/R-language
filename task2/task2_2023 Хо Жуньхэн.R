library(ggplot2)
library(emmeans)
library(multcomp)
library(car)
library(dplyr)
library(multcompView)
df<-read.csv("D:\\CARS.csv")
df$Invoice<- as.numeric(gsub("[,\\$]", "", df$Invoice))
df<-na.omit(df)
data<-df
#task1 (p=8.27e-09<0.01,то Origin влияет MPG_city)
aov_model<-aov(MPG_City~Origin,df)
summary(aov_model)

#task2
ggplot(data = df, aes(x = Origin, y = MPG_City)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "diffogram")
pair<- pairwise.t.test(df$MPG_City, df$Origin, p.adjust.method = "none")
p_values <- pair$p.value
p_values
groups_to_merge <- list()
length(groups_to_merge)
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
#task3
df<-data
average_MPG <- aggregate(MPG_City~ Origin, data = df, FUN = mean)
Origin<-average_MPG$Origin
Origin2 <- c(Origin[-1], Origin[1])
Mean<-average_MPG$MPG_City
Mean2 <- c(Mean[-1], Mean[1])
insignificant_groups<-p_values<0.01
true_or_false<-c(insignificant_groups[1,1],insignificant_groups[2,2],insignificant_groups[2,1])
true_or_false
compare_df<- data.frame(
  group1 = Origin,
  group2 = Origin2,
  mean1 = Mean,
  mean2 = Mean2,
  significant = true_or_false,
  x_start=Mean-1,
  y_start=Mean2+1,
  x_end=Mean+1,
  y_end=Mean2-1
)
ggplot(compare_df, aes(x = mean1, y = mean2, color = significant)) +
  geom_point()+
  geom_segment(data = subset(compare_df, significant ==TRUE), 
               aes(x=x_start,y=y_start,xend = x_end, yend = y_end), size = 1, color = "blue", linetype = "solid") +
  geom_segment(data = subset(compare_df, significant ==FALSE), 
               aes(x=x_start,y=y_start,xend = x_end, yend = y_end), size = 1, color = "red", linetype = "dashed") +
  geom_text(aes(label = group1), hjust = -0.5) +
  geom_text(aes(label = group2), vjust = -0.5) +
  scale_colour_manual(values = c("red", "blue"), 
                      labels = c("Not significant", "Significant")) +
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  coord_cartesian(xlim = c(18, 23), ylim = c(18, 23))+
  geom_segment(aes(x = 20.5, y = 23, xend = 21, yend = 23),color = "blue", 
               linetype = "solid") +
  annotate("text", x = 21, y = 23, label = "Significant", hjust = 0) +
  geom_segment(aes(x = 20.5, y = 22, xend = 21, yend = 22), color = "red",
               linetype = "dashed") +
  annotate("text", x = 21, y = 22, label = "Not Significant", hjust = 0)+
  guides(color = guide_none())
# library(sasLM)
# Diffogram(MPG_City~Origin,df)

#task4 (p стало меньше, а Residuals стали меньше, что указывает на улучшение модели.)
df<-data
aov_model_4<-aov(MPG_City~Origin+Type,df)
summary(aov_model_4)
#task5 Origin*Type:p>0.01, не нужен
sem <- function(x) sd(x) / sqrt(length(x))
plot_interaction <- function (df) {
    na.omit (df) %>% group_by(Origin,Type) %>%
    summarise (meanloss = mean (MPG_City), se = sem (MPG_City) ) %>%
    ggplot (aes (x = Origin, y = meanloss, colour = Type) )+
    geom_line (aes (group = Type) ) +
    geom_pointrange (aes (ymin = meanloss - se, ymax = meanloss + se) )
}
df<-data
aov_model_5<-aov(MPG_City~Origin*Type,df)
summary(aov_model_5)
plot_interaction(df)
sample_mpg_type<-df[c("MPG_City","Origin","Type")]
sample_mpg_type<-subset(sample_mpg_type,Type!="Hybrid")
sample_mpg_type$id<-rownames(sample_mpg_type)
summary(lm(MPG_City~Origin*Type,sample_mpg_type))
sample_mpg_type$Origin<-as.factor(sample_mpg_type$Origin)
sample_mpg_type$Type<-as.factor(sample_mpg_type$Type)
post_test<-glht(aov(MPG_City~Origin*Type,sample_mpg_type),
                linfct=mcp(Origin="Tukey",Type="Tukey"))
summary(post_test)
plot(post_test)
#task6 P<0.0001 (группы европейских + азиатских седанов vs американские траки Они разные)
df<-data
df<-df[c("MPG_City","Origin","Type")]
df<- df[(
  (df$Origin == "USA" & df$Type == "Truck") | 
    (df$Origin == "Asia" & df$Type == "Sedan")|
    (df$Origin == "Europe"& df$Type == "Sedan")
), ]
df$Origin <- ifelse(df$Origin %in% c("Asia", "Europe"), "Asia_Europe", df$Origin)
df$Type <- paste(df$Origin, df$Type, sep = "_")
df<-df[c("MPG_City","Type")]

model<-aov(MPG_City ~ Type, data = df)
emmeans(model, pairwise ~ Type)
plot(emmeans(model, pairwise ~ Type))
#task7(p=9.278e-10<0.01,не близко к нормальному распределению,p=0.01813>0.01,дисперсии в группах равны)
qqnorm(df$MPG_City)
qqline(df$MPG_City)
shapiro.test(df$MPG_City)
#大于0.05则认为接近正态分布
library(car)
bartlett.test(df$MPG_City, df$Type)
#小于0.05则认为相等
#task8(p=7.136e-08<0.01, группы европейских + азиатских седанов vs американские тракиОни разные)
kruskal_result <- kruskal.test(MPG_City ~ Type, data = df)
kruskal_result
#wilcox_result<-wilcox.test(MPG_City ~ Type, data = df)
#wilcox_result

