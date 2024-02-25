library(ggplot2)
library(dplyr)
library(nortest)
library("BSDA")
data <- read.csv("fifa.csv")
data <- data[complete.cases(data),]
data$height_m <- data$height_cm / 100
data$bmi <- data$weight_kg / (data$height_m * data$height_m)


data$value_mil <- data$value_eur / 1000000





#Population Stats

#top 5 players

top_5<- head(data[order(data$value_mil, decreasing = TRUE), ], 5)
ggplot(data = top_5, aes(x = short_name, y = value_mil, fill = value_mil)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FFC300", high = "#900C3F") +
  labs(x = "Football Players", y = "Value (Million)") +
  ggtitle("Football Players and their Values") +
  theme(axis.text.x = element_text(face = "bold"))

#weight
summary(data$weight_kg)
p_weight_mean=mean(data$weight_kg)

p_weight_sd=sd(data$weight_kg)*sqrt((length(data$weight_kg)-1)/length(data$weight_kg))

p_weight_min=min(data$weight_kg)
p_weight_max=max(data$weight_kg)

hist(data$weight_kg,col = "#900C3F")



#height
p_height_mean=mean(data$height_cm)

p_height_sd=sd(data$height_cm)*sqrt((length(data$height_cm)-1)/length(data$height_cm))

p_height_min=min(data$height_cm)
p_height_max=max(data$height_cm)

hist(data$height_cm,col = "#900C3F")



#bmi
p_bmi_mean=mean(data$bmi)

p_bmi_sd=sd(data$bmi)*sqrt((length(data$bmi)-1)/length(data$bmi))

p_bmi_min=min(data$bmi)
p_bmi_max=max(data$bmi)

hist(data$bmi,col = "#900C3F")


#value
p_value_mean=mean(data$value_mil)

p_value_sd=sd(data$value_mil)*sqrt((length(data$value_mil)-1)/length(data$value_mil))

p_value_min=min(data$value_mil)
p_value_max=max(data$value_mil)


hist(data$value_mil,col = "#900C3F")


# Taking a random sample to do our works

sampled_data <- data %>% sample_n(size = 30, replace = FALSE)

# MEAN ESTIMATION FOR SOME FEATURES USING SAMPLE MEANS


cat("Estimated Mean of bmi from sample:",mean(sampled_data$bmi), "Actual Mean:",mean(data$bmi))
cat("Estimated Mean of weight from sample:",mean(sampled_data$weight_kg),"Actual Mean:",mean(data$weight_kg))
cat("Estimated Mean of height from sample:",mean(sampled_data$height_cm),"Actual Mean:",mean(data$height_cm))
cat("Estimated Mean of value from sample:",mean(sampled_data$value_mil),"Actual Mean:",mean(data$value_mil))

cat("Estimated std of bmi from sample:",sd(sampled_data$bmi), "Actual std:",p_bmi_sd)
cat("Estimated std of weight from sample:",sd(sampled_data$weight_kg),"Actual std:",p_weight_sd)
cat("Estimated std of height from sample:",sd(sampled_data$height_cm),"Actual std:",p_height_sd)
cat("Estimated std of value from sample:",sd(sampled_data$value_mil),"Actual std:",p_value_sd)

cat("Estimated variance of bmi from sample:",var(sampled_data$bmi), "Actual std:",p_bmi_sd^2)
cat("Estimated variance of weight from sample:",var(sampled_data$weight_kg),"Actual std:",p_weight_sd^2)
cat("Estimated variance of height from sample:",var(sampled_data$height_cm),"Actual std:",p_height_sd^2)
cat("Estimated variance of value from sample:",var(sampled_data$value_mil),"Actual std:",p_value_sd^2)





plot(sampled_data$bmi, sampled_data$potential, 
     xlab = "bmi", ylab = "potential", 
     main = " bmi vs potential ",
     col = "#F4B183")


plot(data$bmi, data$potential, 
     xlab = "bmi", ylab = "potential", 
     main = " bmi vs potential ",
     col = "#F4B183")

sampled_weights=sampled_data$weight_kg
ad.test(sampled_weights)

hist(sampled_weights,col = "#F4B183")

sampled_heights=sampled_data$height_cm
ad.test(sampled_heights)

hist(sampled_heights,col = "#F4B183")

sampled_bmi=sampled_data$bmi
ad.test(sampled_bmi)

hist(sampled_bmi,col = "#F4B183")


sampled_value=sampled_data$value_mil
ad.test(sampled_value)

hist(sampled_value,col = "#F4B183")




ggplot(data, aes(x = team_position, y = value_mil, fill = team_position)) +
  geom_bar(stat = "summary", fun = mean) +
  labs(x = "Position", y = "Overall value", title = "Player positions and their overall values") +
  theme(axis.text.x = element_text(face = "bold"))

plot(data$overall, data$value_mil, 
     xlab = "overall_stats", ylab = "value", 
     main = " overrall_stats vs values ",
     col = "#F4B183")






#------Confidence Interval 1------------
# Lets assume that we only have our population standard deviation,
# sample size and mean of the sample for bmi values. Find a 99%
# two-sided confidence interval on the mean bmi to find the best point estimation
# for the mean.

xbar <- mean(sampled_bmi)
sd <-p_bmi_sd
n<-30
error <- qnorm(0.995)*sd/sqrt(n)
lb1 <- xbar-error
ub1 <- xbar+error
cat("CONFIDENCE INTERVAL=",lb1,"< mu <",ub1)



#-------Confidence Interval 2------------
# For the given weights of football players, we know that weight values
# approximately normally distributed.For our sampled data, find a 95% lower
# confidence bound on the average weights.

xbar<-mean(sampled_weights)
sd<-p_weight_sd
n<-30
error<-qnorm(0.95)*sd/sqrt(n)
lb2<-xbar-error
cat("CONFIDENCE INTERVAL=",lb2,"< mu")

#-------Confidence Interval 3------------
# Find the %95 confidence interval for the variance and standard  
# deviation of the bmi for our sampled data which has a standard
# deviation of 1.196.
right<-qchisq(0.025,29)
left <-qchisq(0.975,29)
lb3<-(29*var(sampled_bmi)/left)
ub3<-(29*var(sampled_bmi)/right)

#confidence Interval for the Variance
cat("CONFIDENCE INTERVAL=",lb3,"< variance <",ub3)

#confidence Interval for the standard deviation
cat("CONFIDENCE INTERVAL=",sqrt(lb3),"< sd <",sqrt(ub3))

#-------HYPOTHESİS TEST 1------------
# Height of players in our population has a mean 181.19 cm with a standard
# deviation of 6.83. We will test the hypothesis H0:μ= 181.19 against
# H1:μ <181.19 using a random sample that created earlier which has
# the sample size 30.

# 1) What is the type I error probability if the critical region is defined
# as xbar < 179 ?

z_value<- (179 - p_height_mean)/(p_height_sd/sqrt(30))

cat(z_value)

type_I <-pnorm(z_value)
cat("Type I Error:",type_I)


# 2)Find β (type II error),For the case in which the true mean height is 176.5cm.

z_value<- (179-176.5)/(p_height_sd/sqrt(30))
cat(z_value)

type_II <-pnorm(-z_value)
cat("Type II Error:",type_II)

power_of_test<-1-type_II
cat("Power of test:",power_of_test)

#-------HYPOTHESİS TEST 2------------
# From our observation we know that average body mass index of football players
# in our population is 22.82 with the standard deviation 1.359. We wish to test 
# H0:μ = 22.82 H1:μ != 22.82 with the sample we created that has sample size 30.
# What is type I error probability if the acceptance region is 
# 22 < xbar < 23.64 ?

z_value1<- (22-p_bmi_mean)/ (p_bmi_sd/sqrt(30))
cat(z_value1)

z_value2<- (23.64-p_bmi_mean)/ (p_bmi_sd/sqrt(30))
cat(z_value2)

type_I <- pnorm(z_value1)+pnorm(-z_value2)
cat(type_I)

type_I_second <- 1-(pnorm(z_value2)-pnorm(z_value1))
cat(type_I_second)

#-----------REGRESSION PART ------------------

plot(data$overall, data$value_mil, xlab = "overall_stats", ylab = "value", 
     main = " overall_stats vs values ",col = "#F4B183")



data$value_log<-log(data$value_mil)

rows_with_missing <- which(is.na(data$value_log))
print(rows_with_missing)

rows_with_infinite <- which(!is.finite(data$value_log))
print(rows_with_infinite)

subset_data <- subset(data, is.finite(value_log))

plot(subset_data$overall, subset_data$value_log, 
     xlab = "overall_stats", ylab = "log(value)", 
     main = " overrall_stats vs log (values) ",col = "#F4B183")


LinearModel <- lm(value_log ~ overall, data = subset_data)

plot(subset_data[, "overall"], subset_data[, "value_log"],
     main = "Linear Regression", xlab = "Overall Stats", ylab = "values(Log)",
     col = "#F4B183")

abline(LinearModel, col = "red",lwd = 3)

cor(subset_data$overall,subset_data$value_log)

#--------Goodness of Fit Test-----------

sampled_data2 <- data %>% sample_n(size = 200, replace = FALSE)


workrate_counts <- table(data$work_rate)
workrate_percentages <- prop.table(workrate_counts) * 100


print(workrate_percentages)

# According to our data set, work-rate percentages in our data set is 
# calculated above, below we have the results.

#   High/High     High/Low   High/Medium    Low/High      Low/Low 
#   5.442356      4.149071   18.237965      2.354307      0.279772 

#   Low/Medium   Medium/High    Medium/Low   Medium/Medium 
#   2.544341     9.491132       4.819468     52.681588 

# Randomly selected 200 football players from the population have the
# work-rate percentages can be found with same code for the population.

workrate_counts_sample <- table(sampled_data2$work_rate)
workrate_percentages_sample <- prop.table(workrate_counts_sample) * 100

print(workrate_percentages_sample)

# From the code above our results, work-rate percentages for our
# sampled data found like below.

#   High/High     High/Low     High/Medium    Low/High      Low/Low 
#   5.0           2.5          22.5           3.5           0.5 

#   Low/Medium   Medium/High    Medium/Low   Medium/Medium 
#   4.0          6.5            5.5          50.0 



# At alpha = 0.05, does the sample result differ from
# the population statistics ?
# H0: Distribution is the same
# H1: Distribution is not the same

observed<-as.numeric(workrate_counts_sample)
cat(observed)

probs<-as.numeric(workrate_percentages/100)
cat(probs)

chisq.test(observed,p=probs)

# Then I visualize the percentages to see the differences visually.

plot(workrate_percentages, type = "o", col = "blue", 
     xlab = "Work Rate", ylab = "Percentage", main = "Work Rate Percentages")
lines(workrate_percentages_sample, type = "o", col = "red")
legend("topleft", legend = c("Data", "Sampled Data"), 
       col = c("blue", "red"), lty = 1, cex = 0.8)

#---------------ANOVA-----------------
#Creating another sampled data to work on.
sampled_data3 <- data %>% sample_n(size = 100, replace = FALSE)


#Creating a subset to only get the values for given countries

subset_data3 <- subset(sampled_data3, nationality %in% c("Brazil", "France","England"))

brazil_values <- subset_data3$value_mil[subset_data3$nationality == "Brazil"]
france_values <- subset_data3$value_mil[subset_data3$nationality == "France"]
england_values <- subset_data3$value_mil[subset_data3$nationality == "England"]

print(brazil_values)
print(france_values)
print(england_values)

# Checking if, at alpha = 0.05, is there sufficient evidence to say that
# there is a difference between the mean values with using anova test ?

anova <- aov(value_mil~nationality,data=subset_data3)
summary(anova)

#-------------NON PARAMETRİC TEST 1 / sign test ----------------

# For the sample of 30 players, If the median of the sampled data for values in
# million is equal to 2 or not with alpha = 0.05.

SIGN.test(sampled_value,md=2)

#-------------NON PARAMETRİC TEST 2 / paired-sample sign test ----------------

# Players have overall stats and potentials, Using the paired-sample sign test
# with alpha = 0.05, is there evidence of a difference in stats?

SIGN.test(sampled_data$overall,sampled_data$potential)

#-------------NON PARAMETRİC TEST 3 / wilcoxon signed-rank test---------------

# By using the sampled data, use the wilcoxon signed-rank test to see if the
# the mean height of a football player is 1.8 meter with alpha = 0.05.

wilcox.test(sampled_data$height_m,mu=1.8,alternative = "two.sided")

#-------------NON PARAMETRİC TEST 4 / Spearman rank correlation 

# At the 0.05 level of significance, is there enough evidence to say that 
# there is correlation between the overall stats and values of football players.

cor.test(sampled_data$overall,sampled_data$value_mil,method = "spearman")


#-------------End of the Project I hope you like my work -----------------------
#------------------------INE2002 TERM PROJECT-----------------------------------
#--------------------Süleyman Kaan Ataç 2004015---------------------------------

