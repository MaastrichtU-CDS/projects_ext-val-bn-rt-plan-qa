library(classifierplots)
library(caret)
library(ggplot2)

source("MyClassifierPlots.R")

results <- read.csv("C:\\Users\\inigo.bermejo\\Documents\\Src\\bn-for-rt-plan-qa\\hugin_api\\BN_validation\\Output_2_1606135350499.csv")

data2 <- read.csv("C:\\Users\\inigo.bermejo\\Documents\\Data\\BN in RT\\20201123\\approach2finalCLEAN23112020.csv", sep = ';')

summary(data2$description.of.the.error)

#errors
results$Collimator_error <- ifelse (data2$description.of.the.error == "Collimator angle should have been 0", 1, 0)
results$PTV_error <- ifelse (data2$description.of.the.error == "PTV dose should have been 4500cGy", 1, 0)
results$Table_error <- ifelse (data2$description.of.the.error == "table angle should have been 0", 1, 0)
results$Bolus_error <- ifelse (data2$description.of.the.error == "Bolus should have been *custom", 1, 0)
results$Bolus_error <- ifelse (data2$description.of.the.error == "Bolus should have been NONE", 1, results$Bolus_error)

#Remove rows with incompatible evidence
results_no_ie <- results[results$incompatible_evidence==0,]

# Put the results for all variables in 2 columns: predicted and observed
num_rows <- nrow(results_no_ie)
probabilities  = unlist(results_no_ie[,3:22])
errors = results_no_ie[,3:22]
errors[,] = 0
errors$Collimator_Angle = results_no_ie$Collimator_error
errors$PTV_dose_Rx = results_no_ie$PTV_error
errors$Table_Angle = results_no_ie$Table_error
errors$Bolus = results_no_ie$Bolus_error
errors = unlist(errors)

all_results <- data.frame(Predicted = probabilities, Errors = errors)
all_results <- na.omit(all_results)

thresholds <- seq(0,1,by=0.01)

sensitivities <- rep(0,length(thresholds))
specificities <- rep(0,length(thresholds))
i <- 1

lvs <- c("abnormal", "normal")
errors <- factor(ifelse (all_results$Errors == 0, "normal", "abnormal"), lvs)
for(t in thresholds){
  flag <- factor(ifelse(all_results$Predicted <= t, "abnormal", "normal"), lvs)
  sensitivities[i] <- caret::sensitivity(flag, errors)
  specificities[i] <- caret::specificity(flag, errors)
  i <- i+ 1
}


flag <- factor(ifelse(all_results$Predicted <= 0.535, "abnormal", "normal"), lvs)
caret::sensitivity(flag, errors)
caret::specificity(flag, errors)
caret::posPredValue(flag, errors, prevalence = 0.001)

sum(flag == "abnormal")/length(flag)

# Create Line Chart

# convert factor to numeric for convenience


# set up the plot
colors <- rainbow(3)
plot(c(0,1), c(0,1), type="n", xlab="Threshold",
     ylab="Sens/spec" )

lines(thresholds, sensitivities, type="b", lwd=1.5,
      lty=1, col=colors[1], pch=1)
lines(thresholds, specificities, type="b", lwd=1.5,
      lty=3, col=colors[3], pch=2)

# add a title and subtitle

# add a legend
legend(0.08, 0.15, c("Sensitivity", "Specificity"), cex=0.8, col=c(colors[1],colors[3]),
       pch=c(18,19), lty=c(1,2), title="Legend")

count_zero <- apply(results_no_ie[,3:21], 2, FUN=function(x) sum ( x==0 ))

roc_plot1(all_results$Errors, 1-all_results$Predicted, force_bootstrap = TRUE, resamps = 100)

#roc_plot1(results_no_ie$rt_error, min_prob, force_bootstrap = TRUE)

roc_plot1(results_no_ie$Collimator_error, 1-results_no_ie$Collimator_Angle, force_bootstrap = TRUE)

roc_plot1(results_no_ie$PTV_error,  1-results_no_ie$PTV_dose_Rx, force_bootstrap = TRUE)

roc_plot1(results_no_ie$Table_error,  1-results_no_ie$Table_Angle, force_bootstrap = TRUE)

roc_plot1(results_no_ie$Bolus_error,  1-results_no_ie$Bolus, force_bootstrap = TRUE)
