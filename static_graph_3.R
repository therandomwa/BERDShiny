rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
################## data processing #################3
df = read.csv("Data for CYap.csv")
df = as.data.frame(apply(df, 2, function(x){
  str_trim(x, side = c("both", "left", "right"))}))
df$DLT = as.factor(df$DLT)
df$Evaluable = as.factor(df$Evaluable)
df$Evaluable = factor(df$Evaluable, levels = c("1", "0"))
df$Dose_Level = as.integer(as.character(df$Dose_Level))
df$Therapy_Start_Date = as.Date(df$Therapy_Start_Date, "%m/%d/%y")
df$Last_Assessment_Date = as.Date(df$Last_Assessment_Date, "%m/%d/%y")
df$start_time =  as.numeric(difftime(df$Therapy_Start_Date,df$Therapy_Start_Date[1],units="weeks"))
df$end_time = as.numeric(difftime(df$Last_Assessment_Date,df$Therapy_Start_Date[1],units="weeks"))
#df$start_time =  as.numeric(difftime(df$Therapy_Start_Date,df$Therapy_Start_Date[1],units="days") / 30)
#df$end_time = as.numeric(difftime(df$Last_Assessment_Date,df$Therapy_Start_Date[1],units="days") / 30)
df$start_time = df$Therapy_Start_Date
df$end_time = df$Last_Assessment_Date

################ graph #########################
n_dose = unique(df$Dose_Level) # dose counts
ord = sapply(n_dose, function(x){paste0("d",x)}) # y axis names for doses
symboles <- c(NA,4) # for DLT shape
linetype = c(3, 1) # for evaluable line type

df$y2 = 0 # y2 is the new y coord for each patient
ymin = 0.7; ymax = 5.2 # the range of y axis (I don't think it's important)
ygap = (ymax - ymin) / length(n_dose) # the height for each dose in the plot
n_obs = 4 # how many observations do you want to have for each dose around the same time
coord = seq(ymin, ymax, ygap) # start and end coord for each dose category
y_coords = list()
# the y coord for each observation around the same time (got from n_obs)
# let's say you want maximum of 4 obs around the same time, what are their coords?
for (i in 1:(length(coord)-1)) {
  ystart = coord[i]
  yend = coord[i+1]
  ymid = seq(ystart + 0.15, yend - 0.15, length.out = n_obs)
  y_coords[[i]] = ymid
}
names(y_coords) = ord
# initiate the plot
with(df, plot(type="n", x = c(min(start_time), max(end_time)), y = c(ymin, ymax),
     xlab="Time (weeks)", ylab="Assigned dose level", yaxt="n", tcl=-0.2, cex.lab=1.3, cex.axis=1.3,
     cex=0.7,lwd=1.2, yaxs = "i"))
# lines dividing each dose category
abline(h=coord[-c(1, length(coord))], lty=2, col="lightgray")
# add y axis labels for each category
mtext(2, at=coord[-length(coord)]+ygap/2, line=0.2, text=ord, las=0, cex=0.85)

# for each dose, calculate the 
for (d in 1:length(n_dose)){
  test = df[df$Dose_Level == n_dose[d], ]
  count = 0
  y_test = y_coords[[ord[d]]]
  for (i in 1:nrow(test)){
    count = count + 1
    if (count > 4){
      count = 1
    }
    if (i != 1){
      pre_end = test[i-1,"end_time"]
      cur_start = test[i, "start_time"]
      if (cur_start > (pre_end+ (max(df$end_time) - min(df$start_time)) * 0.05)){
        count = 1
      }
    }
    test$y2[i] = y_test[count]
  }
  points(test$end_time, test$y2, pch = symboles[as.numeric(as.character(test$DLT))+1])
  with(test, segments(start_time, y2, end_time, y2, lty = linetype[as.numeric(as.character(test$Evaluable))+1]))
  text(test$start_time, test$y2 + ygap/(3*n_obs), test$ID, cex = 0.7)
}
legend("topright", legend = c("DLT", "Non Evaluable", "Evaluable"), 
       lty = c(NA,3,1),
       pch = c(symboles[2], NA, NA),
       cex = 0.75)

