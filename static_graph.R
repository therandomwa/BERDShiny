# # Static graph option 1 amd 2 in phase I trial

trial = readxl::read_excel("Data for CYap.xlsx")
names(trial) <- gsub(" ", "_", names(trial))
trial$DLT = as.factor(trial$DLT)
trial$Evaluable = as.factor(trial$Evaluable)
library(tidyverse)
trial$start_days =  as.numeric(difftime(trial$Therapy_Start_Date,trial$Therapy_Start_Date[1],units="days") + 1)
trial$end_days = as.numeric(difftime(trial$Last,trial$Therapy_Start_Date[1],units="days") + 1)
trial$DLT = factor(trial$DLT, levels = c("Yes", "No"))
#trial$Evaluable = factor(trial$Evaluable, levels = c("0", "1"))

ggplot(trial) + 
  geom_point(aes(x = start_days, y = Dose_Level, colour = Evaluable)) +
  geom_point(aes(x = end_days, y = Dose_Level, shape = DLT), colour = "orange") + 
  scale_shape_manual(values=c("Yes" = 15, "No" = -10), labels = c("Yes" = "DLT", "No" = "")) + 
  #geom_point(data=subset(trial, DLT=="Yes"), aes(x = end_days, y = Dose_Level), shape = 15, colour = "orange", size = 2) +
  geom_text(aes(x = start_days, y = Dose_Level, label = ID), nudge_y = -0.2, size = 2) +
  geom_text(data=subset(trial, DLT=="Yes"), aes(x = end_days, y = Dose_Level, label = ID), nudge_y = -0.2, size = 2) +
  scale_color_manual(values = c("0" = "skyblue", "1" = "darkgrey")) +
  scale_y_continuous(limits = c(0,5)) + 
  theme_bw()

ggplot(trial) +
  geom_point(aes(x = start_days, y = ID), size = 0.3) +
  geom_point(aes(x = end_days, y = ID), size = 0.3) +
  geom_segment(aes(x = start_days, y = ID, xend = end_days, yend = ID, colour = as.factor(Dose_Level), linetype = Evaluable), size = 1) +
  # geom_point(data=subset(trial, DLT=="Yes"), aes(x = end_days, y = ID), shape = 15, size = 1.3) +
  geom_point(aes(x = end_days, y = ID, shape = DLT))+
  scale_shape_manual(values=c("Yes" = 4, "No" = -10), labels = c("Yes" = "DLT", "No" = "")) + 
  scale_color_manual(values=c("1"="#B3CDE3", "2"="#FFFFB2", "3"="#FECC5C", "4"="#FD8D3C","5"="#F03B20")) +
  scale_linetype_manual(values=c("1" = 1, "0" = 3), labels = c("Evaluable", "Non Evaluable")) +
  scale_y_continuous(breaks = seq(min(trial$ID), max(trial$ID), by = 1)) + 
  labs(shape = "",
       linetype = "",
       color = "Dose Level",
       x = "Days since Enrollment of First Patient",
       y = "Patient ID") + 
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
