# Title: Shiny app for clinical trial visualization
# Author: Aijin Wang, Contact details: aijinwang3@gmail.com
# Script info: phase I, II and III trial visualization
# Columbia University Irving Institute for Clinical and Translational Research
# Last updated: November 6, 2019

library(shiny)
library(tidyverse)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    ##########################################################
    ####               Read in Data                       ####
    ##########################################################
    df = reactive({
        req(input$file1)
        df = read.csv(input$file1$datapath)
        df = df[complete.cases(df),]
        df = as.data.frame(apply(df, 2, function(x){
            str_trim(x, side = c("both", "left", "right"))}))
        df$DLT = as.factor(df$DLT)
        df$Evaluable = as.factor(df$Evaluable)
        df$Dose_Level = as.integer(as.character(df$Dose_Level))
        df$ID = as.integer(as.character(df$ID))
        df = df[order(as.Date(df$Therapy_Start_Date, format="%m/%d/%y")),]
    
        return (df)
    })
    
    output$contents <- renderTable({
        return(df())
    })
    
    ##########################################################
    ####                   Graph 1                        ####
    ##########################################################
    
    plot1 = reactive({
        trial = df()
        trial$Therapy_Start_Date = as.Date(trial$Therapy_Start_Date, "%m/%d/%y")
        trial$Last_Assessment_Date = as.Date(trial$Last_Assessment_Date, "%m/%d/%y")
        if (input$xaxis == "Date"){
            trial$start_time = trial$Therapy_Start_Date
            trial$end_time = trial$Last_Assessment_Date
        } 
        else if (input$xaxis == "Months"){
            trial$start_time =  as.numeric(difftime(trial$Therapy_Start_Date,trial$Therapy_Start_Date[1],units="days") / 30)
            trial$end_time = as.numeric(difftime(trial$Last_Assessment_Date,trial$Therapy_Start_Date[1],units="days") / 30)
        }
        else if (input$xaxis == "Weeks"){
            trial$start_time =  as.numeric(difftime(trial$Therapy_Start_Date,trial$Therapy_Start_Date[1],units="weeks"))
            trial$end_time = as.numeric(difftime(trial$Last_Assessment_Date,trial$Therapy_Start_Date[1],units="weeks"))
        }
        
        p = ggplot(trial) +
            
            geom_point(aes(x = start_time, y = Dose_Level, colour = Evaluable), 
                       size = input$iconsize) +
            geom_point(aes(x = end_time, y = Dose_Level, shape = DLT), 
                       colour = "orange", 
                       size = input$iconsize) +
            geom_text(aes(x = start_time, y = Dose_Level, label = ID), 
                      nudge_y = -0.2, 
                      size = input$textsize) +
            geom_text(data=subset(trial, DLT==1), 
                      aes(x = end_time, y = Dose_Level, label = ID), 
                      nudge_y = -0.2, 
                      size = input$textsize) +
            
            scale_color_manual(values = c("0" = "skyblue", "1" = "darkgrey"), 
                               labels = c("Non Evaluable", "Evaluable")) + # could have more levels
            scale_shape_manual(values=c("1" = 15, "0" = -10), 
                               labels = c("1" = "DLT", "0" = "")) + 
            scale_y_continuous(limits = c(0,max(trial$Dose_Level))) +
            
            labs(x = input$xaxis_label,
                 y = input$yaxis_label,
                 shape = "",
                 color = "") +
            
            theme_light() +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank(),
                  legend.spacing.y = unit(-0.355, "cm"))
        
        if (input$title){
            p = p + labs(title = input$titlename)
        }
        return (p)
    })
    
    output$xaxis_str <- renderUI({
        if (input$xaxis == "Date"){
            xname = "Date of the Enrollment"
            } 
        else if (input$xaxis == "Months"){
            xname = "Months since Enrollment of First Patient"
        }
        else if (input$xaxis == "Weeks"){
            xname = "Weeks since Enrollment of First Patient"
        }
        textInput(inputId = "xaxis_label",
                  label = "X axis label:",
                  value = xname)
    })
    
    # output$plot1 <- renderPlot({
    #     
    #     return(plot1())
    # }, height = 800, width = "auto")
    
    observe({
        output$plot1 <- renderPlot({
            plot1() }, height = input$figureheight, width = input$figurewidth)
    })
    
    output$download1 = downloadHandler(
      filename = 'dose_dlt.png',
      content = function(file){
        png(file, width = input$figurewidth, height = input$figureheight)
        print(plot1())
        dev.off()
      }
    )
    
    ##########################################################
    ####                   Graph 2                        ####
    ##########################################################
    
    plot2 = reactive({
        trial = df()
        trial$Therapy_Start_Date = as.Date(trial$Therapy_Start_Date, "%m/%d/%y")
        trial$Last_Assessment_Date = as.Date(trial$Last_Assessment_Date, "%m/%d/%y")
        if (input$xaxis2 == "Date"){
            trial$start_time = trial$Therapy_Start_Date
            trial$end_time = trial$Last_Assessment_Date
        } 
        else if (input$xaxis2 == "Months"){
            trial$start_time =  as.numeric(difftime(trial$Therapy_Start_Date,trial$Therapy_Start_Date[1],units="days") / 30)
            trial$end_time = as.numeric(difftime(trial$Last_Assessment_Date,trial$Therapy_Start_Date[1],units="days") / 30)
        }
        else if (input$xaxis2 == "Weeks"){
            trial$start_time =  as.numeric(difftime(trial$Therapy_Start_Date,trial$Therapy_Start_Date[1],units="weeks"))
            trial$end_time = as.numeric(difftime(trial$Last_Assessment_Date,trial$Therapy_Start_Date[1],units="weeks"))
        }
        p = ggplot(trial) +
            geom_point(aes(x = start_time, y = ID), 
                       size = input$iconsize2, 
                       pch = ".") +
            geom_point(aes(x = end_time, y = ID, shape = DLT), 
                       size = input$iconsize2) +
            geom_segment(aes(x = start_time, y = ID, xend = end_time, yend = ID, 
                             # colour = as.factor(Dose_Level), 
                             linetype = Evaluable), 
                         size = input$linesize2) +
            geom_text(aes(x = start_time, y = ID, label = Dose_Level), nudge_y = 0.5, size = input$textsize2) +
            scale_shape_manual(values=c("1" = 4, "0" = NA), 
                               labels = c("1" = "DLT", "0" = "")) + 
            #scale_color_manual(values=c("1"="#B3CDE3", "2"="#FFFFB2", "3"="#FECC5C", "4"="#FD8D3C","5"="#F03B20")) +
            scale_linetype_manual(values=c("1" = 1, "0" = 3), 
                                  labels = c("1" = "Evaluable", "0" = "Non Evaluable")) +
            scale_y_continuous(breaks = seq(min(trial$ID), max(trial$ID), by = 1)) + 
            labs(shape = "",
                 linetype = "",
                 color = "Dose Level",
                 x = input$xaxis_label2,
                 y = input$yaxis_label2) + 
            theme_light() +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.spacing.y = unit(-0.38, "cm"))
        if (input$title2){
            p = p + labs(title = input$titlename2)
        }
        
        return (p)
    })
    
    output$xaxis_str2 <- renderUI({
        if (input$xaxis2 == "Date"){
            xname = "Date of the Enrollment"
        } 
        else if (input$xaxis2 == "Months"){
            xname = "Months since Enrollment of First Patient"
        }
        else if (input$xaxis2 == "Weeks"){
            xname = "Weeks since Enrollment of First Patient"
        }
        textInput(inputId = "xaxis_label2",
                  label = "X axis label:",
                  value = xname)
    })
    
  
    observe({
      output$plot2 <- renderPlot({
        plot2() }, height = input$figureheight2, width = input$figurewidth2)
    })
    
    output$download2 = downloadHandler(
      filename = 'patient_dlt.png',
      content = function(file){
        png(file, width = input$figurewidth, height = input$figureheight)
        print(plot2())
        dev.off()
      }
    )
    
    ##########################################################
    ####                   Graph 3                        ####
    ##########################################################
    
    plot3 = function(){
        trial = df()
        trial$Therapy_Start_Date = as.Date(trial$Therapy_Start_Date, "%m/%d/%y")
        trial$Last_Assessment_Date = as.Date(trial$Last_Assessment_Date, "%m/%d/%y")
        if (input$xaxis3 == "Date"){
            trial$start_time = trial$Therapy_Start_Date
            trial$end_time = trial$Last_Assessment_Date
        } 
        else if (input$xaxis3 == "Months"){
            trial$start_time =  as.numeric(difftime(trial$Therapy_Start_Date,trial$Therapy_Start_Date[1],units="days") / 30)
            trial$end_time = as.numeric(difftime(trial$Last_Assessment_Date,trial$Therapy_Start_Date[1],units="days") / 30)
        }
        else if (input$xaxis3 == "Weeks"){
            trial$start_time =  as.numeric(difftime(trial$Therapy_Start_Date,trial$Therapy_Start_Date[1],units="weeks"))
            trial$end_time = as.numeric(difftime(trial$Last_Assessment_Date,trial$Therapy_Start_Date[1],units="weeks"))
        }
        valid_doses = unique(trial$Dose_Level)
        if (input$alldose){
            if(input$dose == ""){
                n_dose = valid_doses
            }
            else{
                n_dose = as.numeric(strsplit(input$dose,",")[[1]])
                n_dose = unique(c(n_dose, valid_doses))
                n_dose = sort(n_dose)

            }
        }
        else{
            n_dose = valid_doses # dose counts
        }

        ord = sapply(n_dose, function(x){paste0("d",x)}) # y axis names for doses
        symboles <- c(NA,4) # for DLT shape
        linetype = c(3, 1) # for evaluable line type

        trial$y2 = 0 # y2 is the new y coord for each patient
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
        with(trial, plot(type="n", x = c(min(start_time), max(end_time)), y = c(ymin, ymax),
                      xlab=input$xaxis_label3, ylab=input$yaxis_label3, yaxt="n", tcl=-0.2, cex.lab=1.3, cex.axis=1.3,
                      cex=0.7,lwd=1.2, yaxs = "i"))
        # lines dividing each dose category
        abline(h=coord[-c(1, length(coord))], lty=2, col="lightgray")
        # add y axis labels for each category
        mtext(2, at=coord[-length(coord)]+ygap/2, line=0.2, text=ord, las=0, cex=0.85)

        # for each dose, calculate the
        for (d in 1:length(n_dose)){
            test = trial[trial$Dose_Level == n_dose[d], ]
            if (nrow(test) != 0){
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
                    if (cur_start > (pre_end+ (max(trial$end_time) - min(trial$start_time)) * 0.045)){
                        count = 1
                    }
                }
                test$y2[i] = y_test[count]
            }
            points(test$end_time, test$y2, pch = symboles[as.numeric(as.character(test$DLT))+1], cex = input$iconsize3)
            with(test, segments(start_time, y2, end_time, y2, lty = linetype[as.numeric(as.character(test$Evaluable))+1], lwd = input$linesize3))
            text(test$start_time, test$y2 + ygap/(3*n_obs), test$ID, cex = input$textsize3)}
        }
        legend("topright", legend = c("DLT", "Non Evaluable", "Evaluable"),
               lty = c(NA,3,1),
               pch = c(symboles[2], NA, NA),
               cex = input$legsize3)
        if (input$title3){
            title(main = input$titlename3)
        }
        
    }
  
    
    output$xaxis_str3 <- renderUI({
        if (input$xaxis3 == "Date"){
            xname = "Date of the Enrollment"
        }
        else if (input$xaxis3 == "Months"){
            xname = "Months since Enrollment of First Patient"
        }
        else if (input$xaxis3 == "Weeks"){
            xname = "Weeks since Enrollment of First Patient"
        }
        textInput(inputId = "xaxis_label3",
                  label = "X axis label:",
                  value = xname)
    })
    output$plot3 <- renderPlot({
               plot3() })
    
    observe({
      output$plot3 <- renderPlot({
        plot3() }, height = input$figureheight3, width = input$figurewidth3)
    })
    
    output$download3 = downloadHandler(
      filename = 'patient_dose.png',
      content = function(file){
        png(file, width = input$figurewidth3, height = input$figureheight3)
        plot3()
        dev.off()
      }
    )
    
    session$onSessionEnded(function() {
        stopApp()
    })
}
