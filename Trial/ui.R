#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(shiny.sanitize.errors = FALSE)
library(shiny)
library(shinythemes)


# Define UI for data upload app ----
uiData <- function(){
    fluidPage(
        sidebarPanel(
            width = 3,
            fileInput(inputId = "file1", 
                      label = "File Input:",
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))),
        
        mainPanel(tableOutput("contents"))
)}

##########################################################
####                   Graph 1                        ####
##########################################################
ui1 = function(){
    fluidPage(
        sidebarPanel(
            width = 3,
            
            sliderInput(inputId = "iconsize",
                        label = "Point size:",
                        min = 0, 
                        max = 10,
                        value = 3, 
                        step = 0.5),
            sliderInput(inputId = "textsize",
                        label = "Text size:",
                        min = 0, 
                        max = 10,
                        value = 5, 
                        step = 0.5),
            
            radioButtons(inputId = "xaxis",
                         label = "X axis unit:",
                         choices = c("Date", "Months", "Weeks"),
                         selected = "Weeks"),
            uiOutput('xaxis_str'),
            textInput(inputId = "yaxis_label",
                      label = "Y axis label:",
                      value = "Dose Level"),
            checkboxInput(inputId = "title", 
                          label = "Include Plot Title"),
            conditionalPanel(
                condition = "input.title",
                textInput(inputId = "titlename",
                          label = "Input plot title name:")),
                
            hr(),
            sliderInput(inputId = "figurewidth",
                        label = "Figure Width(inch):",
                        min = 200, 
                        max = 1500,
                        value = 1000, 
                        step = 100),
            sliderInput(inputId = "figureheight",
                        label = "Figure Height(inch):",
                        min = 200, 
                        max = 1000,
                        value = 600, 
                        step = 100),
            downloadButton("download1", "Save Plot")
        ),
       mainPanel(
           plotOutput("plot1")
       ) 
    )
}

##########################################################
####                   Graph 2                        ####
##########################################################
ui2 = function(){
    fluidPage(
        sidebarPanel(
            width = 3,
            
            sliderInput(inputId = "iconsize2",
                        label = "Point size:",
                        min = 0, 
                        max = 10,
                        value = 5, 
                        step = 1),
            sliderInput(inputId = "textsize2",
                        label = "Text size:",
                        min = 0, 
                        max = 10,
                        value = 5, 
                        step = 0.5),
            sliderInput(inputId = "linesize2",
                        label = "Line width:",
                        min = 0, 
                        max = 2,
                        value = 1, 
                        step = 0.1),
            
            radioButtons(inputId = "xaxis2",
                         label = "X axis unit:",
                         choices = c("Date", "Months", "Weeks"),
                         selected = "Weeks"),
            uiOutput('xaxis_str2'),
            textInput(inputId = "yaxis_label2",
                      label = "Y axis label:",
                      value = "Patient ID"),
            checkboxInput(inputId = "title2", 
                          label = "Include Plot Title"),
            conditionalPanel(
                condition = "input.title2",
                textInput(inputId = "titlename2",
                          label = "Input plot title name:")),
            
            hr(),
            sliderInput(inputId = "figurewidth2",
                        label = "Figure Width(inch):",
                        min = 200, 
                        max = 1500,
                        value = 1000, 
                        step = 100),
            sliderInput(inputId = "figureheight2",
                        label = "Figure Height(inch):",
                        min = 200, 
                        max = 1000,
                        value = 600, 
                        step = 100),
            downloadButton("download2", "Save Plot")
            
            
        ),
        mainPanel(
           plotOutput("plot2")
        ) 
    )
}


#########################################################
###                   Graph 3                        ####
#########################################################
ui3 = function(){
    fluidPage(
        sidebarPanel(
            width = 3,
            checkboxInput(inputId = "alldose", 
                          label = "Include all doses"),
            conditionalPanel(
                condition = "input.alldose",
                textInput(inputId = "dose",
                          label = "Enter all dose levels, using comma to separate(e.g: 1,2,4,5,6):")),
            sliderInput(inputId = "iconsize3",
                        label = "Point size:",
                        min = 0, 
                        max = 7,
                        value = 2, 
                        step = 0.5),
            sliderInput(inputId = "linesize3",
                        label = "Line width:",
                        min = 0, 
                        max = 5,
                        value = 1, 
                        step = 0.2),
            sliderInput(inputId = "textsize3",
                        label = "Text size:",
                        min = 0, 
                        max = 2,
                        value = 1, 
                        step = 0.2),
            sliderInput(inputId = "legsize3",
                        label = "Legend size:",
                        min = 0, 
                        max = 3,
                        value = 1.4, 
                        step = 0.2),
            
            radioButtons(inputId = "xaxis3",
                         label = "X axis unit:",
                         choices = c("Date", "Months", "Weeks"),
                         selected = "Weeks"),
            uiOutput('xaxis_str3'),
            textInput(inputId = "yaxis_label3",
                      label = "Y axis label:",
                      value = "Assigned Dose Level"),
            checkboxInput(inputId = "title3", 
                          label = "Include Plot Title"),
            conditionalPanel(
                condition = "input.title3",
                textInput(inputId = "titlename3",
                          label = "Input plot title name:")),
            
            hr(),
            sliderInput(inputId = "figurewidth3",
                        label = "Figure Width(inch):",
                        min = 200, 
                        max = 1500,
                        value = 1000, 
                        step = 100),
            sliderInput(inputId = "figureheight3",
                        label = "Figure Height(inch):",
                        min = 200, 
                        max = 1000,
                        value = 600, 
                        step = 100),
            downloadButton("download3", "Save Plot")
            
            
        ),
        mainPanel(
            plotOutput("plot3", width = "100%")
        ) 
    )
}


ui <- navbarPage("Phase I Trial Viz",
                 theme = shinytheme("paper"),
                 tabPanel("Data", uiData()),
                 tabPanel("Graph 1", ui1()),
                 tabPanel("Graph 2", ui2()),
                 tabPanel("Graph 3", ui3())
)