#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

ui <- fluidPage(
    
    
    titlePanel("Efficacy Endpoint Visulization for Cancer Clinical Trial"),
    tags$hr(),
    
    tabsetPanel(
        ################################# Panel for Spider ################################# 
        tabPanel("Spider Plot", fluid = TRUE,

                 sidebarPanel(
                     
                     fileInput(
                         'file', 
                         'Choose a spider plot file to upload.'
                     ),
                     
                     tags$hr(),
                     selectInput("pic", "Primay Variable", ""),
                     selectInput("level", "Select a Level", "", multiple = TRUE),
                     radioButtons("changenadir", "Add ChangeNadir?",
                                  choices = c("Yes", "No"),
                                  selected = 'No'),
                     # The dynamic input
                     uiOutput(outputId = 'change'),
                     # add lesion
                     radioButtons("lesion", "Add lesion?",
                                  choices = c("Yes", "No"),
                                  selected = 'No'),
                     radioButtons("ID", "Show ID?",
                                  choices = c("Yes", "No"),
                                  selected = 'No'),
                     # text input
                     textInput(inputId = "notes", 
                               label = "Plot notes", 
                               placeholder = "Enter text for notes")
                     
                     
                 ),
                 mainPanel(column(width = 12,
                                  plotlyOutput("spider", width = "800", height = "800"),
                                  tags$hr(),
                                  tableOutput('contents'))
                           
                 )
        ),
        ############################### Panel for Waterfall ############################## 
        tabPanel(
            "Waterfall Plot", fluid = TRUE,
            sidebarPanel(
                
                fileInput(
                    'file2', 
                    'Choose a waterfall plot file to upload.'
                ),
                
                tags$hr(),
                selectInput("pic2", "Primay Variable", ""),
                selectInput("level2", "Select a Level", "p", multiple = TRUE),
                textInput(inputId = "notes2", 
                          label = "Plot notes", 
                          placeholder = "Enter text for notes"),
                # add lesion
                radioButtons("lesion2", "Add lesion?",
                             choices = c("Yes", "No"),
                             selected = 'No')
            ),
            mainPanel(
                plotlyOutput("waterfall", width = "700", height = "700" ),
                
                tags$hr(),
                tableOutput('contents2')  
            )
            
        ),
        
        ############################### Panel for Waterfall ############################## 
        tabPanel(
            "Swimmer Plot", fluid = TRUE,
            tags$hr(),
            sidebarPanel(
                fileInput(
                    'file3_frame', 
                    'Choose a swimmer plot frame file to upload.'
                ),
                selectInput("pic3", "Primay Variable", ""),
                selectInput("level3", "Select a Level", "", multiple = TRUE),
                tags$hr(),
                

                tags$hr(),
                fileInput(
                    'file3_event', 
                    'Choose a swimmer plot event file to upload.'
                ),
                
                textInput(inputId = "notes3", 
                          label = "Plot notes", 
                          placeholder = "Enter text for notes"),
                textInput(inputId = "reference", 
                          label = "Add a reference line", 
                          placeholder = "Enter week number")
                
                
            ),
            mainPanel(
                plotlyOutput("swimmer"),
                tags$hr(),
                # tableOutput('contents3'),
                # tags$hr(),
                tableOutput('contents5'))
        )
    )
)
