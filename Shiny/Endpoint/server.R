#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plyr); library(dplyr)
library(viridis)
library(plotly)

server <- function(input, output, session) {
    
    ################################### Spider ################################
    
    myData <- reactive({
        if (is.null(input$file)) {
            return(NULL)
        } else {
            read.csv(input$file$datapath, na.strings = "") 
            
        }
    })
    
    ## update 'column' selector
    observeEvent(myData(), {
        col <- colnames(myData())[!(colnames(myData())  %in% c("ID", "changebsl", "week", "changenadir01", "sum"))]
        
        updateSelectInput(session, "pic", choices = col)
    })
    
    ## update 'level' selector
    observeEvent(input$pic, {
        column_levels <- unique(myData()[[input$pic]])
        updateSelectInput(session, "level", 
                          choices = column_levels,
                          label = paste("Choose level in", input$pic), 
                          selected = column_levels)
    }, ignoreInit = TRUE)
    
    output$change <- renderUI({
        
        # This input exists if the `static`
        # one is equal to `Yes` only
        if (input$changenadir == 'Yes') {
            selectInput(inputId = 'change',
                        label = 'changeNadir > 20',
                        choices = c("Yes" = 1,
                                    "No" = 0),
                        multiple = TRUE)
        } else {
            return(NULL)
        }
        
    })
    
    
    # data with primary variable = the selected level
    myData3  <- reactive({
        df = myData()
        # data with primary variable = the selected level
        df = df[df[,input$pic] %in% input$level,]
        df[,input$pic] = as.factor(df[,input$pic])
        df %>%
            # data adding change in sum from the baseline
            group_by(ID) %>%
            arrange(ID, week) %>%
            mutate(changebsl = 100*(sum - first(sum))/(first(sum)))  %>%
            ungroup() %>%
            mutate(changebsl = replace(changebsl, changebsl == "NaN", 0))
        
    })
    
    output$contents <- renderTable({
        req(input$file)
        
        return(myData3())
        
    })
    
    
    
    
    output$spider <- renderPlotly({
        
        if (!is.null(input$file)){
            df = myData3()
            a = df %>%
                group_by(ID) %>%
                plot_ly(x=~week, 
                        y = ~changebsl, 
                        type = "scatter", 
                        color = ~get(input$pic),
                        text = ~ paste("ID:", ID), 
                        mode = "lines") %>%
                add_lines(inherit = FALSE, 
                          y = 20, 
                          x = c(0:max(df$week) ), 
                          line = list(dash = "dash", color = "black"), 
                          name = "reference line 1 (20%)") %>%
                add_lines(inherit = FALSE, 
                          y = -30, 
                          x = c(0:max(df$week)), 
                          line = list(dash = "dash", color = "red"), 
                          name = "reference line 2 (-30%)") %>%
                layout(title = "Spider plot for changes from baseline",
                       xaxis = list(showgrid = FALSE,
                                    title = "Week of Visit", 
                                    range = c(-2,(max(df$week) * 1.2))),
                       yaxis = list(showgrid = FALSE, 
                                    title = "Change from Baseline (%)",
                                    range = c(-100, 100)) ) %>%
                layout(margin = list(l = 50, r = 50, t = 100, b = 100),
                       annotations = list(text = input$notes,
                                          font = list(size = 12),
                                          showarrow = FALSE,
                                          xref = 'paper', x = 0,
                                          yref = 'paper', y = -1,
                                          yanchor = "bottom"))
            
            if (input$changenadir == 'Yes' & 'changenadir01' %in% colnames(df) ) {
                a = a %>%
                    filter(changenadir01 %in% input$change ) %>%
                    add_trace(inherit = FALSE, 
                              x=~week, 
                              y = ~changebsl, 
                              type = "scatter",
                              color = ~ as.factor(changenadir01), 
                              symbol = ~ as.factor(changenadir01), 
                              mode = "markers", 
                              marker = list(size = 7))
            }

            if (input$lesion == 'Yes' & 'lesion' %in% colnames(df) ) {
                df_lesion = df[df$lesion == "1",]
                a = a %>%
                    add_trace(name = "lesion", 
                              inherit = FALSE, 
                              x= ~df_lesion$week, 
                              y =~df_lesion$changebsl-1.5,
                              text = "*", 
                              type = "scatter", 
                              mode = "text", 
                              textposition = 'middle center')
            }

            if (input$ID == 'Yes' ) {
                df_ID = df %>% 
                    group_by(ID) %>%
                    filter(week == max(week)) 
                a = a %>%
                    add_trace(name = "ID", 
                              inherit = FALSE, 
                              x = ~df_ID$week, 
                              y = ~df_ID$changebsl, 
                              text = ~df_ID$ID, 
                              type = "scatter", 
                              mode = "text", 
                              textposition = 'middle right')
            }

            return (a)

        }
        
    })
    
    
    ##########################################  Waterfall ######################################
    
    wfData <- reactive({
        if (is.null(input$file2)) {
            return(NULL)
        } else {
            read.csv(input$file2$datapath)
        }
    }) 
    
    
    
    
    ## update 'column' selector
    observeEvent(wfData(), {
        col2 <- colnames(wfData())[!(colnames(wfData())  %in% c("ID", "changebsl", "sum"))]
        updateSelectInput(session, "pic2", choices = col2)
    })
    
    ## update 'level' selector
    observeEvent(input$pic2, {
        column_levels <- unique(wfData()[[input$pic2]])
        updateSelectInput(session, "level2", 
                          choices = column_levels,
                          label = paste("Choose level in", input$pic2),
                          selected = column_levels)
    }, ignoreInit = TRUE)
    
    wfData2 <-  reactive({
        if (is.null(input$file2)) {
            return(NULL)
        } else {
            wfData() %>%
                group_by(ID) %>%
                arrange(ID, week) %>%
                mutate(changebsl = 100*(sum - first(sum))/first(sum)) %>%
                ungroup() %>%
                mutate(changebsl = replace(changebsl, changebsl == "NaN", 0)) %>%
                group_by(ID) %>%
                mutate(id = row_number()) %>%
                filter(id != 1 ) %>%
                filter(changebsl == min(changebsl) ) %>%
                slice(1) %>% 
                ungroup() %>%
                as.data.frame()
        }
    }) 

    
    output$contents2 <- renderTable({
            return(wfData2())
    })
    
    output$waterfall <- renderPlotly({
        
        if(is.null(input$file2)){
            return(NULL)
        } else {
            a =  wfData2() %>%
                mutate(changebsl = ifelse((wfData2()[,input$pic2] %in% input$level2), changebsl, 0)) %>%
                mutate(id = as.factor(unclass(fct_reorder(ID, desc(changebsl)))))%>%
                mutate(ID = factor(ID, levels(ID)[order(id)])) %>%
                plot_ly() %>%
                add_trace(x = ~ID, 
                          y = ~changebsl,  
                          color = ~ as.factor(get(input$pic2)), 
                          type = "bar", 
                          width = 0.9, 
                          text = ~paste("ID: ", wfData2()$ID)) %>%
                layout(bargap = 4,
                       title = "Waterfall plot for changes in QoL scores",
                       xaxis = list(showgrid = FALSE, title = "", tickangle = -90),
                       yaxis = list(showgrid = FALSE, title = "Best RECIST response (%)", 
                                    range = c(-100, 100))) %>%
                layout(margin = list(l = 50, r = 50, t = 100, b = 250),
                       annotations = list(text = input$notes2,
                                          font = list(size = 12),
                                          showarrow = FALSE,
                                          xref = 'paper', x = 0,
                                          yref = 'paper', y = -1,
                                          yanchor = "bottom"))
            if (input$lesion2 == "Yes" & "lesion" %in% colnames(wfData2())) {
                a = a %>%
                    filter(lesion == 1) %>%
                    add_trace(name = "lesion", 
                              inherit = FALSE, 
                              x = ~ID, 
                              y = ~changebsl,  
                              text = "*", 
                              type = "scatter", 
                              mode = "text", 
                              textposition = 'middle center')        
            } 
            else {
                return(a)
                }
            
        }
        
        
    })
    
    ##########################################  swimmer ######################################
    
    sfData <- reactive({
        if (is.null(input$file3_frame)) {
            return(NULL)
        } else {
            read.csv(input$file3_frame$datapath) 
        }
    }) 
    
    seData <- reactive({
        if (is.null(input$file3_event)) {
            return(NULL)
        } else {
            read.csv(input$file3_event$datapath) 
        }
    }) 
    
    sfData2 <- reactive({
        if (!is.null(input$file3_frame)) {
            sfData() %>%
                group_by(ID) %>%
                filter(week == max(week))
            } else{
                    return(NULL)
                }
        
    }) 
    ## update 'column' selector
    observeEvent(sfData(), {
        col3 <- colnames(sfData())[!(colnames(sfData())  %in% c("ID", "week"))]
        updateSelectInput(session, "pic3", choices = col3)
    })
    
    ## update 'level' selector
    observeEvent(input$pic3, {
        column_levels <- unique(sfData()[[input$pic3]])
        updateSelectInput(session, 
                          "level3", 
                          choices = column_levels,
                          label = paste("Choose level in", input$pic3), 
                          selected = column_levels)
    }, ignoreInit = TRUE)
    
    # output$contents3 <- renderTable({
    #     req(input$file3_frame)
    #     return(sfData2())
    # })
    # 
    # output$contents4 <- renderTable({
    #     req(input$file3_event)
    #     return(df)
    # })
    
    sfData_selected <-  reactive({
            sfData()[sfData()[,input$pic3] %in% input$level3,]
        })
    
    
    
    seData_selected <- reactive({
            join(seData(), sfData_selected(), by = "ID", match = "all", type = "right") 
        })
    
    output$contents5 <- renderTable({
        req(input$file3_frame)
        req(input$file3_event)
        return(seData_selected())
    })
    
    output$swimmer <- renderPlotly ({
        
        if(is.null(input$file3_frame)|is.null(input$file3_event)){
            return(NULL)
        } else {
            
            P = sfData() %>%
                mutate(ID = as.factor(ID)) %>%
                mutate(ID = fct_reorder(ID, week))  %>%
                mutate(week = ifelse(sfData()[,input$pic3] %in% input$level3, week, 0)) %>%
                plot_ly( width = 1000, height = 800) %>%
                add_trace(x = ~week, 
                          y = ~ID,  
                          orientation = "h",
                          color = ~ as.factor(get(input$pic3)), 
                          type = "bar", 
                          width = 0.9) %>%
                
                # events 
                add_trace(x = ~seData_selected()$event_time, 
                          y = ~as.factor(seData_selected()$ID), 
                          type = "scatter", 
                          mode="markers", 
                          symbol = seData_selected()$event,
                          symbols = c('cross', 'diamond', 'square', 'triangle-down', 'triangle-left', 'triangle-right', 'triangle-up'),
                          marker = list(size = 7, color = "black")) %>%
                
                # reference line
                layout(shapes=list(type='line', 
                                   x0= input$reference, 
                                   x1=input$reference, 
                                   y0=0, 
                                   y1=length(sfData()$ID), 
                                   line = list(dash = "dash", color = "red")),
                       # style
                       title = "Swimmers' plot",
                       xaxis = list(showgrid = FALSE, 
                                    title = "Weeks since Enrollment", 
                                    range = c(0, max(sfData()$week)+5), 
                                    titlefont = list(size = 12)),
                       yaxis = list(showgrid = FALSE, 
                                    title = "Subject ID", 
                                    titlefont = list(size = 12))) %>%
                
                # legend position
                layout(legend = list(x = 0.7, y = 0.1)) %>%
                
                # Notes position
                layout(
                    margin = list(l = 50, r = 50, t = 75, b = 150),
                    annotations = list(text = input$notes3,
                                       font = list(size = 12),
                                       showarrow = FALSE,
                                       xref = 'paper', x = 0,
                                       yref = 'paper', y = -0.25,
                                       yanchor = "top"))}
        })
}