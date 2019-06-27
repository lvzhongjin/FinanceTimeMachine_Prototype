
 

function(input, output, session) {
  datasetInput <- reactive({ #modularize reactions; datasetInput() update when inputs change. NOT recompute when it's used in other render* functions.
 
    x0=input$slider
    x1=as.numeric(input$income)*1000
    x2=as.numeric(input$marry)
    print(paste0("x2:",x2))
    x3=as.numeric(input$kids)
    x4=as.numeric(input$sq)
    x5=as.numeric(input$expense)*1000
    x6=input$school
    x7=input$choice
    helper(x0,x1,x2,x3,x4,x5,x6,x7) #the main helper function: easy to debug under the global.R file

  })
  

  
  output$count <- renderPlotly({ #renderPlot
  
    dates = input$slider
    print(input$Reset)
 
    if (!is.null(input$reset)) {
    
      xlab_name=""
      if (sum(grep("Tax",c(input$old, input$new)))>0) {
        xlab_name=paste0("Historical Tax Rates: taxfoundation.org;\n",xlab_name)
      }
      if ("Rent" %in% c(input$old, input$new) | "Saving" %in% c(input$old, input$new)) {
        xlab_name=paste0("Historical Housing Data: S&P/Case-Shiller NY-New York Home Price Index;\n",xlab_name)
      }
      if (sum(grep("Edu",c(input$old, input$new)))>0) {
        xlab_name=paste0("Historical Tuition Data: www.nytimes.com;\n",xlab_name)
      }
      if ("FinWealth" %in% c(input$old, input$new) | "Saving" %in% c(input$old, input$new) | "preTax" %in% c(input$old, input$new) ) {
        xlab_name=paste0("Historical Income Data: www.census.gov;\n",xlab_name)
      }
      myplot=ggplot(data = datasetInput()[port_name %in% c(input$old, input$new),]) +
        scale_y_continuous(labels=dollar_format(prefix="$")) +
        geom_line(aes(x = as.Date(date), y = Value, color = port_name),size=2,linetype = "dotted")  +
        ylab("Dollar Value") + xlim(dates) + xlab(xlab_name) + 
        geom_line(data=old_df,aes(x = as.Date(date), y = Value, color = port_name),size=3) + 
        ggtitle("Your new choice (Dotted line)")+  theme(axis.text.x = element_text(size = 14),
                                                                axis.title.y = element_text(size = 14),
                                                                axis.text.y = element_text(size = 14),
                                                                axis.title.x = element_text(size = 14,hjust=0),
                                                                plot.title=element_text(size=14,face="bold",hjust=0.5))
      old_input<<-c(input$old, input$new)
      old_df<<-datasetInput()[port_name %in% c(input$old, input$new),]
      print(myplot)
      } else {
        old_input<<-c(input$old, input$new)
        old_df<<-datasetInput()[port_name %in% c(input$old, input$new),]
      xlab_name=""
      if (sum(grep("Tax",c(input$old, input$new)))>0) {
      xlab_name=paste0("Historical Tax Rates: taxfoundation.org;\n",xlab_name)
      }
      if ("Rent" %in% c(input$old, input$new) | "Saving" %in% c(input$old, input$new)) {
        xlab_name=paste0("Historical Housing Data: S&P/Case-Shiller NY-New York Home Price Index;\n",xlab_name)
      }
      if (sum(grep("Edu",c(input$old, input$new)))>0) {
        xlab_name=paste0("Historical Tuition Data: www.nytimes.com;\n",xlab_name)
      }
      if ("FinWealth" %in% c(input$old, input$new) | "Saving" %in% c(input$old, input$new)) {
        xlab_name=paste0("Historical Income Data: www.census.gov;\n",xlab_name)
      }
      
      
      gg=ggplot(data = datasetInput()[port_name %in% c(input$old, input$new),],aes(x = as.Date(date), y = Value, color = port_name)) + 
        scale_y_continuous(labels=dollar_format(prefix="$")) +
        geom_line(size=1) +geom_point( size = 2) +
        theme(axis.text.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              axis.title.x = element_text(size = 14,hjust=0)) + 
        ylab("Dollar Value") + xlim(dates) + xlab(xlab_name)
      
      global_line = ggplotly(gg) 
      global_line

    }

  })
  
  
  output$intro_header = renderUI({
    h1('Finance Time Machine (Prototype-00)')
  })
  
  output$intro_author = renderUI({
    h4('Prof. Zhongjin Lu <lvzhongjin@gmail.com>')
  })
  
  output$intro_body1 = renderUI({
    h3('Disclaimer A. This app will NOT tell you which stock to buy: Be suspicious when you get such advice, especially when it is free.')
  })
  
  output$intro_body2 = renderUI({
    div(h3('Disclaimer B. This time machine takes you back to a point of time in the past and 
    allows you to inspect how your decisions shape your financial status today:'), h3('Sustainable life style? Invest in stocks? 
    Private schools affordable? for how many kids? Rent or buy?
    Enough savings for retirement? Paying too much tax? Portfolio optimization?'))
  })
  
  output$intro_body3 = renderUI({
    h3('To play, start with inputs on the left panel and then click "Plots" above. Have fun!')
    })
  
  output$intro_body4 = renderUI({
    p('This version is for people with kids living in NYC. Extensions can be expected.' )
  })
  
  # If checkbox input is checked, set month range equal to the breeding season estimate for the selected species
  observeEvent(input$rooms,{
    print(str(input$rooms))
    if (input$rooms==1){
      updateNumericInput(session, "sq", min = 200)
    } else if (input$rooms==2) {
      updateNumericInput(session, "sq", min = 800)
    } else if (input$rooms==3) {
      updateNumericInput(session, "sq", min = 1100)
    } else if (input$rooms==4) {
      updateNumericInput(session, "sq", min= 1300)
    } 
  })
  
}


 