 
fluidPage(
  titlePanel("Relive your life (NYC version)"),
  sidebarLayout( 
    
    sidebarPanel( width=3, #sidebar begins; control width
      
      selectizeInput(
        inputId = "old", 
        label = "Red  Line", 
        choices =  col_choices,
        selected = "preTax"
        ),
      
      selectizeInput(
        inputId = "new", 
        label = "Blue Line", 
        choices = c(col_choices,'NULL'),
        selected = 'NULL'
        ),
      
      sliderInput("slider", 
                  "Time between Start and Retirement", 
                  min = as.Date("1980-01-01"),
                  max =as.Date("2018-12-30"),
                  value=as.Date(c("1980-01-01", "2010-01-01")),
                  timeFormat="%b %Y",
                  width = "90%"),
      textInput("expense",label = "Current Annual Expenses (Exc. Kids and Housing, in thousands of $)",value=40),
      textInput("income",label = "Current Annual Family Income (thousands of $)",value=200),
      fluidRow(column(4,
      selectizeInput(
        inputId = "kids", 
        label = "No. Kids", 
        choices = c(1,2,3,4) 
      )),
      column(8,
      selectizeInput(
        inputId = "marry", 
        label = "No. Earners in the family", 
        choices = c(0,1,2,3),
        selected =1
      ) )),
      checkboxGroupInput(
        "reset", 
        label = "Track Changes",
        choices = c("Yes"))
    
  
    ),
    mainPanel(
      tabsetPanel(type = "tabs", #tabsetPanel(tabPanel(),tabPanel(),...)
                  tabPanel("Introduction",  #tab1 tabPanel("tab title",tabItem()/*Output ) 
                               tabItem(tabName = "intro",
                                       fluidRow(column(8, align="center", offset = 2, #8+2+2=12, column width
                                                       box(htmlOutput('intro_header'), htmlOutput('intro_author'), width = 20, 
                                                           background = 'light-blue'),
                                                       tags$style(type="text/css", "#string { text-align:center }"))), #row1
                                       fluidRow(column(10, align="left", offset = 1,
                                                       box(htmlOutput('intro_body1'), #div(img(src="range_map.jpg", height=350, width=350)),
                                                           htmlOutput('intro_body2'), htmlOutput('intro_body3'), htmlOutput('intro_body4'), width = 20, background = 'light-blue'),
                                                       tags$style(type="text/css", "#string { text-align:justified }"))) #row2
                               )
                           ), #tab1
                  tabPanel("Plots",
                           tabItem(tabName = "user_port", 
                 
                                   fluidRow(column(12,
                                                   div(plotlyOutput("count"),align="center")) #plotOutput; change output plot
                                   ),
                                   br(),
                                   fluidRow(column(4, checkboxGroupInput("school", label = "Private Schools for", selected ="Nursery-PK",
                                                      choices = c("Nursery-PK","K-5","6-12","College"))),
                                            column(4, 
                                                   selectizeInput(
                                                    inputId = "rooms", 
                                                    label = "No. Bedrooms", 
                                                    choices = c(1,2,3,4),
                                                    selected =1
                                                  ),
                                            
                                                    sliderInput(
                                                    inputId = "sq",
                                                    label = "Sq footage fits your family:",
                                                    min = 200,
                                                    max = 3000,
                                                    value = 1000,
                                                    round = TRUE,
                                                    sep = '')
                                            ),
                                            column(4,checkboxGroupInput(
                                              "choice", 
                                              label = "More Choices",
                                              choices = c("No stocks","Portfolio Optimization","Home Mortgage","Tax Optimization"))) #some of more complicated features are not shown in this version
                                            )
                           )
                           
         
                           ) #tabPanel2 ends here
                  
                  
      ) #tabsetPanel ends
    ) #main panel ends
  ) #sidebar ends
) #fluidpage ends
