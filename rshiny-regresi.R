library(shiny)
# Define UI
ui <- fluidPage(theme =shinythemes::shinytheme('cerulean'),
      titlePanel('Linear Regression Analysis'),
  sidebarLayout(
    sidebarPanel(
      textInput('name','What is your name?'),
      fileInput('file', 'Upload csv file',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )),
      selectInput("separator","Choose the separator",
                  choices = c('semi-colon'=";",
                              'comma'=",",
                              'colon'=":"), selected=";"),
      tags$hr(),
      uiOutput("dependent"),
      uiOutput("independents"),
      tags$hr(),
      actionLink('Help','How to use this apps?'),
      p(em("Developed by"),br("Jamaluddin Rabbani"),style="text-align:center; font-family: times")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Home',
                 fluidRow(column(
                   br(),
                   p('This app is use for regression linear analysis. 
                     You can upload your data(only in csv format),
                     choose the explanatory and response variable,
                     and then you can see the summary of model, some
                     assumption test, and plot on this apps.Still confuse?
                     You can click on How to use this Apps?'),
                   br(),width = 12,style="background-color:white;
                   border-left:8px white"
                   )),
                 br(),
                 fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                          column(
                            p("In regression analysis, you have to satisfy the Gauss-Markov Assumption
                              to check if the model works well for the data at hand. The assumptions are:",
                              style="color:black;text-align:justify"),
                            p("1. Normality of residuals",br(),br(
                              "2. Homoscedasticity"),br(
                                "3. No auto-correlation"),
                              style="color:black;text-align:center;
                              background-color:white;padding:15px;border:1px solid black"),br(),
                            
                            width=8,style="background-color:white;border-radius: 10px"))
                 
                 )
        ,
        tabPanel('Data',DT::dataTableOutput("dataframe")),
        tabPanel('Model',
                 fluidRow(column(width=2),
                          column(
                            h4(p("Summary of model",
                                 style="color:black;text-align:center")),
                            width=8,style="background-color:lavender;border-radius: 10px")),
                 verbatimTextOutput("regTab"),
                 fluidRow(column(
                   br(),
                   p('There is the summary of your model,
                     the significance level of your 
                     explanatory variable represented by *,
                     Multiple dan Adjusted R-squared is a metrics 
                     to know how well your model fit to data.There is 
                     an estimation of each parameter too'),
                   br(),width = 12,style="background-color:lavender;
                   border-left:8px solid blue"
                 )),
                 fluidRow(column(width=2),
                          column(
                            h4(p("Analysis Of Variance Table",
                                 style="color:black;text-align:center")),
                            width=8,style="background-color:lavender;border-radius: 10px")),
                 verbatimTextOutput("tabel_anova")
                 ),
        tabPanel('Assumption',
                 fluidRow(column(width=2),
                          column(
                            h4(p("Checking normality of residuals(Kolmogorov-Smirnov test)",
                                 style="color:black;text-align:center")),
                            width=8,style="background-color:lavender;border-radius: 10px")),
                 verbatimTextOutput("normal_test"),
                 fluidRow(column(
                   br(),
                   p('Your model does not satisfy the normal assumption
                     if the p-value of test below alpha'),
                   br(),width = 12,style="background-color:skyblue"
                   )),
                 br(),
                 fluidRow(column(width=2),
                          column(
                            h4(p("Checking Expected Value of residuals=0",
                                 style="color:black;text-align:center")),
                            width=8,style="background-color:lavender;border-radius: 10px")),
                 verbatimTextOutput("ttest"),
                 fluidRow(column(
                   br(),
                   p('Your model does not satisfy the expected value of residual=0 
                      assumption if the p-value of test below alpha'),
                   br(),width = 12,style="background-color:skyblue"
                   )),
                 fluidRow(column(width=2),
                          column(
                            h4(p("Checking there is autocorrelation or not(Durbin-Watson test)",
                                 style="color:black;text-align:center")),
                            width=8,style="background-color:lavender;border-radius: 10px")),
                 verbatimTextOutput("autocorrelation"),
                 fluidRow(column(
                   br(),
                   p('There is autocorrelation if the p-value of test below alpha'),
                   br(),width = 12,style="background-color:skyblue"
                   )),
                 br(),
                 fluidRow(column(width=2),
                          column(
                            h4(p("VIF test to check multicolinearity between variable",
                                 style="color:black;text-align:center")),
                            width=8,style="background-color:lavender;border-radius: 10px")),
                 verbatimTextOutput('vif_test'),
                 fluidRow(column(
                   br(),
                   p('There is the multicolinearty symptoms if the your VIF value
                     greater than 10.'),
                   br(),width = 12,style="background-color:skyblue"
                   )),
                 br(),
                 fluidRow(column(width=2),
                          column(
                            h4(p("Homoscedasticity test(Breusch-Pagan Test)",
                                 style="color:black;text-align:center")),
                            width=8,style="background-color:lavender;border-radius: 10px")),
                 verbatimTextOutput('homogen_test'),
                 fluidRow(column(
                   br(),
                   p('Your model does not satisfy the homoscedasticity
                     assumption if p-value of the test below alpha.'),
                   br(),width = 12,style="background-color:skyblue"))
              ),
        tabPanel('Plot',
                 fluidRow(column(width=2),
                          column(
                            h4(p("Scatter Plot for explanatory vs response variable",
                                 style="color:black;text-align:center")),
                            width=8,style="background-color:lavender;border-radius: 10px")),
                 plotOutput('scatter'),
                 fluidRow(column(
                   br(),
                   p('This scatter plot only works if the response and explanatory
                     variable is one,each.'),
                   br(),width = 12,style="background-color:pink;
                   border-left:8px lavender"
                   )),
                 fluidRow(column(width=2),
                          column(
                            h4(p("Plot quantile-quantile for residuals",
                                 style="color:black;text-align:center")),
                            width=8,style="background-color:lavender;border-radius: 10px")),
                 plotOutput('normal'),
                 fluidRow(column(
                   br(),
                   p('This plot can be use to check the normality of residuals.
                    If majority of each points follows the red line, the residuals
                     could be normal'),
                   br(),width = 12,style="background-color:pink;
                   border-left:8px lavender"
                   )),
                 fluidRow(column(width=2),
                          column(
                            h4(p("Fitted value vs residual",
                                 style="color:black;text-align:center")),
                            width=8,style="background-color:lavender;border-radius: 10px")),
                 plotOutput('dugaan_vs_sisaan'))
      )
    )
  )
)


# Define server logic
server <- function(input, output,session) {
  observeEvent(input$Help,{
    showModal(modalDialog('Hello',paste(input$name),'!',
                          "First, you have to import the data in csv format.
                          You can choose the separator based on your csv file.After that, you can see 
                          your imported data at 'Data' panel. To construct a linear model, you have to 
                          input at least one response variable and explanatory variable, each.
                          Next, you will see the summary of model on 'summary' panel, the assumption test
                          on 'Assumption' panel, and plot on 'plot' panel.",'Good luck',paste(input$name),'!'))
  })
  df_products_upload <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
    return(df)
  })
  
  output$dataframe<- DT::renderDataTable({
    df <- df_products_upload()
    DT::datatable(df)
  })
  
  output$dependent <- renderUI({
    df <- df_products_upload()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("dependent","Response Variable:",items)
  })
  output$independents <- renderUI({
    df <- df_products_upload()
    if (is.null(df)) return(NULL)
    checkboxGroupInput('independents','Explanatory variable:', names(df))
  })
  output$scatter<-renderPlot({
    data<-df_products_upload()
    plot(data[,input$independents],data[,input$dependent],
         xlab = input$independents,ylab = input$dependent,
         main=paste(input$independents,'vs',input$dependent))
    
  })
  modelq<- reactive({
    lm(as.formula(paste(input$dependent,
                        " ~ ",paste(input$independents,collapse="+"))),
       data=df_products_upload())
  })
  #model
  output$regTab <- renderPrint({
    if(!is.null(input$independents)){
      summary(modelq())
    } else {
      print(data.frame(Warning="Please select Explanatory and Response Variable."))
    }
  })
  #tabel anova
  output$tabel_anova <- renderPrint({
    if(!is.null(input$independents)){
      anova(modelq())
    } else {
      print(data.frame(Warning="Please select Explanatory and Response Variable."))
    }
  })
  #plot normalitas sisaan:
  sisaan<-reactive({
    modelq()$residuals
  })
  output$normal<-renderPlot({
    if(!is.null(input$independents)){
      qqnorm(sisaan())
      qqline(sisaan(),col='red')
    } else {
      print(data.frame(Warning="Please select Explanatory and Response Variable."))
    }
  })
  #plot sisaan vs dugaan
  dugaan<-reactive({
    predict(modelq())
  })
  output$dugaan_vs_sisaan<-renderPlot({
    if(!is.null(input$independents)){
      plot(dugaan(),sisaan(),xlab = 'Fitted values',
           ylab='Residuals',main='Residual vs Fitted')
    } else {
      print(data.frame(Warning="Please select Explanatory and Response Variable."))
    }
  })
  #uji asumsi kenormalan
  normal_residual<-reactive({
    sisaan()-mean(sisaan())/sd(sisaan())
  })
  test_ks<-reactive({
    ks.test(x = normal_residual(),y = 'pnorm')
  })
  output$normal_test<-renderPrint({
    if(!is.null(input$independents)){
      test_ks()
    } else {
      print(data.frame(Warning="Please select Explanatory and Response Variable."))
    }
  })
  #Expected residual=0 assumption
  output$ttest<-renderPrint({
    if(!is.null(input$independents)){
      t.test(sisaan(),mu =0,conf.level = 0.95)
    } else {
      print(data.frame(Warning="Please select Explanatory and Response Variable."))
    }
  })
  #No autocorrelation
  output$autocorrelation<-renderPrint({
    if(!is.null(input$independents)){
      car::dwt(modelq())
    } else {
      print(data.frame(Warning="Please select Explanatory and Response Variable."))
    }
  })
  #Multicollinearity test
  output$vif_test<-renderPrint({
    if(!is.null(input$independents)){
      car::vif(modelq())
    } else {
      print(data.frame(Warning="Please select Explanatory and Response Variable."))
    }
  })
  #homogenity of residual:
  output$homogen_test<-renderPrint({
    if(!is.null(input$independents)){
      lmtest::bptest(modelq())
    } else {
      print(data.frame(Warning="Please select Explanatory and Response Variable."))
    }
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)