#BASE FUNCTION___________________________________________________________________________________________________________________________
library(tidyverse)
library(dplyr)
library(ggplot2)
#' 	
#' 	
#LOAD IN DATA________________________________________________________________________________________________	
df <- read.csv("SCFP2019.csv")[ ,c('EDN_INST', 'PAYEDU1', 'PAYEDU2', 'PAYEDU3','PAYEDU4',
                            'PAYEDU5', 'PAYEDU6', 'INCOME', 'RACE', 'HHSEX', 'AGECL')]	

loan_ratio=df$EDN_INST/df$INCOME	
payment_ratio=(df$PAYEDU1+df$PAYEDU2+df$PAYEDU3+	
                   df$PAYEDU4+df$PAYEDU5+df$PAYEDU6)*12/df$INCOME	

df <- cbind(df, loan_ratio)	
df <- cbind(df, payment_ratio)	

df$RACE=factor(df$RACE)	
df$HHSEX=factor(df$HHSEX)	
df$AGECL=factor(df$AGECL)	
levels(df$RACE)=list("White Non-Hispanic"="1", "Black/African American"="2",	
                     "Hispanic"="3", "Other"="5")	
levels(df$HHSEX)=list("Male"="1", "Female"="2")	
levels(df$AGECL)=list("<35"="1", "35-44"="2","45-54"="3",	
                      "55-64"="4", "65-74"="5", "75+"="6")	

names(df)[names(df) == "RACE"] <- "Race"	
names(df)[names(df) == "HHSEX"] <- "Gender"	
names(df)[names(df) == "AGECL"] <- "Age"	

df <- subset(df, EDN_INST > 0)
#END LOAD DATA______________________________________________________________
#' 	
#' 	
#' 	
#FINAL FUCNTION_______________________________________________________________________________________________	
hist_student_loans<-function(D,RespVar,X1=NULL,X2=NULL){	
    
    #TOTAL___________________________________________________________________________________	
    if(RespVar=="EDN_INST"){	
        #NO X	
        if(is.null(X1) & is.null(X2)){	
            ggplot(D,aes(x=!!sym(RespVar),y = (..count..)/sum(..count..)))+	
                geom_histogram(breaks=c(seq(0, 100000, by=5000),	
                                        500000),position="identity")+	
                coord_cartesian(xlim=c(0,100000))+	
                ggtitle("Total Student Loan Amounts")+	
                scale_x_continuous(name = "Total Amount of Loan")+	
                scale_y_continuous(name = "Percent of People with Loans")	
            #X1 Only      	
        }else if(!is.null(X1) & is.null(X2)){	
            ggplot(D,aes(x=!!sym(RespVar),y = (..count..)/sum(..count..),fill=!!sym(X1)))+	
                geom_histogram(breaks=c(seq(0, 100000, by=5000),	
                                        500000),position="identity",alpha=.4)+	
                coord_cartesian(xlim=c(0,100000))+	
                ggtitle("Total Student Loan Amounts")+	
                scale_x_continuous(name = "Total Amount of Loan")+	
                scale_y_continuous(name = "Percent of People with Loans")	
            #X2 Only    	
        }else if(is.null(X1) & !is.null(X2)){	
            ggplot(D,aes(x=!!sym(RespVar),y = (..count..)/sum(..count..)))+	
                geom_histogram(breaks=c(seq(0, 100000, by=5000),	
                                        500000),position="identity")+	
                coord_cartesian(xlim=c(0,100000))+	
                ggtitle("Total Student Loan Amounts")+	
                scale_x_continuous(name = "Total Amount of Loan")+	
                scale_y_continuous(name = "Percent of People with Loans")+	
                facet_wrap(vars(!!sym(X2)))	
            #BOTH	
        }else if(!is.null(X1) & !is.null(X2)){	
            ggplot(D,aes(x=!!sym(RespVar),y = (..count..)/sum(..count..),fill=!!sym(X1)))+	
                geom_histogram(breaks=c(seq(0, 100000, by=5000),	
                                        500000),position="identity",alpha=.4)+	
                coord_cartesian(xlim=c(0,100000))+	
                ggtitle("Total Student Loan Amounts")+	
                scale_x_continuous(name = "Total Amount of Loan")+	
                scale_y_continuous(name = "Percent of People with Loans")+	
                facet_wrap(vars(!!sym(X2)))	
        }	
        
        #LOAN RATIO________________________________________________________________________________
    }else if(RespVar=="loan_ratio"){	
        #NO X	
        if(is.null(X1) & is.null(X2)){	
            ggplot(D,aes(x=!!sym(RespVar),y = (..count..)/sum(..count..)))+	
                geom_histogram(breaks=c(seq(0, 5, by=.25),	
                                        100),position="identity")+	
                coord_cartesian(xlim=c(0,5))+	
                ggtitle("Ratio of Student Loan Amount to Income")+	
                scale_x_continuous(name = "Loan Amount to Income Ratio")+	
                scale_y_continuous(name = "Percent of People with Loan Amount to Income Ratio")	
            #X1 Only     	
        }else if(!is.null(X1) & is.null(X2)){	
            ggplot(D,aes(x=!!sym(RespVar),y = (..count..)/sum(..count..),fill=!!sym(X1)))+	
                geom_histogram(breaks=c(seq(0, 5, by=.25),	
                                        100),position="identity",alpha=.4)+	
                coord_cartesian(xlim=c(0,5))+	
                ggtitle("Ratio of Student Loan Amount to Income")+	
                scale_x_continuous(name = "Loan Amount to Income Ratio")+	
                scale_y_continuous(name = "Percent of People with Loan Amount to Income Ratio")	
            #X2 Only      	
        }else if(is.null(X1) & !is.null(X2)){	
            ggplot(D,aes(x=!!sym(RespVar),y = (..count..)/sum(..count..)))+	
                geom_histogram(breaks=c(seq(0, 5, by=.25),	
                                        100),position="identity")+	
                coord_cartesian(xlim=c(0,5))+	
                ggtitle("Ratio of Student Loan Amount to Income")+	
                scale_x_continuous(name = "Loan Amount to Income Ratio")+	
                scale_y_continuous(name = "Percent of People with Loan Amount to Income Ratio")+	
                facet_wrap(vars(!!sym(X2)))	
            #BOTH	
        }else if(!is.null(X1) & !is.null(X2)){	
            ggplot(D,aes(x=!!sym(RespVar),y = (..count..)/sum(..count..),fill=!!sym(X1)))+	
                geom_histogram(breaks=c(seq(0, 5, by=.25),	
                                        100),position="identity",alpha=.4)+	
                coord_cartesian(xlim=c(0,5))+	
                ggtitle("Ratio of Student Loan Amount to Income")+	
                scale_x_continuous(name = "Loan Amount to Income Ratio")+	
                scale_y_continuous(name = "Percent of People with Loan Amount to Income Ratio")+	
                facet_wrap(vars(!!sym(X2)))	
        }	
        
        #PAYMENT RATIO________________________________________________________________________________	
    }else if(RespVar=="payment_ratio"){	
        #NO X	
        if(is.null(X1) & is.null(X2)){	
            ggplot(D,aes(x=!!sym(RespVar),y = (..count..)/sum(..count..)))+	
                geom_histogram(breaks=c(seq(0, .4, by=.02),	
                                        100),position="identity")+	
                coord_cartesian(xlim=c(0,.4))+	
                ggtitle("Ratio of Annual Student Loan Payment to Income")+	
                scale_x_continuous(name = "Payment to Income Ratio")+	
                scale_y_continuous(name = "Percent of People with Payment to Income Ratio")	
            #X1 Only     	
        }else if(!is.null(X1) & is.null(X2)){	
            ggplot(D,aes(x=!!sym(RespVar),y = (..count..)/sum(..count..),fill=!!sym(X1)))+	
                geom_histogram(breaks=c(seq(0, .4, by=.02),	
                                        100),position="identity",alpha=.4)+	
                coord_cartesian(xlim=c(0,.4))+	
                ggtitle("Ratio of Annual Student Loan Payment to Income")+	
                scale_x_continuous(name = "Payment to Income Ratio")+	
                scale_y_continuous(name = "Percent of People with Payment to Income Ratio")	
            #X2 Only   	
        }else if(is.null(X1) & !is.null(X2)){	
            ggplot(D,aes(x=!!sym(RespVar),y = (..count..)/sum(..count..)))+	
                geom_histogram(breaks=c(seq(0, .4, by=.02),	
                                        100),position="identity")+	
                coord_cartesian(xlim=c(0,.4))+	
                ggtitle("Ratio of Annual Student Loan Payment to Income")+	
                scale_x_continuous(name = "Payment to Income Ratio")+	
                scale_y_continuous(name = "Percent of People with Payment to Income Ratio")+	
                facet_wrap(vars(!!sym(X2)))	
            #BOTH	
        }else if(!is.null(X1) & !is.null(X2)){	
            ggplot(D,aes(x=!!sym(RespVar),y = (..count..)/sum(..count..),fill=!!sym(X1)))+	
                geom_histogram(breaks=c(seq(0, .4, by=.02),	
                                        100),position="identity",alpha=.4)+	
                coord_cartesian(xlim=c(0,.4))+	
                ggtitle("Ratio of Annual Student Loan Payment to Income")+	
                scale_x_continuous(name = "Payment to Income Ratio")+	
                scale_y_continuous(name = "Percent of People with Payment to Income Ratio")+	
                facet_wrap(vars(!!sym(X2)))	
        }	
    }	
}
#QUARTILE FUNCTION_______________________________________________________________________________________________
quart<-function(RespVar,X1){
    if(typeof(X1)=="list"){
        rbind(quantile(RespVar,na.rm=TRUE))
    }else if (!is.null(X1)){
        do.call("rbind",tapply(RespVar,X1,quantile,na.rm=TRUE))
    }
}



#RUNAPP______________________________________________________________________________________________________________________
library(shiny)

ui <- fluidPage(
    titlePanel("Student Loan Information App"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "equation",h4("Choose the Data to Plot"),
                        c("Total Loan Amount","Loan Amount/Income Ratio",
                          "Annual Loan Payments/Income Ratio")),
            selectInput("var1",h4("Choose a Variable to Overlap"),
                        c("None","Gender","Race","Age")),
            selectInput("var2",h4("Choose a Variable to Separate"),
                        c("None","Gender","Race","Age")),
            helpText("")
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plot"), helpText(br(),
                                                                      "The plots in this app are based on data from the Survey of Consumer Finances (2019). Loan amounts are the total student loan debt held by the household. Demographic information is for the survey respondent and may not be the same person/people for which the student loans were taken out.",
                                                                      br(),br(),
                                                                      "If the ratio of loan payment to income is zero, this may indicate the household has loan payments temporarily deferred or in forebearance.",
                                                                      br(),br(),
                                                                      "The final bar of the histogram shows households with debt higher than the cap level of $100,000 for Total Loan Amount, a ratio higher than the cap level of 5 for Ratio of Student Loan Amount to Income, and a ratio higher than the cap level of 0.4 for Ratio of Payment to Income. Values at the cap level are contained within the second to last bar.",
                                                                      br(),br(),
                                                                      "Percents are out of all households with outstanding student loans.",
                                                                      br(),br(),
                                                                      "Within the 'Loan Amount/Income Ratio' and 'Annual Loan Payments/Income' plots, twenty five data points were removed due to those data points reporting $0 Income, therefore, making the values generated undefined as it is divided by 0.")),
                        tabPanel("Quantiles",h3(textOutput("caption")),
                                 tableOutput("summary"),helpText("Change a 'Variable to Separate' 
                                                                 to group the quantiles of the data"))
            )
        )
    )
)



server<-function(input,output){
    datasetInput <- reactive({
        switch(input$equation,
               "Total Loan Amount"="EDN_INST",
               "Loan Amount/Income Ratio"="loan_ratio",
               "Annual Loan Payments/Income Ratio"="payment_ratio")
    })
    v1Input <- reactive({
        switch(input$var1,
               "None" = NULL,
               "Gender" = "Gender",
               "Race" = "Race",
               "Age" = "Age")
    })
    v2Input <- reactive({
        switch(input$var2,
               "None" = NULL,
               "Gender" = "Gender",
               "Race" = "Race",
               "Age" = "Age")
    })
    output$plot<-renderPlot({
        equation<-datasetInput() 
        var1<-v1Input() 
        var2<-v2Input() 
        hist_student_loans(D=df,RespVar=equation,X1=var1,X2=var2)
    })
    output$summary <- renderTable({
        equation<-datasetInput() 
        var2<-v2Input() 
        a <- df[, equation]
        b <- df[, var2]
        quart(RespVar=a,X=b)
    },striped = TRUE
    ,rownames = TRUE)
    
    output$caption <- renderText({
        var2<-v2Input()
        print(var2)
    })

}


shinyApp(ui, server)




