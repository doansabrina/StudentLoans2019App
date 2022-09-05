library(tidyverse)
library(dplyr)
library(ggplot2)
#' 	
#' 	
#LOAD IN DATA	
df <- read.csv("~/STAT Final/app/function/www/SCFP2019.csv")[ ,c('EDN_INST', 'PAYEDU1', 'PAYEDU2', 'PAYEDU3','PAYEDU4',
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