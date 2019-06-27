library(DescTools)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(zoo)
library(tidyverse)
library(shiny)
library(scales)
library(shinydashboard)
library(plotly)
library(quantmod)

load("./data/inputfile.RData")
col_choices=c(   "FinWealth"   ,     "Saving"
                 , "Rent"   ,  "KidEduCost"    ,  "CostLiving" ,  "EduK12"    ,  "TaxRate"  ,    "preTax",       "afTax",        "Tax",      "EduColPri"  ,"EduColPub"  ,    "condo"
               ,"coop"     ,       "MORTGAGE30US",   "Mkt"     ,   "RF"    ,     "CapTax","Q1",   "Q2",   "Q3",   "Q4",   "top5" )
old_input=c("", "") #old_list=list(c("", ''),data.frame())
old_df=data.frame()
helper=function (x0,x1,x2,x3,x4,x5,x6,x7) {
 
  dfname=c("inc_df","df_exempt","fed_tax_rate", "tuition", "housing","ps") 
  for (i in 1:length(dfname)) { 
    assign(dfname[i],copy(L[[i]])) # create dfs
  }
  for (i in seq(0,2)) {
    print(str(get(paste0("x",i))))
  }
 

  cols=c("Q1",   "Q2",   "Q3",   "Q4",   "top5")
  inc_df[,(cols):=lapply(.SD,function (x) {x*x2}),.SDcols=cols] #adjust for number of earners
if (x1<=inc_df[1,2]) {
  inc_df[,preTax:=Q1*(x1/Q1[1])]
} else if (x1<=inc_df[1,3]) {
  inc_df[,preTax:=Q1+(x1-Q1[1])/(Q2[1]-Q1[1])*(Q2-Q1)]
} else if (x1<=inc_df[1,4]) {
  inc_df[,preTax:=Q2+(x1-Q2[1])/(Q3[1]-Q2[1])*(Q3-Q2)]
} else if (x1<=inc_df[1,5]) {
  inc_df[,preTax:=Q3+(x1-Q3[1])/(Q4[1]-Q3[1])*(Q4-Q3)]
} else if (x1<=inc_df[1,6]) {
  inc_df[,preTax:=Q4+(x1-Q4[1])/(top5[1]-Q4[1])*(top5-Q4)]
} else {
  inc_df[,preTax:=top5*(x1/top5[1])]
}





# Tax exemption
inc_df=merge(inc_df,df_exempt,by="year")
if (x2==1) {
  inc_df[,taxable:=preTax-head_sd-single_ex-x3*kids_ex]
} else if (x2>=2) {
  inc_df[,taxable:=preTax-married_sd-married_ex-x3*kids_ex]
} 
# final_df
fed_tax_rate[,bracket1:=bracket1/100]
fed_tax_rate[,bracket2:=bracket2/100]

final_df=merge(inc_df[,c("preTax","year","taxable","Q1",   "Q2",   "Q3",   "Q4",   "top5")],fed_tax_rate,by="year")

final_df[,Tax:=ifelse(taxable<=income1,taxable*bracket1,
                      ifelse(taxable<income2,bracket1*taxable+ 
                               (taxable-income1)/(income2-income1)*(bracket2-bracket1)*(taxable-income1)/2,
                             income2*bracket2/2+
                               (taxable-income2)*bracket2))]
final_df[,c("TaxRate","afTax"):=list(Tax/taxable,preTax-Tax)] #sanitycheck

#after Tax 
  
final_df[,CostLiving:=afTax*x5/afTax[.N]] #get the income after expenditure
final_df = final_df[inc_df$year>=year(x0[1]) & 
                  inc_df$year<=year(x0[2]),c("year","TaxRate","preTax","afTax","Tax","CostLiving","Q1",   "Q2",   "Q3",   "Q4",   "top5")]

final_df=merge(final_df,tuition,by="year")


final_df[,KidEduCost:=0]
final_df[21:24, KidEduCost:=x3*EduColPub]

# kids education
if (!is.null(x6)) {
  if (("Nursery-PK" %in% x6) & (dim(final_df)[1]>=7)) {
    final_df[5:7, KidEduCost:=x3*EduK12]
  } 
  if ( ("K-5" %in% x6) & (dim(final_df)[1]>=13)) {
    final_df[8:13, KidEduCost:=x3*EduK12]
  }  
  if ( ("6-12"  %in% x6) & (dim(final_df)[1]>=20)) {
    final_df[14:20,KidEduCost:=x3*EduK12]
  } 
  if ( ("College"  %in% x6 ) & (dim(final_df)[1]>=24)) {
    final_df[21:24, KidEduCost:=x3*EduColPri]
  }
}

# Rent

cols=c("condo","coop","Rent")
housing[,(cols):=lapply(.SD,function(x){x*x4}),.SDcol=cols]


final=merge(final_df,housing[,c("year","condo","coop","Rent","MORTGAGE30US")],by="year")

final=final[rep(1:.N,each=12),]

cols=names(final)[-c(1,2,16,17,19)]
final[,(cols):=lapply(.SD,function (x) {x/12}),.SDcols=cols]
final[,month:=seq(1,.N),by=year]
final[,date:=as.yearmon(year+(month-1)/12)]
# 

setDT(ps) #maybe do setkey
ps[,c("year","month"):=list(year(date),month(date))]
ps[,"date"]=NULL
setDT(final)
com_df=merge(final[!is.na(year)],ps,by=c("year","month"))
com_df[,c("FinWealth","Saving","CapTax","CumCapLoss","capgain"):=list(0,afTax-CostLiving-KidEduCost,0,0,0)]
if ("No stocks" %in% x7) {com_df$Mkt=com_df$RF}
if ("Tax Optimization" %in% x7) {
  captaxRate = 0.15
  for (i in (2:dim(com_df)[1])) {
    com_df$FinWealth[i - 1] = com_df$FinWealth[i - 1] + com_df$Saving[i - 1] - com_df$Rent[i - 1]
    com_df$FinWealth[i] = com_df$FinWealth[i - 1] * com_df$Mkt[i] / com_df$Mkt[i - 1]
    if (com_df$Mkt[i] < com_df$Mkt[i - 1]) {
      com_df$CumCapLoss[i] = com_df$CumCapLoss[i - 1] + (com_df$Mkt[i] / com_df$Mkt[i-1] - 1) * com_df$FinWealth[i - 1]
    } else if ((com_df$FinWealth[i] - com_df$FinWealth[i - 1]) < abs(com_df$CumCapLoss[i])) {
      #carry over capital loss
      com_df$CumCapLoss[i] = com_df$CumCapLoss[i] + com_df$FinWealth[i] - com_df$FinWealth[i - 1]
      com_df$CapTax[i] = 0
    } else {

      com_df$CapTax[i] = captaxRate * (com_df$FinWealth[i] - com_df$FinWealth[i-1] + com_df$CumCapLoss[i])
      com_df$CumCapLoss[i] = 0
    }
    com_df$FinWealth[i] = com_df$FinWealth[i] - com_df$CapTax[i]
  }
} else {
  captaxRate = 0.30
  for (i in (2:dim(com_df)[1])) {
    com_df$FinWealth[i - 1] = com_df$FinWealth[i - 1] + com_df$Saving[i - 1] -com_df$Rent[i - 1]
    com_df$FinWealth[i] = com_df$FinWealth[i - 1] * com_df$Mkt[i] / com_df$Mkt[i -  1]
    com_df$capgain[i]=com_df$FinWealth[i] - com_df$FinWealth[i - 1]
    com_df$CapTax[i] = ifelse(
      com_df$FinWealth[i] > com_df$FinWealth[i - 1],
      captaxRate * (com_df$FinWealth[i] - com_df$FinWealth[i - 1]),
      0
    )
    com_df$FinWealth[i] = com_df$FinWealth[i] - com_df$CapTax[i]
  }
}

melt_cumret=melt(com_df,id.vars = c('date'),measure.vars = colnames(com_df)[-1],variable.name = 'port_name',value.name = 'Value')



return (melt_cumret)

}
 