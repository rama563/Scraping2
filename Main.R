###Clearing the Environment
rm(list=ls())
library(finreportr)
library(dplyr)
library(magrittr)
library(xlsx)
library(lubridate)
setwd("K:/HE Project - FAMA/Scraping Code Version 2")
source('Source Functions.R')
winDialog("Please enter the Ticker and Year information",type=c('ok'))
symbol=winDialogString("Company Ticker:",default="")
year=as.numeric(winDialogString("Year:",default=""))
Req_Start_Date=paste0(year,"-04-01")
Req_End_Date=paste0(year+1,"-03-31")
Req_Start_Date<-as.Date(Req_Start_Date)
Req_End_Date<-as.Date(Req_End_Date)
info_df=CompanyInfo(symbol)
CIK <- CompanyInfo(symbol)
CIK <- as.numeric(CIK$CIK)
Reports_Qtr=AnnualReports_RK(symbol,Req_Start_Date,Req_End_Date,"10-Q")
Reports_Year=AnnualReports_RK(symbol,Req_Start_Date,Req_End_Date,"10-K")
if(nrow(Reports_Qtr)>0&nrow(Reports_Year)>0)
{
  Reports=rbind(Reports_Qtr,Reports_Year)
}else{
  if(nrow(Reports_Qtr)==0)
  {
    Reports=Reports_Year
  }else{
    if(nrow(Reports_Year)==0)
    {
      Reports=Reports_Qtr
    }
  }
}
Reports=Reports[Reports$filing.name%in%c("10-Q","10-K"),]


if(nrow(Reports)>0)
{
 Reports$"Accession.no.formatted"<-gsub("-","",Reports$accession.no)
 Reports$"Period_ending"<-""
 Reports$"URL"<-""
 Reports$File_Name<-""
 Reports$"Period"<-ifelse(Reports$filing.name=='10-Q',"Quarterly_Report_Ending_","Annual_Report_Ending_")
 for(i in 1:nrow(Reports))
 {
  temp<-ReportPeriod_RK(symbol,CIK,Reports$Accession.no.formatted[i],Reports$accession.no[i])
  Reports$Period_ending[i]<-temp
 }
 Reports$Period_ending<-as.Date(Reports$Period_ending)
 Reports_bkup<-Reports
 Reports<-Reports[year(Reports$Period_ending)==year,]
 if(nrow(Reports)>0)
 {
 for(i in 1:nrow(Reports))
 {
 Reports$URL[i]<-paste0("https://www.sec.gov/Archives/edgar/data/",CIK,"/",Reports$Accession.no.formatted[i],"/Financial_Report.xlsx")
 Reports$File_Name[i]<-paste0("Output/",toupper(symbol),"_",Reports$Period[i],Reports$Period_ending[i],".xlsx")
 download.file(url = Reports$URL[i], destfile = Reports$File_Name[i], mode="wb") 
 }
 }else{
   print("No 10-Q or 10-K filings are found for given ticker in given year! Please Check....")
 }
}else{
  print("No 10-Q or 10-K filings are found for given ticker in given year! Please Check....")
 }
