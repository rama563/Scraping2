AnnualReports_RK <- function(symbol,Req_Start_Date,Req_End_Date,type) {
  
  options(stringsAsFactors = FALSE)
  url <- paste0("http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", 
                symbol, "&type=",type,"&dateb=&owner=exclude&count=100")
  
  filings <- xml2::read_html(url)
  
  ##   Generic function to extract info
  ExtractInfo <- function(html.node) {
    info <-
      filings %>%
      rvest::html_nodes(html.node) %>%
      rvest::html_text()
    return(info)
  }
  
  ##   Acquire filing name
  filing.name <- ExtractInfo("#seriesDiv td:nth-child(1)")
  
  ##   Error message for function
  if(length(filing.name) == 0) {
    stop("invalid company symbol or foreign logical")
  }
  
  ##   Acquire filing date
  filing.date <- ExtractInfo(".small+ td")
  
  ##   Acquire accession number
  accession.no.raw <- ExtractInfo(".small")
  
  accession.no <-
    gsub("^.*Acc-no: ", "", accession.no.raw) %>%
    substr(1, 20)
  
  ##   Create dataframe
  info.df <- data.frame(filing.name = filing.name, filing.date = filing.date, 
                        accession.no = accession.no)
  
  info.df$filing.date<-as.Date(info.df$filing.date)
  #info.df<-info.df[info.df$filing.date>=Req_Start_Date&info.df$filing.date<=Req_End_Date,]
  return(info.df)
  
}




ReportPeriod_RK <- function(symbol, CIK, accession.no, accession.no.raw) {
  
  url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", 
                accession.no, "/", accession.no.raw, "-index.htm")
  search.result <- xml2::read_html(url)
  
  ##   Generic function to extract info
  ExtractInfo <- function(html.node) {
    info <-
      search.result %>%
      rvest::html_nodes(html.node) %>%
      rvest::html_text()
    return(info)
  }
  
  report.period <- ExtractInfo(".formGrouping+ .formGrouping .info:nth-child(2)")
  return(report.period)
}