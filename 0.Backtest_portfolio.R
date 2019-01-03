# Wrote functions for group two Ratios, geometric mean of ROC, ROA
source('Group2_ratios.R')
source('step1_step2_functions.R')

# STEP 0: set up target data,
library(quantmod)

# Paralell preprocessing with Foreach
library(foreach)
library(doParallel)

registerDoSEQ()
c1=makeCluster(6)
registerDoParallel(c1)
start =Sys.time()

# Step 1. input period, output tickers after filter PROBM ratio in range: 0~7
time=new_date[1:8]

main <- function() {

  start_year=2012
  end_year=2017
  # After get a table with stocks as rows, period as columns
  dat= readRDS('datasets.RDS')
  dat = subset(dat, dat$Date>=201203 & dat$Date<=201712)
  dat_temp= dat
  
  # new_dates= c('1988-03')
  # years=c(1988:2017)
  new_dates=new_date= c(paste(start_year,'-03',sep=''))
  years=years2=c(start_year:2017)
  month=c('03','06','09','12')
  for(i in years){
    for(j in month){
      new_dates=c(new_dates,paste(i,j,sep="-"))
    }
  }
  new_dates=as.character(new_dates)[2:length(new_dates)]
  
  month=c("03","06","09","12")
  for(i in years2){
    for(j in month){
      new_date=c(new_date,paste(i,j,sep=""))
    }
  }
  new_date=as.integer(new_date)[2:length(new_date)]
  df4=matrix(nrow=100,ncol=113)
  
  for(j in 8:length(new_date)){
    time=new_date[(j-7):(j+1)]
    # df used for ratios calculation
    ticker=STEP1(time)
    tickers=STEP2(ticker,time)
    returns=c()
    for(i in 1:100){
      start_date=paste(new_dates[j-7],"-01",sep="")
      end_date=paste(new_dates[j],"-30",sep="")
      ratio=tryCatch({
        price=getSymbols(tickers[i],auto.assign = FALSE,from=start_date,to=end_date)[,6]
        f=first(price)
        l=last(price)
        ratio=(l-f)/f
        return(ratio)
      },error=function(e){
        ratio=NA
        return(ratio)
      })
      
      returns=c(returns,ratio)
    }
    returns[is.na(returns)]=0
    port_return=c()
    port_return[1]=mean(returns[1:10])
    port_return[2]=mean(returns[11:20])
    port_return[3]=mean(returns[21:30])
    port_return[4]=mean(returns[31:40])
    port_return[5]=mean(returns[41:50])
    port_return[6]=mean(returns[51:60])
    port_return[7]=mean(returns[61:70])
    port_return[8]=mean(returns[71:80])
    port_return[9]=mean(returns[81:90])
    port_return[10]=mean(returns[91:100])
    df4[,j-7]=port_return
  }
  
  df4=clean_up(df4)
  df5=sapply(1:112,function(x){return(rank(df4[,x]))})
  finaldf=final_df(df4,df5)
}
main()

end =Sys.time()
end-start
# }