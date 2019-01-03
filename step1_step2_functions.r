STEP1=function(time){
  df=subset(dat_temp, dat_temp$Date>=time[1] & dat_temp$Date<=time[8])
  filter=aggregate(Date~PRIME,df,NROW)
  tick=filter[filter$Date ==8,]$PRIME
  #STEP 1 getPROBM within loop
  dat=subset(df,df$PRIME %in% tick &df$Date %in% c(time[7],time[8]))
  dat$GPMI_ = (dat$`Gross Profit`/dat$Revenue)
  dat$AQI_ = (dat$`Total Assets` - dat$`Total Current Assets` - (dat$`Gross Property, Plant And Equipment` + dat$`Accumulated Depreciation`))/dat$`Total Assets`
  dat$LVGI_ = (dat$`Shares Outstanding (Eop)` * dat$`Total Debt Per Share`)/dat$`Total Assets`
  dat$TATA_ = (dat$`Operating Income` - dat$`Cash Flow From Operations`)/dat$`Total Assets`
  a = subset(dat,Date == time[7])
  b = subset(dat,Date == time[8])
  c = merge(a,b,by='PRIME')
  c$gpmi = c$GPMI_.x/c$GPMI_.y
  c$aqi = c$AQI_.x/c$AQI_.y
  c$sgi = c$Revenue.x/c$Revenue.y
  c$depi = c$`Depreciation, Depletion And Amortization.x`/c$`Depreciation, Depletion And Amortization.y`
  # c$sgai = c$`Selling, General, & Admin. Expense.y`/c$`Selling, General, & Admin. Expense.x`
  c$lvgi = c$LVGI_.y/c$LVGI_.x
  c$tata = c$TATA_.y/c$TATA_.x
  for(i in 58:63){
    c[,i]=clean_up(c[,i])
  }
  # c$total = c$gpmi+c$aqi+c$sgi+c$depi+c$sgai+c$lvgi+c$tata
  c$total = c$gpmi+c$aqi+c$sgi+c$depi+c$lvgi+c$tata
  df3= subset(c,total <= 6 & total >= 0, c('PRIME','total'))
  tick = df3$PRIME
  return(tick)
}

# Step 2.  input tickers filtered by group 1, sort them with group two ratios, design 10 by 10 portforlios
STEP2=function(tick,time){
  df=dat_temp[dat_temp$Date>=time[1] & dat_temp$Date<=time[8],]
  df2=subset(df, df$PRIME %in% tick)
  # df2=aggregate(Date~PRIME,df2,NROW)
  dat=df2
  ROC<- dat$`Operating Income`/(dat$`Gross Property, Plant And Equipment` +
                                  dat$`Accumulated Depreciation` +
                                  dat$`Total Current Assets` -
                                  dat$`Total Current Liabilities`)
  ROC[is.na(ROC) | is.infinite(ROC)] <- 0
  ROC<-getROC(tick)
  dat$ROC=ROC
  
  o1=filter_portfolio(ROC,tick)
  ROA=dat$`Net Income`/dat$`Total Assets`
  ROA=clean_up(ROA)
  dat$ROA=ROA
  ROA=getROA(tick)
  o2=filter_portfolio(ROA,tick)
  CFOA=getCFOA(tick)
  o3=filter_portfolio(CFOA,tick)
  GM=dat$`Gross Profit`/dat$Revenue
  dat$GM=GM
  
  MG=getMG(tick)
  MS=getMS(tick)
  o4=filter_portfolio(MG,tick)
  o5=filter_portfolio(MS,tick)
  table=rbind(o1,o2,o3,o4,o5)
  tickers=as.character(table$tick)
  return(tickers)
}

count=function(df,num){
  count_list=c()
  for(i in 1:nrow(df)){
    count=0
    for(j in 1:ncol(df)){
      if(df[i,j]==num){
        count=count+1
      }
    }
    count_list=c(count_list,count)
  }
  return(count_list)
}  


final_df=function(df4,df5){
  Portfolio=c("roc1","roc2","roa1","roa2","cfoa1","cfoa2","mg1","mg2","ms1","ms2")
  Pos=sapply(1:10,function(x){return(length(which(df4[x,]>0)))})
  Neg=sapply(1:10,function(x){return(length(which(df4[x,]<0)))})
  # Wins=
  # Lost=c(count(df5,10))
  Rank=sapply(1:10,function(x){return(mean(df5[x,]))})
  Gmean=sapply(1:10,function(x){
    v=df4[x,]
    v=v+1
    return(prod(v)^(1/112)-1)
  })
  Amean=sapply(1:10,function(x){return(mean(df4[x,]))})
  
  # final_df=data.frame(Portfolio,Pos,Neg,Rank,Wins,Lost,Gmean,Amean)
  final_df=data.frame(Portfolio,Pos,Neg,Rank,Gmean,Amean)
}