### Break line After get a table with stocks as rows, period as columns
clean_up=function(dat){
  dat[is.na(dat) |is.infinite(dat)]=0
  return(dat)
}
getROC <- Vectorize(function(x){
  ROC_temp      <-dat[dat$PRIME==x,]$ROC
  if (length(ROC_temp)<8){
    return(0)
  }
  inside<- ROC_temp + 1
  ROC8<- prod(inside)^(1/8)-1
  ROC8[is.na(ROC8) | is.infinite(ROC8)] = 0 
  return(ROC8)
})
getROA <- Vectorize(function(x){
  ROA_temp      <-dat[dat$PRIME==x,]$ROA
  if (length(ROA_temp)<8){
    return(0)
  }
  value<- ROA_temp + 1
  ROA8<- prod(value)^(1/8)-1
  ROA8=clean_up(ROA8)
  return(ROA8)
})

filter_portfolio=function(ratio,tick){
  df_=data.frame(ratio,tick)
  df_=df_[order(df_$ratio,decreasing = TRUE),]
  result=rbind(head(df_,10),tail(df_,10))
  return(result)
}

getCFOA <- Vectorize(function(x){
  CFOA_temp      <-dat[dat$PRIME==x,]$`Free Cash Flow`
  if (length(CFOA_temp)<8){
    return(0)
  }
  value=sum(CFOA_temp)/dat[dat$PRIME==x,]$`Total Assets`[1]
  value=clean_up(value)
  return(value)
})

getMS <- Vectorize(function(x){
  MS_temp     <-dat[dat$PRIME==x,]$GM
  if (length(MS_temp)<8){
    return(0)
  }
  value=mean(MS_temp)/sd(MS_temp)
  value=clean_up(value)
  return(value)
})

getMG <- Vectorize(function(x){
  MG_temp      <-dat[dat$PRIME==x,]$GM
  if (length(MG_temp)<8){
    return(0)
  }
  value=(1+(MG_temp[2]/MG_temp[1]))*(1+(MG_temp[3]/MG_temp[2]))*(1+(MG_temp[4]/MG_temp[3]))*
    (1+(MG_temp[5]/MG_temp[4]))*(1+(MG_temp[6]/MG_temp[5]))*(1+(MG_temp[7]/MG_temp[6]))*(1+(MG_temp[8]/MG_temp[7]))
  value=value^(1/7)-1
  value=clean_up(value)
  return(value)
})