
avg_zins<-function(einzahlungen2,end_date, end_sum){
  start_time <- Sys.time()
  a = 0
  max_col=nrow(einzahlungen2)
  anzahlfonds=nrow(end_sum)
  
  einzahlungen2$Jahre<-as.integer(time_length(interval( ymd(einzahlungen2$Datum),end_date),"years"))
  einzahlungen2$Tage<- as.integer(time_length(interval( ymd(einzahlungen2$Datum) %m+% years(einzahlungen2$Jahre),end_date),"days"))
  
  betragsum=sum(einzahlungen$Betrag)
  for (anzf in 1:anzahlfonds){
    end_sum2<-end_sum[anzf,2]
    
    if (betragsum<end_sum2 ){min_zin <- 0.0
    max_zin <- 10.0
    zins_a <- 0.2}
    
    if (betragsum>end_sum2 ){min_zin <- -1.0
    max_zin <- 0
    zins_a <- -0.1}
    
    if (betragsum==end_sum2 ){min_zin <- -1.0
    max_zin <- 10
    zins_a <- 0.0}
    
    
    
    zins_b <- 1.0
    sum_a <- 0
    sum_b <- 0
    
    z= 0
    while ((abs(zins_a-zins_b) >=0.00001   ) ) {
      sum_a <-  0
      zins_b <-  zins_a
      sum_a <- sum(einzahlungen2$Betrag*(1+zins_a)**einzahlungen2$Jahre + einzahlungen2$Betrag*((zins_a/365.25))*einzahlungen2$Tage)
      if (sum_a < end_sum2){
        zins_a <- ((max_zin-zins_a)/2) + zins_a
        min_zin <-  zins_b
      }
      
      if (sum_a > end_sum2){
        zins_a <-  ((zins_a-min_zin)/2) + min_zin
        max_zin <-  zins_b
      }
      
      z <-  z +1
    }
    # print (c(z,(Sys.time() - start_time)))
    
    end_sum[anzf,3]<- zins_a
    # print (end_sum[anzf,])
  }
  #   print (c(z,(Sys.time() - start_time)))
  return <- end_sum
}
