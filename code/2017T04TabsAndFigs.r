#Format tables for 2017 Kachemak Survey SUmmary, mstly from 931_popEstAndCPUE_161101.R used for 2016 report

#LOAD ----
library(tidyverse)
library(stats)
library(plotrix)
library (Hmisc)
options (scipen = 10)
dat_17 <- read.csv('./data/qP_simp_17_170916.csv') # using new size classes from 2017 SD op-plan
dat_old <- read.csv('./data/qP_simp_oldSCs_170916.csv')# using the old pre-2017 size classes 

#Males Main ----

  # New Size Classes 
  m <- dat_17[dat_17$PROJECT_CODE == "T04", c(1:7,16,18,20,22,24,26)] 
  #combine news and olds for P1s and P2s
  m$P1 <- m$MT5_P_ + m$MT6_P_
  m$P2 <- m$MT7_P_ + m$MT8_P_
  names(m)[c(12,13)] <- c('P3','P4')      #"MT9_P_"  "MT10_P_"
  m <- m[,-(8:11)] # remove columns with new old split
  #reorder columns
  m <- m[,c(1,2,3,9,8,11,10,4:7)]
  m
  #write.csv(m,'./output/931PopMales_Main_17.csv')
  
  # Old Size Classes  - samee as above, just differnt file names. 
  m <- dat_old[dat_old$PROJECT_CODE == "T04", c(1:7,16,18,20,22,24,26)] 
  #combine news and olds for P1s and P2s
  m$P1 <- m$MT5_P_ + m$MT6_P_
  m$P2 <- m$MT7_P_ + m$MT8_P_
  names(m)[c(12,13)] <- c('P3','P4')      #"MT9_P_"  "MT10_P_"
  m <- m[,-(8:11)] # remove columns with new old split
  #reorder columns
  m <- m[,c(1,2,3,9,8,11,10,4:7)]
  m
 # write.csv(m,'./output/931PopMales_Main_old.csv')

#Females Main ----
dat_17 %>% filter (PROJECT_CODE == 'T04') %>% select(year = YEAR,
                    FT11_P_, FT11_P_CI_, MF_P_, MF_P_CI_, TF_P_, TF_P_CI_) -> f
  
  write.csv(f,'./output/931PopFems_Main.csv')

##Plot LM ---- 
  
  dat <- dat_17 
  # dat <- dat_old 
  
  #Convert to thousands of crabs
  dat$LM_P <- dat$LM_P_/1000
  dat$LM_P_CI <- dat$LM_P_CI_/1000
  
  dat <- select(dat, "proj" = PROJECT_CODE, "yr" = YEAR,  LM_P, LM_P_CI)
  
  t04 <- dat[dat$proj == "T04",]
  
  #T04
  par(mfrow=c(1,1), las=1, mgp=c(3.2, 1, 0), mar=c(3.8, 4.2, 1, 1), family = "serif", font = 1,
      cex = 1, ps = 12, cex.lab = 1.16, bty = "l")
  
  #LM
  dat <- t04
  plotCI(dat$yr, dat$LM_P, col= "black", lwd=1,  pch= 19, cex = 1.0,
         ui = dat$LM_P + dat$LM_P_CI,
         li = ifelse((dat$LM_P - dat$LM_P_CI) > 0 , (dat$LM_P - dat$LM_P_CI), 0),
         ylab= "Thousands of Crab", xlab = "", xaxt='n', yaxt='n', las = '1',font.lab = 2,
         ylim = c(0,2950))
  mtext("Year", side = 1, line = 2,font=2, cex= 1.16)
  axis(side=1, at=seq(1990, 2017, by=1, las = '1', font = 2))
  axis(side=2, at = seq(0, 2950, by=500, font = 2),
       labels=formatC(seq(0, 2950, by=500),"d", big.mark=','))
  minor.tick(ny = 5, nx = 0 )
  # THRESHOLDS , KG asked not to include thresholds ----
    # lines(x= c(2002,2017), y = c(500,500), lty = 5, col = "black", lwd =2)
    # lines(x= c(2002,2017), y = c(100,100), lty = 3, col = "dimgrey")
    # lines(x= c(2002,2017), y = c(50,50), lty = 5, col = "dimgrey")
    # 
    # detach("package:dplyr", unload=TRUE)
    # library(stats)
    # rec<- dat[dat$yr > 1997,]
    # f5 <- rep(1/5, 5)
    # y_lag <- filter(rec$LM_P, f5, sides=1)
    # points(rec$yr, y_lag, col="dimgrey", cex = 1.0)
    # 
    # legend ("topright", inset = .1,
    #         legend = c("Commercial MSST","Noncommercial 5-year threshold", "Noncommercial 1-year threshold", "5-Year average"),
    #         col = c("black","dimgrey","dimgrey", "dimgrey"),
    #         lty = c(5,3,5, NA), pch = c(NA,NA,NA,1), lwd = c(2,1,1,NA), bty = "n", y.intersp =1.2)

## KING and DUNGY CPUE    ##   from 161104 ----
 

  ##DUNGIES##
  dat <- read.csv("./data/qP_910_170920.csv")
  
  dat <-  dat[c("PROJECT_CODE", "YEAR","n","SM_CBar","SM_varC", "LM_CBar","LM_varC","TM_CBar","TM_varC",
                "TF_CBar","TF_varC")]
  
  ##Calc sample SDs
  dat$SM_SD <- (dat$SM_varC^.5)
  dat$LM_SD <- (dat$LM_varC^.5) 
  dat$TM_SD <- (dat$TM_varC^.5)
  dat$TF_SD <- (dat$TF_varC^.5)
  
  #remove extra cols, reorder and rename 
  dung_pm <- select(dat,"Proj" = PROJECT_CODE, "Year" = YEAR, n,
                    SM_CBar, SM_SD, LM_CBar, LM_SD, TM_CBar, TM_SD, TF_CBar, TF_SD)
  
  # order by project and year 
  dung_pm <- arrange(dung_pm, Proj, Year)
  dung_pm
  write.csv(dung_pm, "./output/910_pm_170920.csv")

## Tanner CH vs CW plot ## ----
  
read.csv ("./data/931_CHCW_T04T05T06_usedInEst_90to17.csv") %>% 
filter(PROJECT_CODE == "T04", YEAR > 2007) -> awl # 2006 had CH too, but excluding to fit on 6 panel plot

yrs <- unique(awl$YEAR)  

par(mfcol=c(3,2))
par(mar=c(3.1,4.1,1,1))
par(mgp = c(2, 1,0))
for (i in yrs)
  {
  awl %>% filter (YEAR == i, SEX_CODE == '1') %>%  
    select(cw = BIOLOGICAL_WIDTH_MM, ch = CHELA_HEIGHT_MM, sc17 = CRAB_SIZE_CLASS_CODE_17) %>% 
    mutate (cw_ln = log(cw), ch_ln = log(ch), rat = ch_ln/cw_ln) -> len
  
  #len %>% filter(cw_ln > 3.8 & cw_ln < 5.2) -> len # exclude few outliers
  
  #t <- .62
  plot (ch_ln ~ cw_ln, data =len, 
        ylim = c(1.25,3.75),
        xlim = c(3.75,5.1),
        cex = .9,
        #col = 'gray20',
        #col = ifelse(rat < t,'gray60','black'),
        #pch = ifelse(rat < t,1,4),
        #cex = ifelse(rat < t,.8,.5),
        xlab = 'ln(carapace width)', ylab = 'ln(chela height)' )        
  abline( v = log(114), lwd = 3, col = 'gray20')
  abline( v = log(140), lwd = 3, lty ="dotted", col = 'gray20')
  
  # lm(ch_ln ~ cw_ln, len, rat < t) -> lm_s 
  # lm(ch_ln ~ cw_ln, len, rat > t) -> lm_l
  # abline(lm_s, col = 'gray60', lty = 'dashed' )
  # abline(lm_l, col = 'black', lty = 'dashed')

  
  legend( x=3.8, y=3.7 , bty = 'n', legend = c('114mm CW', '140mm CW'),
          lwd = c(3,3), lty = c('solid','dotted'), col =c('gray20','gray20'))
  
  #legend(x = 3.8, y = 3.5, bty = 'n', legend = c('large-claw: ln(ch)/ln(cw) > 0.62', 'small-claw: ln(ch)/ln(cw) < 0.62'),
        #col = c("black", 'gray60'), pch = c(1,4), pt.cex = c(.8,.5) )       
  legend('bottomleft', legend = i)
  }  

  # picking rat threshold    
  ggplot(aes(rat))+geom_density(alpha=.2)
    
  # above plot with only raw data
  plot (ch_ln ~ cw_ln, data =len, 
        xlim = c(3.75,5.1),
        xlab = 'ln(carapace width)', ylab = 'ln(chela height)' )        
  abline( v = log(114), lwd = 3)
  abline( v = log(140), lwd = 3, lty ="dotted")
  -
    -      legend( x=3.8, y=3.7 , bty = 'n', legend = c('114mm CW', '140mm CW'),
                   -              lwd = c(3,3), lty = c('solid','dotted'))
