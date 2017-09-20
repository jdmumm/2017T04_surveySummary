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
 #################################################################################

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

  ##########################################################################################################################
  ##########################################################################################################################
  ## KING and DUNGY CPUE    ##   from 161104
  ############################

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
  
  
