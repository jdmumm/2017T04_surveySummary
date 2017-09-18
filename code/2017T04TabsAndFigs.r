#Format tables for 2017 Kachemak Survey SUmmary, mstly from 931_popEstAndCPUE_161101.R used for 2016 report

#LOAD ----
library(tidyverse)
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
  write.csv(m,'./output/931PopMales_Main_17.csv')
  
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
  write.csv(m,'./output/931PopMales_Main_old.csv')
  