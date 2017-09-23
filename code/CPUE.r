## ###################
#    CPUE           ##
######################
library (tidyverse)
library(plotrix)

## LOAD DATA ##########################################################################

events <- read.csv('./data/events.csv')
events %>% filter (USED_IN_ESTIMATE == 'YES', PROJECT_CODE == 'T04') %>%
  select( Event = EVENT_ID,
          year = YEAR,
          Project = PROJECT_CODE, 
          length=TOW_LENGTH_DESIGNATED,
          totCatch = CATCH_WEIGHT) -> event

catch <- read.csv('./data/catchComp.csv')
catch %>% filter (FK_PROJECT_CODE == 'T04') %>%
select(Project = FK_PROJECT_CODE, Event = EVENT_ID, species=SPECIES_CODE, 
                 count=COUNT,
                 wt=SAMPLE_WT_KG, 
                 sample_type = SAMPLE_TYPE) %>% filter(Event %in% event$Event) -> catch

## ESTIMATE CATCH BY TOW, then CPUE, and aggregate by Bed ##############################

s <- '250' # Species of interest
# aggregate sampled catch and species of interest 
catch %>% group_by(Event) %>% 
  summarise(#t1 = sum(sample_wt[species == 99997 & sample_type == 1]),
            t2 = sum(wt[sample_type == 2], na.rm = T),
            s1 = sum(wt[species == s & sample_type == 1], na.rm = T), 
            s2 = sum(wt[species == s & sample_type == 2], na.rm = T),
            totSamp = sum(wt), na.rm = T) %>% 
  # join to event for totCatch,length and set any events with no catch to 0. 
  right_join (select( event, Event,year, totCatch, length)) %>%
  mutate (t1 = totCatch -totSamp) %>%
  replace_na (list(t1 = 0, t2 = 0, s1 = 0, s2 = 0)) %>%
  # expand any T2's, sum components of catch, and calc CPUE   
  mutate ( sTot = s1 + s2 + if_else(t2>0, (t1*s2/t2), 0), #expanded catch of species s by event.  Con to prevent div by 0.
           sCPM = sTot/length)  %>%                    # CPUE cnt/nmi 
  # aggregate by year     
  group_by(year) %>% summarise (tows =  n(),
                                cpue_mean = mean(sCPM), 
                                cpue_sd = sqrt(var(sCPM)), 
                                cpue_cv = 100 * cpue_sd/cpue_mean,
                                totCatch = sum(sTot)) ->  cpm # append species code to output manually 
cpm # CPUE (kg/nmi) with sd, cv and total expanded catch (cnt) by bed, all for sp. s.     

write.csv(cpm_250, './output/cpm_250.csv')
write.csv(cpm_710, './output/cpm_710.csv')
write.csv(cpm_270, './output/cpm_270.csv')

par(mfrow = c(3,1))
par(mar=c(3.1,4.1,2,1))
par(mgp = c(2, 1,0))

dat <- cpm_250
plotCI(dat$year, dat$cpue_mean, col= "black", lwd=1,  pch= 19, cex = 1.0,
       ui = dat$cpue_mean + dat$cpue_sd,
       li = ifelse((dat$cpue_mean - dat$cpue_sd) > 0 , (dat$cpue_mean - dat$cpue_sd), 0),
       xlim = c(1998,2017), 
       ylab = 'CPUE (kg/nmi)', xlab = 'Year', main = 'Pacific Tomcod')
       
dat <- cpm_710
plotCI(dat$year, dat$cpue_mean, col= "black", lwd=1,  pch= 19, cex = 1.0,
       ui = dat$cpue_mean + dat$cpue_sd,
       li = ifelse((dat$cpue_mean - dat$cpue_sd) > 0 , (dat$cpue_mean - dat$cpue_sd), 0),
       xlim = c(1998,2017), 
       ylab = 'CPUE (kg/nmi)', xlab = 'Year', main = 'Sablefish')

dat <- cpm_270
plotCI(dat$year, dat$cpue_mean, col= "black", lwd=1,  pch= 19, cex = 1.0,
       ui = dat$cpue_mean + dat$cpue_sd,
       li = ifelse((dat$cpue_mean - dat$cpue_sd) > 0 , (dat$cpue_mean - dat$cpue_sd), 0),
       xlim = c(1998,2017), 
       ylab = 'CPUE (kg/nmi)', xlab = 'Year', main = 'Walleye Pollock')
