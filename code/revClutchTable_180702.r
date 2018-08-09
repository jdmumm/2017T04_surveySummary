#New Clutch Fullness and Egg Dev Table including SE, instead of pooled by year per CK reqquest 180521
#LOAD ----
library(tidyverse)

options (scipen = 10)
dat_17 <- read.csv('./data/qP_simp_17_170916.csv') # using new size classes from 2017 SD op-plan
dat_old <- read.csv('./data/qP_simp_oldSCs_170916.csv')# using the old pre-2017 size classes 
events <- read.csv('./data/events.csv')
events %>% filter (PROJECT_CODE == 'T04', GEAR_PERFORMANCE_CODE == '1') %>%
  select( Event = EVENT_ID,
          year = YEAR,
          Project = PROJECT_CODE, 
          length=TOW_LENGTH_DESIGNATED,
          totCatch = CATCH_WEIGHT, 
          Used = USED_IN_ESTIMATE) -> event
awl <- read.csv('./data/T04931_awlClutch_thru2017.csv') %>%
  select ( Event = EVENT_ID, 
           Project = PROJECT_CODE, 
           species = SPECIES_CODE, 
           sex = SEX_CODE,
           full = FULLNESS_PERCENT,
           ed = CRAB_EGG_DEVELOMENT_CODE,
           cc = CLUTCH_CONDITION_CODE,
           Mat = MAT_CLASS
           ) -> awl

# Prep data ----
  awl %>% right_join (event %>% select (year, Event, Project, Used))  %>%
    filter (Used == 'YES', Project == 'T04', species == '931', sex == 2, Mat == 'MAT' ,
      !is.na (full)) -> clutch

# fullness ----  
  # reduce full from numeric to categorical - Barren, full, partial 
    clutch %>% mutate (F = ifelse(full == 0, "b",(ifelse(full >= 90, "f","p")))) -> clutch 
    
  # Calc freq b,f,p by event
    # to wide and back to long so that all cats are present for all events
    clutch %>% group_by (year, Event, F) %>% summarize (n = n()) %>% spread("F","n") -> wide
    wide %>% gather("F", "n",3:5) %>% as.data.frame() %>% arrange (Event)-> long 
    long$n[is.na(long$n)] <- 0  # replcae na counts with 0
  # prop of p,f,e by event 
    long %>% 
      left_join (clutch %>% group_by (year, Event) %>% summarize (tot = n())) %>%  # join total obs by event 
      mutate (prop = n/tot) -> byEvent 
      
    byEvent %>% group_by (year, F) %>% summarize ( # calc mean of prop and SE by year. 
        tows = n(), # This is only includes tows with mat fems. If no mat fems, not an obs. 
        mean = mean (prop), 
        se = (var(prop)^.5)/(tows^.5)) -> full_l
  # spread F across cols  
    full_l %>% gather (variable, value, -(year:tows)) %>%
    unite(temp, F, variable) %>%
      spread(temp, value) %>% 
      select (year, tows, b_mean, b_se, p_mean, p_se, f_mean, f_se) -> full
  # write
    #full %>% write.csv("./output/full.csv")

# egg development ----  #added 8/9 by copying fullness block above.  Could/should combine analysis of all 3 vars (Full,ED,CC) to reduce duplicated code. ----
   
    # Calc freq by event
    # to wide and back to long so that all cats are present for all events
    clutch %>% group_by (year, Event, ed) %>% summarize (n = n()) %>% spread("ed","n") -> wide
    wide %>% gather("ed", "n",3:5) %>% as.data.frame() %>% arrange (Event)-> long 
    long$n[is.na(long$n)] <- 0  # replcae na counts with 0
    # prop by event 
    long %>% 
      left_join (clutch %>% group_by (year, Event) %>% summarize (tot = n())) %>%  # join total obs by event 
      mutate (prop = n/tot) -> byEvent 
    
    byEvent %>% group_by (year, ed) %>% summarize ( # calc mean of prop and SE by year. 
      tows = n(), # This is only includes tows with mat fems. If no mat fems, not an obs. 
      mean = mean (prop), 
      se = (var(prop)^.5)/(tows^.5)) -> ed_l
    # spread F across cols  
    ed_l %>% gather (variable, value, -(year:tows)) %>%
      unite(temp, ed, variable) %>%
      spread(temp, value) %>% 
      select (year, tows, '1_mean', '1_se', '2_mean', '2_se', '4_mean', '4_se') -> ed
    # write
    ed %>% write.csv("./output/ed.csv") 
    