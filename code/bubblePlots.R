# make bubble plots of Tannner by year and length, 190308
# NOTE: Ideally T2s would be expanded seperatedly from T1s.  T2s are present in 2011-13.  
# So for each event would be 2 seperate distributions to expand.  Totals would A+B and C+D.  

# LOAD ----
set.seed(15343437) 
library (FSA)
library(tidyverse)
library(extrafont)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

read.csv('./data/events.csv')%>% 
  filter (PROJECT_CODE == 'T04', USED_IN_ESTIMATE == "YES") %>%
  select( Event = EVENT_ID,
          year = YEAR,
          length=TOW_LENGTH_DESIGNATED) -> event
read.csv('./data/awl_shellfish_190301.csv') %>% 
  filter (SPECIES_CODE == '931', SEX_CODE == 1) %>% 
  select (Event = EVENT_ID, 
          bw = BIOLOGICAL_WIDTH_MM, 
          freq = FREQUENCY) -> awl 
read.csv('./data/C_17_170921.csv') %>% 
  select( Event = EVENT_ID,
          tm = TM_T) -> tot

# expand awl recs by freq - early years 1 rec represents >1 crab
awl %>% uncount (freq) -> awl

# filter awl by used in estimate  
awl %>% right_join (event) -> awl 

# Expand length freq by event ----

#create empty df to hold final expanded awl 
dat <-data.frame(Event=factor(), bw = numeric())

events <- unique(awl$Event)  
#i <- '2017T04040' # start w one event 

for(i in events){    
  Sys.sleep(0.1)
  print ( c("####",paste (i, "   START") , "####"))
  
  # select 1 tow from awl 
  awl %>% filter (Event == i) -> awl.i
  
  #num measured
  m <- nrow(awl.i)
  
  #tot caught 
  t <- tot[tot$Event == i, "tm"]
  
  
  #create empty vectors for expanded size dists so that object exists even if no crab in it, prevents latter errors when combining
  eawl <- numeric(0)
    
  #expand 
  if(t > 0 & m > 1 & t - m > 1)   # conditional to prevent terminal errors 
  {eawl <- expandLenFreq(awl.i$bw, w=5, total = t, decimals = 1) } # mb used width of .1 mm for catch up report. 
  eawl # note these are only the additional, not measured crabs.
  length(eawl) - (t - m) #compare num of expanded to tot caught minus measured.  Should = 0
  
  #compare hists.  
  par(mfrow= c(3,1))
  hist (awl.i$bw, breaks = seq(0,200,5), main  = c("measured", i), freq = T, col = 'red')
  hist (eawl, breaks = seq(0,200,5), main  = c("additional", i), freq = T, col = 'blue')
  
  #combine measured with additional lengths
  all <- c(awl.i$bw, eawl)
  
  #compare total measurements in expanded awls to tot cnt from CC. Difs should equal 0. 
  length(all) - t
  
  #assign event to vector of lenghts 
  r <- length(all)
  all.df <- data.frame(Event = as.factor(rep(i, r)),
                     bw = all)
  
  # compare hists to first 2
  hist (all.df$bw, breaks = seq(0,200,5), main  = c("combined", i), freq = T, col = 'purple')
  
  nrow(all.df) - t # compare num recs in expanded df to total from CC
  
  dat <- rbind(dat,all.df) # append to main df for all events 
  
  Sys.sleep(0.2) 
  print ( c("####",paste (i, "   COMPLETE") , "####"))
  }
write.csv (dat,"output/expanded931male_Lengths.csv")

## ERROR CHECKING                                                                                                  
  #compare totals
  nrow(dat)
  sum(tot %>% right_join (event) %>% select (tm))
  
  ## compare histograms of awl to dat (combined measured + expanded)
  library (lattice)
  par(mfcol = c(2,1))
  # all
  hist (~ bw , data = awl, breaks = seq(0,200,1), main  = c("measured", "all tows"), freq = T, col = 'red' )
  hist (~ bw , data = dat, breaks = seq(0,200,1), main  = c("expanded", "all tows"), freq = T, col = 'blue')

#______________________________________________________________________________________________________

## bubble plot from expanded awl  ----   
dat <-   read.csv ("output/expanded931male_Lengths.csv")
dat$bw %>% cut_interval (width = 5, boundary = 0, labels = F, n= 40 ) -> dat$bin # bin
dat$cp <- (dat$bin) *5 + 2.5
# join tow lengths 
dat %>% left_join (event) -> dat
dat%>% group_by(Event, bin) %>% summarize (year = first(year),
                                           cnt = n(), 
                                           towLength = first(length), 
                                           cpm = cnt/ towLength,
                                           cp = first(cp))   -> cpmByBinTow

cpmByBinTow %>% group_by (year, cp) %>% summarize (cpm = mean(cpm)) -> cpmByBinYear

ggplot(cpmByBinYear, aes(x= year, y = cp, size= cpm ))+ 
  geom_point(pch = 1) + 
  scale_x_continuous(breaks = seq(1990,2018,1)) +
  scale_y_continuous(breaks = seq(10,200,10)) +
  #scale_size_continuous (range =c(0.001,7)) +
  scale_size (range = c(0,9), name = "Crab per nmi") +
  theme( axis.text.x = element_text(angle=90, vjust= 0), legend.position = "right") + 
  labs (x = 'Year', y = 'Carapace width (mm)' ) + 
  geom_hline (aes(yintercept = 140, linetype = "solid"), show.legend = F) + 
  geom_hline (aes(yintercept = 114, linetype = "dotted"), show.legend = F) -> bub

bub %>% ggsave(file =  './figs/bubble_m931_T04.png', dpi = 300, width = 9, height = 5.5, units = "in")

