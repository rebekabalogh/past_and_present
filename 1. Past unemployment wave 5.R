##########################################################################
############ Deriving length of past unemployment exposure
##########################################################################

## This part of my code derives the (approximate) length of time individuals were
# exposed to unemployment prior to them being interviewed in wave 4. Doing so, it 
# draws on different modules of the data: the employment status history modules 
# asked in waves 1 and 5, as well as the annual event history modules from waves 2 to 4. 
# It draws on the working paper by Wright (2020) although the code is mine.


## Setting up for analysis
# packages needed
search() #search packages that are already installed
packages <- c('tidyverse','naniar','haven','sjlabelled', 'survey', 'fastDummies', 'zoo')
p <-  packages[!(packages %in% installed.packages()[,"Package"])]

lapply(p, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)
rm(list=ls())

setwd("")
path <- ""


##############
## Deriving spells from wave 5 lifetime employment status history module. 
##############

## extracting wave 4 interview dates (for 'backdating' purposes)
d_indresp <- read_dta(file=paste0(path, "ukhls_w4/d_indresp.dta")) 

date_wave4 <- dplyr::select(d_indresp,pidp, d_intdatd_dv, d_intdatm_dv, d_intdaty_dv)
rm(d_indresp)

## extracting wave 5 interview dates 
e_indresp <- read_dta(file=paste0(path, "ukhls_w5/e_indresp.dta"))

date_wave5 <- dplyr::select(e_indresp,pidp, e_intdatd_dv, e_intdatm_dv, e_intdaty_dv, e_jbstat)
rm(e_indresp)

e_empstat <- read_dta(file=paste0(path, "ukhls_w5/e_empstat.dta")) # reading in wave 5 lifetime employment status history module
dim(e_empstat)  

e_empstat <- left_join(e_empstat, date_wave4, by="pidp") # only keeping observations in e_empstat for now
e_empstat <- left_join(e_empstat, date_wave5, by="pidp") # only keeping observations in e_empstat for now

rm(date_wave4, date_wave5)
missval <- c(-9, -8, -7, -2, -1)

e_empstat<-e_empstat[!is.na(e_empstat$pidp), ]

e_empstat <- e_empstat %>%  arrange(pidp, e_spellno) # sort by id var and spellnumber

e_empstat <- e_empstat  %>%  dplyr::mutate_at(vars(d_intdatd_dv, d_intdatm_dv, d_intdaty_dv, e_intdatd_dv, e_intdatm_dv, e_intdaty_dv, e_leshem, e_leshsy4, e_leshst), ~replace(., .<0, NA)) # specifying UKHLS missing values as NA
e_empstat <- e_empstat  %>%  dplyr::mutate_at(c("d_intdatd_dv", "d_intdatm_dv", "d_intdaty_dv", "e_intdatd_dv", "e_intdatm_dv", "e_intdaty_dv", "e_leshem", "e_leshsy4"), as.numeric)

## Setting spell year to missing if month missing
e_empstat$e_leshem[is.na(e_empstat$e_leshsy4)] <- NA


## Deriving the number of spells an individual reported (maxspell) and the year in which a subsequent spell started and what empl.status  that was
e_empstat <- e_empstat %>%
  arrange(pidp) %>%
  group_by(pidp) %>%
  dplyr::mutate(maxspell = max(e_spellno))  %>%
  dplyr::mutate(lead_emplstatus=lead(e_leshst)) %>% # subsequent empl status
  dplyr::mutate(lead_year=lead(e_leshsy4)) %>% # year in which subsequent spell starts
  ungroup()

## if second last observation and lead date missing, it means that is the current spell
e_empstat$maxspellminus <- e_empstat$maxspell-1
e_empstat$current <- ifelse(e_empstat$maxspellminus==e_empstat$e_spellno & e_empstat$lead_emplstatus==0 & is.na(e_empstat$lead_year), 1,
                            0)



## removing lines with missing year, month and e_leshst==0
e_empstat<-e_empstat %>%  filter(!(e_empstat$e_leshst==0 & is.na(e_empstat$e_leshsy4)))

## establishing lower and upper bounds for startdate (winter/december	13		winter/january/february	14	spring	15	summer	16 autumn	17)
e_empstat$X <- paste(e_empstat$e_leshsy4, e_empstat$e_leshem) # to identify spells starting in the same month/season

# by pidp AND X -> create _n & N
e_empstat <- e_empstat %>%
  group_by(pidp, X) %>%
  dplyr::mutate(no=row_number()) %>% # # of spells in the same season
  dplyr::mutate(N=max(no)) %>% # total number of spells in the same season
  ungroup()



## establish earliest month in which spell could have started. If multiple spells in the same season, divide the periods equally
e_empstat$lowmonth <- if_else(e_empstat$e_leshem==13, 12,
                              if_else(e_empstat$e_leshem==14 & e_empstat$no==1, 1,
                                      if_else(e_empstat$e_leshem==14 & e_empstat$no==2 & e_empstat$N==2, 2,
                                              if_else(e_empstat$e_leshem==15 & e_empstat$no==1, 3,
                                                      if_else(e_empstat$e_leshem==15 & e_empstat$no==2 & e_empstat$N==2, 4, 
                                                              if_else(e_empstat$e_leshem==16 & e_empstat$no==1, 6, 
                                                                      if_else(e_empstat$e_leshem==16 & e_empstat$no==2 & e_empstat$N==2, 7, 
                                                                              if_else(e_empstat$e_leshem==16 & e_empstat$no==2 & e_empstat$N==3, 7, 
                                                                                      if_else(e_empstat$e_leshem==16 & e_empstat$no==3 & e_empstat$N==3, 8, 
                                                                                              if_else(e_empstat$e_leshem==17 & e_empstat$no==1, 9, 
                                                                                                      if_else(e_empstat$e_leshem==17 & e_empstat$no==2 & e_empstat$N==2, 10, 
                                                                                                              if_else(e_empstat$e_leshem==17 & e_empstat$no==2 & e_empstat$N==3, 10, 
                                                                                                                      if_else(e_empstat$e_leshem==17 & e_empstat$no==3 & e_empstat$N==3, 11, ,

                                                                                                                                                                                                                                                            e_empstat$e_leshem)))))))))))))
temp <- e_empstat[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4),]


## multiple spells started in the same year but with unknown month -> divide year equally
e_empstat$lowmonth[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4) & e_empstat$N==1] <- 1
e_empstat$lowmonth[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4) & e_empstat$N==2 & e_empstat$no==1] <- 1
e_empstat$lowmonth[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4) & e_empstat$N==2 & e_empstat$no==2] <- 7
e_empstat$lowmonth[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4) & e_empstat$N==3 & e_empstat$no==1] <- 1
e_empstat$lowmonth[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4) & e_empstat$N==3 & e_empstat$no==2] <- 5
e_empstat$lowmonth[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4) & e_empstat$N==3 & e_empstat$no==3] <- 9



## in a similar vein, establish last month in which spell could have started
e_empstat$upmonth <- if_else(e_empstat$e_leshem==13, 12,
                             if_else(e_empstat$e_leshem==14 & e_empstat$no==1, 1,
                                     if_else(e_empstat$e_leshem==14 & e_empstat$no==2 & e_empstat$N==2, 2,
                                             if_else(e_empstat$e_leshem==15 & e_empstat$no==1, 3,
                                                     if_else(e_empstat$e_leshem==15 & e_empstat$no==2 & e_empstat$N==2, 4, 
                                                             if_else(e_empstat$e_leshem==16 & e_empstat$no==1, 6, 
                                                                     if_else(e_empstat$e_leshem==16 & e_empstat$no==2 & e_empstat$N==2, 7, 
                                                                             if_else(e_empstat$e_leshem==16 & e_empstat$no==2 & e_empstat$N==3, 7, 
                                                                                     if_else(e_empstat$e_leshem==16 & e_empstat$no==3 & e_empstat$N==3, 8, 
                                                                                             if_else(e_empstat$e_leshem==17 & e_empstat$no==1, 9, 
                                                                                                     if_else(e_empstat$e_leshem==17 & e_empstat$no==2 & e_empstat$N==2, 10, 
                                                                                                             if_else(e_empstat$e_leshem==17 & e_empstat$no==2 & e_empstat$N==3, 10, 
                                                                                                                     if_else(e_empstat$e_leshem==17 & e_empstat$no==3 & e_empstat$N==3, 11, ,
                                                                                                                             e_empstat$e_leshem)))))))))))))
e_empstat$upmonth[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4) & e_empstat$N==1] <- 12 
e_empstat$upmonth[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4) & e_empstat$N==2 & e_empstat$no==1] <- 6
e_empstat$upmonth[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4) & e_empstat$N==2 & e_empstat$no==2] <- 12
e_empstat$upmonth[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4) & e_empstat$N==3 & e_empstat$no==1] <- 4
e_empstat$upmonth[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4) & e_empstat$N==3 & e_empstat$no==2] <- 8
e_empstat$upmonth[is.na(e_empstat$e_leshem) & !is.na(e_empstat$e_leshsy4) & e_empstat$N==3 & e_empstat$no==3]  <- 12

## create date columns (both Y-m and Y-m-d formats)
e_empstat$startdate_lowerbound_months <- as.yearmon(paste(e_empstat$e_leshsy4, e_empstat$lowmonth), "%Y %m")
e_empstat$startdate_upperbound_months<-  as.yearmon(paste(e_empstat$e_leshsy4, e_empstat$upmonth), "%Y %m")

e_empstat$day <- 1
e_empstat$day[e_empstat$no==2 & e_empstat$N==2 & !is.na(e_empstat$e_leshem)] <- 15

e_empstat$startdate_lowerbound<-base::as.Date(with(e_empstat,paste(e_empstat$e_leshsy4, e_empstat$lowmonth, e_empstat$day, sep="-")),"%Y-%m-%d")
e_empstat$startdate_upperbound<-base::as.Date(with(e_empstat,paste(e_empstat$e_leshsy4, e_empstat$upmonth,e_empstat$day, sep="-")),"%Y-%m-%d") 


## next, adjusting upper and lower start date boundaries based on adjacent spells
e_empstat <- e_empstat%>%
  arrange(pidp, e_spellno) %>%
  group_by(pidp)%>%
  dplyr::mutate(lag_upperdate=dplyr::lag(startdate_upperbound)) %>%
  dplyr::mutate(lag_year=dplyr::lag(e_leshsy4)) %>%  # previous spell's year
  dplyr::mutate(lead_year=dplyr::lead(e_leshsy4)) %>% # subsequent spell's year
  ungroup()




# if previous spell started after current spell was estimated to -> adjust
e_empstat$startdate_lowerbound <- dplyr::if_else(is.na(e_empstat$e_leshem) & e_empstat$lag_upperdate>e_empstat$startdate_lowerbound & !is.na(e_empstat$startdate_lowerbound) & e_empstat$e_leshsy4==e_empstat$lag_year, e_empstat$lag_upperdate,
                                                 e_empstat$startdate_lowerbound)

e_empstat <- e_empstat%>%
  arrange(pidp, e_spellno) %>%
  group_by(pidp)%>%
  dplyr::mutate(lead_lowerdate=dplyr::lead(startdate_lowerbound)) %>%
  ungroup()

# if subsequent spell started before current upper bound -> adjust
e_empstat$startdate_upperbound <- dplyr::if_else(is.na(e_empstat$e_leshem) & e_empstat$lead_lowerdate<e_empstat$startdate_upperbound & !is.na(e_empstat$lead_lowerdate) & e_empstat$e_leshsy4==e_empstat$lead_year, e_empstat$lead_lowerdate,
                                                 e_empstat$startdate_upperbound)


## taking mid-point of lower and upper bounds for startdate
e_empstat$startdate <- as.Date((as.numeric(e_empstat$startdate_lowerbound) + as.numeric(e_empstat$startdate_upperbound))/2, origin = '1970-01-01')

## if missing spell
e_empstat <- e_empstat %>%
  group_by(pidp, X) %>%
  dplyr::select(-c(startdate_lowerbound,startdate_upperbound)) %>%
  dplyr::mutate(missingspell=(is.na(startdate))) %>% # # of spells in the same season
  dplyr::mutate(nofmissing=row_number(missingspell)) %>% # total number of spells in the same season
  ungroup()


## interview date
e_empstat$intdate_wave5 <- base::as.Date(with(e_empstat,paste(e_empstat$e_intdaty_dv, e_empstat$e_intdatm_dv, e_empstat$e_intdatd_dv, sep="-")),"%Y-%m-%d")

## generating subsequent spell's startdate  -> I assume this is the end of a given spell 
e_empstat <- e_empstat %>%
  arrange(pidp, e_spellno) %>%
  group_by(pidp) %>%
  dplyr::mutate(lead_startdate=lead(startdate)) %>%
  dplyr::mutate(lag_startdate=lag(startdate)) %>%
  ungroup()



## create end date (start of following spell or interview date if current spell)
e_empstat$enddate <- dplyr::if_else(e_empstat$current==1,as.Date(e_empstat$intdate_wave5),
                                    e_empstat$lead_startdate)
e_empstat <- e_empstat %>%
  arrange(pidp, e_spellno) %>%
  group_by(pidp) %>%
  dplyr::mutate(imputationflag=(is.na(startdate) | is.na(enddate))) %>% # creating imputation flag to filter for outlier imputed values
  dplyr::mutate(anymissing=max(imputationflag)) %>%
  ungroup()


## imputing startdates where both year and month is missing
e_empstat$startdate <- if_else(is.na(e_empstat$startdate), as.Date((as.numeric(e_empstat$lag_startdate) + as.numeric(e_empstat$enddate))/2, origin = '1970-01-01'),
                               e_empstat$startdate)


## re-calculating end-dates
e_empstat <- e_empstat %>%
  arrange(pidp, e_spellno) %>%
  group_by(pidp) %>%
  dplyr::select(-c(lead_startdate, lag_startdate, enddate)) %>%
  dplyr::mutate(lead_startdate=lead(startdate)) %>%
  dplyr::mutate(lag_startdate=lag(startdate)) %>%
  ungroup()
e_empstat$enddate <- dplyr::if_else(e_empstat$current==1,as.Date(e_empstat$intdate_wave5),
                                    e_empstat$lead_startdate)

e_empstat$startdate <- if_else(is.na(e_empstat$startdate), as.Date((as.numeric(e_empstat$lag_startdate) + as.numeric(e_empstat$enddate))/2, origin = '1970-01-01'),
                               e_empstat$startdate)


## backdating spells -> calculate wave 4 interview date
e_empstat$intdate_wave4 <- base::as.Date(with(e_empstat,paste(e_empstat$d_intdaty_dv, e_empstat$d_intdatm_dv, e_empstat$d_intdaty_dv, sep="-")),"%Y-%m-%d")

e_empstat <- e_empstat[!is.na(e_empstat$intdate_wave4),] # removing those that weren't interviewed in wave 4

e_empstat <- e_empstat %>% 
  dplyr:: filter(e_empstat$intdate_wave4>=e_empstat$startdate | is.na(e_empstat$startdate)) 



e_empstat$enddate <- if_else(e_empstat$intdate_wave4<e_empstat$enddate & !is.na(e_empstat$enddate),e_empstat$intdate_wave4,
                             e_empstat$enddate) # if enddate is after interview date -> replace that



e_empstat$career_start_w5<- if_else(e_empstat$e_spellno==1, e_empstat$startdate,NA)

e_empstat <- e_empstat %>%
  arrange(pidp, e_spellno) %>%
  group_by(pidp) %>%
  tidyr::fill(career_start_w5) %>%
  ungroup()


## calculating spell duration
e_empstat$dur <- lubridate::interval(e_empstat$startdate, e_empstat$enddate) %/% months(1)


## dropping participants with non-chronological dates recorded
e_empstat <- e_empstat %>%
  arrange(pidp, e_spellno) %>%
  group_by(pidp) %>%
  dplyr::mutate(lead_month=lead(e_leshem)) %>%
  dplyr::mutate(lag_month=lag(e_leshem)) %>%
  ungroup()

e_empstat <- e_empstat %>%
  arrange(pidp, e_spellno) %>%
  group_by(pidp) %>%
  dplyr::mutate(minus_dur=(dur<0)) %>%
  dplyr::mutate(nonchrono=max(minus_dur, na.rm=TRUE)) %>%
  ungroup()


# if -1 and leshem is >12 then just set it to 0 (just due to being in the same season)
e_empstat$dur[e_empstat$dur==-1 & (e_empstat$e_leshem>12  | e_empstat$lead_month>12 | e_empstat$lag_month>12)] <- 0


# removing non-chronological dates
e_empstat <- e_empstat %>%   filter(!nonchrono==1) 
e_empstat$unemp_dur <- ifelse(e_empstat$e_leshst==4, e_empstat$dur,
                              if_else(is.na(e_empstat$dur), NA_real_,
                                      0))



imputed <- e_empstat[e_empstat$imputationflag==TRUE,]
e_empstat$unemp_dur[e_empstat$imputationflag==TRUE & e_empstat$unemp_dur>12 & !is.na(e_empstat$unemp_dur)] <- 0 # remove very long imputed values

## creating flag for being included in emp history
e_empstat <- e_empstat %>%
  arrange(pidp, e_spellno) %>%
  group_by(pidp) %>%
  dplyr::mutate(allNA=all(is.na(dur))) %>%
  ungroup()



e_empstat$emphistory_w5 <- 1



###### summarising total past exposure to unemployment
e_empstat <- e_empstat %>%
  arrange(pidp, e_spellno) %>%
  group_by(pidp) %>%
  dplyr::mutate(total_UE_months=sum(unemp_dur, na.rm = TRUE)) %>%
  ungroup()




e_empstat$total_UE_months[e_empstat$allNA==TRUE] <- NA # if only missing spells -> set to missing



# KEEPING one line per observation
n_distinct(e_empstat$pidp) 
e_empstat <- e_empstat %>% 
  arrange(pidp, e_spellno) %>%
  group_by(pidp) %>%
  dplyr::select(pidp, total_UE_months, emphistory_w5, career_start_w5) %>%
  dplyr::rename(e_total_UE_months=total_UE_months) %>%
  filter(row_number() == 1) %>%
  ungroup

rm(temp, imputed)


save(e_empstat, file = "e_empstat_cleaned.Rda") # saving 
