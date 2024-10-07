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
packages <- c('dplyr','naniar','haven','sjlabelled', 'survey', 'fastDummies', 'zoo', 'survey')
p <-  packages[!(packages %in% installed.packages()[,"Package"])]

lapply(p, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)
rm(list=ls())

setwd("")
path <- ""

##############
## Deriving spells from wave 1 lifetime employment status history module. 
# NB: As this module was proven to be lengthy, only individuals in the first half of the year were asked, the rest were asked in wave 5.

##############

## extracting interview dates from wave 1
    a_indresp <- read_dta(file=paste0(path, "ukhls_w1/a_indresp.dta")) 
    date_wave1 <- dplyr::select(a_indresp,pidp, a_intdatd_dv, a_intdatm_dv, a_intdaty_dv, a_age_dv, a_jbstat)
    rm(a_indresp)

## reading in empl status history file
    a_empstat <- read_dta(file=paste0(path, "ukhls_w1/a_empstat.dta")) 
    dim(a_empstat)
    a_empstat <- left_join(a_empstat, date_wave1, by="pidp") # merging with interview dates, only keeping observations in a_empstat
    rm(date_wave1)


missval <- c(-9, -8, -7, -2, -1) # setting UKHLS missing values


a_empstat<-a_empstat[!is.na(a_empstat$pidp), ]
a_empstat <- a_empstat %>% arrange(pidp, a_spellno) 



# setting minus values to missing (UKHLS has different missing values for refusal, proxy etc...)
    a_empstat <- a_empstat  %>%
      dplyr::mutate_at(vars(a_intdatd_dv, a_intdatm_dv, a_intdaty_dv,  a_leshem, a_leshsy4, a_leshst), ~replace(., .<0, NA))

## making sure these variables are read in as numeric
    a_empstat <- a_empstat  %>%
      dplyr::mutate_at(c("a_intdatd_dv", "a_intdatm_dv", "a_intdaty_dv", "a_leshem", "a_leshsy4"), as.numeric)

## if year of a spell is missing, setting month to missing too.

        a_empstat$a_leshem[is.na(a_empstat$a_leshsy4)] <- NA

## deriving the number of spells an individual reported (maxspell) and the year in which a subsequent spell started and what empl.status  that was
    a_empstat <- a_empstat %>%
      arrange(pidp) %>%
      group_by(pidp) %>%
      dplyr::mutate(maxspell = max(a_spellno))  %>%
      dplyr::mutate(lead_emplstatus=lead(a_leshst)) %>% # subsequent empl status
      dplyr::mutate(lead_year=lead(a_leshsy4)) %>% # year in which subsequent spell starts
      ungroup()

## if second last observation and lead date missing, it means that is the current spell
    a_empstat$maxspellminus <- a_empstat$maxspell-1

        a_empstat$current <- ifelse(a_empstat$maxspellminus==a_empstat$a_spellno & a_empstat$lead_emplstatus==0 & is.na(a_empstat$lead_year), 1,
                                0)



## removing lines with missing year, month and a_leshst==0 -> this indicates that second last spell is on-going
 
    a_empstat<-a_empstat %>%
      filter(!(a_empstat$a_leshst==0 & is.na(a_empstat$a_leshsy4))) 
    

## establishing lower and upper bounds, first for seasons (winter/december	13		winter/january/february	14	spring	15	summer	16 autumn	17)
    a_empstat$X <- paste(a_empstat$a_leshsy4, a_empstat$a_leshem) # this is to identify spells starting in the same month/season


# by pidp AND newly created variable X -> create _n (rank of spell starting in the same month/season) & N (total number of spells starting in the same month/season)
    a_empstat <- a_empstat %>%
      group_by(pidp, X) %>%
      dplyr::mutate(no=row_number()) %>% # # of spells in the same season
      dplyr::mutate(N=max(no)) %>% # total number of spells in the same season
      ungroup()


# dividing up season equally if multiple spells. otherwise lower bound is month in which spell started
a_empstat$lowmonth <- if_else(a_empstat$a_leshem==13, 12,
                              if_else(a_empstat$a_leshem==14 & a_empstat$no==1, 1,
                                      if_else(a_empstat$a_leshem==14 & a_empstat$no==2 & a_empstat$N==2, 2,
                                              if_else(a_empstat$a_leshem==15 & a_empstat$no==1, 3,
                                                      if_else(a_empstat$a_leshem==15 & a_empstat$no==2 & a_empstat$N==2, 4, 
                                                              if_else(a_empstat$a_leshem==16 & a_empstat$no==1, 6, 
                                                                      if_else(a_empstat$a_leshem==16 & a_empstat$no==2 & a_empstat$N==2, 7, 
                                                                              if_else(a_empstat$a_leshem==16 & a_empstat$no==2 & a_empstat$N==3, 7, 
                                                                                      if_else(a_empstat$a_leshem==16 & a_empstat$no==3 & a_empstat$N==3, 8, 
                                                                                              if_else(a_empstat$a_leshem==17 & a_empstat$no==1, 9, 
                                                                                                      if_else(a_empstat$a_leshem==17 & a_empstat$no==2 & a_empstat$N==2, 10, 
                                                                                                              if_else(a_empstat$a_leshem==17 & a_empstat$no==2 & a_empstat$N==3, 10, 
                                                                                                                      if_else(a_empstat$a_leshem==17 & a_empstat$no==3 & a_empstat$N==3, 11, ,
                                                                                                                              a_empstat$a_leshem)))))))))))))
# if same year but missing month -> establishing lower bound, which depends on number of spells within the same year with missing month
    temp <- a_empstat[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4),]

    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==1] <- 1
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==2 & a_empstat$no==1] <- 1
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==2 & a_empstat$no==2] <- 7
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==3 & a_empstat$no==1] <- 1
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==3 & a_empstat$no==2] <- 5
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==3 & a_empstat$no==3] <- 9
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==4 & a_empstat$no==1] <- 1
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==4 & a_empstat$no==2] <- 4
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==4 & a_empstat$no==3] <- 7
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==4 & a_empstat$no==4] <- 10


# repeat the same to establish upper boundary
a_empstat$upmonth <- if_else(a_empstat$a_leshem==13, 12,
                             if_else(a_empstat$a_leshem==14 & a_empstat$no==1, 1,
                                     if_else(a_empstat$a_leshem==14 & a_empstat$no==2 & a_empstat$N==2, 2,
                                             if_else(a_empstat$a_leshem==15 & a_empstat$no==1, 3,
                                                     if_else(a_empstat$a_leshem==15 & a_empstat$no==2 & a_empstat$N==2, 4, 
                                                             if_else(a_empstat$a_leshem==16 & a_empstat$no==1, 6, 
                                                                     if_else(a_empstat$a_leshem==16 & a_empstat$no==2 & a_empstat$N==2, 7, 
                                                                             if_else(a_empstat$a_leshem==16 & a_empstat$no==2 & a_empstat$N==3, 7, 
                                                                                     if_else(a_empstat$a_leshem==16 & a_empstat$no==3 & a_empstat$N==3, 8, 
                                                                                             if_else(a_empstat$a_leshem==17 & a_empstat$no==1, 9, 
                                                                                                     if_else(a_empstat$a_leshem==17 & a_empstat$no==2 & a_empstat$N==2, 10, 
                                                                                                             if_else(a_empstat$a_leshem==17 & a_empstat$no==2 & a_empstat$N==3, 10, 
                                                                                                                     if_else(a_empstat$a_leshem==17 & a_empstat$no==3 & a_empstat$N==3, 11, ,
                                                                                                                             a_empstat$a_leshem)))))))))))))
    a_empstat$upmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==1] <- 12 
    a_empstat$upmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==2 & a_empstat$no==1] <- 6
    a_empstat$upmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==2 & a_empstat$no==2] <- 12
    a_empstat$upmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==3 & a_empstat$no==1] <- 4
    a_empstat$upmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==3 & a_empstat$no==2] <- 8
    a_empstat$upmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==3 & a_empstat$no==3]  <- 12
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==4 & a_empstat$no==1] <- 3
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==4 & a_empstat$no==2] <- 6
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==4 & a_empstat$no==3] <- 9
    a_empstat$lowmonth[is.na(a_empstat$a_leshem) & !is.na(a_empstat$a_leshsy4) & a_empstat$N==4 & a_empstat$no==4] <- 12


# creating dates of upper and lower bounds -> both year-month and year-month-day formats
    a_empstat$startdate_lowerbound_months <- as.yearmon(paste(a_empstat$a_leshsy4, a_empstat$lowmonth), "%Y %m")
    a_empstat$startdate_upperbound_months<-  as.yearmon(paste(a_empstat$a_leshsy4, a_empstat$upmonth), "%Y %m")
    a_empstat$day <- 1
    a_empstat$day[a_empstat$no==2 & a_empstat$N==2 & !is.na(a_empstat$a_leshem)] <- 15
    a_empstat$startdate_lowerbound<-base::as.Date(with(a_empstat,paste(a_empstat$a_leshsy4, a_empstat$lowmonth, a_empstat$day, sep="-")),"%Y-%m-%d")
    a_empstat$startdate_upperbound<-base::as.Date(with(a_empstat,paste(a_empstat$a_leshsy4, a_empstat$upmonth,a_empstat$day, sep="-")),"%Y-%m-%d") 


## Next, adjusting upper and lower start date boundaries based on adjacent spells
    a_empstat <- a_empstat%>%
      arrange(pidp, a_spellno) %>%
      group_by(pidp) %>%
      dplyr::mutate(lag_upperdate=dplyr::lag(startdate_upperbound)) %>% #previous spell's upper bound
      dplyr::mutate(lag_year=dplyr::lag(a_leshsy4)) %>% #previous spell's year
      dplyr::mutate(lead_year=dplyr::lead(a_leshsy4)) %>% # subsequent spell's year
      ungroup()
    


# adjusting lower bound if previous spell's start date is after that (and is in the same year)
    a_empstat$startdate_lowerbound <- dplyr::if_else(is.na(a_empstat$a_leshem) & a_empstat$lag_upperdate>a_empstat$startdate_lowerbound & !is.na(a_empstat$startdate_lowerbound) & a_empstat$a_leshsy4==a_empstat$lag_year, a_empstat$lag_upperdate,
                                                     a_empstat$startdate_lowerbound)
    
    a_empstat <- a_empstat %>%
      arrange(pidp, a_spellno) %>%
      group_by(pidp)%>%
      dplyr::mutate(lead_lowerdate=dplyr::lead(startdate_lowerbound)) %>% # subsequent spell's lower bound
      ungroup()
    


# adjusting lower bound if previous spell's start date is before that (and is in the same year)
    a_empstat$startdate_upperbound <- dplyr::if_else(is.na(a_empstat$a_leshem) & a_empstat$lead_lowerdate<a_empstat$startdate_upperbound & !is.na(a_empstat$lead_lowerdate) & a_empstat$a_leshsy4==a_empstat$lead_year, a_empstat$lead_lowerdate,
                                                     a_empstat$startdate_upperbound) 


## Taking mid-point of lower and upper bound to establish start date of a given spell
    a_empstat$startdate <- as.Date((as.numeric(a_empstat$startdate_lowerbound) + as.numeric(a_empstat$startdate_upperbound))/2, origin = '1970-01-01')

###### missing spell
    a_empstat <- a_empstat %>%
      group_by(pidp, X) %>%
      dplyr::select(-c(startdate_lowerbound,startdate_upperbound, lag_upperdate, lead_lowerdate, lag_year, lead_year)) %>%
      dplyr::mutate(missingspell=(is.na(startdate))) %>% # whether spell is missing
      dplyr::mutate(nofmissing=row_number(missingspell)) %>%  # the rank in case multiple adjacent missing spells
      ungroup()
    


## interview date
    a_empstat$intdate_wave1 <- base::as.Date(with(a_empstat,paste(a_empstat$a_intdaty_dv, a_empstat$a_intdatm_dv, a_empstat$a_intdatd_dv, sep="-")),"%Y-%m-%d")


## generating subsequent spell's startdate  -> I assume this is the end of a given spell
    a_empstat <- a_empstat %>%
      arrange(pidp, a_spellno) %>%
      group_by(pidp) %>%
      dplyr::mutate(lead_startdate=lead(startdate)) %>%
      dplyr::mutate(lag_startdate=lag(startdate)) %>%
      ungroup()

## creating end date of a spell (start of subsequent spell or interView date if current spell)
    a_empstat$enddate <- dplyr::if_else(a_empstat$current==1,as.Date(a_empstat$intdate_wave1),
                                        a_empstat$lead_startdate)
    

    a_empstat <- a_empstat %>%
      arrange(pidp, a_spellno) %>%
      group_by(pidp) %>%
      dplyr::mutate(imputationflag=(is.na(startdate) | is.na(enddate))) %>% # creating imputation flag 
      dplyr::mutate(anymissing=max(imputationflag)) %>%
      ungroup()
    



## imputing startdates where both year and month is missing
    a_empstat$startdate <- if_else(is.na(a_empstat$startdate), as.Date((as.numeric(a_empstat$lag_startdate) + as.numeric(a_empstat$enddate))/2, origin = '1970-01-01'),
                                   a_empstat$startdate)



## re-calculating end-dates
    a_empstat <- a_empstat %>%
      arrange(pidp, a_spellno) %>%
      group_by(pidp) %>%
      dplyr::select(-c(lead_startdate, lag_startdate, enddate)) %>%
      dplyr::mutate(lead_startdate=lead(startdate)) %>%
      dplyr::mutate(lag_startdate=lag(startdate)) %>%
      ungroup()


    a_empstat$enddate <- dplyr::if_else(a_empstat$current==1,as.Date(a_empstat$intdate_wave1),
                                        a_empstat$lead_startdate)

    
    a_empstat$startdate <- if_else(is.na(a_empstat$startdate), as.Date((as.numeric(a_empstat$lag_startdate) + as.numeric(a_empstat$enddate))/2, origin = '1970-01-01'),
                                   a_empstat$startdate)
    



    a_empstat$career_start<- if_else(a_empstat$a_spellno==1, a_empstat$startdate,NA)

    a_empstat <- a_empstat %>%
      arrange(pidp, a_spellno) %>%
      group_by(pidp) %>%
      tidyr::fill(career_start) %>%
      ungroup()


## calculating duration of spells
    a_empstat$dur <- lubridate::interval(a_empstat$startdate, a_empstat$enddate) %/% months(1)


## dropping participants' data who recorded non-chronological dates
    a_empstat <- a_empstat %>%
      arrange(pidp, a_spellno) %>%
      group_by(pidp) %>%
      dplyr::mutate(lead_month=lead(a_leshem)) %>% 
      dplyr::mutate(lag_month=lag(a_leshem)) %>%
      ungroup()
    
    a_empstat <- a_empstat %>%
      arrange(pidp, a_spellno) %>%
      group_by(pidp) %>%
      dplyr::mutate(minus_dur=(dur<0)) %>%
      dplyr::mutate(nonchrono=max(minus_dur, na.rm=TRUE)) %>%
      ungroup()
    



# if -1 and leshem is >12 then just set it to 0 (it is just due to being in the same season)
    a_empstat$dur[a_empstat$dur==-1 & (a_empstat$a_leshem>12  | a_empstat$lead_month>12 | a_empstat$lag_month>12)] <- 0


   
    a_empstat <- a_empstat %>% filter(!nonchrono==1) # # remove non-chronological dates


## Creating column recording the length of spell if it was unemployment
    a_empstat$unemp_dur <- ifelse(a_empstat$a_leshst==4, a_empstat$dur,
                              if_else(is.na(a_empstat$dur), NA_real_,
                                      0))



## seeing if imputed values are higher than non-imputed values on avarage
imputed <- a_empstat[a_empstat$imputationflag==TRUE,]
a_empstat$unemp_dur[a_empstat$imputationflag==TRUE & a_empstat$unemp_dur>12 & !is.na(a_empstat$unemp_dur)] <- 0 # remove very long imputed values (currently if longer than 12 months)

## creating flag for being included in emp history module in wave 1 and whether all missing data
    a_empstat <- a_empstat %>%
      arrange(pidp, a_spellno) %>%
      group_by(pidp) %>%
      dplyr::mutate(allNA=all(is.na(dur))) %>%
      ungroup()


  a_empstat$emphistory_w1 <- 1


###### summarising total past exposure to unemployment
    a_empstat <- a_empstat %>%
      arrange(pidp, a_spellno) %>%
      group_by(pidp) %>%
      dplyr::mutate(total_UE_months=sum(unemp_dur, na.rm = TRUE)) %>%
      ungroup()





    a_empstat$total_UE_months[a_empstat$allNA==TRUE] <- NA # if only missing spells -> set to missing
    a_empstat$total_UE_months[a_empstat$allNA==TRUE & a_empstat$maxspell==2 & a_empstat$a_jbstat==6 & a_empstat$a_leshst==7] <- 0 # if only ever been family carer -> 0
    a_empstat$total_UE_months[a_empstat$allNA==TRUE & a_empstat$maxspell==2 & a_empstat$a_jbstat==8 & a_empstat$a_leshst==9] <- 0 # if only ever LT sick -> 0




n_distinct(a_empstat$pidp)

## keeping one line per observation for subsequent analyses
    a_empstat <- a_empstat %>% 
      arrange(pidp, a_spellno) %>%
      group_by(pidp) %>%
      dplyr::select(pidp, total_UE_months, emphistory_w1, career_start) %>%
      dplyr::rename(a_total_UE_months=total_UE_months) %>%
      filter(row_number() == 1) %>%
      ungroup
rm(imputed, temp)

save(a_empstat, file = "a_empstat_cleaned.Rda") # saving 


############################################################
## deriving spells in-between interview dates -> waves 2,3,4
###########################################################

search() #search packages that are already installed
packages <- c('tidyverse','naniar','haven','sjlabelled', 'survey', 'fastDummies', 'zoo')
p <-  packages[!(packages %in% installed.packages()[,"Package"])]

lapply(p, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)
rm(list=ls())

setwd("")
path <- ""


# create empty data frame to fill  with id variable from 4th wave
annualevent <- read_dta(paste0(path, "ukhls_w4/d_indresp.dta")) %>%  dplyr::select(pidp, d_indscus_lw, d_strata, d_psu)

for (wn in 2:4) {
  wl <- paste0(letters[wn],"_")
  m1 <- wn-1
  ml <- paste0(letters[m1],"_")
  print(m1)
  wave_data <- read_dta(paste0(path, "ukhls_w",wn,"/", wl, "indresp.dta")) %>% # reading in wave file
    dplyr::rename_at(vars(-pidp), str_sub, start = 3L) %>% ## deleting wave prefix from column names (e.g. "")
    dplyr::select(pidp,intdatd_dv,intdatm_dv, intdaty_dv, notempchk, empchk, cstat, ff_jbstat, ff_ivlolw, ivfio,
           matches("empstend"), matches("nxtst"),
           matches("nextelse"), matches("statend"), matches("currstat"), matches("nxtjbend")) # variables needed

  
  tempfile <- read_dta(paste0(path, "ukhls_w",m1,"/", ml, "indresp.dta")) %>% # reading in previous wave file
    dplyr::rename_at(vars(-pidp), str_sub, start = 3L) %>%
    dplyr::select(pidp,intdatd_dv,intdatm_dv, intdaty_dv) %>%
    dplyr::rename(intday=intdatd_dv) %>%
    dplyr::rename(intmonth=intdatm_dv) %>%
    dplyr::rename(intyear=intdaty_dv)
  
  wave_data <- left_join(wave_data, tempfile, by="pidp") # only keeping observations in a_empstat for now
  rm(tempfile)
  
  # set to missing
  wave_data <- wave_data  %>%
    dplyr::mutate_at(vars(-pidp), ~replace(., .<0, NA))
  
  ## interview dates
  wave_data$priorintdate<- base::as.Date(with(wave_data,paste(wave_data$intyear, wave_data$intmonth, wave_data$intday, sep="-")),"%Y-%m-%d")
  wave_data$currentintdate <- base::as.Date(with(wave_data,paste(wave_data$intdaty_dv, wave_data$intdatm_dv, wave_data$intdatd_dv, sep="-")),"%Y-%m-%d")

  
  ## end of ff_jbstat (original), see figure
  wave_data$date_empstend <- base::as.Date(with(wave_data,paste(wave_data$empstendy4, wave_data$empstendm, wave_data$empstendd, sep="-")),"%Y-%m-%d")

  wave_data$date_empstend[wave_data$date_empstend<wave_data$priorintdate & !is.na(wave_data$date_empstend)] <- NA 
  ## end of nxtst/nxtstelse (zero)
  wave_data$date_nxtstend<- base::as.Date(with(wave_data,paste(wave_data$nxtstendy4, wave_data$nxtstendm, wave_data$nxtstendd, sep="-")),"%Y-%m-%d")
  wave_data$date_nxtjbend<- base::as.Date(with(wave_data,paste(wave_data$nxtjbendy4, wave_data$nxtjbendm, wave_data$nxtjbendd, sep="-")),"%Y-%m-%d")
  wave_data$date_zero <- ifelse(!is.na(wave_data$date_nxtstend), wave_data$date_nxtstend,
                                 ifelse(!is.na(wave_data$date_nxtjbend),wave_data$date_nxtjbend,
                                         NA))
  wave_data$date_zero <- as.Date(wave_data$date_zero, origin="1970-01-01")
  
  wave_data$date_zero[wave_data$date_zero<wave_data$priorintdate & !is.na(wave_data$date_zero)] <- NA 
  
  
  ## unemployment duration in each of the n+2 periods
  wave_data$date_statend1 <- base::as.Date(with(wave_data,paste(wave_data$statendy41, wave_data$statendm1, wave_data$statendd1, sep="-")),"%Y-%m-%d")
  
  wave_data$duration_original <- if_else(wave_data$ff_jbstat==3 & wave_data$notempchk==2, lubridate::interval(wave_data$priorintdate, wave_data$date_empstend) %/% months(1),
                                         0)
  wave_data$duration_zero <- if_else(wave_data$nxtstelse==1 & wave_data$cstat==1, lubridate::interval(wave_data$date_empstend, wave_data$date_zero) %/% months(1),
                                     if_else(wave_data$nxtstelse==1 & wave_data$cstat==2, lubridate::interval(wave_data$date_empstend, wave_data$currentintdate) %/% months(1),
                                             0))
  wave_data$duration_1 <- if_else(wave_data$nextelse1==1 & wave_data$currstat1==1, lubridate::interval(wave_data$date_zero, wave_data$date_statend1) %/% months(1),
                                  if_else(wave_data$nextelse1==1 & wave_data$currstat1==2, lubridate::interval(wave_data$date_zero, wave_data$currentintdate) %/% months(1),
                                          0))
  
  ## creating end dates for statend -> end of nextelsei + replacing 
  
  library(dplyr)
  df <- dplyr::select(wave_data,contains("statendd"))
  
  for (x in 2:ncol(df)) {
    print(x)
    xx <- x-1
    print(xx)
    
    d <- paste0("statendd",x)
    tempday <- grep(d, names(wave_data))
    wave_data$day <- wave_data[[tempday]]
    wave_data$day[wave_data$day<0] <- NA
    
    m <- paste0("statendm",x)
    tempmonth <- grep(m, names(wave_data))
    wave_data$month <- wave_data[[tempmonth]]
    wave_data$month[wave_data$month<0] <- NA
    
    y <- paste0("statendy4",x)
    tempyear <- grep(y, names(wave_data))
    wave_data$year <- wave_data[[tempyear]]
    wave_data$year[wave_data$year<0] <- NA
    
    
    wave_data$tempdate <- base::as.Date(with(wave_data,paste(wave_data$year, wave_data$month, wave_data$day, sep="-")),"%Y-%m-%d")
    wave_data[paste0("date_statend",x)] <- wave_data$tempdate # end of nextstati/nextelsei
    
    
    c <- paste0("currstat",x)
    tempcurr <- grep(c, names(wave_data))
    wave_data$icurrstat <- wave_data[[tempcurr]]
    
    n <- paste0("nextelse",x)
    tempnext <- grep(n, names(wave_data))
    wave_data$inextelse<- wave_data[[tempnext]]
    
    
    minus <- paste0("date_statend",xx)
    tempminus <- grep(minus, names(wave_data))
    wave_data$enddate_minus <- wave_data[[tempminus]]
    
    # replacing in loop -> otherwise, stays the same
    
    wave_data$temp_duration <- if_else(wave_data$inextelse==1 & wave_data$icurrstat==1, lubridate::interval(wave_data$enddate_minus, wave_data$tempdate) %/% months(1),
                                       if_else(wave_data$inextelse==1 & wave_data$icurrstat==2, lubridate::interval(wave_data$enddate_minus, wave_data$currentintdate) %/% months(1),
                                               0))
    
    wave_data[paste0("duration_",x)] <- wave_data$temp_duration# end of nextstati/nextelsei
    
    rm(d,m,y,tempyear, tempmonth, tempday, tempcurr, tempminus, tempnext, minus, n, c)
    wave_data = subset(wave_data, select = -c(enddate_minus, inextelse, icurrstat, tempdate, temp_duration) )
  }

  wave_data$sumall <- rowSums(wave_data[colnames(wave_data)[grepl("duration_*",colnames(wave_data))]], na.rm=TRUE) # checked, this gives the same result as above!

  wave_data$unemp_dur <- if_else(wave_data$ff_jbstat==3 & wave_data$notempchk==1, lubridate::interval(wave_data$priorintdate, wave_data$currentintdate) %/% months(1),
                                 if_else(wave_data$empchk==1, 0,
                                         wave_data$sumall ))
  # checking if not double counting 
  for (x in 1:(ncol(df)-1)){
    xx <- x+1
  check <- paste0("currstat",x)
  tempcheck <- grep(check, names(wave_data))
  wave_data$check <- wave_data[[tempcheck]] 
  
  for (y in xx:ncol(df))
  check2 <- paste0("duration_",y)
  tempcheck2 <- grep(check2, names(wave_data))
  wave_data$check2 <- wave_data[[tempcheck2]] 
  
  check <- wave_data %>% filter(check==3 & check2>0)
  print(dim(check))
  rm(check)
  }
  
  
  # checking if unemployment is not longer than period between two interviews
  
  wave_data$intcheck <- lubridate::interval(wave_data$priorintdate, wave_data$currentintdate) %/% months(1)
  check <- wave_data %>% filter(unemp_dur>intcheck)
  print(dim(check))
  rm(check)


  wave_data$unemp_dur[wave_data$unemp_dur<0] <- NA


  # set to 0, unless empchk AND notempchk are both missing
  wave_data$unemp_dur[is.na(wave_data$unemp_dur)] <- 0
  wave_data$unemp_dur[(wave_data$empchk==2 | wave_data$notempchk==2) & is.na(wave_data$empstendy4)] <- NA
  wave_data$unemp_dur[is.na(wave_data$empchk) & is.na(wave_data$notempchk)] <- NA
  

  # saving and merging it
  annual <- wave_data %>% dplyr::select(pidp, unemp_dur)  %>%
    dplyr::rename_at(vars(-pidp), ~ paste0(wl,.)) 
  
  
  annualevent <- left_join(annualevent, annual, by = "pidp") # left join
  rm(wave_data, annual)
  
}




#save(annualevent, file = "annualevent.Rda")

load("a_empstat_cleaned.Rda")
load("e_empstat_cleaned.Rda")
load("annualevent.Rda")





unemployment <- left_join(annualevent, a_empstat, by = "pidp")
unemployment <- left_join(unemployment, e_empstat, by = "pidp")

w1 <- read_dta(paste0(path, "ukhls_w1/a_indresp.dta")) %>%  dplyr::select(pidp, a_jbstat, a_school, a_fenow)
unemployment <- left_join(unemployment, w1, by = "pidp")



unemployment$summedpast <-  rowSums(unemployment[c("a_total_UE_months", "b_unemp_dur", "c_unemp_dur", "d_unemp_dur")], na.rm = TRUE)



# defining the total past exposure to unemployment in months
unemployment$totalexposure <- ifelse(unemployment$emphistory_w5==1 & is.na(unemployment$emphistory_w1), unemployment$e_total_UE_months,
                                     ifelse(unemployment$emphistory_w1==1 & is.na(unemployment$emphistory_w5), unemployment$summedpast,
                                            ifelse(unemployment$emphistory_w1==1 & !is.na(unemployment$emphistory_w5), unemployment$summedpast,
                                                   ifelse((unemployment$a_school==3 | unemployment$a_fenow==3) & unemployment$emphistory_w5!=1, unemployment$summedpast, 
                                                   NA))))
unemployment$totalexposure[unemployment$emphistory_w5!=1 & unemployment$emphistory_w1!=1 & unemployment$a_school!=3 & unemployment$a_fenow!=3] <- NA # if not in either the wave 1 and wave 5 module, and not in FT education in wave 1
unemployment$totalexposure[unemployment$emphistory_w1==1 & (is.na(unemployment$b_unemp_dur) | is.na(unemployment$c_unemp_dur) | is.na(unemployment$d_unemp_dur))] <- NA # if asked the module in wave 1 but either wave 2, wave 3 or wave 4 annual event history module is missing




temp <- read_dta(file=paste0(path, "ukhls_w5/e_indresp.dta")) %>%
  dplyr::select(pidp, e_indscus_lw)

xwavedat_ex <- read_dta(file=paste0(path, "ukhls_wx/xwavedat.dta")) %>% dplyr::select(pidp, psu, strata)

unemployment <- left_join(unemployment, temp, by = "pidp")
unemployment <- left_join(unemployment, xwavedat_ex, by = "pidp")

rm(temp)


unemployment$e_indscus_lw[is.na(unemployment$e_indscus_lw)] <- 0

options(survey.lonely.psu="adjust") 

unemployment %>%
  srvyr::as_survey_design(ids=psu, strata=strata, weights=e_indscus_lw) %>%
  srvyr::summarise(Mean_exposure= srvyr::survey_mean(totalexposure, na.rm = T, vartype = NULL), Min_exposure=min(totalexposure, na.rm = T),
                   Max_exposure=max(totalexposure, na.rm = T), prop_NA = sum(is.na(totalexposure))/ n()) %>%
  srvyr::ungroup()



unemployment$unemployment_present <- 1
save(unemployment, file = "unemployment.Rda") 


unemployment <- left_join(annualevent, a_empstat, by = "pidp")
unemployment <- left_join(unemployment, e_empstat, by = "pidp")

# for those individuals that were in FT education in wave 1, they weren't asked the lifetime empl.status histpry module. for them, we can just add up b_unemp_dur", "c_unemp_dur", "d_unemp_dur if they weren't in the wave 5 module
w1 <- read_dta(paste0(path, "ukhls_w1/a_indresp.dta")) %>%  dplyr::select(pidp, a_jbstat, a_school, a_fenow)
unemployment <- left_join(unemployment, w1, by = "pidp")



unemployment$summedpast <-  rowSums(unemployment[c("a_total_UE_months", "b_unemp_dur", "c_unemp_dur", "d_unemp_dur")], na.rm = TRUE)


# defining the total past exposure to unemployment in months
unemployment$totalexposure <- ifelse(unemployment$emphistory_w5==1 & is.na(unemployment$emphistory_w1), unemployment$e_total_UE_months,
                                     ifelse(unemployment$emphistory_w1==1 & is.na(unemployment$emphistory_w5), unemployment$summedpast,
                                            ifelse(unemployment$emphistory_w1==1 & !is.na(unemployment$emphistory_w5), unemployment$summedpast,
                                                   ifelse((unemployment$a_school==3 | unemployment$a_fenow==3) & unemployment$emphistory_w5!=1, unemployment$summedpast, 
                                                          NA))))
unemployment$totalexposure[unemployment$emphistory_w5!=1 & unemployment$emphistory_w1!=1 & unemployment$a_school!=3 & unemployment$a_fenow!=3] <- NA # if not in either the wave 1 and wave 5 module, and not in FT education in wave 1
unemployment$totalexposure[unemployment$emphistory_w1==1 & (is.na(unemployment$b_unemp_dur) | is.na(unemployment$c_unemp_dur) | is.na(unemployment$d_unemp_dur))] <- NA # if asked the module in wave 1 but either wave 2, wave 3 or wave 4 annual event history module is missing



temp <- read_dta(file=paste0(path, "ukhls_w5/e_indresp.dta")) %>%
  dplyr::select(pidp, e_indscus_lw)

xwavedat_ex <- read_dta(file=paste0(path, "ukhls_wx/xwavedat.dta")) %>% dplyr::select(pidp, psu, strata, feend_dv)

unemployment <- left_join(unemployment, temp, by = "pidp")
unemployment <- left_join(unemployment, xwavedat_ex, by = "pidp")

rm(temp)

svy_indresp <- svydesign(id=~d_psu, strata=~d_strata, weights=~d_indscus_lw, data=unemployment) 
options(survey.lonely.psu="adjust") 
unemployment$e_indscus_lw[is.na(unemployment$e_indscus_lw)] <- 0

unemployment %>%
  srvyr::as_survey_design(ids=psu, strata=strata, weights=e_indscus_lw) %>%
  srvyr::summarise(Mean_exposure= srvyr::survey_mean(totalexposure, na.rm = T, vartype = NULL), Min_exposure=min(totalexposure, na.rm = T),
                   Max_exposure=max(totalexposure, na.rm = T), prop_NA = sum(is.na(totalexposure))/ n()) %>%
  srvyr::ungroup()




interview <- read_dta(file=paste0(path, "ukhls_w4/d_indresp.dta")) %>%
  dplyr::select(pidp, d_intdatd_dv, d_intdatm_dv, d_intdaty_dv, d_age_dv)
unemployment <- left_join(unemployment, interview, by = "pidp")
rm(interview)

unemployment$intdate_wave4 <- base::as.Date(with(unemployment,paste(unemployment$d_intdaty_dv, unemployment$d_intdatm_dv, unemployment$d_intdaty_dv, sep="-")),"%Y-%m-%d")



unemployment$startc <- if_else(!is.na(unemployment$career_start), unemployment$career_start,
                               unemployment$career_start_w5)
class(unemployment$startc)                          

unemployment$tempcareerlength <- lubridate::interval(unemployment$startc, unemployment$intdate_wave4) %/% months(1)

unemployment$feend_dv[unemployment$feend_dv<0] <- NA

unemployment$careerlength <- if_else(!is.na(unemployment$tempcareerlength), unemployment$tempcareerlength,
                                         if_else((unemployment$a_school==3 | unemployment$a_fenow==3) & unemployment$emphistory_w5!=1,
(unemployment$d_age_dv-unemployment$feend_dv)*12, NA))

unemployment$percentage_ue <- unemployment$totalexposure/unemployment$careerlength*100

unemployment$unemployment_present <- 1
save(unemployment, file = "unemployment.Rda") 





