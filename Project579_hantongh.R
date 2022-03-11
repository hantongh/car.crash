setwd("C:/Users/second/Desktop/BIOST 579/Project/Data")

### Import data
# Person data
# Age, VehicleSeizure, AlcoholSuspected, AlcoholTest, AlcoholResultType, AirbagSwitch, AirbagDeployed, 
# Ejection, Gender, Race, Injury, Protection, Trapped, PersonType, VisionObstruction, 
# ContributingCircumstance1, ContributingCircumstance2, ContributingCircumstance3, VehicleType, crash_date
person <- read.csv("C:/Users/second/Desktop/BIOST 579/Project/Data/Persons_Involved_in_Crashes.csv")
summary(person)

# Location data
# LocationRelationToRoad, LocationInNearIndicator, LocationCity, LocationRoadNameOn, LocationRampIndicator,
# LocationFeetFromRoad, LocationMilesFromRoad, LocationDirectionFromRoad, LocationAtFromIndicator, 
# LocationRoadNameAt, LocationDirectionToRoad, LocationRoadNameTo, FirstHarmfulEvent, MostHarmfulEvent,
# RoadClassification, RoadFeature, TrafficControlType, WeatherCondition1, WeatherCondition2, 
# WeatherContributedToCrash, UpdateDate, Crash_Date_Day, Crash_Date_DOW, Crash_Date_DOW_Num, 
# Crash_Date_Hour, Crash_Date_Month, Crash_Date_Month_Num, Crash_Date_Year, 
# drivers, passengers, pedestrians, pedalcyclists, other_person_type, unknown_person_type,
# killed, type_a_injury, type_b_injury, type_c_injury, no_injury, injury_unknown, 
# LocationLatitude, LocationLongitude
location <- read.csv("C:/Users/second/Desktop/BIOST 579/Project/Data/Reported_Crash_Locations.csv")



### Keep useful variables
## Person data
# 1. Find accidents in Raleigh & in year 2015 - 2020
city_name <- as.data.frame(levels(person$City))
# Select out cities with names like Raleigh (NC), considering typo
like.raleigh <- as.vector(city_name[c(3196, 3203:3205, 3207, 3209, 3211:3238),])
# Check if these records have duplicate keys
person_ral <- person[which(person$City %in% like.raleigh),]
check <- unique(person_ral[,c("ï..key_crash","City")])
sum(duplicated(check)) # equals 0 indicates no same key for two different cities
# Select from year 2015 - 2020
person_ral <- person_ral[which(substr(as.character(person_ral$crash_date),1,4) %in% c(2015:2020)),]

# 2. Delete unuseful columns
colnames(person_ral)[1] <- "key_crash"
# Keep key, age, alcohol-related, 
person_ral_var <- person_ral[,c(1,8,10:12,14,16:19,21,23,27)]
person_ral_var[person_ral_var==""] <- NA
person_ral_var <- droplevels(person_ral_var)

summary(person_ral_var)

# 3. Count cases with no NAs
# na_count <- apply(person_ral_var, 1, function(x) sum(is.na(x)))
# person_nona <- person_ral_var[na_count==0,]
# length(unique(person_nona$key_crash)) # 99946

# 4. Output file
write.csv(person_ral_var, "C:/Users/second/Desktop/BIOST 579/Project/Data/Persons_Involved_in_Crashes_2015-2020.csv",
          row.names = F)

## Location data
# 1. Find accidents in year 2015 - 2020
location_1520 <- location[which(location$Crash_Date_Year %in% c(2015:2020)),]

# 2. Delete unuseful rows
location_var <- location_1520[,c(4:5, 24, 26, 31, 35:46)]
summary(location_var)

# 3. Count cases with no NAs
# na_count <- apply(location_var, 1, function(x) sum(is.na(x)))
# location_nona <- location_var[na_count==0,]

nrow(location_var[na_count==0,]) # 137477

### Descriptive statistics
summary(person_nona[,c(-1,-3,-4,-13)])
summary(location_nona[,c(-1,-2)])

# 4. Output file
write.csv(location_var, "C:/Users/second/Desktop/BIOST 579/Project/Data/Reported_Crash_Locations_2015-2020.csv",
          row.names = F)


### Merge data
## Person-wise to case-wise
setwd("C:/Users/second/Desktop/BIOST 579/Project/Data")
person_ral_var <- read.csv("Persons_Involved_in_Crashes_2015-2020.csv")
location_ral_var <- read.csv("Reported_Crash_Locations_2015-2020.csv")

summary(person_ral_var)

# Unknown person type
key_unknown <- unique(person_ral_var[which(person_ral_var$PersonType=="Unknown"),"key_crash"])

# Drivers with missing age and alcohol suspected
drivers <- person_ral_var[which(person_ral_var$PersonType=="Driver"),]
driversna <- drivers[!complete.cases(drivers[,c(2,3)]),]
key_miss <- unique(driversna)$key_crash

# Delete these cases
person_clean <- droplevels(person_ral_var[-which(person_ral_var$key_crash %in% c(key_unknown, key_miss)),])
key_person <- unique(person_clean$key_crash)

# Find corresponding cases in location data set
location_clean <- location_ral_var[which(location_ral_var$key_crash %in% key_person),]
keys <- unique(location_clean$key_crash)
person_clean <- person_clean[which(person_clean$key_crash %in% keys),]

# Order data set according to key
person_clean <- person_clean[order(person_clean$key_crash),]
location_clean <- location_clean[order(location_clean$key_crash),]

# Summary
summary(person_clean)
summary(location_clean)


library(dplyr)
library(stringr)
person_subset <- person_clean[c(1:20),]

person_summary <- person_clean %>%
  mutate(TeenDriver = ifelse(PersonType == "Driver" & Age<20, 1, 0),
         SeniorDriver = ifelse(PersonType == "Driver" & Age>64, 1, 0),
         IntoxicatedSuspected = ifelse(PersonType == "Driver" &
                                         word(AlcoholSuspected,1)=="Yes",1,
                                       ifelse(PersonType == "Driver" &
                                                AlcoholSuspected=="Unknown",NA,0)),
         ChildPassenger = ifelse(PersonType == "Passenger" & Age<13 ,1,0),
         NoAirbag = ifelse(PersonType %in% c("Driver", "Passenger") &
                                               AirbagDeployed == "No Air Bag(s)",1,
                           ifelse(PersonType %in% c("Driver", "Passenger") &
                                    AirbagDeployed == "Unknown", NA, 0)),
         NoSeatBelt = ifelse(PersonType %in% c("Driver", "Passenger") &
                               Protection == "None used",1,
                             ifelse(PersonType %in% c("Driver", "Passenger") & 
                                      Protection == "Unable to determine",NA,0)))

person_case_sum_na <- aggregate(x=person_summary[,c(14:19)], 
                                by = list(person_summary$key_crash), FUN = sum)
person_case_sum <- aggregate(x=person_summary[,c(14:19)], 
                             by = list(person_summary$key_crash), function(x) sum(x,na.rm = T))


for (i in c(1:nrow(person_case_sum))) {
  if (sum(is.na(person_case_sum_na[i,]))==0){
    next
  } else {
    for (j in c(2:ncol(person_case_sum))){
      if (person_case_sum[i,j]>0){
        person_case_sum_na[i,j]=1
      }
    }
  }
}

summary(person_case_sum_na)
person_sum <- person_case_sum_na[,-1]
person_sum[person_sum>0] <- 1
person_sum$key_crash <- person_case_sum_na$Group.1


case.dat <- merge(location_clean, person_sum, by="key_crash")

# Clean out unecessary variables and alter some variables
case.dat <- case.dat[,c(1,3,5,12,18:23)]
case.dat[87300,4] <- 1

case.dat[is.na(case.dat$killed),"killed"] <- 0
case.dat[case.dat$killed>0,"killed"] <- 1


case.dat <- case.dat %>% 
  mutate(TimeOfDay=cut(Crash_Date_Hour, 
                      breaks=c(-1, 5, 12, 17,23), labels=c("Night","Morning","Afternoon","Night")))


summary(case.dat)
na_count <- apply(case.dat, 1, function(x) sum(is.na(x))==0)
case.dat <- case.dat[na_count,c(1:2,11,4:10)]
sum(case.dat$killed)


# Write out case file
write.csv(case.dat,"Crash_Data_2015-2020.csv", row.names = F)


### Table 1
case.dat.table <- case.dat
case.dat.table$killed <- factor(case.dat.table$killed, levels = c(0,1), 
                                labels = c("Non-fatal","Fatal"))
case.dat.table$TeenDriver <- factor(case.dat.table$TeenDriver, levels = c(0,1), 
                                labels = c("No","Yes"))
case.dat.table$SeniorDriver <- factor(case.dat.table$SeniorDriver, levels = c(0,1), 
                                    labels = c("No","Yes"))
case.dat.table$IntoxicatedSuspected <- factor(case.dat.table$IntoxicatedSuspected, levels = c(0,1), 
                                    labels = c("No","Yes"))
case.dat.table$ChildPassenger <- factor(case.dat.table$ChildPassenger, levels = c(0,1), 
                                    labels = c("No","Yes"))
case.dat.table$NoAirbag <- factor(case.dat.table$NoAirbag, levels = c(0,1), 
                                        labels = c("No","Yes"))
case.dat.table$NoSeatBelt <- factor(case.dat.table$NoSeatBelt, levels = c(0,1), 
                                        labels = c("No","Yes"))

label(case.dat.table$WeatherCondition1) <- "WeatherCondition"

library(table1)
table1(~ WeatherCondition1 + TimeOfDay + TeenDriver + SeniorDriver +
         IntoxicatedSuspected + ChildPassenger + 
         NoAirbag + NoSeatBelt| killed, data=case.dat.table)



### Analysis
setwd("C:/Users/second/Desktop/BIOST 579/Project/Data")
case.dat <- read.csv("Crash_Data_2015-2020.csv")

# Model 1
mod1 <- glm(killed ~ relevel(WeatherCondition1, "Clear") + TimeOfDay, data=case.dat, 
            family=binomial)
summary(mod1)

mod1_stat <- mod1 %>% coef %>% exp %>% round(3)
mod1_conf <- mod1 %>% confint %>% exp %>% round(3)
mod1.summary <- cbind(mod1_stat, mod1_conf,
                      round(p.adjust(coef(summary(mod1))[,4], method = "BH"),4))

# Model 2
mod2 <- glm(killed ~ TeenDriver + SeniorDriver + IntoxicatedSuspected + ChildPassenger,
            data=case.dat, family=binomial)
summary(mod2)

mod2_stat <- mod2 %>% coef %>% exp %>% round(3)
mod2_conf <- mod2 %>% confint %>% exp %>% round(3)
mod2.summary <- cbind(mod2_stat, mod2_conf,
                      round(p.adjust(coef(summary(mod2))[,4], method = "BH"),4))

# Model 3
mod3 <- glm(killed ~ NoAirbag + NoSeatBelt,
            data=case.dat, family=binomial)
summary(mod3)

mod3_stat <- mod3 %>% coef %>% exp %>% round(3)
mod3_conf <- mod3 %>% confint %>% exp %>% round(3)
mod3.summary <- cbind(mod3_stat, mod3_conf,
                      round(p.adjust(coef(summary(mod3))[,4], method = "BH"),4))

# Model 4
mod4 <- glm(killed ~ relevel(WeatherCondition1, "Clear") + TimeOfDay +
            TeenDriver + SeniorDriver + IntoxicatedSuspected + ChildPassenger +
            NoAirbag + NoSeatBelt,
            data=case.dat, family=binomial)

mod4_stat <- mod4 %>% coef %>% exp %>% round(3)
mod4_conf <- mod4 %>% confint %>% exp %>% round(3)
mod4.summary <- cbind(mod4_stat, mod4_conf,
                      round(p.adjust(coef(summary(mod4))[,4], method = "BH"),4))


### Discussion
# Multicollinearity
car::vif(mod4)

case.num <- case.dat[,c(-1,-4)]
case.num$WeatherCondition1 <- as.numeric(factor(case.num$WeatherCondition1))
case.num$TimeOfDay <- as.numeric(factor(case.num$TimeOfDay))

library(sjPlot)
sjp.corr(case.num)

# Categories with very small event rate
levels(case.dat$WeatherCondition1)

case.dat.supp <- case.dat[which(case.dat$WeatherCondition1 %in% c("Clear", "Rain",
                                                                  "Cloudy", "Fog, smog, smoke")),]
case.dat.supp <- droplevels(case.dat.supp)

# Mod Supp
mod1s <- glm(killed ~ relevel(WeatherCondition1, "Clear") + TimeOfDay, data=case.dat.supp, 
            family=binomial)
summary(mod1s)

mod1s_stat <- mod1s %>% coef %>% exp %>% round(3)
mod1s_conf <- mod1s %>% confint %>% exp %>% round(3)
mod1s.summary <- cbind(mod1s_stat, mod1s_conf,
                      round(p.adjust(coef(summary(mod1s))[,4], method = "BH"),4))

mod4s <- glm(killed ~ relevel(WeatherCondition1, "Clear") + TimeOfDay +
              TeenDriver + SeniorDriver + IntoxicatedSuspected + ChildPassenger +
              NoAirbag + NoSeatBelt,
            data=case.dat.supp, family=binomial)

mod4s_stat <- mod4s %>% coef %>% exp %>% round(3)
mod4s_conf <- mod4s %>% confint %>% exp %>% round(3)
mod4s.summary <- cbind(mod4s_stat, mod4s_conf,
                      round(p.adjust(coef(summary(mod4s))[,4], method = "BH"),4))
