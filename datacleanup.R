# Clean up
library(tidyverse)
library(dplyr)
scdf <- read_csv("safecity.csv")
range(scdf$YEAR, na.rm= TRUE) #1990 2018
df2 <- filter(scdf, YEAR >= 2013) 
df3 <- select(df2, "INCIDENT ID", "DATE", "YEAR", "MONTH", "TIME", "LONGITUDE",
              "LATITUDE",
              "Catcalls/Whistles", 
              "Commenting", "Sexual Invites", "Ogling/Facial Expressions/Staring",
              "Taking Pictures", "Indecent Exposure", "Touching /Groping", 
              "Stalking",  "Rape / Sexual Assault", "Chain Snatching" , "Others",
              "VERBAL ABUSE", "PHYSICAL ABUSE", "SERIOUS PHYSICAL ABUSE", 
              "OTHER ABUSE", "City Code", "Area Code", "Taxi", "Car", "Train", 
              "Bus", "Metro", "Auto", "Rail", "Airport", "Road", "Travel", 
              "Passenger", "Rickshaw", "Driver", "Conductor", "Flag Car", 
              "Flag Train", "Flag Bus", "Flag Metro", "Flag Auto", "Flag Rail")
df3 <- select(df2, "INCIDENT ID","DATE", "YEAR", "TIME", "LONGITUDE","LATITUDE",
              "Catcalls/Whistles", 
              "Commenting", "Sexual Invites", "Ogling/Facial Expressions/Staring",
              "Taking Pictures", "Indecent Exposure", "Touching /Groping", 
              "Stalking", "Rape / Sexual Assault", "Chain Snatching" , "Others",
              "VERBAL ABUSE", "PHYSICAL ABUSE", "SERIOUS PHYSICAL ABUSE", 
              "OTHER ABUSE", "City Code", "Area Code", "Taxi", "Car", "Train", 
              "Bus", "Metro", "Auto", "Rail", "Airport", "Road", "Travel", 
              "Passenger", "Rickshaw", "Driver", "Conductor", "Flag Car", 
              "Flag Train", "Flag Bus", "Flag Metro", "Flag Auto", "Flag Rail")
#Rename variables
library(reshape)
df4 <- rename(df3,c('INCIDENT ID'='id', "DATE" = "date", "YEAR" = "year", 
                    "MONTH" = "month",
                    "TIME" = "time", "LONGITUDE" = "X", "LATITUDE" = "Y", 
                    "Catcalls/Whistles"= "catcall", "Commenting" = "comment", 
                    "Sexual Invites" = "sexualinvite",
                    "Ogling/Facial Expressions/Staring" ="staring", 
                    "Taking Pictures"= "takepic",
                    "Indecent Exposure" = "indecentexpo", 
                    "Touching /Groping" = "touch",
                    "Stalking"="stalking", "Rape / Sexual Assault" = "sexassault",
                    "Chain Snatching"= "chainsnatch", "Others"="others", 
                    "VERBAL ABUSE"= "verbalabuse","PHYSICAL ABUSE"= "phyabuse",
                    "SERIOUS PHYSICAL ABUSE" = "seriousphyabuse", 
                    "OTHER ABUSE"= "otherabuse",
                    "City Code" = "citycode", "Area Code" = "areacode", 
                    "Taxi"= "taxi", "Car" ="car", "Train"= "train", 
                    "Bus"= "bus", "Metro"= "metro", "Auto" = "auto", 
                    "Rail" = "rail", "Airport" = "airport", "Road" = "road", 
                    "Travel"= "travel", "Passenger" = "passenger", 
                    "Rickshaw" = "rickshaw", "Driver" = "driver", 
                    "Conductor" = "conductor","Flag Car" = "flagcar", 
                    "Flag Train" = "flagtrain", "Flag Bus" = "flagbus", 
                    "Flag Metro" = "flagmetro", "Flag Auto" = "flagauto", 
                    "Flag Rail" = "flagrail"))
write_csv(df4, file = "SafeCityClean.csv")


