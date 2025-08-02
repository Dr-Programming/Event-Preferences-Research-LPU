
dayscholar_dataSet <- dataSet[dataSet$Type.of.Residence != "Hostler", ]

summary(dayscholar_dataSet) # lentgh = 69

unique_reasons <- unique(unlist(strsplit(as.character(dayscholar_dataSet$What.is.the.reason.not.to.attend.late.night.events.), ", ")))
#for creating the cloumns in the table
for (reason in unique_reasons) {
  dayscholar_dataSet[[reason]] <- NA
}

df_male <- dayscholar_dataSet[dayscholar_dataSet$Gender != "Female", ]

for (a in 1:nrow(dayscholar_dataSet)){
  reasons <- unlist(strsplit(as.character(dayscholar_dataSet[a, "What.is.the.reason.not.to.attend.late.night.events."]), ", "))
  # filling the particular column with entry
  for(reason in reasons){
    dayscholar_dataSet[a, reason] <- "Yes"
  }
}

dayscholar_dataSet[is.na(dayscholar_dataSet)] <- "No"

dayscholar_dataSet <- subset(dayscholar_dataSet, select = -What.is.the.reason.not.to.attend.late.night.events.)

#number of person interested in each event
D_cultural_count <- sum(dayscholar_dataSet$`Cultural Events` == "Yes") #27.53
D_anime_count <- sum(dayscholar_dataSet$`Anime Events` == "Yes") #13.04
D_coke_count <- sum(dayscholar_dataSet$`Coke Studio` == "Yes") #44.92
D_artist_count <- sum(dayscholar_dataSet$`Artist Performance` == "Yes") #20.28
D_esports_count <- sum(dayscholar_dataSet$`E-Sports Tournament/Events` == "Yes") #18.84
D_band_count <- sum(dayscholar_dataSet$`Band Performance` == "Yes") #11.59
D_dj_count <- sum(dayscholar_dataSet$`DJ Nights` == "Yes") #33.33
D_concerts_count <- sum(dayscholar_dataSet$Concerts == "Yes") #31.88
D_edm_count <- sum(dayscholar_dataSet$`Electronic Dance Music(EDM) Night` == "Yes") #13.04


day_yes <- sum(dayscholar_dataSet$Are.you.willing.to.attend.late.night.events. == "Yes") # 65.2
day_no <- sum(dayscholar_dataSet$Are.you.willing.to.attend.late.night.events. == "No") # 34.8


problem_1 <- sum(dayscholar_dataSet$`Strict Parents` == "Yes")
problem_2 <- sum(dayscholar_dataSet$`Lack of Interest` == "Yes")
problem_3 <- sum(dayscholar_dataSet$`Lack of Transport` == "Yes")
problem_4 <- sum(dayscholar_dataSet$`Safety Issue` == "Yes")

male_entries <- dayscholar_dataSet[dayscholar_dataSet$Gender != "Female", ]
summary(male_entries)

female_entries <- dayscholar_dataSet[dayscholar_dataSet$Gender != "Male", ]
summary(female_entries)

male_no <- sum(male_entries$Are.you.willing.to.attend.late.night.events. == "No") # 34
female_no <- sum(female_entries$Are.you.willing.to.attend.late.night.events. == "No") #36.8
df_male_no <- male_entries[male_entries$Are.you.willing.to.attend.late.night.events. == "No",]
df_female_no <- female_entries[female_entries$Are.you.willing.to.attend.late.night.events. == "No", ]
df_lackInterest <- dayscholar_dataSet[dayscholar_dataSet$`Lack of Interest` == "Yes", ]
df_lackInterest_boys <- df_lackInterest[df_lackInterest$Gender != "Female", ]
df_lackInterest_girls <- df_lackInterest[df_lackInterest$Gender != "Male", ]

boy_problem_1 <- sum(df_male_no$`Strict Parents` == "Yes") # 11.76
boy_problem_2 <- sum(df_male_no$`Lack of Interest` == "Yes") # 47.05

boy_problem_3 <- sum(df_male_no$`Lack of Transport` == "Yes") # 35.29
boy_problem_4 <- sum(df_male_no$`Safety Issue` == "Yes") # 35.29

girl_problem_1<- sum(df_female_no$`Strict Parents`=="Yes")# 42.85
girl_problem_2<- sum(df_female_no$ `Lack of Interest`=="Yes")# 14.28
girl_problem_3<- sum(df_female_no$`Lack of Transport`=="Yes")# 71.42
girl_problem_4<- sum(df_female_no$`Safety Issue`=="Yes")# 28.57


####
D_national_artist <- sum(dayscholar_dataSet$What.kind.of.Artist.would.you.like.to.listen.to. == "National Artist")
D_regional_artist <- sum(dayscholar_dataSet$What.kind.of.Artist.would.you.like.to.listen.to. == "Regional Artist")
D_international_artist <- sum(dayscholar_dataSet$What.kind.of.Artist.would.you.like.to.listen.to. == "International Artist")








