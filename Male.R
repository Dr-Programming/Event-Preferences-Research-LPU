
male_dataSet <- dataSet[dataSet$Gender != "Female", ]
summary(male_dataSet)

# number of person enjoying each event
M_cultural_count <- sum(male_dataSet$`Cultural Events` == "Yes") #27.1
M_anime_count <- sum(male_dataSet$`Anime Events` == "Yes") #18.6
M_coke_count <- sum(male_dataSet$`Coke Studio` == "Yes") #48
M_artist_count <- sum(male_dataSet$`Artist Performance` == "Yes") #20.1
M_esports_count <- sum(male_dataSet$`E-Sports Tournament/Events` == "Yes") #29.4
M_band_count <- sum(male_dataSet$`Band Performance` == "Yes") #19.3
M_dj_count <- sum(male_dataSet$`DJ Nights` == "Yes") #38.7
M_concerts_count <- sum(male_dataSet$Concerts == "Yes") #37.2
M_edm_count <- sum(male_dataSet$`Electronic Dance Music(EDM) Night` == "Yes") #21.7



