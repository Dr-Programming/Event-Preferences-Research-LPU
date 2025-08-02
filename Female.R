
female_dataSet <- dataSet[dataSet$Gender != "Male", ]
summary(female_dataSet)

# number of person enjoying each event
F_cultural_count <- sum(female_dataSet$`Cultural Events` == "Yes") #43.3
F_anime_count <- sum(female_dataSet$`Anime Events` == "Yes") #11.3
F_coke_count <- sum(female_dataSet$`Coke Studio` == "Yes") #56.6
F_artist_count <- sum(female_dataSet$`Artist Performance` == "Yes") #20.7
F_esports_count <- sum(female_dataSet$`E-Sports Tournament/Events` == "Yes") #9.4
F_band_count <- sum(female_dataSet$`Band Performance` == "Yes") #20.7
F_dj_count <- sum(female_dataSet$`DJ Nights` == "Yes") #37.7
F_concerts_count <- sum(female_dataSet$Concerts == "Yes") #52.8
F_edm_count <- sum(female_dataSet$`Electronic Dance Music(EDM) Night` == "Yes") #20.7