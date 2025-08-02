
hostel_dataSet <- dataSet[dataSet$Type.of.Residence != "Day Scholar", ]

hostel_df <- hostel_dataSet[ hostel_dataSet$What.is.your.monthly.Pocket.Money != 50000, ]
summary(hostel_df)
summary(hostel_dataSet) #length = 113

hostel_dataSet <- subset(hostel_dataSet, select = -Are.you.willing.to.attend.late.night.events.)
hostel_dataSet <- subset(hostel_dataSet, select = -What.is.the.reason.not.to.attend.late.night.events.)

#number of person enjoying each event 
H_cultural_count <- sum(hostel_dataSet$`Cultural Events` == "Yes") #34.51
H_anime_count <- sum(hostel_dataSet$`Anime Events` == "Yes") #18.58
H_coke_count <- sum(hostel_dataSet$`Coke Studio` == "Yes") #53.98
H_artist_count <- sum(hostel_dataSet$`Artist Performance` == "Yes") #20.35
H_esports_count <- sum(hostel_dataSet$`E-Sports Tournament/Events` == "Yes") #26.54
H_band_count <- sum(hostel_dataSet$`Band Performance` == "Yes") #24.77
H_dj_count <- sum(hostel_dataSet$`DJ Nights` == "Yes") #41.59
H_concerts_count <- sum(hostel_dataSet$Concerts == "Yes") #47.78
H_edm_count <- sum(hostel_dataSet$`Electronic Dance Music(EDM) Night` == "Yes") #26.54

###
H_national_artist <- sum(hostel_dataSet$What.kind.of.Artist.would.you.like.to.listen.to. == "National Artist")
H_regional_artist <- sum(hostel_dataSet$What.kind.of.Artist.would.you.like.to.listen.to. == "Regional Artist")
H_international_artist <- sum(hostel_dataSet$What.kind.of.Artist.would.you.like.to.listen.to. == "International Artist")






