dataSet <- read.csv("P:/R Studio Projects/Event Preference Survey Responses.csv")
library(tidyr)
library(ggplot2)

#identifing the unique columns
unique_events <- unique(unlist(strsplit(as.character(dataSet$What.kind.of.events.do.you.enjoy.organized.in.LPU.), ", ")))
#for creating the cloumns in the table
for (event in unique_events) {
  dataSet[[event]] <- NA
}

#spliting for each category in one row
for (i in 1:nrow(dataSet)){
  categories <- unlist(strsplit(as.character(dataSet[i, "What.kind.of.events.do.you.enjoy.organized.in.LPU."]), ", "))
  # filling the particular column with entry
  for(category in categories){
    dataSet[i, category] <- "Yes"
  }
}

# for setting the NA in columns to No
dataSet[is.na(dataSet)] <- "No"

#dropping the orginal column
dataSet <- subset(dataSet, select = -What.kind.of.events.do.you.enjoy.organized.in.LPU.)

dataSet <- dataSet[dataSet$Gender != "Others", ]

dataSet <- dataSet[nchar(dataSet$Registration.Number) == 8, ]


summary(dataSet)

# number of person enjoying each event
cultural_count <- sum(dataSet$`Cultural Events` == "Yes")
anime_count <- sum(dataSet$`Anime Events` == "Yes")
coke_count <- sum(dataSet$`Coke Studio` == "Yes")
artist_count <- sum(dataSet$`Artist Performance` == "Yes")
esports_count <- sum(dataSet$`E-Sports Tournament/Events` == "Yes")
band_count <- sum(dataSet$`Band Performance` == "Yes")
dj_count <- sum(dataSet$`DJ Nights` == "Yes")
concerts_count <- sum(dataSet$Concerts == "Yes")
edm_count <- sum(dataSet$`Electronic Dance Music(EDM) Night` == "Yes")

national_artist <- sum(dataSet$What.kind.of.Artist.would.you.like.to.listen.to. == "National Artist")
regional_artist <- sum(dataSet$What.kind.of.Artist.would.you.like.to.listen.to. == "Regional Artist")
international_artist <- sum(dataSet$What.kind.of.Artist.would.you.like.to.listen.to. == "International Artist")


##################################

df_event_count <- data.frame(
                  event_category = c("Cultural Events",
                               "Anime Events",
                               "Coke Studio",
                               "Artist Performance",
                               "E-Sports Events",
                               "Band Performance",
                               "DJ Nights",
                               "Concerts",
                               "EDM Night"),
                  count_value = c(cultural_count,
                            anime_count,
                            coke_count,
                            artist_count,
                            esports_count,
                            band_count,
                            dj_count,
                            concerts_count,
                            edm_count))

#creating the bar chart of number of people intrested in what events.
event_count_barChart <- ggplot(df_event_count, aes(x = event_category, y = count_value,fill = factor(event_category)))+
                                  geom_bar(stat = "identity")+geom_col()
                            
                                  labs(title = "People Intrested in Each Type of Event",x = "Type of Event",y = "No. of People")+
                                    scale_fill_manual()


#######################################################

df_event_count_detail <- data.frame(
                         event_category = c("Cultural Events",
                                            "Anime Events",
                                            "Coke Studio",
                                            "Artist",
                                            "E-Sports Events",
                                            "Band",
                                            "DJ Nights",
                                            "Concerts",
                                            "EDM Night"),
                         hostel_count = c(34.51, 18.58, 53.98, 20.35, 26.54, 24.77, 41.59, 47.78, 26.54),
                         dayscholar_count = c(27.53, 13.04, 44.92, 20.28, 18.84, 11.59, 33.33, 31.88, 13.04))

#creating the bar chart of number of people from Hostel VS Dayscholar intrested in what events.
df_event_count_detail_long <- pivot_longer(df_event_count_detail, cols = c(hostel_count, dayscholar_count), names_to = "variables", values_to = "values")
GROUP_event_count_barChart <- ggplot(df_event_count_detail_long, aes(x = event_category, y = values, fill = variables)) + 
                              geom_bar(stat = "identity", position = "dodge") + 
                              labs(title = "Hostelers & Day-Scholars Intrested in Each Type of Event", x = "Type of event", y = "Percentage of People") + 
                              scale_fill_manual(values = c("hostel_count"="#7abaa1", "dayscholar_count"="#fdbb9f"))

################################################################

df_event_count_gender <- data.frame(
  event_category = c("Cultural Events",
                     "Anime Events",
                     "Coke Studio",
                     "Artist",
                     "E-Sports Events",
                     "Band",
                     "DJ Nights",
                     "Concerts",
                     "EDM Night"),
  male_count = c(27.1, 18.6, 48, 20.1, 29.4, 19.3, 38.7, 37.2, 21.7),
  female_count = c(43.3, 11.3, 56.6, 20.7, 9.4, 20.7, 37.7, 52.8, 20.7))

#creating the bar chart of number of people from Hostel VS Dayscholar intrested in what events.
df_event_count_gender_long <- pivot_longer(df_event_count_gender, cols = c(male_count, female_count), names_to = "variables", values_to = "values")
GROUP_GENDER_event_count_barChart <- ggplot(df_event_count_gender_long, aes(x = event_category, y = values, fill = variables)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Male & Female Intrested in Each Type of Event", x = "Type of event", y = "Percentage of People") + 
  scale_fill_manual(values = c("male_count"="#56578c", "female_count"="pink"))

##############################################################
df_passes_count <- data.frame(
  category <- c("Rs.200-300", "Rs.300-400", "Rs.400-500", "Rs.500+"),
  values <- c(74.72, 11.54, 6.6, 7.14)
)

GROUP_passes_count_piechart <- ggplot(df_passes_count, aes(x="", y=values, fill=category))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  labs(title = "Pass price preference")+
 scale_fill_manual(values = c("Rs.200-300"="#e68798","Rs.300-400"="#ff5978","Rs.400-500"="#ffc786","Rs.500+"="red"))
####################################################################

df_day_attend <- data.frame(
  category <- c("Yes", "No"),
  values <- c(65.2, 34.8)
)

DAY_attend_piechart <- ggplot(df_day_attend, aes(x="", y=values, fill=category))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  labs(title = "Day-Scholars willing to attend late night events")+
  scale_fill_manual(values = c("Yes"="#00a86b","No"="red"))

##################################################################

df_day_problems <- data.frame(
  category <- c("Strict Parents",
                "Lack of Interest",
                "Lack of Transport",
                "Safety Issue"),
  values <- c(problem_1, problem_2, problem_3, problem_4)
)

DAY_problems_barchart <- ggplot(df_day_problems, aes(x=category, y=values))+
                         geom_bar(stat="identity", fill="orange")+
                         labs(title = "Problems faced", x="Type of Problem", y="No. of People")



##########################################################

df_day_girl_attend <- data.frame(
  category <- c("Yes", "No"),
  values <- c(63.2, 36.8)
)

DAY_girl_attend_piechart <- ggplot(df_day_girl_attend, aes(x="", y=values, fill=category))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  labs(title = "Day-Scholars(GIRLS) willing to attend late night events")+
  scale_fill_manual(values = c("Yes"="#00a86b","No"="red"))


df_day_boy_attend <- data.frame(
  category <- c("Yes", "No"),
  values <- c(66, 34)
)

DAY_boy_attend_piechart <- ggplot(df_day_boy_attend, aes(x="", y=values, fill=category))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  labs(title = "Day-Scholars(BOYS) willing to attend late night events")+
  scale_fill_manual(values = c("Yes"="#00a86b","No"="red"))

################################################

df_problem_gender <- data.frame(
  category = c("Safety Issue",
               "Lack of Transport",
               "Lack of Interest",
               "Strict Parents"),
  male_percent = c(35.29, 35.29, 47.05, 11.76),
  female_percent = c(28.57, 71.42, 14.28, 42.85))

#creating the bar chart of number of people from Hostel VS Dayscholar intrested in what events.
df_problem_gender_long <- pivot_longer(df_problem_gender, cols = c(male_percent, female_percent), names_to = "variables", values_to = "values")
GROUP_GENDER_problems_barChart <- ggplot(df_problem_gender_long, aes(x = category, y = values, fill = variables)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Problems Faced by Male & Females during late night events", x = "Type of Problem", y = "Percentage of People") + 
  scale_fill_manual(values = c("male_percent"="#56578c", "female_percent"="pink"))

##############################################


df_day_lackInterest <- data.frame(
  category <- c("Male", "Female"),
  values <- c(88.88, 11.11)
)

DAY_lackInterest_piechart <- ggplot(df_day_lackInterest, aes(x="", y=values, fill=category))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  labs(title = "Lack of Interest in Late-night Events in Male and Female Day-Scholar")+
  scale_fill_manual(values = c("Male"="#56578c", "Female"="pink"))


df_day_boys_lackInterest <- data.frame(
  event_category = c("Cultural Events",
                     "Anime Events",
                     "Coke Studio",
                     "Artist Performance",
                     "E-Sports Events",
                     "Band Performance",
                     "DJ Nights",
                     "Concerts",
                     "EDM Night"),
  count_value = c(12.5,
                  25,
                  25,
                  0,
                  62.5,
                  0,
                  12.5,
                  0,
                  0))


day_boys_lackInterest_barChart <- ggplot(df_day_boys_lackInterest, aes(x = event_category, y = count_value,fill=factor(event_category)))+
  geom_bar(stat = "identity", color="black")+
  labs(title = "People Intrested in Each Type of Event", x = "Type of Event", y = "Percentage of People")


##############################################################

df_artist_general <- data.frame(
  category <- c("National", "International", "Regional"),
  values <- c(64.83, 24.18, 10.99)
)

GENERAL_artist_piechart <- ggplot(df_artist_general, aes(x="", y=values, fill=category))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  labs(title = "Artist Preference of entire population")+
  scale_fill_manual(values = c("International"="pink","National"="skyblue","Regional"="#a567a8"))

df_artist_hostel <- data.frame(
  category <- c("National", "International", "Regional"),
  values <- c(68.14, 23.01, 8.85)
)

HOSTEL_artist_piechart <- ggplot(df_artist_hostel, aes(x="", y=values, fill=category))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  labs(title = "Artist Preference of Hostel Students")+
  scale_fill_manual(values = c("International"="pink","National"="skyblue","Regional"="#a567a8"))

df_artist_day <- data.frame(
  category <- c("National", "International", "Regional"),
  values <- c(59.42, 26.09, 14.49)
)

DAY_artist_piechart <- ggplot(df_artist_day, aes(x="", y=values, fill=category))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  labs(title = "Artist Preference of Day-Scholars")+
  scale_fill_manual(values = c("International"="pink","National"="skyblue","Regional"="#a567a8"))




