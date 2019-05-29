library(tidyverse)
library(scales)

theme_set(theme_minimal())


play_by_play <- read_csv("PlayByPlay_2019/Events_2019.csv")
players_2019 <- read_csv("PlayByPlay_2019/Players_2019.csv")


# get the list of event types to see which would relate to shooting 
play_by_play %>% count(EventType) %>% pull(EventType)

# there are some shooting variables that can probably be condensed - tip ins and dunks
paint_attempts_made <- c("made2_dunk", "made2_lay") 
paint_attempts_missed <- c("miss2_dunk", "miss2_lay") 

# create variables for field goals made, and also field goals attempted (which includes the sum of FGs made and FGs missed)
FGM <- c("made2_dunk", "made2_jump", "made2_lay",  "made2_tip",  "made3_jump")
FGA <- c(FGM, "miss2_dunk", "miss2_jump" ,"miss2_lay",  "miss2_tip",  "miss3_jump")



# Rework DF so only shots are included and whatever lead to the shot --------

shooting_analysis <- play_by_play %>% 
  mutate(GameID = paste(Season, DayNum, WTeamID, LTeamID, sep = "_")) %>% 
  group_by(GameID, ElapsedSeconds) %>% 
  mutate(EventType2 = lag(EventType),
         EventPlayeID2 = lag(EventPlayerID))

rm(play_by_play);gc()

shooting_analysis <- shooting_analysis %>% 
  mutate(FGVariable = ifelse(EventType %in% FGA | EventType2 %in% FGA, "Yes", "No")) %>% 
  filter(FGVariable == "Yes") #%>% 
  # filter(!is.na(EventType2))


# create a variable for if the shot was made, but then the second event was also a made shot
shooting_analysis <- shooting_analysis %>% 
  mutate(Alert = ifelse(EventType %in% FGM & EventType2 %in% FGM, "Alert", "OK"))

shooting_analysis$EventType2[is.na(shooting_analysis$EventType2)] <- "no_second_event"


# create a variable for if there was an assist on the FGM:
shooting_analysis <- shooting_analysis %>% 
  mutate(AssistedFGM = ifelse(EventType %in% FGM & EventType2 == "assist", "Assisted", 
                              ifelse(EventType %in% FGM & EventType2 != "assist", "Solo", 
                                     ifelse(EventType %in% FGM & EventType2 == "no_second_event", "Solo", "None"))))


# add player names
shooting_analysis <- shooting_analysis %>% 
  left_join(players_2019 %>% select(PlayerID, PlayerName), by = c("EventPlayerID" = "PlayerID")) %>% 
  left_join(players_2019 %>% select(PlayerID, PlayerName), by = c("EventPlayerID2" = "PlayerID")) 


league_averages <- shooting_analysis %>% 
  filter(AssistedFGM != "None") %>% 
  ggplot(aes(x=EventType, fill = AssistedFGM)) +
  geom_bar(stat = "count", position = "fill") +
  scale_fill_manual(values = c("orange", "steelblue")) +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  ggtitle("NCAA AVERAGES")


ja_morrant <- shooting_analysis %>% 
  filter(AssistedFGM != "None") %>% 
  filter(EventPlayerID == 656216) %>% 
  ggplot(aes(x=EventType, fill = AssistedFGM)) +
  geom_bar(stat = "count", position = "fill") +
  scale_fill_manual(values = c("orange", "steelblue")) +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  ggtitle("JA MORRANT GETS HIS OWN BUCKETS")


gridExtra::grid.arrange(league_averages, ja_morrant)


threes_made <- shooting_analysis %>% 
  filter(EventType == 'made3_jump') %>% 
  group_by(EventPlayerID, AssistedFGM) %>%
  summarise(n = n()) %>% 
  mutate(PercentAssisted = n / sum(n)) %>% ungroup()


threes_made %>% 
  filter(AssistedFGM == "Solo") %>% 
  arrange(desc(n)) %>% head(10)



percent_assisted <- shooting_analysis %>% 
  filter(AssistedFGM != "None", Alert == "OK") %>% 
  group_by(EventPlayerID, AssistedFGM) %>% 
  summarise(n_FGM = n()) %>% 
  mutate(PercentAssisted = n_FGM / sum(n_FGM)) %>% 
  group_by(EventPlayerID) %>% 
  mutate(total_made = sum(n_FGM)) %>% 
  left_join(players_2019 %>% select(PlayerID, PlayerName), by = c("EventPlayerID" = "PlayerID")) 

percent_assisted %>% 
  filter(AssistedFGM == "Solo",
         total_made > 50) %>% 
  arrange(desc(PercentAssisted)) %>% head(10)


percent_assisted %>% 
  filter(AssistedFGM == "Solo",
         total_made > 50) %>% 
  arrange(desc(PercentAssisted)) %>%
  ggplot(aes(x= PercentAssisted)) + geom_histogram()


percent_assisted %>% 
  filter(AssistedFGM == "Solo", total_made > 50) %>% 
  arrange(desc(PercentAssisted)) %>%
  ggplot(aes(x= PercentAssisted, y= total_made)) + geom_point() +geom_smooth(method = "lm")


percent_assisted %>% 
  distinct(EventPlayerID, .keep_all = T) %>% 
  ggplot(aes(x= total_made)) + geom_histogram()


percent_assisted %>% 
  filter(str_detect(PlayerName, "MORANT"))

percent_assisted %>% 
  filter(AssistedFGM == "Solo") %>% 
  filter(PercentAssisted >= 0.75 & total_made >200)








different_shot_types <- shooting_analysis %>% 
  filter(AssistedFGM != "None") %>%
  group_by(EventPlayerID, EventType, AssistedFGM) %>% 
  summarise(n_shots = n()) %>%
  mutate(proportion = n_shots / sum(n_shots)) %>% 
  left_join(players_2019 %>% select(PlayerID, PlayerName), by = c("EventPlayerID" = "PlayerID")) 




different_shot_types %>% 
  filter(EventType == "made3_jump") %>% 
  arrange(desc(n_shots)) %>% View()






