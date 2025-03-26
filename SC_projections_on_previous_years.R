#compare players
#read in all players with "Mid", "Defender" (or key defender), "Forward" (or key forward), "Ruck"

install.packages("ggplot2")
install.packages("gtools")
library(ggplot2)
library(fitzRoy)
library(gtools)

# 2024 season -------------------------------------------------------------
##pull data
season_24_all_SC <- fetch_player_stats(season = 2024, comp = "AFLM", source = "footywire")

season_24_all_SC <- season_24_all_SC %>% select(-Venue, -Opposition, -Status, -Match_id, -GA, -CP, -UP, -ED, -DE, -CM, -MI5, -One.Percenters, -BO, -TOG, -K, -HB, -D, -M, -G, -B, -T, -HO, -I50, -CL, -CG, -R50, -FF, -FA, -AF, -CCL, -SCL, -SI, -MG, -TO, -ITC, -T5)

season_24_position <- fetch_player_details_afl(season = 2024, comp = "AFLM")

season_24_position$Player <- sprintf("%s %s", season_24_position$firstName, season_24_position$surname)

season_24_position <- season_24_position %>% select(-id, -providerId, -dateOfBirth, -heightInCm, -weightInKg, -draftYear, -draftType, -draftPosition, -recruitedFrom, -debutYear, -data_accessed, -firstName, -surname)

season_24_all <- merge(season_24_all_SC, season_24_position, by="Player")

season_24_all <- season_24_all %>%
  arrange(Player, as.Date(Date, format="%Y-%m-%d"))

defenders_24 <- season_24_all %>% filter(grepl("DEFENDER", position))
mids_24 <- season_24_all %>% filter(grepl("MIDFIELDER", position))
forwards_24 <- season_24_all %>% filter(grepl("FORWARD", position))
rucks_24 <- season_24_all %>% filter(grepl("RUCK", position))

##plot data; SC points across year
### 5 increase across time

forwards_pct_changes24 <- forwards_24 %>%
  mutate(
    pct_change = (SC - lag(SC)) / lag(SC) * 100
  )
ggplot(forwards_pct_changes24, aes(x = Date, y = pct_change, colour = Player, group = Player)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ Team, scales = "free_y")

ggplot(forwards_pct_changes24 %>% filter(Team == "West Coast"), 
       aes(x = Date, y = pct_change, colour = Player, group = Player)) +
  geom_line() +  
  geom_point() +  
  theme_minimal() +  
  labs(title = "Percentage Change for West Coast Forwards Across '24 Season",
       x = "Round", 
       y = "Percentage Change") +  
  theme(legend.position = "right")


# 2025 season -------------------------------------------------------------

season_25_all <- fetch_player
