#compare players
#read in all players with "Mid", "Defender" (or key defender), "Forward" (or key forward), "Ruck"


# 2024 season -------------------------------------------------------------

library(fitzRoy)

season_24_all_SC <- fetch_player_stats(season = 2024, comp = "AFLM", source = "footywire")

season_24_all_SC <- season_24_all_SC %>% select(-Venue, -Opposition, -Status, -Match_id, -GA, -CP, -UP, -ED, -DE, -CM, -MI5, -One.Percenters, -BO, -TOG, -K, -HB, -D, -M, -G, -B, -T, -HO, -I50, -CL, -CG, -R50, -FF, -FA, -AF, -CCL, -SCL, -SI, -MG, -TO, -ITC, -T5)

season_24_position <- fetch_player_details_afl(season = 2024, comp = "AFLM")

season_24_position$Player <- sprintf("%s %s", season_24_position$firstName, season_24_position$surname)

season_24_position <- season_24_position %>% select(-id, -providerId, -dateOfBirth, -heightInCm, -weightInKg, -draftYear, -draftType, -draftPosition, -recruitedFrom, -debutYear, -data_accessed, -firstName, -surname)

season_24_all <- merge(season_24_all_SC, season_24_position, by="Player")

defenders_24 <- season_24_all %>% filter(grepl("DEFENDER", position))

# 2025 season -------------------------------------------------------------

season_25_all <- fetch_player
