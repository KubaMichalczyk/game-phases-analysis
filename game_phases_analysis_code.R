library(tidyverse)
library(lubridate)
library(rms)

source('strata_pitch_plot.R')
# loading my xG model
load("xGmodel.rda")

# laoding data
chances <- read_csv('chances_from_2017-07-01.csv', 
                    col_types = cols(icon = col_factor(levels = NULL),
                                     chanceRating = col_factor(levels = NULL),
                                     type = col_factor(levels = NULL),
                                     time = col_character(),
                                     location_x = col_double(),
                                     location_y = col_double(),
                                     bodyPart = col_factor(levels = NULL),
                                     shotQuality = col_factor(levels = 0:5),
                                     defPressure = col_factor(levels = 0:5),
                                     numDefPlayers = col_factor(levels = 0:11),
                                     numAttPlayers = col_factor(levels = 0:11),
                                     outcome = col_factor(levels = NULL),
                                     primaryType = col_factor(levels = NULL),
                                     secondaryType = col_factor(levels = NULL),
                                     type_of_attack = col_factor(levels = NULL),
                                     primaryLocation_x = col_double(),
                                     primaryLocation_y = col_double()))

chances <- chances %>% rename(id = X1)

chances %>% glimpse()

# Data preparation ----
# Creating variables that my model uses
chances <- chances %>%
  mutate(dang_moment = (type == 'Dangerous Moment'),
         type_of_attack = case_when(
           bodyPart == 'Foot' & primaryType == 'Open Play Pass' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Head' & primaryType == 'Open Play Pass' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Other' & primaryType == 'Open Play Pass' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Foot' & primaryType == 'Cross Low' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Head' & primaryType == 'Cross Low' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Other' & primaryType == 'Cross Low' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Foot' & primaryType == 'Cross High' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Head' & primaryType == 'Cross High' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Other' & primaryType == 'Cross High' ~ paste(bodyPart, primaryType, sep = '_'),
           primaryType == 'Corner' ~ 'Corner',
           primaryType == 'Free Kick' ~ 'Free Kick',
           primaryType %in% c('Shot (Opposition Rebound)', 'Shot (Deflection)', 'Shot (Woodwork Rebound)') ~
             'Rebound',
           type == 'Direct Free-Kick' ~ 'Direct Free-Kick',
           TRUE ~ 'Other not assisted'
         )
  )

chances <- chances %>%
  mutate(goal = (icon == 'goal'),
         dist = sqrt(location_x^2+location_y^2)/sqrt(136^2 + 420^2),
         angle = abs(atan2(location_x, location_y)/(pi/2)))

chances <- chances %>% 
  separate(time, c('min', 'sec'), sep = ":", remove = FALSE) %>% 
  mutate(min = as.numeric(min), sec = as.numeric(sec))

chances <- chances %>%
  group_by(gsm_id) %>%
  arrange(min, sec, .by_group = TRUE) %>%
  mutate(hometeam_score = lag(goal * (team == hometeam_team1), default = 0),
         awayteam_score = lag(goal * (team == awayteam_team2), default = 0),
         game_state = ifelse(team == hometeam_team1, hometeam_score - awayteam_score, awayteam_score - hometeam_score)) %>%
  select(-hometeam_score, -awayteam_score) %>%
  ungroup()

# Predict expected goals
chances <- chances %>% mutate(xG = predict(xGmodel, type = 'response', newdata = chances))

# We expect to see some NAs in xG column as my model doesn't predict xG for penalties and own goals.
chances %>% filter(is.na(xG)) %>% count()
chances %>% filter(is.na(xG)) %>% select(icon) %>% unique()
# But it would be a huge loss of information if we decide to exclude these chances from our analysis.
# Therefore, I'll make use of @StrateBet's chanceRating (and their conversion rate as documented) in case 
# of own goals and calculate conversion rate for penalties

chances %>% filter(icon %in% c('penawarded', 'owngoal', 'penmissed')) 
# Here we should notice that penalties are recorded twice - one for penawarded event and one for 
# executing a penalty (recorded as goal/penmissed). Therefore we can exclude penawarded from the dataset
# to avoid calculating xG for a penalty twice.
chances <- chances %>% filter(!icon == 'penawarded')

# To make use out of chanceRating let's tidy it first.
chances %>% ggplot() + geom_bar(aes(chanceRating)) + coord_flip()

# There are some chances with missing values of chanceRating.
# Let's have a look why and then tidy the values to 7 levels.
chances %>% filter(chanceRating == "-") %>% select(icon, type) %>% unique()
# Ok, so these are just penalties missed.
chances <- chances %>% mutate(chanceRating = fct_recode(chanceRating, 'Penalty' = '-'))
chances <- chances %>%
  mutate(chanceRating = case_when(
    str_detect(chanceRating, '[Ss]uperb') ~ 'Superb',
    str_detect(chanceRating, '[Gg]reat') ~ 'Great',
    str_detect(chanceRating, '[Vv]ery[ ]*[Gg]ood*') ~ 'Very good',
    str_detect(chanceRating, '^[Gg]ood') ~ 'Good',
    str_detect(chanceRating, '[Ff]airly') ~ 'Fairly good',
    str_detect(chanceRating, '[Pp]oor') ~ 'Poor',
    chanceRating == 'Penalty' ~ 'Penalty'),
    chanceRating = fct_relevel(chanceRating,
                               c('Poor', 'Fairly good', 'Good', 'Very good', 'Great', 'Superb', 'Penalty')))

chanceRating_cvr <- c('Poor' = 0.03, 'Fairly good' = 0.05, 'Good' = 0.08, 'Very good' = 0.22, 'Great' = 0.43, 'Superb' = 0.83, 'Penalty' = 0.7469)

chances <- chances %>% mutate(xG = case_when(icon == 'owngoal' ~ chanceRating_cvr[chanceRating],
                                             chanceRating == 'Penalty' ~ chanceRating_cvr[chanceRating],
                                             TRUE ~ xG))

# Splitting a game into phases ----
chances %>% filter(between(min, 45, 46)) %>% count()

chances <- chances %>% 
  mutate(phase = case_when(between(min, 0, 14) ~ "1",
                           between(min, 15, 29) ~ "2",
                           between(min, 30, 44) ~ "3",
                           between(min, 45, 59) ~ "4",
                           between(min, 60, 74) ~ "5",
                           min >= 75 ~ "6"),
         phase = factor(phase, levels = c(1, 2, 3, 4, 5, 6)))

# Standarising phase 6 ----
chances <- chances %>% group_by(gsm_id) %>% arrange(desc(min), desc(sec), .by_group = TRUE) %>% 
  mutate(game_duration = max('90:00', first(time, order_by = c(desc(min), desc(sec))))) %>% ungroup()

# In how many cases we would have to assume 90 minutes as a duration of a game?
chances %>% select(gsm_id) %>% unique() %>% count()
chances %>% group_by(gsm_id) %>% summarise(game_duration = first(game_duration)) %>% 
  filter(game_duration == '90:00') %>% count()

chances <- chances %>% 
  mutate(phase6_adj = 15 * 60 /((minute(ms(game_duration)) * 60 + second(ms(game_duration))) - 75 * 60),
         xG = ifelse(phase == '6', xG * phase6_adj, xG)) %>% select(-phase6_adj)

# Finally, let's explore the data. 
# *This code creates only a raw plot (without a tweaks like adding logos etc.)
off_intensity <- chances %>% group_by(team, phase) %>% summarise(team_xG = sum(xG, na.rm = TRUE)) %>%
  mutate(xG_prop = team_xG / sum(team_xG)) %>% rename(xG = team_xG)
off_intensity %>% 
  ggplot() + geom_bar(aes(phase, xG_prop), stat = 'identity', fill = '#133E6B') + facet_wrap(~team) +
  theme_minimal() + ylab('Offensive intensity') + xlab('Phase') +
  theme(strip.text = element_text(color = '#4F5F6B', face = 'bold', size = 10),
        axis.title.x = element_text(color = '#4F5F6B', face = 'bold'),
        axis.title.y = element_text(color = '#4F5F6B', face = 'bold'),
        axis.text.x = element_text(color = '#4F5F6B'),
        axis.text.y = element_text(color = '#4F5F6B')) +
  scale_y_continuous(labels = function(x) paste0(x*100, '%'))

def_intensity <- chances %>% mutate(team_conceded = ifelse(team == hometeam_team1, awayteam_team2, hometeam_team1)) %>% 
  group_by(team_conceded, phase) %>% summarise(team_xGA = sum(xG, na.rm = TRUE)) %>% 
  mutate(xGA_prop = team_xGA/sum(team_xGA)) %>% rename(xGA = team_xGA)
def_intensity %>% 
  ggplot() + geom_bar(aes(phase, xGA_prop), stat = 'identity', fill = '#133E6B') + facet_wrap(~team_conceded) +
  theme_minimal() + ylab('Defensive intensity') + xlab('Phase') + 
  theme(strip.text = element_text(color = '#4F5F6B', face = 'bold', size = 10),
        axis.title.x = element_text(color = '#4F5F6B', face = 'bold'),
        axis.title.y = element_text(color = '#4F5F6B', face = 'bold'),
        axis.text.x = element_text(color = '#4F5F6B'),
        axis.text.y = element_text(color = '#4F5F6B')) +   
  scale_y_continuous(labels = function(x) paste0(x*100, '%'))

# Creating the names for facets.
phase_names <- paste0('Phase ', levels(chances$phase))
names(phase_names) <- levels(chances$phase)

intensity <- left_join(off_intensity, def_intensity, by = c("team" = "team_conceded", "phase" = "phase"))
intensity %>% ggplot() + geom_text(aes(xG_prop, xGA_prop, label = team, color = team), size = 3) + 
  facet_wrap(~phase, labeller = as_labeller(phase_names)) + 
  scale_y_continuous(labels = function(x) paste0(x*100, '%')) + 
  scale_x_continuous(labels = function(x) paste0(x*100, '%')) +
  xlab('xG percentage')  + ylab('xGA percentage') + guides(color = FALSE)

data_plot <- chances %>% arrange(kickoffDate) %>% 
  filter(hometeam_team1 == 'Real Madrid' | awayteam_team2 == 'Real Madrid') %>% 
  mutate(team_conceded = ifelse(team == hometeam_team1, awayteam_team2, hometeam_team1)) %>%
  mutate(., game_no = group_indices(., kickoffDate)) %>% 
  mutate(half = fct_collapse(phase, '1' = c('1', '2', '3'), '2' = c('4', '5', '6'))) %>% 
  group_by(game_no, half) %>% 
  summarise(opposite_team = first(ifelse(hometeam_team1 == 'Real Madrid', awayteam_team2, hometeam_team1)), goals = sum(goal * (team == 'Real Madrid')), goals_conceded = sum(goal * (!team == 'Real Madrid')), team_xG = sum(xG * (team == 'Real Madrid')), team_xGA = sum(xG * (!team == 'Real Madrid'))) %>% 
  group_by(half) %>% mutate(agg_goals = cumsum(goals), agg_goals_conceded = cumsum(goals_conceded),
                            agg_team_xG = cumsum(team_xG), agg_team_xGA = cumsum(team_xGA)) 

data_plot_long <- data_plot %>% gather(key, value, goals, goals_conceded, team_xG, team_xGA, agg_goals, agg_goals_conceded, agg_team_xG, agg_team_xGA)

x_labels <- data_plot$opposite_team
names(x_labels) <- data_plot$game_no
x_labels <- x_labels[c(TRUE, FALSE)]

data_plot_long %>% filter(key %in% c('agg_goals', 'agg_goals_conceded', 'agg_team_xG', 'agg_team_xGA')) %>% 
  mutate(against = factor(ifelse(key %in% c('agg_goals_conceded', 'agg_team_xGA'), 1, 0)),
         expected = factor(ifelse(key %in% c('agg_team_xG', 'agg_team_xGA'), 1, 0))) %>% 
  ggplot() + facet_grid(~against, labeller = as_labeller(c('0' = 'Created', '1' = 'Conceded'))) +
  geom_step(aes(factor(game_no), value, color = half, linetype = expected, size = expected,
                group = interaction(half, key))) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = .4, hjust = 1),
        axis.title.y = element_text(size = 10, face = 'bold'),
        strip.text = element_text(color = '#4F5F6B', face = 'bold', size = 14)) +
  scale_x_discrete(labels = x_labels) + xlab(NULL) + ylab(NULL) +
  scale_linetype_manual(values = c('1' = 'solid', '0' = 'dashed'),
                        labels = c('Actual goals', 'Expected goals'))  + 
  scale_size_manual(values = c('1' = 1, '0' = .5)) + guides(size = FALSE) +
  scale_color_discrete(labels = c('1st half', '2nd half')) +
  labs(linetype = NULL, color = NULL)

data_plot <- chances %>% arrange(kickoffDate) %>% 
  filter(hometeam_team1 == 'Barcelona' | awayteam_team2 == 'Barcelona') %>% 
  mutate(team_conceded = ifelse(team == hometeam_team1, awayteam_team2, hometeam_team1)) %>%
  mutate(., game_no = group_indices(., kickoffDate)) %>% 
  mutate(half = fct_collapse(phase, '1' = c('1', '2', '3'), '2' = c('4', '5', '6'))) %>% 
  group_by(game_no, half) %>% 
  summarise(opposite_team = first(ifelse(hometeam_team1 == 'Barcelona', awayteam_team2, hometeam_team1)), goals = sum(goal * (team == 'Barcelona')), goals_conceded = sum(goal * (!team == 'Barcelona')), team_xG = sum(xG * (team == 'Barcelona')), team_xGA = sum(xG * (!team == 'Barcelona'))) %>% 
  group_by(half) %>% mutate(agg_goals = cumsum(goals), agg_goals_conceded = cumsum(goals_conceded),
                            agg_team_xG = cumsum(team_xG), agg_team_xGA = cumsum(team_xGA)) 

data_plot_long <- data_plot %>% gather(key, value, goals, goals_conceded, team_xG, team_xGA, agg_goals, agg_goals_conceded, agg_team_xG, agg_team_xGA)

x_labels <- data_plot$opposite_team
names(x_labels) <- data_plot$game_no
x_labels <- x_labels[c(TRUE, FALSE)]

data_plot_long %>% filter(key %in% c('agg_goals', 'agg_goals_conceded', 'agg_team_xG', 'agg_team_xGA')) %>% 
  mutate(against = factor(ifelse(key %in% c('agg_goals_conceded', 'agg_team_xGA'), 1, 0)),
         expected = factor(ifelse(key %in% c('agg_team_xG', 'agg_team_xGA'), 1, 0))) %>% 
  ggplot() + facet_grid(~against, labeller = as_labeller(c('0' = 'Created', '1' = 'Conceded'))) +
  geom_step(aes(factor(game_no), value, color = half, linetype = expected, size = expected,
                group = interaction(half, key))) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = .4, hjust = 1),
        axis.title.y = element_text(size = 10, face = 'bold'),
        strip.text = element_text(color = '#4F5F6B', face = 'bold', size = 14)) +
  scale_x_discrete(labels = x_labels) + xlab(NULL) + ylab(NULL) +
  scale_linetype_manual(values = c('1' = 'solid', '0' = 'dashed'),
                        labels = c('Actual goals', 'Expected goals'))  +
  scale_size_manual(values = c('1' = 1, '0' = .5)) + guides(size = FALSE) +
  scale_color_discrete(labels = c('1st half', '2nd half')) +
  labs(linetype = NULL, color = NULL)
