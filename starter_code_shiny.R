library(tidyverse)
library(janitor)

################################################################################################
# Fiter out screen values of administrators
################################################################################################

admin <- c("55e906bd8d133cef1975080aa2bf8ff142bb1d6a",
           "13641bfecade7ce327bbd9f3741cc7d89a23f535",
           "6477a9db4f10c1f2cd6c6ed915994fecab250301",
           "e293ce9071b68d76b4df45f9cbfa201bf6f9578b",
           "0014ffdbb6ad31b4dea168e85f7ebe38073b251c",
           "b59ba5bcb53076fe0a9df9fa53412c45d24ad542",
           "6fb2fed5ac1550f8eaf96deb87c7fe043fc94f35",
           "d6572beed87a35946d1422d63792b08490222099",
           "0e055693fa64c3587a503f68bf5a548bba205946",
           "97cf40a403101ea3e86b72e33b4f43571a97e0ae",
           "45716e1930e059719bc2a399ad02f7a98fa9b8e1",
           "79fe2526bb9262f9ae1c9a30f1b4add61a480a31",
           "201ae316509e99033e8ff009f8403e76f9bfd0d6",
           "4c1b55bcc15f2260ce268b55e8bed45de033f58d",
           "3dafa1552229acc110728c8f34c91bde1f3f2c6f",
           "435d02b1f642f3f8900635560262ea9266670619",
           "a622ba478862c19d4fe51a3b955069b940ad7cc5",
           "cee4b1927f4291ca07dd7ca7fd2766a280a421bd",
           "02a70c2052bd6360919bea5a4c0d2af18a81e249",
           "44d76cf80e5fe541a5692f8ea671e0a2d9e92b6e")

################################################################################################
################################################################################################


################################################################################################
# WeeklyEffort
################################################################################################
weekly_effort <- read_csv("StanfordEdX/engagement_Medicine_MedStats_Summer2014_weeklyEffort.csv")

weekly_effort <- weekly_effort %>% clean_names()

weekly_effort <- weekly_effort %>% 
  filter(!(anon_screen_name %in% admin))

################################################################################################
# EventXtract
################################################################################################
event_xtract <- read_csv("StanfordEdX/Medicine_MedStats_Summer2014_EventXtract.csv") 

names(event_xtract) <- gsub("'", '', names(event_xtract))
event_xtract <- event_xtract %>% 
  filter(!(anon_screen_name %in% admin))

################################################################################################
# ActivityGrade
################################################################################################
activity_grade <- read_csv("StanfordEdX/Medicine_MedStats_Summer2014_ActivityGrade.csv")

names(activity_grade) <- gsub("'", '', names(activity_grade))
activity_grade <- activity_grade %>% 
  filter(!(anon_screen_name %in% admin))

################################################################################################
# Video Interaction
################################################################################################
video_int <- read_csv("StanfordEdX/Medicine_MedStats_Summer2014_VideoInteraction.csv")

names(video_int) <- gsub("'", '', names(video_int))

# Take out edx.forum.searched in column event_type because that is unnecessary
video_int <- video_int %>% 
  filter(event_type != "edx.forum.searched")

################################################################################################
# Final Grades
################################################################################################
final_grade <- read_csv("final_grades.csv")

################################################################################################
# Dropouts:
# Filter out for Solutions and get number of unique video ids for each student
################################################################################################
condition_2 <- video_int %>% 
  filter(!(str_detect(resource_display_name, "Solutions"))) %>% 
  group_by(anon_screen_name) %>% 
  summarise(num_videos = length(unique(video_id))) %>% 
  mutate(prop_videos = num_videos / max(num_videos)) %>% 
  filter(prop_videos < 0.5000)


total_dropout <- condition_2 %>% 
  dplyr::select(anon_screen_name) %>% 
  pull()


################################################################################################
# Discover Characteristics of Dropouts
################################################################################################

weekly_effort_new <- event_xtract %>% 
  dplyr::select(anon_screen_name) %>% 
  inner_join(weekly_effort, by = "anon_screen_name") %>%
  dplyr::select(anon_screen_name, week, effort_sec) %>% 
  dplyr::distinct() %>% 
  filter(week != 11) %>%  # Only look at 10 weeks-filter out week11 because only 14 students have data for week11
  as.data.frame()

week_seq <- seq(from = as.Date("2014/06/24"), to = as.Date("2014/09/08"), by = "week")

# Make new dataframe out of video dataset (Number of times students pressed "Play")
video_int_clus <- video_int %>% 
  mutate(time = as.Date(time),
         video_week = case_when(
           time <= week_seq[2] & time >= week_seq[1] ~ 1,
           time <= week_seq[3] & time >= as.Date("2014-07-02") ~ 2,
           time <= week_seq[4] & time >= as.Date("2014-07-09") ~ 3,
           time <= week_seq[5] & time >= as.Date("2014-07-16") ~ 4,
           time <= week_seq[6] & time >= as.Date("2014-07-23") ~ 5,
           time <= week_seq[7] & time >= as.Date("2014-07-30") ~ 6,
           time <= week_seq[8] & time >= as.Date("2014-08-06") ~ 7,
           time <= week_seq[9] & time >= as.Date("2014-08-13") ~ 8,
           time <= week_seq[10] & time >= as.Date("2014-08-20") ~ 9,
           time <= week_seq[11] & time >= as.Date("2014-08-27") ~ 10,
           TRUE ~ 0)) %>% 
  filter(video_week != 0) %>%
  dplyr::select(anon_screen_name, video_week, event_type) %>% 
  filter(event_type == "play_video") %>% 
  group_by(anon_screen_name, video_week) %>% 
  summarise(play_video_num = n()) %>% 
  spread(key = video_week, value = play_video_num) %>% 
  replace_na(list("1" = 0,
                  "2" = 0,
                  "3" = 0,
                  "4" = 0,
                  "5" = 0,
                  "6" = 0,
                  "7" = 0,
                  "8" = 0,
                  "9" = 0,
                  "10" = 0)) %>% 
  as.data.frame()

# Manipulate weekly_effort
weekly_effort_new %<>% 
  spread(key = week, value = effort_sec) %>% 
  replace_na(list("1" = 0,
                  "2" = 0,
                  "3" = 0,
                  "4" = 0,
                  "5" = 0,
                  "6" = 0,
                  "7" = 0,
                  "8" = 0,
                  "9" = 0,
                  "10" = 0)) 

video_int_clus %<>% column_to_rownames(var = "anon_screen_name")
weekly_effort_new %<>% column_to_rownames(var = "anon_screen_name")

names(video_int_clus) <- paste("week", names(video_int_clus), "video", sep = "_")
names(weekly_effort_new) <- paste("week", names(weekly_effort_new), "effort", sep = "_")

video_int_clus %<>% rownames_to_column(var = "anon_screen_name")
weekly_effort_new %<>% rownames_to_column(var = "anon_screen_name")


new_clus <- weekly_effort_new %>% 
  inner_join(video_int_clus, by = "anon_screen_name")

# First Stage
new_clust_first <- new_clus %>% 
  dplyr::select(anon_screen_name, week_1_effort, week_1_video,
         week_2_effort, week_2_video,
         week_3_effort, week_3_video) %>% 
  column_to_rownames(var = "anon_screen_name")

# Second Stage
new_clust_second <- new_clus %>% 
  dplyr::select(anon_screen_name, week_1_effort, week_1_video,
         week_2_effort, week_2_video,
         week_3_effort, week_3_video, week_4_effort, week_4_video,
         week_5_effort, week_5_video,
         week_6_effort, week_6_video) %>% 
  column_to_rownames(var = "anon_screen_name")

# Third Stage
new_clust_third <- new_clus %>% 
  dplyr::select(anon_screen_name, week_1_effort, week_1_video,
         week_2_effort, week_2_video,
         week_3_effort, week_3_video, week_4_effort, week_4_video,
         week_5_effort, week_5_video,
         week_6_effort, week_6_video, week_7_effort, week_7_video,
         week_8_effort, week_8_video,
         week_9_effort, week_9_video,
         week_10_effort, week_10_video) %>% 
  column_to_rownames(var = "anon_screen_name")

# Set seed
set.seed(42)

# K-Means on First Stage
new_clust_first_kmeans <- kmeans(scale(new_clust_first), centers = 6)

# K-Means on Second Stage
new_clust_second_kmeans <- kmeans(scale(new_clust_second), centers = 6)

# K-Means on Third Stage
new_clust_third_kmeans <- kmeans(scale(new_clust_third), centers = 6)

# Clean Up First Stage using broom::augment
new_clust_first_kmeans <- broom::augment(new_clust_first_kmeans, new_clust_first)

new_clust_first_kmeans <- new_clust_first_kmeans %>% 
  dplyr::rename(anon_screen_name = .rownames,
         cluster = .cluster)

new_clust_first_kmeans %<>% remove_rownames()

# Clean Up Second Stage using broom::augment
new_clust_second_kmeans <- broom::augment(new_clust_second_kmeans, new_clust_second)

new_clust_second_kmeans <- new_clust_second_kmeans %>% 
  dplyr::rename(anon_screen_name = .rownames,
         cluster = .cluster)

new_clust_second_kmeans %<>% remove_rownames()

# Clean Up Third Stage using broom::augment
new_clust_third_kmeans <- broom::augment(new_clust_third_kmeans, new_clust_third)

new_clust_third_kmeans <- new_clust_third_kmeans %>% 
  dplyr::rename(anon_screen_name = .rownames,
         cluster = .cluster)

new_clust_third_kmeans %<>% remove_rownames()

# Dropout clusters: 
# Group1: Cluster 6
# Group2: Cluster 3
# Group3: Cluster 4
dropout_group1 <- new_clust_first_kmeans %>% 
  mutate(cluster = as.character(cluster)) %>% 
  filter(cluster == "6")

dropout_group2 <- new_clust_second_kmeans %>%
  mutate(cluster = as.character(cluster)) %>% 
  filter(cluster == "3")


dropout_group3 <- new_clust_third_kmeans %>% 
  mutate(cluster = as.character(cluster)) %>% 
  filter(cluster == "4")


################################################################################################
# All the student ID's with data in all three datasets:
################################################################################################
student_id <- reduce(list(event_xtract$anon_screen_name,
                          activity_grade$anon_screen_name,
                          weekly_effort$anon_screen_name), intersect)
