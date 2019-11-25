library(tidyverse)

# Screen values of administrators-Fiter out!

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


# Read, observe, and manipulate engagement_Medicine_MedStats_Summer2014_weeklyEffort.csv
weekly_effort <- read_csv("StanfordEdX/engagement_Medicine_MedStats_Summer2014_weeklyEffort.csv")

## Notes: Platform column / Course column has identical, single values (OpenEdX and weekly_efforticine...)
names(weekly_effort) <- tolower(names(weekly_effort))
weekly_effort <- weekly_effort %>% 
  rename(effort_sec = `effort (sec)`)
weekly_effort <- weekly_effort %>% 
  filter(!(anon_screen_name %in% admin))


# Read and manipulate dataset
event_xtract <- read_csv("StanfordEdX/Medicine_MedStats_Summer2014_EventXtract.csv") 

names(event_xtract) <- gsub("'", '', names(event_xtract))
event_xtract <- event_xtract %>% 
  filter(!(anon_screen_name %in% admin))

event_xtract_shiny <- event_xtract %>% 
  select(-starts_with("video_"), 
         -starts_with("goto")) 

# Read and manipulate dataset StanfordEdX/Medicine_MedStats_Summer2014_ActivityGrade.csv
activity_grade <- read_csv("StanfordEdX/Medicine_MedStats_Summer2014_ActivityGrade.csv")

names(activity_grade) <- gsub("'", '', names(activity_grade))
activity_grade <- activity_grade %>% 
  filter(!(anon_screen_name %in% admin))


# Make Video Interaction Dataset  ----------------------------------------------
video_int <- read_csv("StanfordEdX/Medicine_MedStats_Summer2014_VideoInteraction.csv")

names(video_int) <- gsub("'", '', names(video_int))

# Take out edx.forum.searched in column event_type because that is unnecessary
video_int <- video_int %>% 
  filter(event_type != "edx.forum.searched")

# William's Final Grades
final_grade <- read_csv("final_grades.csv")

# Dropouts:
# Filter out for Solutions and get number of unique video ids for each student
condition_2 <- video_int %>% 
  filter(!(str_detect(resource_display_name, "Solutions"))) %>% 
  group_by(anon_screen_name) %>% 
  summarise(num_videos = length(unique(video_id))) %>% 
  mutate(prop_videos = num_videos / max(num_videos)) %>% 
  filter(prop_videos < 0.5000)

### RESULT: 4996 (approx 5000) students watched less than 50% of the videos

total_dropout <- condition_2 %>% 
  select(anon_screen_name) %>% 
  pull()

# All the student ID's with data in all three datasets:
student_id <- reduce(list(event_xtract$anon_screen_name,
            activity_grade$anon_screen_name,
            weekly_effort$anon_screen_name), intersect)

