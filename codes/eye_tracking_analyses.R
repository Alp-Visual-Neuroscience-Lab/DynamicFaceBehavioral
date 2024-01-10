library(readxl)
library(tidyverse)
library(ggplot2)

exp1_raw <- read_excel("/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_1/data_experiment_1_62_participants_raw.xlsx")
exp1_raw$subject_code <- as.factor(exp1_raw$subject_code)
exp1_raw_distinct <- exp1_raw %>% group_by(subject_code) %>% summarize(n = n())


t_n_c_exp1 <- exp1_raw %>%
  as.data.frame() %>%
  select(
    Block_Nr, Block_Name, Task_Nr, Task_Name, Trial_Nr, 
    Trial_Id, accuracy_, Condition_Id, "error-gaze", 
    gaze_coord, keyvalue, response_, responsetime, single_video, 
    completed, end_time, exp_subject_id, group_name, 
    pixelDensityPerMM, rec_session_id, Screen_Height, Screen_Width, 
    session_name, session_nr, start_time, subject_code, time_delay_offset, 
    unlocked
  )
exclude_subjects_exp1 <- c("3507", "1579","3582","2232","1847","1468","1817","1270","1543","2450","2796","3592","848", "3609", "3631", "3646")
t_n_c_exp1 <- subset(t_n_c_exp1, !(subject_code %in% exclude_subjects_exp1))


#CONDITIONS BASED ON VIDEO FILES
#Upright vs Inverted
df_exp1 <- t_n_c_exp1 %>%
  mutate(Condition = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video)  & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    TRUE ~ "Inverted"  # Default value for other cases
  ))


df_exp1_filtered <- subset(df_exp1, responsetime >= 100 & responsetime <= 3000)

##to explore eyetracking data, clean the gaze_coord
df_exp1_filtered$gaze_coord <- gsub('"', '', df_exp1_filtered$gaze_coord) 
df_exp1_filtered$gaze_coord <- gsub("times=", "", df_exp1_filtered$gaze_coord)
df_exp1_filtered$gaze_coord <- gsub("---values=;", "\t", df_exp1_filtered$gaze_coord)
df_exp1_filtered$gaze_coord <- gsub("---values=", "\t", df_exp1_filtered$gaze_coord)


#splitting time and cord in the gaze_coord
df_exp1_filtered <- cbind(df_exp1_filtered, do.call(rbind, strsplit(as.character(df_exp1_filtered$gaze_coord), "\t")))
colnames(df_exp1_filtered)[ncol(df_exp1_filtered)-1] <- "Times"
colnames(df_exp1_filtered)[ncol(df_exp1_filtered)] <- "Coords"

df_exp1_filtered$row_number <-seq_len(nrow(df_exp1_filtered)) #number the lines to combine them later

#separate the coordinates in the cells into rows to get their average
df_c_exp1 <- df_exp1_filtered %>%
  separate_longer_delim("Coords", delim= ";")

length(unique(df_c_exp1$rec_session_id))

df_c_exp1 <- df_c_exp1 %>%
  filter(Coords != "") %>% 
  filter(Coords != "NaN,NaN") #filter out empty rows
length(unique(df_c_exp1$rec_session_id))


#Divide them to the columns to get separate averages of x and y
df_c_exp1 <- cbind(df_c_exp1, do.call(rbind, strsplit(as.character(df_c_exp1$"Coords"), ","))) #x ve y leri ayır
#name columns
colnames(df_c_exp1)[ncol(df_c_exp1)-1] <- "X_Coord"
colnames(df_c_exp1)[ncol(df_c_exp1)] <- "Y_Coord"
#make them numeric
df_c_exp1$X_Coord <- as.numeric(df_c_exp1$X_Coord)
df_c_exp1$Y_Coord <- as.numeric(df_c_exp1$Y_Coord)

#To combine the rows we separated, take their average according to the combination column we determined beforehand.
df_c_1 <- df_c_exp1 %>% 
  group_by(row_number) %>%
  summarize(X_Coord = mean(X_Coord, na.rm = T),
            Y_Coord = mean(Y_Coord, na.rm = T))
#Now combine the average values of each trial with the real data
df_exp1_filtered_coordinates_combined <- merge(df_exp1_filtered, df_c_1, by = "row_number", all.x = TRUE)

summary <- df_exp1_filtered_coordinates_combined %>% 
  group_by(Condition) %>% 
  summarize(mean_x = mean(X_Coord),
            mean_y = mean(Y_Coord))

df_exp1_filtered_coordinates_combined$VisualDegrees_X <- (df_exp1_filtered_coordinates_combined$X_Coord * 32) / 800
df_exp1_filtered_coordinates_combined$VisualDegrees_Y <- (df_exp1_filtered_coordinates_combined$Y_Coord * 32) / 800

summary <- df_exp1_filtered_coordinates_combined %>% 
  group_by(Condition) %>% 
  summarize(mean_x = mean(VisualDegrees_X),
            mean_y = mean(VisualDegrees_Y))

df_exp1_filtered_coordinates_combined$Value <- df_exp1_filtered_coordinates_combined$VisualDegrees_X + df_exp1_filtered_coordinates_combined$VisualDegrees_Y

df_exp1_filtered_coordinates_combined$Condition <- factor(df_exp1_filtered_coordinates_combined$Condition, levels = c("Upright", "Inverted"))

#heatmap
ggplot(df_exp1_filtered_coordinates_combined, aes(x = VisualDegrees_X, y = VisualDegrees_Y)) +
  geom_bin2d(aes(fill = stat(count)), bins = 50,width = 0.01, height = 0.01) +
  scale_fill_gradient(low = "blue",high = "red") +
  labs(x = "Y-coordinate in Visual Degrees", y = "Y-coordinate in Visual Degrees") +
  facet_wrap(~Condition)+ 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.background = element_rect(fill = 'white'),  # Set background color
        panel.grid.major = element_line(color = 'gray', size = 0.1),  # Set grid color
        panel.grid.minor = element_line(color = 'gray', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12))
ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/experiment_1_eyetracking_plot_orientation.png", width = 11, height = 5, dpi = 600,bg = "white")


#coordinates of the fixation cross
X_coord_cross <- 336.31
Y_coord_cross <- 187.71

#convert to visual degrees
VisualDegrees_X_cross <- (X_coord_cross * 32) / 800
VisualDegrees_Y_cross <- (Y_coord_cross * 32) / 800

num_higher <- sum(df_exp1_filtered_coordinates_combined$VisualDegrees_Y > VisualDegrees_Y_cross)
num_lower <- sum(df_exp1_filtered_coordinates_combined$VisualDegrees_Y < VisualDegrees_Y_cross)
total_participants <- nrow(df_exp1_filtered_coordinates_combined)
percentage_higher <- (num_higher / total_participants) * 100
percentage_lower <- (num_lower / total_participants) * 100

cat("Percentage of participants with Eye gaze higher than fixation cross:", VisualDegrees_Y_cross, "is:", percentage_higher, "%\n")
cat("Percentage of participants with Eye gaze lower than fixation cross:", VisualDegrees_Y_cross, "is:", percentage_lower, "%\n")

#CONDITIONS BASED ON VIDEO FILES
#Blink vs No-Blink

df_exp1 <- t_n_c_exp1 %>%
  mutate(Condition = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video)  ~ "With-Blink",
    grepl("_rev3000-BLINK_edited.mp4", single_video)  ~ "With-Blink",
    grepl("_normal_meannorm3000-BLINK_edited.mp4_rotated", single_video)  ~ "With-Blink",
    grepl("_rev3000-BLINK_edited.mp4_rotated", single_video) ~ "With-Blink",
    grepl("_normal_meannorm3000-NON-BLINK", single_video) ~ "No-Blink",
    grepl("_rev3000-NON-BLINK", single_video)  ~ "No-Blink",
    grepl("_normal_meannorm3000-NON-BLINK", single_video) ~ "No-Blink",
    grepl("_rev3000-NON-BLINK", single_video) ~ "No-Blink",
    TRUE ~ ""  # Default value for other cases
  ))

df_exp1_filtered <- subset(df_exp1, responsetime >= 100 & responsetime <= 3000)

##to explore eyetracking data, clean the gaze_coord
df_exp1_filtered$gaze_coord <- gsub('"', '', df_exp1_filtered$gaze_coord) 
df_exp1_filtered$gaze_coord <- gsub("times=", "", df_exp1_filtered$gaze_coord)
df_exp1_filtered$gaze_coord <- gsub("---values=;", "\t", df_exp1_filtered$gaze_coord)
df_exp1_filtered$gaze_coord <- gsub("---values=", "\t", df_exp1_filtered$gaze_coord)


#splitting time and cord in the gaze_coord
df_exp1_filtered <- cbind(df_exp1_filtered, do.call(rbind, strsplit(as.character(df_exp1_filtered$gaze_coord), "\t")))
colnames(df_exp1_filtered)[ncol(df_exp1_filtered)-1] <- "Times"
colnames(df_exp1_filtered)[ncol(df_exp1_filtered)] <- "Coords"

df_exp1_filtered$row_number <-seq_len(nrow(df_exp1_filtered)) #number the lines to combine them later

#separate the coordinates in the cells into rows to get their average
df_c_exp1 <- df_exp1_filtered %>%
  separate_longer_delim("Coords", delim= ";")

length(unique(df_c_exp1$rec_session_id))

df_c_exp1 <- df_c_exp1 %>%
  filter(Coords != "") %>% 
  filter(Coords != "NaN,NaN") #filter out empty rows
length(unique(df_c_exp1$rec_session_id))

#Divide them to the columns to get separate averages of x and y
df_c_exp1 <- cbind(df_c_exp1, do.call(rbind, strsplit(as.character(df_c_exp1$"Coords"), ","))) #x ve y leri ayır
#name columns
colnames(df_c_exp1)[ncol(df_c_exp1)-1] <- "X_Coord"
colnames(df_c_exp1)[ncol(df_c_exp1)] <- "Y_Coord"
#make them numeric
df_c_exp1$X_Coord <- as.numeric(df_c_exp1$X_Coord)
df_c_exp1$Y_Coord <- as.numeric(df_c_exp1$Y_Coord)

#To combine the rows we separated, take their average according to the combination column we determined beforehand.
df_c_1 <- df_c_exp1 %>% 
  group_by(row_number) %>%
  summarize(X_Coord = mean(X_Coord, na.rm = T),
            Y_Coord = mean(Y_Coord, na.rm = T))
#Now combine the average values of each trial with the real data

df_exp1_filtered_coordinates_combined <- merge(df_exp1_filtered, df_c_1, by = "row_number", all.x = TRUE)

df_exp1_filtered_coordinates_combined$VisualDegrees_X <- (df_exp1_filtered_coordinates_combined$X_Coord * 32) / 800
df_exp1_filtered_coordinates_combined$VisualDegrees_Y <- (df_exp1_filtered_coordinates_combined$Y_Coord * 32) / 800


num_higher <- sum(df_exp1_filtered_coordinates_combined$VisualDegrees_Y > VisualDegrees_Y_cross)
num_lower <- sum(df_exp1_filtered_coordinates_combined$VisualDegrees_Y < VisualDegrees_Y_cross)
total_participants <- nrow(df_exp1_filtered_coordinates_combined)
percentage_higher <- (num_higher / total_participants) * 100
percentage_lower <- (num_lower / total_participants) * 100

cat("Percentage of participants with Eye gaze higher than fixation cross:", VisualDegrees_Y_cross, "is:", percentage_higher, "%\n")
cat("Percentage of participants with Eye gaze lower than fixation cross:", VisualDegrees_Y_cross, "is:", percentage_lower, "%\n")

summary <- df_exp1_filtered_coordinates_combined %>% 
  group_by(Condition) %>% 
  summarize(mean_x = mean(VisualDegrees_X),
            mean_y = mean(VisualDegrees_Y))

df_exp1_filtered_coordinates_combined$Value <- df_exp1_filtered_coordinates_combined$VisualDegrees_X + df_exp1_filtered_coordinates_combined$VisualDegrees_Y

df_exp1_filtered_coordinates_combined$Condition <- factor(df_exp1_filtered_coordinates_combined$Condition, levels = c("With-Blink", "No-Blink"))

#heatmap
ggplot(df_exp1_filtered_coordinates_combined, aes(x = VisualDegrees_X, y = VisualDegrees_Y)) +
  geom_bin2d(aes(fill = stat(count)), bins = 50,width = 0.01, height = 0.01) +
  scale_fill_gradient(low = "blue",high = "red") +
  labs(x = "X-coordinate in Visual Degrees", y = "Y-coordinate in Visual Degrees") +
  facet_wrap(~Condition)+ 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.background = element_rect(fill = 'white'),  # Set background color
        panel.grid.major = element_line(color = 'gray', size = 0.1),  # Set grid color
        panel.grid.minor = element_line(color = 'gray', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12))
ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/experiment_1_eyetracking_plot_Blink.png", width = 11, height = 5, dpi = 600,bg = "white")


#experiment 2

exp2_raw <- read_excel("/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/Experiment_2_raw_data.xlsx")

exp2_raw$subject_code <- as.factor(exp2_raw$subject_code)
exp2_raw_distinct <- exp2_raw %>% group_by(subject_code) %>% summarize(n = n())

#selecting
t_n_c <- exp2_raw %>%
  as.data.frame() %>%
  filter(Task_Name == "Upright-Block" | Task_Name == "Rotated-Block") %>% #nec trials
  .[ , c("Block_Nr", "Block_Name", "Task_Nr", "Task_Name", "Trial_Nr", 
         "Trial_Id", "accuracy_", "array_ind", "Condition_Id", "error-gaze", 
         "gaze_coord", "keyvalue", "response_", "responsetime", "single_video", 
         "completed", "end_time", "exp_subject_id", "FaceDetectionRate", "group_name", 
         "pixelDensityPerMM", "rec_session_id", "Screen_Height", "Screen_Width", 
         "session_name", "session_nr", "start_time", "subject_code", "time_delay_offset", 
         "unlocked") , drop = FALSE]

#excluding subjects 
#added 2995 because missing data 
exclude_subjects_exp21 <- c("1238", "2180","2450","2762","2783","2797","2816","2824","2837","4885","6949","7002")


exclude_subjects_exp2 <- c("2995","1238","2180","2450","2762","2783","2797","2816","2824","2837","4885","6949","7002","2294", "2313", "2714", "2906", "3455","5026", "5910","5659")
t_n_c <- subset(t_n_c, !(subject_code %in% exclude_subjects_exp2))


#CONDITIONS BASED ON VIDEO FILES
df_exp2 <- t_n_c %>%
  mutate(Condition = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video)  & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    TRUE ~ "Inverted"  # Default value for other cases
  ))


#exclude rows with RT greater than 3000 or less than 100
df_exp2_filtered <- subset(df_exp2, responsetime >= 100 & responsetime <= 3000)


##to explore eyetracking data, clean the gaze_coord
df_exp2_filtered$gaze_coord <- gsub('"', '', df_exp2_filtered$gaze_coord) 
df_exp2_filtered$gaze_coord <- gsub("times=", "", df_exp2_filtered$gaze_coord)
df_exp2_filtered$gaze_coord <- gsub("---values=;", "\t", df_exp2_filtered$gaze_coord)
df_exp2_filtered$gaze_coord <- gsub("---values=", "\t", df_exp2_filtered$gaze_coord)

#splitting time and cord in the gaze_coord
df_exp2_filtered <- cbind(df_exp2_filtered, do.call(rbind, strsplit(as.character(df_exp2_filtered$gaze_coord), "\t")))
colnames(df_exp2_filtered)[ncol(df_exp2_filtered)-1] <- "Times"
colnames(df_exp2_filtered)[ncol(df_exp2_filtered)] <- "Coords"

df_exp2_filtered$row_number <-seq_len(nrow(df_exp2_filtered)) #number the lines to combine them later

#separate the coordinates in the cells into rows to get their average
df_c <- df_exp2_filtered %>%
  separate_longer_delim("Coords", delim= ";")

length(unique(df_c$row_number))
length(unique(df_c$rec_session_id))

df_c <- df_c %>%
  filter(Coords != "") %>% 
  filter(Coords != "NaN,NaN") #filter out empty rows
length(unique(df_c$rec_session_id))
#Divide them to the columns to get separate averages of x and y
df_c <- cbind(df_c, do.call(rbind, strsplit(as.character(df_c$"Coords"), ","))) #x ve y leri ayır
#name columns
colnames(df_c)[ncol(df_c)-1] <- "X_Coord"
colnames(df_c)[ncol(df_c)] <- "Y_Coord"
#make them numeric
df_c$X_Coord <- as.numeric(df_c$X_Coord)
df_c$Y_Coord <- as.numeric(df_c$Y_Coord)

#To combine the rows we separated, take their average according to the combination column we determined beforehand.
df_c_2 <- df_c %>% 
  group_by(row_number) %>%
  summarize(X_Coord = mean(X_Coord, na.rm = T),
         Y_Coord = mean(Y_Coord, na.rm = T))
#Now combine the average values of each trial with the real data
df_exp2_filtered_coordinates_combined <- merge(df_exp2_filtered, df_c_2, by = "row_number", all.x = TRUE)

#convert to visual degrees
df_exp2_filtered_coordinates_combined$VisualDegrees_X <- (df_exp2_filtered_coordinates_combined$X_Coord * 32) / 800
df_exp2_filtered_coordinates_combined$VisualDegrees_Y <- (df_exp2_filtered_coordinates_combined$Y_Coord * 32) / 800

num_higher2 <- sum(df_exp2_filtered_coordinates_combined$VisualDegrees_Y > VisualDegrees_Y_cross)
num_lower2 <- sum(df_exp2_filtered_coordinates_combined$VisualDegrees_Y < VisualDegrees_Y_cross)
total_participants2 <- nrow(df_exp2_filtered_coordinates_combined)
percentage_higher2 <- (num_higher2 / total_participants2) * 100
percentage_lower2 <- (num_lower2 / total_participants2) * 100

cat("Percentage of participants with Eye gaze higher than fixation cross:", VisualDegrees_Y_cross, "is:", percentage_higher2, "%\n")
cat("Percentage of participants with Eye gaze lower than fixation cross:", VisualDegrees_Y_cross, "is:", percentage_lower2, "%\n")


summary <- df_exp2_filtered_coordinates_combined %>% 
  group_by(Condition) %>% 
  summarize(mean_x = mean(VisualDegrees_X),
            mean_y = mean(VisualDegrees_Y))
#write.csv(df_exp2_filtered_coordinates_combined, "data_cleaned.csv")



summary <- df_exp2_filtered_coordinates_combined %>% 
  group_by(Condition) %>% 
  summarize(mean_x = mean(X_Coord),
            mean_y = mean(Y_Coord))

df_exp2_filtered_coordinates_combined$Value <- df_exp2_filtered_coordinates_combined$VisualDegrees_X + df_exp2_filtered_coordinates_combined$VisualDegrees_Y

df_exp2_filtered_coordinates_combined$Condition <- factor(df_exp2_filtered_coordinates_combined$Condition, levels = c("Upright", "Inverted"))

#heatmap
exp2_eyetracking_plt <- ggplot(df_exp2_filtered_coordinates_combined, aes(x = VisualDegrees_X, y = VisualDegrees_Y)) +
  geom_bin2d(aes(fill = stat(count)), bins = 50,width = 0.01, height = 0.01) +
  scale_fill_gradient(low = "blue",high = "red") +
  labs(x = "X-coordinate in Visual Degrees", y = "Y-coordinate in Visual Degrees") +
  facet_wrap(~Condition)+ 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.background = element_rect(fill = 'white'),  # Set background color
        panel.grid.major = element_line(color = 'gray', size = 0.1),  # Set grid color
        panel.grid.minor = element_line(color = 'gray', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12))
exp2_eyetracking_plt


ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/experiment_2_eyetracking_plot_orientation.png", width = 11, height = 5, dpi = 600,bg = "white")
#CONDITIONS BASED ON VIDEO FILES
#Blink vs No-Blink


df_exp2 <- t_n_c %>%
  mutate(Condition = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video)  ~ "With-Blink",
    grepl("_rev3000-BLINK_edited.mp4", single_video)  ~ "With-Blink",
    grepl("_normal_meannorm3000-BLINK_edited.mp4_rotated", single_video)  ~ "With-Blink",
    grepl("_rev3000-BLINK_edited.mp4_rotated", single_video) ~ "With-Blink",
    grepl("_normal_meannorm3000-NON-BLINK", single_video) ~ "No-Blink",
    grepl("_rev3000-NON-BLINK", single_video)  ~ "No-Blink",
    grepl("_normal_meannorm3000-NON-BLINK", single_video) ~ "No-Blink",
    grepl("_rev3000-NON-BLINK", single_video) ~ "No-Blink",
    TRUE ~ ""  # Default value for other cases
  ))

df_exp2_filtered <- subset(df_exp2, responsetime >= 100 & responsetime <= 3000)

##to explore eyetracking data, clean the gaze_coord
df_exp2_filtered$gaze_coord <- gsub('"', '', df_exp2_filtered$gaze_coord) 
df_exp2_filtered$gaze_coord <- gsub("times=", "", df_exp2_filtered$gaze_coord)
df_exp2_filtered$gaze_coord <- gsub("---values=;", "\t", df_exp2_filtered$gaze_coord)
df_exp2_filtered$gaze_coord <- gsub("---values=", "\t", df_exp2_filtered$gaze_coord)


#splitting time and cord in the gaze_coord
df_exp2_filtered <- cbind(df_exp2_filtered, do.call(rbind, strsplit(as.character(df_exp2_filtered$gaze_coord), "\t")))
colnames(df_exp2_filtered)[ncol(df_exp2_filtered)-1] <- "Times"
colnames(df_exp2_filtered)[ncol(df_exp2_filtered)] <- "Coords"

df_exp2_filtered$row_number <-seq_len(nrow(df_exp2_filtered)) #number the lines to combine them later

#separate the coordinates in the cells into rows to get their average
df_c_exp2 <- df_exp2_filtered %>%
  separate_longer_delim("Coords", delim= ";")

length(unique(df_c_exp2$rec_session_id))

df_c_exp2 <- df_c_exp2 %>%
  filter(Coords != "") %>% 
  filter(Coords != "NaN,NaN") #filter out empty rows
length(unique(df_c_exp2$rec_session_id))

#Divide them to the columns to get separate averages of x and y
df_c_exp2 <- cbind(df_c_exp2, do.call(rbind, strsplit(as.character(df_c_exp2$"Coords"), ","))) #x ve y leri ayır
#name columns
colnames(df_c_exp2)[ncol(df_c_exp2)-1] <- "X_Coord"
colnames(df_c_exp2)[ncol(df_c_exp2)] <- "Y_Coord"
#make them numeric
df_c_exp2$X_Coord <- as.numeric(df_c_exp2$X_Coord)
df_c_exp2$Y_Coord <- as.numeric(df_c_exp2$Y_Coord)

#To combine the rows we separated, take their average according to the combination column we determined beforehand.
df_c_2 <- df_c_exp2 %>% 
  group_by(row_number) %>%
  summarize(X_Coord = mean(X_Coord, na.rm = T),
            Y_Coord = mean(Y_Coord, na.rm = T))
#Now combine the average values of each trial with the real data

df_exp2_filtered_coordinates_combined <- merge(df_exp2_filtered, df_c_2, by = "row_number", all.x = TRUE)

#convert to visual degrees
df_exp2_filtered_coordinates_combined$VisualDegrees_X <- (df_exp2_filtered_coordinates_combined$X_Coord * 32) / 800
df_exp2_filtered_coordinates_combined$VisualDegrees_Y <- (df_exp2_filtered_coordinates_combined$Y_Coord * 32) / 800

num_higher2 <- sum(df_exp2_filtered_coordinates_combined$VisualDegrees_Y > VisualDegrees_Y_cross)
num_lower2 <- sum(df_exp2_filtered_coordinates_combined$VisualDegrees_Y < VisualDegrees_Y_cross)
total_participants2 <- nrow(df_exp2_filtered_coordinates_combined)
percentage_higher2 <- (num_higher2 / total_participants2) * 100
percentage_lower2 <- (num_lower2 / total_participants2) * 100

cat("Percentage of participants with Eye gaze higher than fixation cross:", VisualDegrees_Y_cross, "is:", percentage_higher2, "%\n")
cat("Percentage of participants with Eye gaze lower than fixation cross:", VisualDegrees_Y_cross, "is:", percentage_lower2, "%\n")


summary <- df_exp2_filtered_coordinates_combined %>% 
  group_by(Condition) %>% 
  summarize(mean_x = mean(VisualDegrees_X),
            mean_y = mean(VisualDegrees_Y))

df_exp2_filtered_coordinates_combined$Value <- df_exp2_filtered_coordinates_combined$VisualDegrees_X + df_exp2_filtered_coordinates_combined$VisualDegrees_Y

df_exp2_filtered_coordinates_combined$Condition <- factor(df_exp2_filtered_coordinates_combined$Condition, levels = c("With-Blink", "No-Blink"))

#heatmap
ggplot(df_exp2_filtered_coordinates_combined, aes(x = VisualDegrees_X, y = VisualDegrees_Y)) +
  geom_bin2d(aes(fill = stat(count)), bins = 50,width = 0.01, height = 0.01) +
  scale_fill_gradient(low = "blue",high = "red") +
  labs(x = "X-coordinate in Visual Degrees", y = "Y-coordinate in Visual Degrees") +
  facet_wrap(~Condition)+ 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.background = element_rect(fill = 'white'),  # Set background color
        panel.grid.major = element_line(color = 'gray', size = 0.1),  # Set grid color
        panel.grid.minor = element_line(color = 'gray', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12))
ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/experiment_2_eyetracking_plot_Blink.png", width = 11, height = 5, dpi = 600,bg = "white")


