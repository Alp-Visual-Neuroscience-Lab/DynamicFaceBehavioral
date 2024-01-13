library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)

## Eye tracking graphs exp1
dir("/Users/suuser/Desktop/face_revision/")
exp1_raw <- read_excel("/Users/suuser/Desktop/face_revision/data/data_experiment_1_62_participants_raw.xlsx")
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
#Upright Blink Upright NoBlink Inverted Blink Inverted NoBlink
df_exp1UIBNB <- t_n_c_exp1 %>%
  mutate(Condition = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright Blink",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright Blink",
    grepl("_normal_meannorm3000-BLINK_edited.mp4_rotated", single_video) ~ "Inverted Blink",
    grepl("_rev3000-BLINK_edited.mp4_rotated", single_video) ~ "Inverted Blink",
    grepl("_normal_meannorm3000-NON-BLINK", single_video) & !grepl('rotated', single_video) ~ "Upright No Blink",
    grepl("_rev3000-NON-BLINK", single_video) & !grepl('rotated', single_video) ~ "Upright No Blink",
    grepl("_normal_meannorm3000-NON-BLINK", single_video) ~ "Inverted No Blink",
    grepl("_rev3000-NON-BLINK", single_video) ~ "Inverted No Blink",
    TRUE ~ ""  # Default value for other cases
  ))
df_exp1UIBNB <- slice(df_exp1UIBNB, -1)
df_exp1_filtered <- subset(df_exp1UIBNB, responsetime >= 100 & responsetime <= 3000)

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

#800 32
#450 18
Screen_Height_Unq <- unique(df_exp1_filtered_coordinates_combined$Screen_Height)
Screen_Width_Unq <- unique(df_exp1_filtered_coordinates_combined$Screen_Width)

#convert X_Y coord to the unit
df_exp1_filtered_coordinates_combined$YCoord_New <- (df_exp1_filtered_coordinates_combined$Y_Coord * 450) / df_exp1_filtered_coordinates_combined$Screen_Height
df_exp1_filtered_coordinates_combined$XCoord_New <- (df_exp1_filtered_coordinates_combined$X_Coord * 800) / df_exp1_filtered_coordinates_combined$Screen_Width

#calculate Visual degree based on unit
df_exp1_filtered_coordinates_combined$VisualDegrees_XNew <- (df_exp1_filtered_coordinates_combined$XCoord_New * 32) / 800
df_exp1_filtered_coordinates_combined$VisualDegrees_YNew <- (df_exp1_filtered_coordinates_combined$YCoord_New * 18) / 450

summary <- df_exp1_filtered_coordinates_combined %>% 
  group_by(Condition) %>% 
  summarize(mean_x = mean(VisualDegrees_XNew),
            mean_y = mean(VisualDegrees_YNew))

df_exp1_filtered_coordinates_combined$Value <- df_exp1_filtered_coordinates_combined$VisualDegrees_XNew + df_exp1_filtered_coordinates_combined$VisualDegrees_YNew

#df_exp1_filtered_coordinates_combined$Condition <- factor(df_exp1_filtered_coordinates_combined$Condition, levels = c("Upright Blink", "Upright NoBlink","Inverted Blink", "Inverted NoBlink" ))

#heatmap
ggplot(df_exp1_filtered_coordinates_combined, aes(x = VisualDegrees_XNew, y = VisualDegrees_YNew)) +
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
ggsave("/Users/suuser/Desktop/face_revision/dynamic face perception1/figures/experiment_1_eyetracking_plot.png", width = 11, height = 5, dpi = 600,bg = "white")

## Eye tracking analysis exp1
#coordinates of the fixation cross
X_coord_cross <- 10.49#383.06
Y_coord_cross <- 10.49#233 this is visual degree 0.41 for 450 800 scale

#X_coord_cross <- 336.31
#Y_coord_cross <- 187.71

#convert to visual degrees
VisualDegrees_X_cross <- (X_coord_cross * 32) / 800
VisualDegrees_Y_cross <- (Y_coord_cross * 18) / 450

#If in 4
#VisualDegree of Fix_cross_per subj
#convert X_Y coord to the unit
df_exp1_filtered_coordinates_combined$VisıualDegFixYCoord_New <- (VisualDegrees_Y_cross * df_exp1_filtered_coordinates_combined$Screen_Height) /450 
df_exp1_filtered_coordinates_combined$VisıualDegFixXCoord_New <- (VisualDegrees_X_cross * df_exp1_filtered_coordinates_combined$Screen_Width) /800 


#draw 2 visual degree radius
#coordinates of the fixation cross
X_coord_cross_ROI <- 75 #50.11#383.06
Y_coord_cross_ROI <- 75 #50.11#233 roi_radius <- 2

#convert to visual degrees
VisualDegrees_X_cross_ROI <- (X_coord_cross_ROI * 32) / 800
VisualDegrees_Y_cross_ROI <- (Y_coord_cross_ROI * 18) / 450
condition_levels <- unique(df_exp1_filtered_coordinates_combined$Condition)

percentage_higher_by_condition <- sapply(condition_levels, function(cond) {
  subset_data <- df_exp1_filtered_coordinates_combined[df_exp1_filtered_coordinates_combined$Condition == cond, ]
  num_higher <- sum(subset_data$VisualDegrees_YNew-(subset_data$VisıualDegFixYCoord_New+VisualDegrees_Y_cross_ROI) > VisualDegrees_Y_cross_ROI)
  total_participants <- nrow(subset_data)
  return((num_higher / total_participants) * 100)
})

resultH <- data.frame(Condition = condition_levels, Percentage_Higher = percentage_higher_by_condition)


percentage_lower_by_condition <- sapply(condition_levels, function(cond) {
  subset_data <- df_exp1_filtered_coordinates_combined[df_exp1_filtered_coordinates_combined$Condition == cond, ]
  num_higher <- sum(subset_data$VisualDegrees_YNew-(subset_data$VisıualDegFixYCoord_New - VisualDegrees_Y_cross_ROI) < VisualDegrees_Y_cross_ROI)
  total_participants <- nrow(subset_data)
  return((num_higher / total_participants) * 100)
})

resultL <- data.frame(Condition = condition_levels, Percentage_Lower = percentage_lower_by_condition)


percentage_within_by_condition <- sapply(condition_levels, function(cond) {
  subset_data <- df_exp1_filtered_coordinates_combined[df_exp1_filtered_coordinates_combined$Condition == cond, ]
  num_higher <- sum(subset_data$VisualDegrees_YNew-(subset_data$VisıualDegFixYCoord_New+VisualDegrees_Y_cross_ROI) <= VisualDegrees_Y_cross_ROI)
  total_participants <- nrow(subset_data)
  return((num_higher / total_participants) * 100)
})

resultEqExp1 <- data.frame(Condition = condition_levels, Percentage_Eq = percentage_within_by_condition)


## Eye tracking graphs exp2
exp2_raw <- read_excel("/Users/suuser/Desktop/face_revision/data/Experiment_2_raw_data.xlsx")
exp2_raw$subject_code <- as.factor(exp2_raw$subject_code)
exp2_raw_distinct <- exp2_raw %>% group_by(subject_code) %>% summarize(n = n())

t_n_c_exp2 <- exp2_raw %>%
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

exclude_subjects_exp2 <- c("2995","1238","2180","2450","2762","2783","2797","2816","2824","2837","4885","6949","7002","2294", "2313", "2714", "2906", "3455","5026", "5910","5659")
t_n_c_exp2 <- subset(t_n_c_exp2, !(subject_code %in% exclude_subjects_exp2))


#CONDITIONS BASED ON VIDEO FILES
#Upright Blink Upright NoBlink Inverted Blink Inverted NoBlink
df_exp2UIBNB <- t_n_c_exp2 %>%
  mutate(Condition = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright Blink",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright Blink",
    grepl("_normal_meannorm3000-BLINK_edited.mp4_rotated", single_video) ~ "Inverted Blink",
    grepl("_rev3000-BLINK_edited.mp4_rotated", single_video) ~ "Inverted Blink",
    grepl("_normal_meannorm3000-NON-BLINK", single_video) & !grepl('rotated', single_video) ~ "Upright No Blink",
    grepl("_rev3000-NON-BLINK", single_video) & !grepl('rotated', single_video) ~ "Upright No Blink",
    grepl("_normal_meannorm3000-NON-BLINK", single_video) ~ "Inverted No Blink",
    grepl("_rev3000-NON-BLINK", single_video) ~ "Inverted No Blink",
    TRUE ~ ""  # Default value for other cases
  ))
#df_exp2UIBNB <- slice(df_exp2UIBNB, -1)
df_exp2_filtered <- subset(df_exp2UIBNB, responsetime >= 100 & responsetime <= 3000)

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

#800 32
#450 18
Screen_Height_Unq <- unique(df_exp2_filtered_coordinates_combined$Screen_Height)
Screen_Width_Unq <- unique(df_exp2_filtered_coordinates_combined$Screen_Width)

#convert X_Y coord to the unit
df_exp2_filtered_coordinates_combined$YCoord_New <- (df_exp2_filtered_coordinates_combined$Y_Coord * 450) / df_exp2_filtered_coordinates_combined$Screen_Height
df_exp2_filtered_coordinates_combined$XCoord_New <- (df_exp2_filtered_coordinates_combined$X_Coord * 800) / df_exp2_filtered_coordinates_combined$Screen_Width

#calculate Visual degree based on unit
df_exp2_filtered_coordinates_combined$VisualDegrees_XNew <- (df_exp2_filtered_coordinates_combined$XCoord_New * 32) / 800
df_exp2_filtered_coordinates_combined$VisualDegrees_YNew <- (df_exp2_filtered_coordinates_combined$YCoord_New * 18) / 450

summary <- df_exp2_filtered_coordinates_combined %>% 
  group_by(Condition) %>% 
  summarize(mean_x = mean(VisualDegrees_XNew),
            mean_y = mean(VisualDegrees_YNew))

df_exp2_filtered_coordinates_combined$Value <- df_exp2_filtered_coordinates_combined$VisualDegrees_XNew + df_exp2_filtered_coordinates_combined$VisualDegrees_YNew
#df_exp2_filtered_coordinates_combined$Condition <- factor(df_exp2_filtered_coordinates_combined$Condition, levels = c("Upright Blink", "Upright NoBlink","Inverted Blink", "Inverted NoBlink" ),drop = FALSE)

#heatmap
ggplot(df_exp2_filtered_coordinates_combined, aes(x = VisualDegrees_XNew, y = VisualDegrees_YNew)) +
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
ggsave("/Users/suuser/Desktop/face_revision/dynamic face perception2/figures/experiment_2_eyetracking_plot.png", width = 11, height = 5, dpi = 600,bg = "white")


## Eye tracking analysis exp2
#coordinates of the fixation cross
X_coord_cross <- 10.49#383.06
Y_coord_cross <- 10.49#233 this is visual degree 0.41 for 450 800 scale

#X_coord_cross <- 336.31
#Y_coord_cross <- 187.71

#convert to visual degrees
VisualDegrees_X_cross <- (X_coord_cross * 32) / 800
VisualDegrees_Y_cross <- (Y_coord_cross * 18) / 450

#If in 4
#VisualDegree of Fix_cross_per subj
#convert X_Y coord to the unit
df_exp2_filtered_coordinates_combined$VisıualDegFixYCoord_New <- (VisualDegrees_Y_cross * df_exp2_filtered_coordinates_combined$Screen_Height) /450 
df_exp2_filtered_coordinates_combined$VisıualDegFixXCoord_New <- (VisualDegrees_X_cross * df_exp2_filtered_coordinates_combined$Screen_Width) /800 


#draw 2 visual degree radius
#coordinates of the fixation cross
X_coord_cross_ROI <- 75 #50.11#383.06
Y_coord_cross_ROI <- 75 #50.11#233 roi_radius <- 2

#convert to visual degrees
VisualDegrees_X_cross_ROI <- (X_coord_cross_ROI * 32) / 800
VisualDegrees_Y_cross_ROI <- (Y_coord_cross_ROI * 18) / 450
condition_levels <- unique(df_exp2_filtered_coordinates_combined$Condition)

percentage_higher_by_condition <- sapply(condition_levels, function(cond) {
  subset_data <- df_exp2_filtered_coordinates_combined[df_exp2_filtered_coordinates_combined$Condition == cond, ]
  num_higher <- sum(subset_data$VisualDegrees_YNew-(subset_data$VisıualDegFixYCoord_New+VisualDegrees_Y_cross_ROI) > VisualDegrees_Y_cross_ROI)
  total_participants <- nrow(subset_data)
  return((num_higher / total_participants) * 100)
})

resultHExp2 <- data.frame(Condition = condition_levels, Percentage_Higher = percentage_higher_by_condition)


percentage_lower_by_condition <- sapply(condition_levels, function(cond) {
  subset_data <- df_exp2_filtered_coordinates_combined[df_exp2_filtered_coordinates_combined$Condition == cond, ]
  num_higher <- sum(subset_data$VisualDegrees_YNew-(subset_data$VisıualDegFixYCoord_New - VisualDegrees_Y_cross_ROI) < VisualDegrees_Y_cross_ROI)
  total_participants <- nrow(subset_data)
  return((num_higher / total_participants) * 100)
})

resultLExp2 <- data.frame(Condition = condition_levels, Percentage_Lower = percentage_lower_by_condition)


percentage_within_by_condition <- sapply(condition_levels, function(cond) {
  subset_data <- df_exp2_filtered_coordinates_combined[df_exp2_filtered_coordinates_combined$Condition == cond,]
  num_higher <- sum(subset_data$VisualDegrees_YNew-(subset_data$VisıualDegFixYCoord_New+VisualDegrees_Y_cross_ROI) <= VisualDegrees_Y_cross_ROI)
  total_participants <- nrow(subset_data)
  return((num_higher / total_participants) * 100)
})

resultEqExp2 <- data.frame(Condition = condition_levels, Percentage_Eq = percentage_within_by_condition)

