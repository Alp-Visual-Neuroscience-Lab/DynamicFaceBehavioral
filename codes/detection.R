library(dplyr)
library(ggplot2)
library(readxl)
require(MASS)
library(tidyr)
library(patchwork)
library(writexl)


df_raw_data <- read_excel("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/exp1.xlsx")

participant_counts <- table(df_raw_data$subject_code)

participant_counts_df <- as.data.frame(participant_counts)
colnames(participant_counts_df) <- c("subject_code", "data_points_count")
View(participant_counts_df)


articipant_counts <- table(exp2_df_raw_data$)

participant_counts_df2 <- as.data.frame(articipant_counts)
colnames(participant_counts_df2) <- c("subject_code", "data_points_count")
View(participant_counts_df2)

df2_raw_data <- read.csv("/Users/gulcelale/Downloads/exp2_rawdata.csv",header = TRUE)



#target = sequence blink rotated

df_raw_data_detection_sq_blink_rot <- df_raw_data %>%
  mutate(Detection = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4_rotated", single_video) & response == "ArrowRight" ~ "Hit",
    grepl("_normal_meannorm3000-BLINK_edited.mp4_rotated", single_video) & response == "ArrowLeft" ~ "Miss",
    grepl("_rev3000-BLINK_edited.mp4_rotated", single_video) & response == "ArrowRight" ~ "FalseAlarm",
    grepl("_rev3000-BLINK_edited.mp4_rotated", single_video) & response == "ArrowLeft" ~ "CorrectRejection",
    TRUE ~ ""  # Default value for other cases
  ))

#exclude participants
exclude_subjects_exp1 <- c("3507", "1579","3582","2232","1847","1468","1817","1270","1543","2450","2796","3592")

filtered_df_raw_data_detection <- df_raw_data_detection_sq_blink_rot %>%
  filter(!subject_code %in% exclude_subjects_exp1)

#calculate hit rate
result_Hits <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    Hits = sum(Detection == 'Hit'),
    miss = sum(Detection == 'Miss'),
  ) %>%
  mutate(Hit_Rate = Hits / (Hits + miss))

exp1_subj_code <- result_Hits$subject_code
exp1_subj_code <- as.data.frame(exp1_subj_code)
write.table(result_Hits$subject_code, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_subject_list_in_order.csv", sep = ",", row.names = FALSE,col.names  = FALSE)

result_FA <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    FalseAlarms = sum(Detection == 'FalseAlarm'),  # Count 'False Alarm' values per participant
    Corr_rejections = sum(Detection == 'CorrectRejection'), # Count '_rev' videos per participant
  ) %>%
  mutate(FalseAlarm_Rate = FalseAlarms / (FalseAlarms+Corr_rejections)) 

result_Hits <- result_Hits %>%
  mutate(Number_of_Signals = Hits / Hit_Rate)
plot(result_Hits$Hit_Rate)
result_FA <- result_FA %>%
  mutate(Number_of_Signals_FA = FalseAlarms / FalseAlarm_Rate)
plot(result_FA$FalseAlarm_Rate)

result_Hits$Hit_Rate[result_Hits$Hit_Rate == 0] <- 0.5 / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 0]
result_Hits$Hit_Rate[result_Hits$Hit_Rate == 1] <- (result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1] - 0.5) / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1]

result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 0] <- 0.5 / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 0]
result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 1] <- (result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1] - 0.5) / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1]


write.table(result_Hits$Number_of_Signals, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_Dprime_sequence_noblink_upright_hitrate_numsignals.csv", sep = ",", row.names = FALSE, col.names  = FALSE)
write.table(result_FA$Number_of_Signals_FA, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_Dprime_sequence_noblink_upright_fa_numsignals.csv", sep = ",", row.names = FALSE, col.names  = FALSE)


write.table(result_Hits$Hit_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_Dprime_sequence_blink_rotated_hit_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)
write.table(result_FA$FalseAlarm_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_Dprime_sequence_blink_rotated_FA_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)

#target = sequence-noblink rotated
df_raw_data_detection_sq_no_blink_rot <- df_raw_data %>%
  mutate(Detection = case_when(
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4_rotated", single_video) & response == "ArrowRight" ~ "Hit",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4_rotated", single_video) & response == "ArrowLeft" ~ "Miss",
    grepl("_rev3000-NON-BLINK_edited.mp4_rotated", single_video) & response == "ArrowRight" ~ "FalseAlarm",
    grepl("_rev3000-NON-BLINK_edited.mp4_rotated", single_video) & response == "ArrowLeft" ~ "CorrectRejection",
    TRUE ~ ""  # Default value for other cases
  ))

exclude_subjects_exp1 <- c("3507", "1579","3582","2232","1847","1468","1817","1270","1543","2450","2796","3592")

#exclude participants
filtered_df_raw_data_detection <- df_raw_data_detection_sq_no_blink_rot %>%
  filter(!subject_code %in% exclude_subjects_exp1)

#calculate hit rate
result_Hits <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    Hits = sum(Detection == 'Hit'),
    miss = sum(Detection == 'Miss'),
  ) %>%
  mutate(Hit_Rate = Hits / (Hits + miss))

#calculate false alarm rate
result_FA <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    FalseAlarms = sum(Detection == 'FalseAlarm'),  # Count 'False Alarm' values per participant
    Corr_rejections = sum(Detection == 'CorrectRejection'), # Count '_rev' videos per participant
  ) %>%
  mutate(FalseAlarm_Rate = FalseAlarms / (FalseAlarms+Corr_rejections)) 

result_Hits <- result_Hits %>%
  mutate(Number_of_Signals = Hits / Hit_Rate)

result_FA <- result_FA %>%
  mutate(Number_of_Signals_FA = FalseAlarms / FalseAlarm_Rate)

result_Hits$Hit_Rate[result_Hits$Hit_Rate == 0] <- 0.5 / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 0]
result_Hits$Hit_Rate[result_Hits$Hit_Rate == 1] <- (result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1] - 0.5) / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1]

result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 0] <- 0.5 / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 0]
result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 1] <- (result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1] - 0.5) / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1]


write.table(result_Hits$Hit_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_Dprime_sequence_noblink_rotated_hit_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)
write.table(result_FA$FalseAlarm_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_Dprime_sequence_noblink_rotated_FA_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)

#target = sequence-blink upright

df_raw_data_detection_sq_blink_up <- df_raw_data %>%
  mutate(Detection = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & response == "ArrowRight" & !grepl('rotated', single_video) ~ "Hit",
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & response == "ArrowLeft" & !grepl('rotated', single_video) ~ "Miss",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & response == "ArrowRight" & !grepl('rotated', single_video) ~ "FalseAlarm",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & response == "ArrowLeft" & !grepl('rotated', single_video) ~ "CorrectRejection",
    TRUE ~ ""  # Default value for other cases
  ))


exclude_subjects_exp1 <- c("3507", "1579","3582","2232","1847","1468","1817","1270","1543","2450","2796","3592")

filtered_df_raw_data_detection <- df_raw_data_detection_sq_blink_up %>%
  filter(!subject_code %in% exclude_subjects_exp1)

result_Hits <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    Hits = sum(Detection == 'Hit'),
    miss = sum(Detection == 'Miss'),
  ) %>%
  mutate(Hit_Rate = Hits / (Hits + miss))

result_FA <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    FalseAlarms = sum(Detection == 'FalseAlarm'),  # Count 'False Alarm' values per participant
    Corr_rejections = sum(Detection == 'CorrectRejection'), # Count '_rev' videos per participant
  ) %>%
  mutate(FalseAlarm_Rate = FalseAlarms / (FalseAlarms+Corr_rejections)) 

result_Hits <- result_Hits %>%
  mutate(Number_of_Signals = Hits / Hit_Rate)

result_FA <- result_FA %>%
  mutate(Number_of_Signals_FA = FalseAlarms / FalseAlarm_Rate)

result_Hits$Hit_Rate[result_Hits$Hit_Rate == 0] <- 0.5 / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 0]
result_Hits$Hit_Rate[result_Hits$Hit_Rate == 1] <- (result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1] - 0.5) / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1]

result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 0] <- 0.5 / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 0]
result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 1] <- (result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1] - 0.5) / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1]

write.table(result_Hits$Hit_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_Dprime_sequence_blink_upright_hit_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)
write.table(result_FA$FalseAlarm_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_Dprime_sequence_blink_upright_FA_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)

#target = sequence-noblink upright
df_raw_data_detection_sq_no_blink_up <- df_raw_data %>%
  mutate(Detection = case_when(
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video) & response == "ArrowRight" & !grepl('rotated', single_video) ~ "Hit",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video) & response == "ArrowLeft" & !grepl('rotated', single_video) ~ "Miss",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & response == "ArrowRight" & !grepl('rotated', single_video) ~ "FalseAlarm",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & response == "ArrowLeft" & !grepl('rotated', single_video) ~ "CorrectRejection",
    TRUE ~ ""  # Default value for other cases
  ))

exclude_subjects_exp1 <- c("3507", "1579","3582","2232","1847","1468","1817","1270","1543","2450","2796","3592")

filtered_df_raw_data_detection <- df_raw_data_detection_sq_no_blink_up %>%
  filter(!subject_code %in% exclude_subjects_exp1)

result_Hits <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    Hits = sum(Detection == 'Hit'),
    miss = sum(Detection == 'Miss'),
  ) %>%
  mutate(Hit_Rate = Hits / (Hits + miss))

result_FA <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    FalseAlarms = sum(Detection == 'FalseAlarm'),  # Count 'False Alarm' values per participant
    Corr_rejections = sum(Detection == 'CorrectRejection'), # Count '_rev' videos per participant
  ) %>%
  mutate(FalseAlarm_Rate = FalseAlarms / (FalseAlarms+Corr_rejections)) 

result_Hits <- result_Hits %>%
  mutate(Number_of_Signals = Hits / Hit_Rate)

result_FA <- result_FA %>%
  mutate(Number_of_Signals_FA = FalseAlarms / FalseAlarm_Rate)

result_Hits$Hit_Rate[result_Hits$Hit_Rate == 0] <- 0.5 / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 0]
result_Hits$Hit_Rate[result_Hits$Hit_Rate == 1] <- (result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1] - 0.5) / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1]

result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 0] <- 0.5 / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 0]
result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 1] <- (result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1] - 0.5) / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1]


write.table(result_Hits$Hit_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_Dprime_sequence_noblink_upright_hit_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)
write.table(result_FA$FalseAlarm_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_Dprime_sequence_noblink_upright_FA_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)

##
#Experiment 2

exp2_df_raw_data <- read_excel("/Users/gulcelale/Desktop/dynamic face perception2/exp_2_new/Exp2_categorical_inc answers.xlsx")

#target = sequence blink rotated
exp2_df_raw_data_detection_sq_blink_rot <- exp2_df_raw_data %>%
  mutate(Detection = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4_rotated", single_video) & response == "ArrowRight" ~ "Hit",
    grepl("_normal_meannorm3000-BLINK_edited.mp4_rotated", single_video) & response == "ArrowLeft" ~ "Miss",
    grepl("_rev3000-BLINK_edited.mp4_rotated", single_video) & response == "ArrowRight" ~ "FalseAlarm",
    grepl("_rev3000-BLINK_edited.mp4_rotated", single_video) & response == "ArrowLeft" ~ "CorrectRejection",
    TRUE ~ ""  # Default value for other cases
  ))

#exclude participants
exclude_subjects_exp2 <- c("1238", "2180","2450","2762","2783","2797","2816","2824","2837","4885","6949","7002")

filtered_df_raw_data_detection <- exp2_df_raw_data_detection_sq_blink_rot %>%
  filter(!subject_code %in% exclude_subjects_exp2)

#calculate hit rate
result_Hits <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    Hits = sum(Detection == 'Hit'),
    miss = sum(Detection == 'Miss'),
  ) %>%
  mutate(Hit_Rate = Hits / (Hits + miss))
exp2_subj_code <- result_Hits$subject_code
exp2_subj_code <- as.data.frame(exp2_subj_code)

result_FA <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    FalseAlarms = sum(Detection == 'FalseAlarm'),  # Count 'False Alarm' values per participant
    Corr_rejections = sum(Detection == 'CorrectRejection'), # Count '_rev' videos per participant
  ) %>%
  mutate(FalseAlarm_Rate = FalseAlarms / (FalseAlarms+Corr_rejections)) 

result_Hits <- result_Hits %>%
  mutate(Number_of_Signals = Hits / Hit_Rate)

result_FA <- result_FA %>%
  mutate(Number_of_Signals_FA = FalseAlarms / FalseAlarm_Rate)

result_Hits$Hit_Rate[result_Hits$Hit_Rate == 0] <- 0.5 / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 0]
result_Hits$Hit_Rate[result_Hits$Hit_Rate == 1] <- (result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1] - 0.5) / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1]

result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 0] <- 0.5 / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 0]
result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 1] <- (result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1] - 0.5) / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1]


write.table(result_Hits$Hit_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp2_Dprime_sequence_blink_rotated_hit_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)
write.table(result_FA$FalseAlarm_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp2_Dprime_sequence_blink_rotated_FA_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)


#target = sequence-noblink rotated
exp2_df_raw_data_detection_sq_no_blink_rot <- exp2_df_raw_data %>%
  mutate(Detection = case_when(
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4_rotated", single_video) & response == "ArrowRight" ~ "Hit",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4_rotated", single_video) & response == "ArrowLeft" ~ "Miss",
    grepl("_rev3000-NON-BLINK_edited.mp4_rotated", single_video) & response == "ArrowRight" ~ "FalseAlarm",
    grepl("_rev3000-NON-BLINK_edited.mp4_rotated", single_video) & response == "ArrowLeft" ~ "CorrectRejection",
    TRUE ~ ""  # Default value for other cases
  ))

#exclude participants
exclude_subjects_exp2 <- c("1238", "2180","2450","2762","2783","2797","2816","2824","2837","4885","6949","7002")

#exclude participants
filtered_df_raw_data_detection <- exp2_df_raw_data_detection_sq_no_blink_rot %>%
  filter(!subject_code %in% exclude_subjects_exp2)

#calculate hit rate
result_Hits <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    Hits = sum(Detection == 'Hit'),
    miss = sum(Detection == 'Miss'),
  ) %>%
  mutate(Hit_Rate = Hits / (Hits + miss))

#calculate false alarm rate
result_FA <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    FalseAlarms = sum(Detection == 'FalseAlarm'),  # Count 'False Alarm' values per participant
    Corr_rejections = sum(Detection == 'CorrectRejection'), # Count '_rev' videos per participant
  ) %>%
  mutate(FalseAlarm_Rate = FalseAlarms / (FalseAlarms+Corr_rejections)) 

result_Hits <- result_Hits %>%
  mutate(Number_of_Signals = Hits / Hit_Rate)

result_FA <- result_FA %>%
  mutate(Number_of_Signals_FA = FalseAlarms / FalseAlarm_Rate)

result_Hits$Hit_Rate[result_Hits$Hit_Rate == 0] <- 0.5 / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 0]
result_Hits$Hit_Rate[result_Hits$Hit_Rate == 1] <- (result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1] - 0.5) / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1]

result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 0] <- 0.5 / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 0]
result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 1] <- (result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1] - 0.5) / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1]

write.table(result_Hits$subject_code, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp2_subject_list_in_order.csv", sep = ",", row.names = FALSE,col.names  = FALSE)


write.table(result_Hits$Hit_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp2_Dprime_sequence_noblink_rotated_hit_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)
write.table(result_FA$FalseAlarm_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp2_Dprime_sequence_noblink_rotated_FA_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)

#target = sequence-blink upright

exp2_df_raw_data_detection_sq_blink_up <- exp2_df_raw_data %>%
  mutate(Detection = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & response == "ArrowRight" & !grepl('rotated', single_video) ~ "Hit",
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & response == "ArrowLeft" & !grepl('rotated', single_video) ~ "Miss",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & response == "ArrowRight" & !grepl('rotated', single_video) ~ "FalseAlarm",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & response == "ArrowLeft" & !grepl('rotated', single_video) ~ "CorrectRejection",
    TRUE ~ ""  # Default value for other cases
  ))

filtered_df_raw_data_detection <- exp2_df_raw_data_detection_sq_blink_up %>%
  filter(!subject_code %in% exclude_subjects_exp2)

result_Hits <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    Hits = sum(Detection == 'Hit'),
    miss = sum(Detection == 'Miss'),
  ) %>%
  mutate(Hit_Rate = Hits / (Hits + miss))

result_FA <- filtered_df_raw_data_detection %>%
  group_by(subject_code) %>%  # Group by participant
  summarise(
    FalseAlarms = sum(Detection == 'FalseAlarm'),  # Count 'False Alarm' values per participant
    Corr_rejections = sum(Detection == 'CorrectRejection'), # Count '_rev' videos per participant
  ) %>%
  mutate(FalseAlarm_Rate = FalseAlarms / (FalseAlarms+Corr_rejections)) 


result_Hits <- result_Hits %>%
  mutate(Number_of_Signals = Hits / Hit_Rate)

result_FA <- result_FA %>%
  mutate(Number_of_Signals_FA = FalseAlarms / FalseAlarm_Rate)

result_Hits$Hit_Rate[result_Hits$Hit_Rate == 0] <- 0.5 / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 0]
result_Hits$Hit_Rate[result_Hits$Hit_Rate == 1] <- (result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1] - 0.5) / result_Hits$Number_of_Signals[result_Hits$Hit_Rate == 1]

result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 0] <- 0.5 / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 0]
result_FA$FalseAlarm_Rate[result_FA$FalseAlarm_Rate == 1] <- (result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1] - 0.5) / result_FA$Number_of_Signals_FA[result_FA$FalseAlarm_Rate == 1]


write.table(result_Hits$Hit_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp2_Dprime_sequence_blink_upright_hit_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)
write.table(result_FA$FalseAlarm_Rate, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp2_Dprime_sequence_blink_upright_FA_rate.csv", sep = ",", row.names = FALSE,col.names  = FALSE)


seq_blink_rotated_exp1 <- read.csv("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/dprime/Exp1_Dprime_C_sequence_blink_rotated.csv", sep = ",",header = FALSE, row.names =  NULL) 
seq_noblink_rotated_exp1 <- read.csv("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/dprime/Exp1_Dprime_C_sequence_noblink_rotated.csv", sep = ",",header = FALSE, row.names =  NULL) 
seq_blink_upright_exp1 <- read.csv("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/dprime/Exp1_Dprime_C_sequence_blink_upright.csv", sep = ",",header = FALSE, row.names =  NULL) 
seq_noblink_upright_exp1 <- read.csv("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/dprime/Exp1_Dprime_C_sequence_noblink_upright.csv", sep = ",",header = FALSE, row.names =  NULL) 

seq_blink_rotated_exp2 <- read.csv("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/dprime/Exp2_Dprime_C_sequence_blink_rotated.csv", sep = ",",header = FALSE, row.names =  NULL) 
seq_noblink_rotated_exp2 <- read.csv("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/dprime/Exp2_Dprime_C_sequence_noblink_rotated.csv", sep = ",",header = FALSE, row.names =  NULL) 
seq_blink_upright_exp2 <- read.csv("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/dprime/Exp2_Dprime_C_sequence_blink_upright.csv", sep = ",",header = FALSE, row.names =  NULL) 
seq_noblink_upright_exp2 <- read.csv("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/dprime/Exp2_Dprime_C_sequence_noblink_upright.csv", sep = ",",header = FALSE, row.names =  NULL) 

combined_df_exp1 <-  cbind(seq_blink_upright_exp1$V1, seq_noblink_upright_exp1$V1,seq_blink_rotated_exp1$V1, seq_noblink_rotated_exp1$V1)
colnames(combined_df_exp1) <- c("Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated")
write.table(combined_df_exp1, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_Dprime_all_conditions_before_exclusion_NEW.csv", sep = ",", row.names = FALSE,col.names  = TRUE)

boxplot(combined_df_exp1, main = "d-prime values for experiment 1",names = c("Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated"),xlab = "Conditions", ylab = "d-prime")

combined_df_exp2 <-  cbind(seq_blink_upright_exp2$V1, seq_noblink_upright_exp2$V1,seq_blink_rotated_exp2$V1, seq_noblink_rotated_exp2$V1)
colnames(combined_df_exp2) <- c("Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated")
write.table(combined_df_exp2, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp2_Dprime_all_conditions_before_exclusion_NEW.xlsx", sep = ",", row.names = FALSE,col.names  = TRUE)
write_xlsx(combined_df_exp2, 'Exp2_Dprime_all_conditions_before_exclusion.xlsx')

combined_df_exp2 <- as.data.frame(combined_df_exp2)
write.table(combined_df_exp2, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp2_Dprime_all_conditions_before_exclusion_NEW.csv", row.names = FALSE,col.names  = TRUE)

boxplot(combined_df_exp2, main = "d-prime values for experiment 2",names = c("Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated"),xlab = "Conditions", ylab = "d-prime")


combined_df_c_exp1 <-  cbind(seq_blink_upright_exp1$V2, seq_noblink_upright_exp1$V2,seq_blink_rotated_exp1$V2, seq_noblink_rotated_exp1$V2)
boxplot(combined_df_c_exp1, main = "c values for experiment 1",names = c("Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated"),xlab = "Conditions", ylab = "C")

combined_df_c_exp2 <-  cbind(seq_blink_upright_exp2$V2, seq_noblink_upright_exp2$V2,seq_blink_rotated_exp2$V2, seq_noblink_rotated_exp2$V2)
boxplot(combined_df_c_exp2, main = "c values for experiment 2",names = c("Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated"),xlab = "Conditions", ylab = "C")


participant_means <- rowMeans(combined_df_exp1)

new_df_exp1 <- data.frame(Average = participant_means)
sum(which=(new_df_exp1$Average <0))

participant_means <- rowMeans(combined_df_exp2)

new_df_exp2 <- data.frame(Average = participant_means)
sum(which=(new_df_exp2$Average <0))


exclude_row <- function(row) {
  max_abs_value <- max(abs(row))
  max_value <- max(row)
  
  if (max_abs_value > max_value) {
    return(TRUE)  # exclude the row
  } else if (all(row < 0)) {
    return(TRUE)  # exclude the row
  } else {
    return(FALSE)  # keep the row
  }
}

all_exp <- rbind(combined_df_subj_codes_exp1,combined_df_subj_codes_exp2)
all_exp <- as.data.frame(all_exp)
colnames(all_exp) <- c("subject_code","Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated")
combined_df_subj_codes_exp2 <- as.data.frame(combined_df_subj_codes_exp2)
colnames(combined_df_subj_codes_exp2) <- c("subject_code","Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated")

write_xlsx(combined_df_subj_codes_exp2, 'Dprime_exp2_before_exclusion.xlsx')

write.table(all_exp, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Dprime_all_experiments_before_exclusion.xlsx", sep = ",", row.names = FALSE,col.names  = TRUE)

subject_code_exp1<- as.data.frame(result_Hits$subject_code)

combined_df_subj_codes_exp1 <- cbind(subject_code_exp1, combined_df_exp1) 

#apply the function to each row 
exclude_rows_exp1 <- apply(combined_df_exp1, 1, exclude_row)
write_xlsx(combined_df_subj_codes_exp1, 'Exp1_Dprime_all_conditions_before_exclusion_NEW.xlsx')

filtered_combined_df_subj_codes_exp1 <- combined_df_subj_codes_exp1[!exclude_rows_exp1, ]
excluded_subject_codes_exp1 <- combined_df_subj_codes_exp1[exclude_rows_exp1, 1]

filtered_data_exp1 <- combined_df_exp1[!exclude_rows_exp1, ]
filtered_data_exp1_2 <- combined_df_subj_codes_exp1[!exclude_rows_exp1, ]

colnames(filtered_data_exp1) <- c("Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated")
write.table(filtered_data_exp1, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_Dprime_all_conditions_after_exclusion_NEW.csv", sep = ",", row.names = FALSE,col.names  = TRUE)
boxplot(filtered_data_exp1, main = "dprime values for experiment 1 after exclusion",names = c("Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated"),xlab = "Conditions", ylab = "d-prime values")

filtered_data_exp1_2 <- as.data.frame(filtered_data_exp1_2)
write_xlsx(filtered_data_exp1_2, 'Exp1_Dprime_all_conditions_after_exclusion.xlsx')

#run results_Hits for exp2 again
subject_code_exp2<- as.data.frame(result_Hits$subject_code)
understood_data <- read_excel("/Users/gulcelale/Documents/ENS210/lab-6-lalegulce/_lab/6/Exp2_Dprime_all_conditions_28_understood.xlsx")
subject_code_understood_data<- as.data.frame(understood_data$subject_code)

colnames(subject_code_understood_data) <- c("subjcode")
colnames(excluded_subject_codes_exp2) <- c("subjcode")
excluded_subject_codes_exp2 <- data.frame(excluded_subject_codes_exp2)

#choose 3 random numbers of understanding
set.seed(123) 
selected_numbers <- subject_code_exp2 %>%
  filter(!subject_code_exp2 %in% codes) %>%
  sample_n(3)
selected_numbers

combined_df_subj_codes_exp2 <- cbind(subject_code_exp2, combined_df_exp2) 
write_xlsx(combined_df_subj_codes_exp2, 'Exp2_Dprime_all_conditions_before_exclusion_NEW.xlsx')

exclude_rows_exp2 <- apply(combined_df_exp2, 1, exclude_row)
excluded_subject_codes_exp2 <- combined_df_subj_codes_exp2[exclude_rows_exp2, 1]

filtered_combined_df_subj_codes_exp2 <- combined_df_subj_codes_exp2[!exclude_rows_exp2, ]

filtered_data_exp2 <- combined_df_exp2[!exclude_rows_exp2, ]

filtered_data_exp2_2 <- combined_df_subj_codes_exp2[!exclude_rows_exp2, ]

filtered_data_exp2_2 <- as.data.frame(filtered_data_exp2_2)

write_xlsx(result_Hits$subject_code, 'Exp2_Dprime_all_conditions_after_exclusion.xlsx')

colnames(filtered_data_exp2) <- c("Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated")
write.table(filtered_data_exp2, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp2_Dprime_all_conditions_after_exclusion_NEW.csv", sep = ",", row.names = FALSE,col.names  = TRUE)
boxplot(filtered_data_exp2, main = "dprime values for experiment 2 after exclusion",names = c("Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated"),xlab = "Conditions", ylab = "d-prime values")

excluded_subject_codes2 <- combined_df_subj_codes_exp2[excluded_rows_exp2, 1]

all_exp <- rbind(filtered_data_exp1_2,filtered_data_exp2_2)
write.table(all_exp, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Dprime_all_experiments_after_exclusion.xslx", sep = ",", row.names = FALSE,col.names  = TRUE)
write.table(filtered_combined_df_subj_codes_exp2, "/Users/gulcelale/Desktop/dynamic face perception2/exp1_new/exp2_new/Exp2_Dprime_all_conditions_after_exclusion_subject_code_NEW.csv", sep = ",", row.names = FALSE,col.names  = TRUE)




##criterion C
#run result_Hits for exp2 again!
combined_df_c_exp2_subj_codes <- cbind(result_Hits$subject_code, combined_df_c_exp2) 

combined_df_c_exp1_subj_codes <- cbind(result_Hits$subject_code, combined_df_c_exp1) 

filtered_combined_df_subj_codes_exp1 <-  as.data.frame(filtered_combined_df_subj_codes_exp1)

subject_codes_to_keep <- filtered_combined_df_subj_codes_exp1$V1

combined_df_c_exp1_subj_codes <-  as.data.frame(combined_df_c_exp1_subj_codes)

filtered_combined_df_c_exp1_subj_codes <- combined_df_c_exp1_subj_codes[
  combined_df_c_exp1_subj_codes$V1 %in% subject_codes_to_keep,
]


filtered_combined_df_subj_codes_exp2 <-  as.data.frame(filtered_combined_df_subj_codes_exp2)

subject_codes_to_keep <- filtered_combined_df_subj_codes_exp2$V1

combined_df_c_exp2_subj_codes <-  as.data.frame(combined_df_c_exp2_subj_codes)

filtered_combined_df_c_exp2_subj_codes <- combined_df_c_exp2_subj_codes[
  combined_df_c_exp2_subj_codes$V1 %in% subject_codes_to_keep,
]

colnames(filtered_combined_df_c_exp1_subj_codes) <- c("subject_code","Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated")

colnames(filtered_combined_df_c_exp2_subj_codes) <- c("subject_code","Blink-Upright", "No-Blink-Upright", "Blink-Rotated", "No-Blink-Rotated")

write.table(filtered_combined_df_c_exp2_subj_codes, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp2_c_participants_excluded.csv", sep = ",", row.names = FALSE,col.names  = TRUE)
write.table(filtered_combined_df_c_exp1_subj_codes, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_c_participants_excluded.csv", sep = ",", row.names = FALSE,col.names  = TRUE)

write.table(combined_df_c_exp2_subj_codes, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp2_c_participants_beforeexclusion.csv", sep = ",", row.names = FALSE,col.names  = TRUE)
write.table(combined_df_c_exp1_subj_codes, "/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/data_analys/exp1/Exp1_c_participants_beforeexclusion.csv", sep = ",", row.names = FALSE,col.names  = TRUE)

write_xlsx(combined_df_c_exp2_subj_codes, 'Exp2_C_all_conditions_before_exclusion_NEW.xlsx')
write_xlsx(combined_df_c_exp1_subj_codes, 'Exp1_C_all_conditions_before_exclusion_NEW.xlsx')


