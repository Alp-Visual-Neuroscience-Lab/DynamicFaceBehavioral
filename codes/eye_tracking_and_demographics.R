library(dplyr)
library(ggplot2)
library(readxl)
require(MASS)
library(tidyr)
library(patchwork)
#install.packages("patchwork")

#experiment 1
df <- read.csv("/Users/gulcelale/Desktop/dynamic face perception2/timeseries_exp1.csv",sep = ";") 
exp_1_raw_data <- read_excel("/Users/gulcelale/Desktop/dynamic face perception2/data_62participants.xlsx")
tbl1 <- table(exp_1_raw_data$subject_code,exp_1_raw_data$single_video)
tbl1 <- as.data.frame(tbl1)
which(tbl1$Freq!=1) #all shown once
#exp_1_raw_data <- read_excel("/Users/gulcelale/Documents/ENS210/lab-6-lalegulce/_lab/6/Exp1_Dprime_all_conditions_after_exclusion.xlsx")
length(unique(df$Exp_Subject_Id))
times_id<-unique(df$Exp_Subject_Id) 
rawdata_id <- unique(exp_1_raw_data$exp_subject_id) 
times_id[!(times_id %in% rawdata_id)]
colnames(df)
df_filtered<- df %>%
  filter(Exp_Subject_Id != 197878)

length(unique(df_filtered$Exp_Subject_Id)) #62

#excluded participants
exclude_subjects_exp1 <- c("3507", "1579","3582","2232","1847","1468","1817","1270","1543","2450","2796","3592","848", "3609", "3631", "3646")
#extract Exp_Subject_Ids
filtered_rows <- exp_1_raw_data[exp_1_raw_data$subject_code %in% exclude_subjects_exp1, ]
unique(filtered_rows$exp_subject_id)

# Access the "Exp_Id" column in the filtered rows
respective_exp_ids <- filtered_rows$exp_subject_id
unique(respective_exp_ids)

df_all_filtered <- subset(df_filtered, !(Exp_Subject_Id %in% respective_exp_ids))
length(unique(df_all_filtered$Exp_Subject_Id))

unique_subjectcode <- unique(df_all_filtered$Exp_Subject_Id)

mean_data <- df_all_filtered %>%
  group_by(Exp_Subject_Id) %>%
  summarize(mean_X = mean(value_X,na.rm = TRUE), mean_Y = mean(value_Y,na.rm = TRUE))

 
write_xlsx(mean_data, "/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_1/eyetracking_data.xlsx")

colnames(exp_1_raw_data)[colnames(exp_1_raw_data) == "rec_session_id"] <- "Rec_Session_Id"
merged_df <- merge(exp_1_raw_data, df_all_filtered, by = c("Trial_Id", "Rec_Session_Id"))

length(unique(merged_df$Rec_Session_Id)) #46


merged_df_upright <- merged_df %>%
  mutate(Condition = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video)  & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    TRUE ~ ""  # Default value for other cases
  ))
merged_df_upright_filtered <- merged_df_upright%>% filter(Condition != "")

merged_df_inverted <- merged_df %>%
  mutate(Condition = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video)  & grepl('rotated', single_video) ~ "Inverted",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    TRUE ~ ""  # Default value for other cases
  ))
merged_df_inverted_filtered <- merged_df_inverted%>% filter(Condition != "")

upright_df <- merged_df_upright_filtered %>%
  select(Trial_Id, Rec_Session_Id, value_X, value_Y, single_video, Condition)

inverted_df <- merged_df_inverted_filtered %>%
  select(Trial_Id, Rec_Session_Id, value_X, value_Y, single_video, Condition)

mean_upright_data <- upright_df %>%
  group_by(Rec_Session_Id) %>%
  summarize(mean_X = mean(value_X,na.rm = TRUE), mean_Y = mean(value_Y,na.rm = TRUE))

mean_inverted_data <- inverted_df %>%
  group_by(Rec_Session_Id) %>%
  summarize(mean_X = mean(value_X,na.rm = TRUE), mean_Y = mean(value_Y,na.rm = TRUE))

write_xlsx(mean_upright_data, "/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_1/eyetracking_upright.xlsx")
write_xlsx(mean_inverted_data, "/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_1/eyetracking_inverted.xlsx")

# f1 <- kde2d(mean_data$mean_X, mean_data$mean_Y, n = 50, lims = c(0, 800, 0, 450))
# dens <- kde2d(mean_data$mean_X, mean_data$mean_Y)  # Overrode default bandwidth
# 
# #set the font  to Arial for the x and y-axis labels
# par(family = "Arial")
# 
# #create the filled contour plot 
# filled.contour(f1, xlab = "X Coordinates", ylab = "Y Coordinates")
# 
# #reset the font family to the default (if needed) after the plot
# #par(family = "default")
# 
# save(filename="/Users/gulcelale/Desktop/dynamic face perception2/figures/eyetracking_exp1.png",res = 600)
# #png("eyetracking_exp1.png", res = 600)
# #dev.off()


#experiment 2
df2 <- read.csv("/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/data_timeseries_exp2/timeseries_values.csv",sep = ";",row.names=NULL)
#exp_2_raw_data <- read_excel("/Users/gulcelale/Desktop/dynamic face perception2/exp_2_new/Exp2_raw_data.xlsx")
exp_2_raw_data <- read_excel("/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/Experiment_2_raw_data.xlsx")
tbl <- table(exp_2_raw_data$rec_session_id,exp_2_raw_data$single_video)
tbl <- as.data.frame(tbl)
which(tbl$Freq!=1)

resulttbl <- tbl %>%
  group_by(Var1) %>%
  summarise(num_videos = sum(Freq))

View(resulttbl)
which(resulttbl$num_videos!=416) 

tbl2 <- table(exp_2_raw_data2$subject_code,exp_2_raw_data2$single_video)
tbl2 <- as.data.frame(tbl2)

which(tbl2$Freq!=2)
#df22 <- read.delim("/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/data_timeseries_exp2/trials_and_sessions.csv",sep = ",",row.names=NULL)
#which((df2$Exp_Subject_Id == exp_2_raw_data$exp_subject_id)==TRUE)

length(unique(df2$Exp_Subject_Id)) #61

length(unique(exp_2_raw_data$exp_subject_id)) #90

exclude_subjects_exp2 <- c("1238", "2180","2450","2762","2783","2797","2816","2824","2837","4885","6949","7002","2294", "2313", "2714", "2906", "3455","5026", "5910")

#c("351265", "413574","436648","346553","419336","433836","352769","437296","433324","4885","6949","7002","2294", "2313", "2714", "2906", "3455","5026", "5910")

#exclude subjects 
exp2_data_filtered <- exp_2_raw_data[!exp_2_raw_data$subject_code %in% exclude_subjects_exp2, ]

length(unique(exp2_data_filtered$exp_subject_id)) #71

exp_subj_ids <- exp2_data_filtered$exp_subject_id
unique_respective_exp_ids <- unique(exp_subj_ids)
unique_respective_exp_ids <- as.character(unique_respective_exp_ids)
df2_all_filtered <- df2[df2$Exp_Subject_Id %in% unique_respective_exp_ids, ]

length(unique(df2_all_filtered$Exp_Subject_Id)) #47

mean_data_exp2 <- df2_all_filtered %>%
  group_by(Exp_Subject_Id) %>%
  summarize(mean_X = mean(value_X,na.rm = TRUE), mean_Y = mean(value_Y,na.rm = TRUE))

write_xlsx(mean_data_exp2, "/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/eyetracking_data.xlsx")

colnames(exp_2_raw_data)[colnames(exp_2_raw_data) == "rec_session_id"] <- "Rec_Session_Id"
merged_df_exp2 <- merge(exp_2_raw_data, df2_all_filtered, by = c("Trial_Id", "Rec_Session_Id"))
length(unique(merged_df_exp2$Rec_Session_Id)) #47


merged_df_exp2_upright <- merged_df_exp2 %>%
  mutate(Condition = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video)  & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & !grepl('rotated', single_video) ~ "Upright",
    TRUE ~ ""  # Default value for other cases
  ))

merged_df_exp2_upright_filtered <- merged_df_exp2_upright%>% filter(Condition != "")

merged_df_exp2_inverted <- merged_df_exp2 %>%
  mutate(Condition = case_when(
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    grepl("_normal_meannorm3000-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    grepl("_rev3000-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    grepl("_normal_meannorm3000-NON-BLINK_edited.mp4", single_video)  & grepl('rotated', single_video) ~ "Inverted",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted",
    grepl("_rev3000-NON-BLINK_edited.mp4", single_video) & grepl('rotated', single_video) ~ "Inverted", 
    TRUE ~ ""  # if non of the conditions are satisfied put ""
  ))
merged_df_exp2_inverted_filtered <- merged_df_exp2_inverted%>% filter(Condition != "")

upright_df_exp2 <- merged_df_exp2_upright_filtered %>%
  select(Trial_Id, Rec_Session_Id, value_X, value_Y, single_video, Condition)

inverted_df_exp2 <- merged_df_exp2_inverted_filtered %>%
  select(Trial_Id, Rec_Session_Id, value_X, value_Y, single_video, Condition)

mean_upright_data_exp2 <- upright_df_exp2 %>%
  group_by(Rec_Session_Id) %>%
  summarize(mean_X = mean(value_X,na.rm = TRUE), mean_Y = mean(value_Y,na.rm = TRUE))

mean_inverted_data_exp2 <- inverted_df_exp2 %>%
  group_by(Rec_Session_Id) %>%
  summarize(mean_X = mean(value_X,na.rm = TRUE), mean_Y = mean(value_Y,na.rm = TRUE))

write_xlsx(mean_upright_data_exp2, "/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/eyetracking_upright_exp2.xlsx")
write_xlsx(mean_inverted_data_exp2, "/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/eyetracking_inverted_exp2.xlsx")





# f1 <- kde2d(mean_data_exp2$mean_X, mean_data_exp2$mean_Y, n = 50, lims = c(0, 800, 0, 450))
# dens <- kde2d(mean_data_exp2$mean_X, mean_data_exp2$mean_Y)  # Overrode default bandwidth
# 
# par(family = "Arial")
# 
# filled.contour(f1, xlab = "X Coordinates", ylab = "Y Coordinates")
# 
# #reset the font family to the default (if needed) after the plot
# #par(family = "default")
# 
# save(filename="/Users/gulcelale/Desktop/dynamic face perception2/figures/eyetracking_exp2.png",res = 600)
# ggplot(x)






#demographics
       
#experiment 2                   
exp_2_demog_data <- read_excel("/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/Demographics_exp2.xlsx")

exclude_subjects_exp2 <- c("1238", "2180","2450","2762","2783","2797","2816","2824","2837","4885","6949","7002","2294", "2313", "2714", "2906", "3455","5026", "5910")
exp_2_demog_data_filtered <- subset(exp_2_demog_data, !(exp_2_demog_data$subject_code %in% exclude_subjects_exp2))

exp2_female_data <- exp_2_demog_data_filtered[exp_2_demog_data_filtered$selected_gender == "female", ]
mean(exp2_female_data$selected_age)
sd(exp2_female_data$selected_age)
exp2_male_data <- exp_2_demog_data_filtered[exp_2_demog_data_filtered$selected_gender == "male", ]
mean(exp2_male_data$selected_age)
sd(exp2_male_data$selected_age)

#experiment 1
exp_1_data <- read_excel("/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_1/data_experiment_1_62_participants_raw.xlsx")
exp_1_demog_data <- exp_1_data[ , c("selected_age", "selected_gender", "subject_code")]                

exclude_subjects_exp1 <-  c("3507", "1579","3582","2232","1847","1468","1817","1270","1543","2450","2796","3592","848", "3609", "3631", "3646")

exp_1_demog_data_filtered <- subset(exp_1_demog_data, !(exp_1_demog_data$subject_code %in% exclude_subjects_exp1))

length(unique(exp_1_demog_data_filtered$subject_code))


summary_stats <- aggregate(selected_age ~ selected_gender + subject_code, data = exp_1_demog_subj_data_filtered, FUN = mean)

exp1_female_data <- summary_stats[summary_stats$selected_gender == "female", ]
mean(exp1_female_data$selected_age)
sd(exp1_female_data$selected_age)
exp1_male_data <- summary_stats[summary_stats$selected_gender == "male", ]
mean(exp1_male_data$selected_age)
sd(exp1_male_data$selected_age)
