#demographics

#experiment 2                   
exp_2_demog_data <- read_excel("/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/Demographics_exp2.xlsx")
length(unique(exp_2_demog_data$exp_subject_id))
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