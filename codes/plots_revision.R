library(tidyr)
library(ggplot2)
library(ggpubr)
library(writexl)
library(readxl)
library(patchwork)
install.packages('extrafont')
#library(extrafont)

font_import()

  loadfonts(device = "postscript")
loadfonts(device = "")

#EXP1
#reaction time
RT_df_exp1 = read_xlsx("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/RT_log_exp1_understanding_46_excluded_exp1.xlsx")

melted_data_rt_exp1 <- reshape2::melt(RT_df_exp1, id.vars = c("Subject_Code", "Understanding"))

melted_data_rt_exp1$Orientation <- factor(sub(".*-(Upright|Rotated)", "\\1", melted_data_rt_exp1$variable),
                                  levels = c("Upright", "Rotated"),labels = c("Upright", "Inverted"))
melted_data_rt_exp1$Blink <- factor(sub("(Blink|No-Blink)-.*", "\\1", melted_data_rt_exp1$variable),
                            levels = c("Blink", "No-Blink"),labels = c("With-Blink", "No-Blink"))

melted_data_rt_exp1$Understanding <- factor(melted_data_rt_exp1$Understanding, levels = c("1", "0"),labels = c("Understood", "Did not understand"))

# Define custom colors for Blink and NoBlink
blink_colors <- c("#00AFBB", "#E7B800")


bxp <- ggboxplot(
  melted_data_rt_exp1, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("Reaction Time \n (log10)")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Presence of an Eye Blink") + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
bxp

ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/RT_46_exp1.png", width = 6, height = 6, dpi = 600)



bxp <- ggboxplot(
  melted_data_rt_exp1, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("Reaction Time \n (log10)")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Presence of an Eye Blink") + 
  facet_grid(~Understanding) + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
bxp

ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/RT_exp1_understanding_7.12.png", width = 6, height = 6, dpi = 600)


#accuracy

accuracy_df_exp1 = read_xlsx("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/accuracy_understanding_46_excluded_exp1.xlsx")

melted_data_accuracy_exp1 <- reshape2::melt(accuracy_df_exp1, id.vars = c("Subject_Code", "Understanding"))

melted_data_accuracy_exp1$Orientation <- factor(sub(".*-(Upright|Rotated)", "\\1", melted_data_accuracy_exp1$variable),
                                          levels = c("Upright", "Rotated"),labels = c("Upright", "Inverted"))
melted_data_accuracy_exp1$Blink <- factor(sub("(Blink|No-Blink)-.*", "\\1", melted_data_accuracy_exp1$variable),
                                    levels = c("Blink", "No-Blink"),labels = c("With-Blink", "No-Blink"))

melted_data_accuracy_exp1$Understanding <- factor(melted_data_accuracy_exp1$Understanding, levels = c("1", "0"),labels = c("Understood", "Did not understand"))

# Define custom colors for Blink and NoBlink
blink_colors <- c("#00AFBB", "#E7B800")

bxp <- ggboxplot(
  melted_data_accuracy_exp1, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("Accuracy Rate")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Presence of an Eye Blink") + 
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "darkgray") + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
bxp

ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/accuracy_46_exp1.png", width = 6, height = 6, dpi = 600)

#dprime
dprime_df_exp1 = read_xlsx("/Users/gulcelale/Documents/ENS210/lab-6-lalegulce/_lab/6/Exp1_Dprime_all_conditions_after_exclusion_understanding.xlsx")

melted_data <- reshape2::melt(dprime_df_exp1, id.vars = c("subject_code", "Understanding"))

melted_data$Orientation <- factor(sub(".*-(Upright|Rotated)", "\\1", melted_data$variable),
                                  levels = c("Upright", "Rotated"),labels = c("Upright", "Inverted"))
melted_data$Blink <- factor(sub("(Blink|NoBlink)-.*", "\\1", melted_data$variable),
                            levels = c("Blink", "NoBlink"),labels = c("With-Blink", "No-Blink"))

melted_data$Understanding <- factor(melted_data$Understanding, levels = c("1", "0"),labels = c("Understood", "Did not understand"))

# Define custom colors for Blink and NoBlink
blink_colors <- c("#00AFBB", "#E7B800")


bxpdprime_understanding <- ggboxplot(
  melted_data, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("D-prime")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Presence of an Eye Blink") + 
  facet_grid(~Understanding) + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
bxpdprime_understanding

ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/dprime_exp1_understanding_7.12.png", width = 6, height = 6, dpi = 600)

bxp_dprime_exp1 <- ggboxplot(
  melted_data, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("D-prime")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Presence of an Eye Blink") + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12),
        axis.title.x = element_text(margin = margin(t = 10))) 
bxp_dprime_exp1

ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/dprime_exp1_7.12.png", width = 6, height = 6, dpi = 600)

#criterion C
c_df_exp1 = read_xlsx("/Users/gulcelale/Documents/ENS210/lab-6-lalegulce/_lab/6/Exp1_C_all_conditions_after_exclusion_NEW.xlsx")

melted_data_c_exp1 <- reshape2::melt(c_df_exp1, id.vars = c("subject_code", "Understanding"))

melted_data_c_exp1$Orientation <- factor(sub(".*-(Upright|Rotated)", "\\1", melted_data_c_exp1$variable),
                                    levels = c("Upright", "Rotated"),labels = c("Upright", "Inverted"))
melted_data_c_exp1$Blink <- factor(sub("(Blink|NoBlink)-.*", "\\1", melted_data_c_exp1$variable),
                              levels = c("Blink", "NoBlink"),labels = c("With-Blink", "No-Blink"))

melted_data_c_exp1$Understanding <- factor(melted_data_c_exp1$Understanding, levels = c("1", "0"),labels = c("Understood", "Did not understand"))

# Define custom colors for Blink and NoBlink
blink_colors <- c("#00AFBB", "#E7B800")

bxpc_1 <- ggboxplot(
  melted_data_c_exp1, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("Criteron (C)")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors) + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12),
        axis.title.x = element_text(margin = margin(t = 10)))
bxpc_1

ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/c_exp1_7.12.png", width = 6, height = 6, dpi = 600)

ggarrange(bxp_dprime_exp1, bxpc_1, ncol=2, nrow=1, labels = c("A","B"),common.legend = TRUE, legend="top")
ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/c_and_dprime_exp1_13.12.png", width = 6, height = 5, dpi = 600,bg = "white")


bxp_c_understanding <- ggboxplot(
  melted_data_c_exp1, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("Criterion (C)")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors,
                     name = "Presence of an Eye Blink") +
  facet_grid(~Understanding) +
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12))

ggarrange(bxpdprime_understanding, bxp_c_understanding, ncol=2, nrow=1, labels = c("A","B"),common.legend = TRUE, legend="top")

ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/c_and_dprime_exp1_understanding_13.12.png", width = 11, height = 5, dpi = 600,bg = "white")

#EXP2
#reaction time
RT_df_exp2 = read_xlsx("/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/RT_log_exp2_understanding_61_excluded.xlsx")

table(RT_df_exp2$understanding) #12 did not understand, 49 understood
df_0 <- RT_df_exp2 %>% filter(understanding == 0)
df_1 <- RT_df_exp2 %>% filter(understanding == 1)

#keep all rows with 0's and randomly sample 12 rows with 1's
random_df_1 <- df_1 %>% sample_n(12)

exp2_RT_understanding_random_df <- bind_rows(df_0, random_df_1)

#View(exp2_RT_understanding_random_df)

melted_data_rt_exp2 <- reshape2::melt(RT_df_exp2, id.vars = c("subject_code", "understanding"))

melted_data_rt_exp2$Orientation <- factor(sub(".*-(Upright|Rotated)", "\\1", melted_data_rt_exp2$variable),
                                          levels = c("Upright", "Rotated"),labels = c("Upright", "Inverted"))
melted_data_rt_exp2$Blink <- factor(sub("(Blink|No-Blink)-.*", "\\1", melted_data_rt_exp2$variable),
                                    levels = c("Blink", "No-Blink"),labels = c("With-Blink", "No-Blink"))

#melted_data_rt_exp2$understanding <- factor(melted_data_rt_exp2$understanding, levels = c("1", "0"),labels = c("Understood", "Did not understand"))

# Define custom colors for Blink and NoBlink
blink_colors <- c("#00AFBB", "#E7B800")

bxp <- ggboxplot(
  melted_data_rt_exp2, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("Reaction Time \n (log10)")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Presence of an Eye Blink") + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
bxp

ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/RT_61_exp2.png", width = 6, height = 6, dpi = 600)

random_selected_participants <- random_df_1$subject_code 
RT_exp2_random_df_1 <- RT_df_exp2 %>% filter(subject_code %in% random_selected_participants)


exp2_RT_understanding_random_df <- bind_rows(df_0, RT_exp2_random_df_1)
exp2_RT_understanding_random_df<- exp2_RT_understanding_random_df[order(exp2_RT_understanding_random_df$subject_code,decreasing = FALSE),]

write.table(exp2_RT_understanding_random_df, "/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/Exp2_RT_understanding_24.csv", sep = ",",row.names = FALSE,col.names  = TRUE)
write_xlsx(exp2_RT_understanding_random_df,"Exp2_RT_understanding_24.xlsx")

#View(exp2_RT_understanding_random_df)

melted_data_rt_understanding_exp2 <- reshape2::melt(exp2_RT_understanding_random_df, id.vars = c("subject_code", "understanding"))

melted_data_rt_understanding_exp2$Orientation <- factor(sub(".*-(Upright|Rotated)", "\\1", melted_data_rt_understanding_exp2$variable),
                                          levels = c("Upright", "Rotated"),labels = c("Upright", "Inverted"))
melted_data_rt_understanding_exp2$Blink <- factor(sub("(Blink|No-Blink)-.*", "\\1", melted_data_rt_understanding_exp2$variable),
                                    levels = c("Blink", "No-Blink"),labels = c("With-Blink", "No-Blink"))

melted_data_rt_understanding_exp2$understanding <- factor(melted_data_rt_understanding_exp2$understanding, levels = c("1", "0"),labels = c("Understood", "Did not understand"))

bxp <- ggboxplot(
  melted_data_rt_understanding_exp2, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("Reaction Time \n (log10)")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Presence of an Eye Blink") + 
  facet_grid(~understanding) + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
bxp

ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/RT_exp2_understanding_12.12.png", width = 6, height = 6, dpi = 600)



#accuracy

# accuracy_df_exp2 = read_xlsx("/Users/gulcelale/Desktop/dynamic face perception2/exp_2_new/accuracy_62_excluded_exp2_forjasp.xlsx")
# 
# melted_data_accuracy_exp2 <- reshape2::melt(accuracy_df_exp2, id.vars = c("subj_code", "understanding"))
# 
# melted_data_accuracy_exp2$Orientation <- factor(sub(".*-(Upright|Rotated)", "\\1", melted_data_accuracy_exp2$variable),
#                                                 levels = c("Upright", "Rotated"),labels = c("Upright", "Inverted"))
# melted_data_accuracy_exp2$Blink <- factor(sub("(Blink|No-Blink)-.*", "\\1", melted_data_accuracy_exp2$variable),
#                                           levels = c("Blink", "No-Blink"),labels = c("With-Blink", "No-Blink"))
# 
# melted_data_accuracy_exp2$understanding <- factor(melted_data_accuracy_exp2$understanding, levels = c("1", "0"),labels = c("Understood", "Did not understand"))
# 
# # Define custom colors for Blink and NoBlink
# blink_colors <- c("#00AFBB", "#E7B800")
# 
# bxp <- ggboxplot(
#   melted_data_accuracy_exp2, x = "Orientation", y = "value",
#   color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("Accuracy Rate")  +
#   scale_fill_manual(values = my_colors) +  # Change the fill colors
#   scale_color_manual(values = my_colors, 
#                      name = "Presence of an Eye Blink") + 
#   geom_hline(yintercept = 0.5, linetype = "dotted", color = "darkgray") + 
#   theme(axis.line = element_line(colour = 'black', size = 0.1),
#         panel.border = element_rect(color = "black", fill = NA),
#         panel.spacing = unit(0.6, "lines"),
#         text=element_text(family="Arial", face="bold", size=12),
#         strip.background = element_blank(),
#         strip.text = element_text(face = "bold", family = "Arial", size = 12),
#         legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
# bxp
# 
# ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/accuracy_61_exp2.png", width = 6, height = 6, dpi = 600)


#dprime
dprime_df_exp2 = read_xlsx("/Users/gulcelale/Documents/ENS210/lab-6-lalegulce/_lab/6/Exp2_Dprime_all_conditions_after_exclusion_NEW.xlsx")

table(dprime_df_exp2$understanding) #12 did not understand, 49 understood

random_selected_participants <- random_df_1$subject_code 
#select the corresponding rows from dprime
dprime_exp2_random_df_1 <- dprime_df_exp2 %>% filter(subject_code %in% random_selected_participants)

df_dprime_0 <- dprime_df_exp2 %>% filter(understanding == 0)

exp2_dprime_understanding_random_df <- bind_rows(df_dprime_0, dprime_exp2_random_df_1)
exp2_dprime_understanding_random_df<- exp2_dprime_understanding_random_df[order(exp2_dprime_understanding_random_df$subject_code,decreasing = FALSE),]

write.table(exp2_dprime_understanding_random_df, "/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/Exp2_dprime_understanding_24.csv", sep = ",",row.names = FALSE,col.names  = TRUE)


exp2_dprime_understanding_random_df$subject_code == exp2_RT_understanding_random_df$subject_code
#View(exp2_RT_understanding_random_df)


melted_data_exp2 <- reshape2::melt(dprime_df_exp2, id.vars = c("subject_code", "understanding"))

melted_data_exp2$Orientation <- factor(sub(".*-(Upright|Rotated)", "\\1", melted_data_exp2$variable),
                                       levels = c("Upright", "Rotated"),labels = c("Upright", "Inverted"))
melted_data_exp2$Blink <- factor(sub("(Blink|No-Blink)-.*", "\\1", melted_data_exp2$variable),
                                 levels = c("Blink", "NoBlink"),labels = c("With-Blink", "No-Blink"))

bxp_dprime_exp2 <- ggboxplot(
  melted_data_exp2, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("D-prime")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Presence of an Eye Blink") + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
bxp_dprime_exp2



ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/dprime_exp2_7.12.png", width = 6, height = 6, dpi = 600)


#understanding
exp2_dprime_understanding_random_df <- read.csv("/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/Exp2_dprime_understanding_24.csv",sep = ',')
colnames(exp2_dprime_understanding_random_df) <- c("subject_code", "Blink-Upright", "NoBlink-Upright", "Blink-Rotated", "NoBlink-Rotated","understanding")

melted_data_dprime_exp2_understanding <- reshape2::melt(exp2_dprime_understanding_random_df, id.vars = c("subject_code", "understanding"))

melted_data_dprime_exp2_understanding$Orientation <- factor(sub(".*-(Upright|Rotated)", "\\1", melted_data_dprime_exp2_understanding$variable),
                                  levels = c("Upright", "Rotated"),labels = c("Upright", "Inverted"))
melted_data_dprime_exp2_understanding$Blink <- factor(sub("(Blink|NoBlink)-.*", "\\1", melted_data_dprime_exp2_understanding$variable),
                            levels = c("Blink", "NoBlink"),labels = c("With-Blink", "No-Blink"))

melted_data_dprime_exp2_understanding$understanding <- factor(melted_data_dprime_exp2_understanding$understanding, levels = c("1", "0"),labels = c("Understood", "Did not understand"))

# Define custom colors for Blink and NoBlink
blink_colors <- c("#00AFBB", "#E7B800")


bxp_dprime_exp2_understanding <- ggboxplot(
  melted_data_dprime_exp2_understanding, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("D-prime")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors,
                     name = "Presence of an Eye Blink") +
  facet_grid(~understanding) +
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12))
bxp_dprime_exp2_understanding

ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/dprime_exp2_understanding_12.12.png", width = 6, height = 6, dpi = 600)

#C criterion
c_df_exp2 = read_xlsx("/Users/gulcelale/Documents/ENS210/lab-6-lalegulce/_lab/6/Exp2_C_all_conditions_after_exclusion__NEW.xlsx")

table(c_df_exp2$understanding) #12 did not understand, 49 understood

random_selected_participants <- random_df_1$subject_code 
#select the corresponding rows from c
c_df_exp2_random_df_1 <- c_df_exp2 %>% filter(subject_code %in% random_selected_participants)

df_c_0 <- c_df_exp2 %>% filter(understanding == 0)

exp2_c_understanding_random_df <- bind_rows(df_c_0, c_df_exp2_random_df_1)
exp2_c_understanding_random_df<- exp2_c_understanding_random_df[order(exp2_c_understanding_random_df$subject_code,decreasing = FALSE),]

#check if the subject_codes are the same
exp2_c_understanding_random_df$subject_code == exp2_RT_understanding_random_df$subject_code
exp2_c_understanding_random_df$subject_code == exp2_dprime_understanding_random_df$subject_code 

write.table(exp2_c_understanding_random_df, "/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/Exp2_c_understanding_24.csv", sep = ",",row.names = FALSE,col.names  = TRUE)
write.table(dprime_df_exp2, "/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/Exp2_d_understanding_all.csv", sep = ",",row.names = FALSE,col.names  = TRUE)


melted_data_c_exp2 <- reshape2::melt(c_df_exp2, id.vars = c("subject_code", "understanding"))

melted_data_c_exp2$Orientation <- factor(sub(".*-(Upright|Rotated)", "\\1", melted_data_c_exp2$variable),
                                  levels = c("Upright", "Rotated"),labels = c("Upright", "Inverted"))
melted_data_c_exp2$Blink <- factor(sub("(Blink|NoBlink)-.*", "\\1", melted_data_c_exp2$variable),
                            levels = c("Blink", "NoBlink"),labels = c("With-Blink", "No-Blink"))
melted_data_c_exp2$understanding <- factor(melted_data_c_exp2$understanding, levels = c("1", "0"),labels = c("Understood", "Did not understand"))


# Define custom colors for Blink and NoBlink
blink_colors <- c("#00AFBB", "#E7B800")

bxp_c_exp2 <- ggboxplot(
  melted_data_c_exp2, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("Criterion (C)")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Presence of an Eye Blink") + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
bxp_c_exp2

ggarrange(bxp_dprime_exp2, bxp_c_exp2, ncol=2, nrow=1, labels = c("A","B"),common.legend = TRUE, legend="top")
ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/c_and_dprime_exp2_13.12.png", width = 6, height = 5, dpi = 600,bg = "white")

#understanding 
exp2_c_understanding_random_df <- read.csv("/Users/gulcelale/Desktop/dynamic face perception2/dynamic_face_perception_all_data/experiment_2/Exp2_c_understanding_24.csv",sep = ',')
colnames(exp2_c_understanding_random_df) <- c("subject_code", "Blink-Upright", "NoBlink-Upright", "Blink-Rotated", "NoBlink-Rotated","understanding")

melted_data_c_exp2_understanding <- reshape2::melt(exp2_c_understanding_random_df, id.vars = c("subject_code", "understanding"))

melted_data_c_exp2_understanding$Orientation <- factor(sub(".*-(Upright|Rotated)", "\\1", melted_data_c_exp2_understanding$variable),
                                                            levels = c("Upright", "Rotated"),labels = c("Upright", "Inverted"))
melted_data_c_exp2_understanding$Blink <- factor(sub("(Blink|NoBlink)-.*", "\\1", melted_data_c_exp2_understanding$variable),
                                                      levels = c("Blink", "NoBlink"),labels = c("With-Blink", "No-Blink"))

melted_data_c_exp2_understanding$understanding <- factor(melted_data_c_exp2_understanding$understanding, levels = c("1", "0"),labels = c("Understood", "Did not understand"))

# Define custom colors for Blink and NoBlink
blink_colors <- c("#00AFBB", "#E7B800")


bxp_c_understanding_exp2 <- ggboxplot(
  melted_data_c_exp2_understanding, x = "Orientation", y = "value",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Orientation") + ylab("Criterion (C)")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors,
                     name = "Presence of an Eye Blink") +
  facet_grid(~understanding) +
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12))
bxp_c_understanding_exp2

ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/c_exp2_understanding_12.12.png", width = 6, height = 6, dpi = 600)

ggarrange(bxp_dprime_exp2_understanding, bxp_c_understanding_exp2, ncol=2, nrow=1, labels = c("A","B"),common.legend = TRUE, legend="top")

ggsave("/Users/gulcelale/Desktop/dynamic face perception2/figures/c_and_dprime_exp2_13.12.png", width = 11, height = 5, dpi = 600,bg = "white")


