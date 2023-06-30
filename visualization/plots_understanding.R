library(dplyr)
library(readxl)
library(corrplot)
library(grid)
library(ggplot2)
library(wesanderson)
library(RColorBrewer)
library(ggsignif)
library(ggpubr)
library(xlsx)

#EXPERIMENT 1
rt.df = read_xlsx("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/RT_exp1_log10_50_graph_understanding.xlsx")
rt.df$Direction_of_Speech <- factor(rt.df$Direction_of_Speech, levels = c("Sequence", "Reverse"))
rt.df$Position_Of_Face <- factor(rt.df$Position_Of_Face, levels = c("Upright", "Inverted"))
rt.df$Blink <- factor(rt.df$Blink, levels = c("With-Blink", "No-Blink"))
rt.df$Understanding <- factor(rt.df$Understanding, levels = c("1", "0"), 
                              labels = c("Understood", "Did not understand"))
rt.df$Participant <- rep(1:(nrow(rt.df)/8), each = 8)
my_colors <- c("#00AFBB","#E7B800")


#used the following code for the publication plots
bxp <- ggboxplot(
  rt.df, x = "Direction_of_Speech", y = "Log_RT",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Timeline Order") + ylab("Correct Reaction Time \n (log10)")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Existence of an Eye Blink") + 
  facet_grid(Understanding ~Position_Of_Face) + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
bxp


#accuracy
acc.df = read_xlsx("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/Accuracy_Exp1_50_graph_understanding.xlsx")
acc.df$Direction_of_Speech <- factor(acc.df$Direction_of_Speech, levels = c("Sequence", "Reverse"))
acc.df$Position_Of_Face <- factor(acc.df$Position_Of_Face, levels = c("Upright", "Inverted"))
acc.df$Blink <- factor(acc.df$Blink, levels = c("With-Blink", "No-Blink"))
acc.df$Understanding <- factor(acc.df$Understanding, levels = c("1", "0"), 
                               labels = c("Understood", "unUnderstood"))
acc.df$Participant <- rep(1:(nrow(acc.df)/8), each = 8)
my_colors <- c("#00AFBB","#E7B800")


#used the following code for the publication plots
bxp <- ggboxplot(
  acc.df, x = "Direction_of_Speech", y = "Log_RT",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Timeline Order") + ylab("Accuracy Rate")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Existence of an Eye Blink") + 
  facet_grid(Understanding ~Position_Of_Face) + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
bxp

#EXPERIMENT 2

#ACCURACY
#14 understood 14 not understood
#accuracy
acc_28_sub = read_xlsx("/Users/gulcelale/Desktop/dynamic face perception2/exp_2_new/accuracy_28_exp2_understanding_ceren_graph.xlsx")
acc_28_sub$Direction_of_Speech <- factor(acc_28_sub$Direction_of_Speech, levels = c("Sequence", "Reverse"))
acc_28_sub$Position_Of_Face <- factor(acc_28_sub$Position_Of_Face, levels = c("Upright", "Inverted"))
acc_28_sub$Blink <- factor(acc_28_sub$Blink, levels = c("With-Blink", "No-Blink"))
acc_28_sub$Understanding <- factor(acc_28_sub$Understanding, levels = c("1", "0"), 
                               labels = c("Understood", "Did not understand"))
acc_28_sub$Participant <- rep(1:(nrow(acc_28_sub)/8), each = 8)
my_colors <- c("#00AFBB","#E7B800")



bxp <- ggboxplot(
  acc_28_sub, x = "Direction_of_Speech", y = "Log_RT",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Timeline Order") + ylab("Accuracy Rate")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Existence of an Eye Blink") + 
  facet_grid(Understanding ~Position_Of_Face) + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
bxp

ggsave("bxp.png", width = 6, height = 6, dpi = 300)


#RT
rt_28_sub = read_xlsx("/Users/gulcelale/Desktop/dynamic face perception2/exp_2_new/RT_28_exp2_understanding_ceren_graph.xlsx")
rt_28_sub$Direction_of_Speech <- factor(rt_28_sub$Direction_of_Speech, levels = c("Sequence", "Reverse"))
rt_28_sub$Position_Of_Face <- factor(rt_28_sub$Position_Of_Face, levels = c("Upright", "Inverted"))
rt_28_sub$Blink <- factor(rt_28_sub$Blink, levels = c("With-Blink", "No-Blink"))
rt_28_sub$Understanding <- factor(rt_28_sub$Understanding, levels = c("1", "0"), 
                                   labels = c("Understood", "Did not understand"))
rt_28_sub$Participant <- rep(1:(nrow(rt_28_sub)/8), each = 8)
my_colors <- c("#00AFBB","#E7B800")



bxp <- ggboxplot(
  rt_28_sub, x = "Direction_of_Speech", y = "Log_RT",
  color = "Blink",add = "jitter", size = 0.75) + xlab("Timeline Order") + ylab("Correct Reaction Time \n (log10)")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, 
                     name = "Existence of an Eye Blink") + 
  facet_grid(Understanding ~Position_Of_Face) + 
  theme(axis.line = element_line(colour = 'black', size = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.6, "lines"),
        text=element_text(family="Arial", face="bold", size=12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12)) 
bxp

ggsave("bxp.png", width = 6, height = 6, dpi = 300)
