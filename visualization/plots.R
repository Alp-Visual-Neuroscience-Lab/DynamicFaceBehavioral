library(dplyr)
library(readxl)
library(corrplot)
library(grid)
library(ggplot2)
library(wesanderson)
library(RColorBrewer)
library(ggsignif)
library(ggpubr)

#EXPERIMENT 1
#CORRECT RESPONSE TIME
df = read_xlsx("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/RT_exp1_log10_50_forgraph.xlsx")
#isNA(df)
df$Direction_of_Speech <- factor(df$Direction_of_Speech, levels = c("Sequence", "Reverse"))
df$Position_Of_Face <- factor(df$Position_Of_Face, levels = c("Upright", "Inverted"))
df$Blink <- factor(df$Blink, levels = c("With-Blink", "No-Blink"))
df$Participant <- rep(1:(nrow(df)/8), each = 8)
my_colors <- c("#00AFBB","#E7B800")


#used this for the publication plots
bxp <- ggboxplot(
  df, x = "Direction_of_Speech", y = "Log_RT",
  color = "Blink",
  facet.by = "Position_Of_Face",add = "jitter", size = 0.75) + 
  xlab("Timeline Order") + ylab("Correct Reaction Time \n (log10)")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, name = "Existence of an Eye Blink") + 
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        text = element_text(family = "Arial", face = "bold", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12))
bxp




#ACCURACY RATE
acc.df = read_xlsx("/Users/gulcelale/Desktop/dynamic face perception2/exp_1_new/Accuracy_Exp1_50_forgraph.xlsx")
acc.df$Direction_of_Speech <- factor(acc.df$Direction_of_Speech, levels = c("Sequence", "Reverse"))
acc.df$Position_Of_Face <- factor(acc.df$Position_Of_Face, levels = c("Upright", "Inverted"))
acc.df$Blink <- factor(acc.df$Blink, levels = c("With-Blink", "No-Blink"))
acc.df$Participant <- rep(1:(nrow(acc.df)/8), each = 8)
my_colors <- c("#00AFBB","#E7B800")



#used this for the publication plots
bxp <- ggboxplot(
  acc.df, x = "Direction_of_Speech", y = "Log_RT",
  color = "Blink",
  facet.by = "Position_Of_Face",add = "jitter", size = 0.75) + 
  xlab("Timeline Order") + ylab("Accuracy Rate")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, name = "Existence of an Eye Blink") + 
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        text = element_text(family = "Arial", face = "bold", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12))
bxp


#EXPERIMENT 2
#CORRECT RESPONSE TIME 
df = read_xlsx("/Users/gulcelale/Desktop/dynamic face perception2/exp_2_new/CorrectResponseRT_log_68_exp2_forgraph.xlsx")
#isNA(df)
df$Direction_of_Speech <- factor(df$Direction_of_Speech, levels = c("Sequence", "Reverse"))
df$Position_Of_Face <- factor(df$Position_Of_Face, levels = c("Upright", "Inverted"))
df$Blink <- factor(df$Blink, levels = c("With-Blink", "No-Blink"))
df$Participant <- rep(1:(nrow(df)/8), each = 8)
my_colors <- c("#00AFBB","#E7B800")

#dev.off()
# bxp <- ggboxplot(
#   df, x = "Direction_of_Speech", y = "Log_RT",
#   color = "Blink",
#   facet.by = "Position_Of_Face",add = "jitter", size = 0.75) + xlab("Timeline Order") + ylab("Correct Reaction Time \n (log10)")  +
#   scale_fill_manual(values = my_colors) +  # Change the fill colors
#   scale_color_manual(values = my_colors, name = "Existance of an Eye Blink") + theme(strip.background = element_blank(),strip.text = element_text(face = "bold", family = "Arial", size = 12),text=element_text(family="Arial", face="bold", size=12))
# bxp


#used this for the publication plots
bxp <- ggboxplot(
  df, x = "Direction_of_Speech", y = "Log_RT",
  color = "Blink",
  facet.by = "Position_Of_Face",add = "jitter", size = 0.75) + 
  xlab("Timeline Order") + ylab("Correct Reaction Time \n (log10)")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, name = "Existence of an Eye Blink") + 
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        text = element_text(family = "Arial", face = "bold", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12))
bxp




#ACCURACY RATE
acc.df = read_xlsx("/Users/gulcelale/Desktop/dynamic face perception2/exp_2_new/accuracy_68_exp2_forgraph.xlsx")
acc.df$Direction_of_Speech <- factor(acc.df$Direction_of_Speech, levels = c("Sequence", "Reverse"))
acc.df$Position_Of_Face <- factor(acc.df$Position_Of_Face, levels = c("Upright", "Inverted"))
acc.df$Blink <- factor(acc.df$Blink, levels = c("With-Blink", "No-Blink"))
acc.df$Participant <- rep(1:(nrow(acc.df)/8), each = 8)
my_colors <- c("#00AFBB","#E7B800")

#dev.off()
# bxp <- ggboxplot(
#   acc.df, x = "Direction_of_Speech", y = "Log_RT",
#   color = "Blink",
#   facet.by = "Position_Of_Face",add = "jitter", size = 0.75) + labs(
#     subtitle = "Accuracy Rates by Position of Face, Direction of Speech, and Blink") + xlab("Direction of Speech") + ylab("Accuracy Rate")  +
#   scale_fill_manual(values = my_colors) +  # Change the fill colors
#   scale_color_manual(values = my_colors) + theme(strip.background = element_blank(),strip.text = element_text(face = "bold", family = "Arial", size = 12),text=element_text(family="Arial", face="bold", size=12))
# bxp


#used this for the publication plots
bxp <- ggboxplot(
  acc.df, x = "Direction_of_Speech", y = "Log_RT",
  color = "Blink",
  facet.by = "Position_Of_Face",add = "jitter", size = 0.75) + 
  xlab("Timeline Order") + ylab("Accuracy Rate")  +
  scale_fill_manual(values = my_colors) +  # Change the fill colors
  scale_color_manual(values = my_colors, name = "Existence of an Eye Blink") + 
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", family = "Arial", size = 12),
        text = element_text(family = "Arial", face = "bold", size = 12),
        legend.text = element_text(family = "Arial", face = "bold", size = 12))
bxp


