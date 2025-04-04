# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(ggrepel)
library(gapminder)
library(ggExtra)
library(extrafont)
font_import()

mex_dbd <- read.delim("./mex_dbd.tsv", header=FALSE, comment.char="#")

theme_set(theme_get() + theme(text = element_text(family = 'Arial')))


##### CHS ####
data1 <- read.csv("./Figure_4_train_on_CHS_predict_on_GHTS_nodup_RR.txt")
data2 <- read.csv("./Figure_4_train_on_CHS_predict_on_GHTS_nodup_RF.txt")
data3 <- read.csv("./Figure_4_train_on_CHS_predict_on_GHTS_nodup_RS.txt")

df.list <- list(data1, data2, data3)
res <- lapply(df.list, function(x) subset(x, select = c("TF_name", 
                                                        "roc_auc_test_H_PWM", "roc_auc_test_H",
                                                        "pr_auc_test_H_PWM", "pr_auc_test_H")))


df <- do.call("rbind", res)
data_CHS = df %>% 
  mutate(ROC_delta_M = roc_auc_test_H-roc_auc_test_H_PWM, PR_delta_M = pr_auc_test_H-pr_auc_test_H_PWM) %>% 
  group_by(TF_name) %>% 
  summarise(ROC_delta_M=mean(ROC_delta_M), PR_delta_M=mean(PR_delta_M)) %>% 
  ungroup() %>% 
  merge(data1 %>% select(c("Seq_count_positives", "Count", "TF_name")))





data1 <- read.csv("./Figure_4_train_on_GHTS_predict_on_CHS_nodup_RR.txt")
data2 <- read.csv("./Figure_4_train_on_GHTS_predict_on_CHS_nodup_RF.txt")
data3 <- read.csv("./Figure_4_train_on_GHTS_predict_on_CHS_nodup_RS.txt")


df.list <- list(data1, data2, data3)
res <- lapply(df.list, function(x) subset(x, select = c("TF_name", 
                                                        "roc_auc_test_H_PWM", "roc_auc_test_H",
                                                        "pr_auc_test_H_PWM", "pr_auc_test_H")))


df <- do.call("rbind", res)
data_GHTS = df %>% 
  mutate(ROC_delta_M = roc_auc_test_H-roc_auc_test_H_PWM, PR_delta_M = pr_auc_test_H-pr_auc_test_H_PWM) %>% 
  group_by(TF_name) %>% 
  summarise(ROC_delta_M=mean(ROC_delta_M), PR_delta_M=mean(PR_delta_M)) %>% 
  ungroup() %>% 
  merge(data1 %>% select(c("Seq_count_positives", "Count", "TF_name")))


xx = data_CHS %>% mutate(xx=(ROC_delta_M+PR_delta_M)/2)
yy = data_GHTS %>% mutate(yy=(ROC_delta_M+PR_delta_M)/2)
xxyy = xx %>% merge(yy, by="TF_name")


xxyy = xxyy %>% filter(!TF_name %in% c("FLYWCH1", "CAMTA2"))

xxyy$Seq_count_positives = pmin(xxyy$Seq_count_positives.x, xxyy$Seq_count_positives.y)
xxyy$Count = pmin(xxyy$Count.x, xxyy$Count.y)


xxyy_TFfam = xxyy %>% merge(mex_dbd, by.y="V1", by.x="TF_name")
xxyy_TFfam$TF_family = xxyy_TFfam$V2

library(pracma)
xxyy_TFfam$distance_from_center = hypot(xxyy_TFfam$xx, xxyy_TFfam$yy)
xxyy_TFfam$mean_gain = (xxyy_TFfam$xx+xxyy_TFfam$yy)/2


xxyy_TFfam$TF_family[xxyy_TFfam$TF_family == "unknown"] = "Unknown"

library(ggbeeswarm)
library(reshape2)
df_means <- melt(xxyy_TFfam %>% group_by(TF_family) %>% reframe(mean=mean(xx)))

a = ggplot(xxyy_TFfam, aes(reorder(TF_family,xx), xx)) + 
  geom_beeswarm(size = 5, cex = 0.5, alpha=0.3)+
  geom_point(data=df_means,  mapping=aes(x = TF_family, y = value), col="red", shape="|", stroke = 2, size=5)+
  theme_minimal(base_size = 21) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        plot.margin = margin(5.5, 5.5, 5.5, 0),
        panel.grid = element_line(size = 0.5),
        axis.text.y.left = element_blank(),
        panel.border = element_rect(colour = "gray", fill=NA, size=0.5))+
  guides(
    x = guide_axis(title = ""),
    x.sec = guide_axis(title = "Average gain, ChIP-Seq"),
    y = guide_axis(),
    y.sec = "none"
  )+
  coord_flip() #+ 
  #scale_y_reverse(lim = c(0.25, -0.01))

a

df_means <- melt(xxyy_TFfam %>% group_by(TF_family) %>% reframe(mean=mean(yy)))

b = ggplot(xxyy_TFfam, aes(reorder(TF_family,xx), yy)) + 
  geom_beeswarm(size = 5, cex = 0.5, alpha=0.3)+
  geom_point(data=df_means,  mapping=aes(x = TF_family, y = value), col="red", shape="|", stroke = 2, size=5)+
  theme_minimal(base_size = 21) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        plot.margin = margin(5.5, 5.5, 5.5, 0),
        panel.grid = element_line(size = 0.5),
        axis.text.y.left = element_text(margin = margin(0, 15.5, 0, 5.5), 
                                        hjust = 0.5),
        panel.border = element_rect(colour = "gray", fill=NA, size=0.5))+
  guides(
    x = guide_axis(title = ""),
    x.sec = guide_axis(title = "Average gain, GHT-SELEX"),
    y = guide_axis(),
    y.sec = "none"
  )+
  coord_flip()+
  ylim(-0.01, 0.25)

b
library(patchwork)
b
c = a+b
c

library(Cairo)

Cairo(file='Figure_4B.pdf', type="pdf", width=310, height=185, units="mm")
c
dev.off()


