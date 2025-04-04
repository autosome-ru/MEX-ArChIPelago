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


data1 <- read.csv("./Figure_S8_train_on_GHTS_predict_on_CHS_nodup_random_to_random_LogisticRegression.txt")
data2 <- read.csv("./Figure_S8_train_on_GHTS_predict_on_CHS_nodup_random_to_foreigns_LogisticRegression.txt")
data3 <- read.csv("./Figure_S8_train_on_GHTS_predict_on_CHS_nodup_random_to_shades_LogisticRegression.txt")


df.list <- list(data1, data2, data3)
res <- lapply(df.list, function(x) subset(x, select = c("TF_name", 
                                                        "roc_auc_test_H_PWM", "roc_auc_test_H",
                                                        "pr_auc_test_H_PWM", "pr_auc_test_H")))


df <- do.call("rbind", res)

data_GHTS_1 = df %>% 
  group_by(TF_name) %>% 
  summarise(roc_auc_test_H=mean(roc_auc_test_H), pr_auc_test_H=mean(pr_auc_test_H)) %>% 
  ungroup()

data_GHTS_1 = data_GHTS_1 %>% filter(!TF_name %in% c("FLYWCH1", "CAMTA2"))



#####

data1 <- read.csv("./Figure_S8_train_on_GHTS_predict_on_CHS_nodup_RR.txt")
data2 <- read.csv("./Figure_S8_train_on_GHTS_predict_on_CHS_nodup_RF.txt")
data3 <- read.csv("./Figure_S8_train_on_GHTS_predict_on_CHS_nodup_RS.txt")


df.list <- list(data1, data2, data3)
res <- lapply(df.list, function(x) subset(x, select = c("TF_name", 
                                                        "roc_auc_test_H_PWM", "roc_auc_test_H",
                                                        "pr_auc_test_H_PWM", "pr_auc_test_H")))


df <- do.call("rbind", res)
data_GHTS_2 = df %>% 
  group_by(TF_name) %>% 
  summarise(roc_auc_test_H=mean(roc_auc_test_H), pr_auc_test_H=mean(pr_auc_test_H)) %>% 
  ungroup()

data_GHTS_2 = data_GHTS_2 %>% filter(!TF_name %in% c("FLYWCH1", "CAMTA2"))

data_GHTS = merge(data_GHTS_1, data_GHTS_2, by="TF_name")

data_GHTS = data_GHTS %>% dplyr::select(c("TF_name", "roc_auc_test_H.x", "roc_auc_test_H.y"))
colnames(data_GHTS) = c("TF_name", "LogisticRegression", "RandomForestClassifier")

library(reshape2) 
data_GHTS = melt(data_GHTS, id = c("TF_name")) 


image = ggplot(data_GHTS, aes(x=variable, y=value)) + 
  geom_jitter(position=position_jitter(width=0.3), alpha=0.9, size=2.5, color="magenta", shape=21) +
  geom_boxplot(alpha = 0, show.legend = FALSE, outlier.shape = NA) + 
  labs(x="", y="auROC") + 
  theme_minimal(base_size = 25) +
  theme(legend.position="right",
        legend.box="vertical", 
        legend.margin=margin())+
  theme(panel.grid = element_line(size = 0.5),
        text = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"))


library(Cairo)

Cairo(file='Figure_S8G_GHTS.pdf', type="pdf", width=80, height=200, units="mm")
image
dev.off()

#


data1 <- read.csv("./Figure_S8_train_on_CHS_predict_on_GHTS_nodup_random_to_random_LogisticRegression.txt")
data2 <- read.csv("./Figure_S8_train_on_CHS_predict_on_GHTS_nodup_random_to_foreigns_LogisticRegression.txt")
data3 <- read.csv("./Figure_S8_train_on_CHS_predict_on_GHTS_nodup_random_to_shades_LogisticRegression.txt")


df.list <- list(data1, data2, data3)
res <- lapply(df.list, function(x) subset(x, select = c("TF_name", 
                                                        "roc_auc_test_H_PWM", "roc_auc_test_H",
                                                        "pr_auc_test_H_PWM", "pr_auc_test_H")))


df <- do.call("rbind", res)

data_GHTS_1 = df %>% 
  group_by(TF_name) %>% 
  summarise(roc_auc_test_H=mean(roc_auc_test_H), pr_auc_test_H=mean(pr_auc_test_H)) %>% 
  ungroup()

data_GHTS_1 = data_GHTS_1 %>% filter(!TF_name %in% c("FLYWCH1", "CAMTA2"))



#####

data1 <- read.csv("./Figure_S8_train_on_CHS_predict_on_GHTS_nodup_RR.txt")
data2 <- read.csv("./Figure_S8_train_on_CHS_predict_on_GHTS_nodup_RF.txt")
data3 <- read.csv("./Figure_S8_train_on_CHS_predict_on_GHTS_nodup_RS.txt")


df.list <- list(data1, data2, data3)
res <- lapply(df.list, function(x) subset(x, select = c("TF_name", 
                                                        "roc_auc_test_H_PWM", "roc_auc_test_H",
                                                        "pr_auc_test_H_PWM", "pr_auc_test_H")))


df <- do.call("rbind", res)
data_GHTS_2 = df %>% 
  group_by(TF_name) %>% 
  summarise(roc_auc_test_H=mean(roc_auc_test_H), pr_auc_test_H=mean(pr_auc_test_H)) %>% 
  ungroup()

data_GHTS_2 = data_GHTS_2 %>% filter(!TF_name %in% c("FLYWCH1", "CAMTA2"))

data_GHTS = merge(data_GHTS_1, data_GHTS_2, by="TF_name")

data_GHTS = data_GHTS %>% dplyr::select(c("TF_name", "roc_auc_test_H.x", "roc_auc_test_H.y"))
colnames(data_GHTS) = c("TF_name", "LogisticRegression", "RandomForestClassifier")

library(reshape2) 
data_GHTS = melt(data_GHTS, id = c("TF_name")) 


image = ggplot(data_GHTS, aes(x=variable, y=value)) + 
  geom_jitter(position=position_jitter(width=0.3), alpha=0.9, size=2.5, color="magenta", shape=21) +
  geom_boxplot(alpha = 0, show.legend = FALSE, outlier.shape = NA) + 
  labs(x="", y="auROC") + 
  theme_minimal(base_size = 25) +
  theme(legend.position="right",
        legend.box="vertical", 
        legend.margin=margin())+
  theme(panel.grid = element_line(size = 0.5),
        text = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"))

library(Cairo)

Cairo(file='Figure_S8G_CHS.pdf', type="pdf", width=80, height=200, units="mm")
image
dev.off()



