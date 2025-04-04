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
  merge(data1 %>% dplyr::select(c("Seq_count_positives", "Count", "TF_name")))





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
  merge(data1 %>% dplyr::select(c("Seq_count_positives", "Count", "TF_name")))


xx = data_CHS %>% mutate(xx=(ROC_delta_M+PR_delta_M)/2)
yy = data_GHTS %>% mutate(yy=(ROC_delta_M+PR_delta_M)/2)
xxyy = xx %>% merge(yy, by="TF_name")


xxyy = xxyy %>% filter(!TF_name %in% c("FLYWCH1", "CAMTA2"))

xxyy$Seq_count_positives = pmin(xxyy$Seq_count_positives.x, xxyy$Seq_count_positives.y)
xxyy$Count = pmin(xxyy$Count.x, xxyy$Count.y)


TFs_to_select = c("ZNF773", "ZNF43", "GABPA")


image = xxyy %>%
  arrange(desc(Seq_count_positives)) %>%
  mutate(TF_name = factor(TF_name, TF_name)) %>%
  ggplot(aes(x=xx, y=yy, size=Seq_count_positives, fill=Count)) + # 
  geom_point(alpha=0.5, shape=21, color="black") +
  
  scale_size(range = c(1, 15), breaks= c(500, 1000, 5000, 10000, 50000), name="Min. number \nof positives") +
  
  scale_fill_viridis(discrete=F, option="C", name="Min. number \nof PWMs") +
  theme_minimal(base_size = 21) +
  theme(legend.position="right",
        legend.box="vertical", 
        legend.margin=margin())+
  ylab("Average gain, GHT-SELEX") +
  xlab("Average gain, ChIP-Seq") + 
  geom_hline(yintercept=0, linetype="dotted") + 
  geom_vline(xintercept = 0, linetype="dotted")+

  geom_text_repel(data = subset(xxyy, (xxyy$TF_name %in% TFs_to_select)|(xx < 0)|(xx>0.1)|(yy<0)|(yy>0.15)),
                  aes(label = TF_name), 
                  point.padding = 1,
                  min.segment.length = 0,
                  max.time = 1, max.iter = 1e5,
                  #box.padding = 0.3, 
                  segment.curvature = -0.1,
                  #segment.ncp = 3,
                  segment.angle = 20,
                  size=5)+
  guides(color = guide_legend(order=1),
         size = guide_legend(order=2),
         shape = "none")+ coord_fixed(ratio = 1)

subset(xxyy, (xxyy$TF_name %in% TFs_to_select)|(xx < 0)|(xx>0.1)|(yy<0)|(yy>0.15))$TF_name




library(Cairo)

Cairo(file='Figure_4A.pdf', type="pdf", width=310, height=185, units="mm")
ggMarginal(image, type = "histogram", 
           #margins = "x",
           #color = "gray",
           fill = "white", 
           size=8)
dev.off()

