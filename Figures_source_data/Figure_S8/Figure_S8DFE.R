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


Figure 1
##### CHS ####
data1 <- read.csv("./Figure_S8_train_on_GHTS_predict_on_CHS_nodup_RR.txt")
data2 <- read.csv("./Figure_S8_train_on_GHTS_predict_on_CHS_nodup_RF.txt")
data3 <- read.csv("./Figure_S8_train_on_GHTS_predict_on_CHS_nodup_RS.txt")

data1$`Negative set` = "Random"
data2$`Negative set` = "Aliens"
data3$`Negative set` = "Shades"

df.list <- list(data1, data2, data3)
res <- lapply(df.list, function(x) subset(x, select = c("TF_name", 
                                                        "roc_auc_test_H_PWM", "roc_auc_test_H",
                                                        "pr_auc_test_H_PWM", "pr_auc_test_H", "Negative set")))

df <- do.call("rbind", res)
data = df %>% 
  mutate(ROC_delta_H = roc_auc_test_H-roc_auc_test_H_PWM, PR_delta_H = pr_auc_test_H-pr_auc_test_H_PWM) %>% 
  group_by(TF_name) %>% 
  summarise(ROC_delta_H=mean(ROC_delta_H), PR_delta_H=mean(PR_delta_H)) %>% 
  ungroup() %>% 
  merge(data1 %>% dplyr::select(c("Seq_count_positives", "Count", "TF_name")))

df = df %>% 
  merge(data1 %>% dplyr::select(c("Seq_count_positives", "Count", "TF_name")))

data = data %>% merge(mex_dbd, by.y="V1", by.x="TF_name")
data$TF_family = data$V2

data = data %>% filter(!TF_name %in% c("FLYWCH1", "CAMTA2"))

my_pal <- function(range = c(1, 6)) {
  force(range)
  function(x) scales::rescale(x, to = range, from = c(0, 1))
}


c = data %>%
  arrange(desc(Seq_count_positives)) %>%
  mutate(TF_name = factor(TF_name, TF_name)) %>%
  ggplot(aes(x=PR_delta_H, y=ROC_delta_H, size=Seq_count_positives, fill=Count)) + # 
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(1, 15), breaks= c(500, 1000, 5000, 10000, 50000),
             #labels = c(5*10^2, 10^3, 5*10^3, 10^4, 5*10^4),
             name="Size of \nthe positive \nset") +
  scale_fill_viridis(discrete=F, option="C", name="Number of \nPWMs") +
  theme_minimal(base_size = 21) +
  theme(legend.position="right",
        legend.box="vertical", 
        legend.margin=margin())+
  ylab("\u0394auROC") +
  xlab("\u0394auPRC") + 
  geom_hline(yintercept = 0, linetype="dotted") + 
  geom_vline(xintercept = 0, linetype="dotted") + 
  geom_text_repel(data = subset(data, (ROC_delta_H < 0)|(ROC_delta_H>0.1)|(PR_delta_H<0)|(PR_delta_H>0.2)),
                  aes(label = TF_name), 
                  point.padding = 1,
                  min.segment.length = 3,
                  max.time = 1, max.iter = 1e5,
                  #box.padding = 0.3, 
                  segment.curvature = -0.1,
                  #segment.ncp = 3,
                  segment.angle = 20,
                  size=5)+
  guides(color = guide_legend(order=1),
         size = guide_legend(order=2),
         shape = "none")

a = df %>%
  arrange(desc(Seq_count_positives)) %>%
  ggplot(aes(x=roc_auc_test_H_PWM, y=roc_auc_test_H, color=`Negative set`)) + # 
  geom_point(alpha=0.7, shape=21, size=5)+
  theme_minimal(base_size = 21) +
  theme(legend.position="none",
        axis.text.x=element_text(angle=90, vjust = 0.5))+
  ylab("auROC") +
  xlab("auROC best PWM") + 
  expand_limits(x=0, y=0) +
  geom_abline(intercept = 0, linetype="dotted") +
  guides(color=guide_legend(nrow=3, byrow=TRUE)) 

a = ggMarginal(a, type = "histogram", 
               #margins = "x",
               #color = "gray",
               fill = "white", 
               size=8, groupColour = TRUE)

b = df %>%
  arrange(desc(Seq_count_positives))  %>%
  ggplot(aes(x=pr_auc_test_H_PWM, y=pr_auc_test_H, color=`Negative set`)) + # 
  geom_point(alpha=0.7, shape=21, size=5)+
  theme_minimal(base_size = 21) +
  theme(legend.position="none",
        axis.text.x=element_text(angle=90, vjust = 0.5))+
  ylab("auPRC") +
  xlab("auPRC best PWM") + 
  expand_limits(x=0, y=0) +
  geom_abline(intercept = 0, linetype="dotted") +
  guides(color=guide_legend(nrow=3, byrow=TRUE)) 

b = ggMarginal(b, type = "histogram", 
               #margins = "x",
               #color = "gray",
               fill = "white", 
               size=8, groupColour = TRUE)

plot = ggarrange(
  
  ggarrange(a, b, nrow = 2, ncol = 1, labels = c("D", "E"),
            font.label=list(size=25), vjust = -0.3 
            #heights = c(1, 1.5)
  ),
  
  ggarrange(c, 
            labels = c("F"),
            ncol = 1, nrow = 1, 
            font.label=list(size=25), vjust = -0.3),  
  widths = c(1, 2.5), ncol = 2, nrow = 1
)



image = annotate_figure(plot, top = text_grob("", 
                                      color = "black", face = "bold", size = 25))
library(Cairo)

Cairo(file='Figure_S8DFE.pdf', type="pdf", width=310, height=185, units="mm")
image
dev.off()


