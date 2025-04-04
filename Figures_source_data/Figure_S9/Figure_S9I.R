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
data1 <- read.csv("./Figure_S9_train_on_CHS_predict_on_GHTS_nodup_RR.txt")
data_CHS = data1

data_CHS %>% dplyr::select(c("Seq_count_shades", "Seq_count_alien", "Seq_count_random", "Seq_count_positives", "TF_name")) %>%
  melt(id.vars = c("TF_name"),
       measure.vars = c("Seq_count_shades", "Seq_count_positives")) %>%  #"Seq_count_alien", "Seq_count_random")) %>%
  ggplot(aes(x = value, fill=variable)) +
  geom_density(alpha = .5)+
  theme_minimal(base_size = 21)+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+ scale_y_sqrt()+
  ylab("Density") +
  xlab("Size of the set")


data1 <- read.csv("./Figure_S9_train_on_GHTS_predict_on_CHS_nodup_RR.txt")

data_GHTS = data1

data_GHTS %>% dplyr::select(c("Seq_count_shades", "Seq_count_alien", "Seq_count_random", "Seq_count_positives", "TF_name")) %>%
  melt(id.vars = c("TF_name"),
       measure.vars = c("Seq_count_shades", "Seq_count_positives")) %>%  #"Seq_count_alien", "Seq_count_random")) %>%
  ggplot(aes(x = value, fill=variable)) +
  geom_density(alpha = .5)+
  theme_minimal(base_size = 21)+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+ scale_y_sqrt()+
  ylab("Density") +
  xlab("Size of the set")


library(reshape)
xxyy = data_CHS %>% merge(data_GHTS, by="TF_name") %>% dplyr::select(c("TF_name", "Seq_count_positives.x", "Seq_count_positives.y"))
colnames(xxyy) = c("TF_name", "CHS", "GHTS")

df = xxyy %>% melt(id.vars = c("TF_name"),
              measure.vars = c("CHS", "GHTS"))



cdat = df %>%
  group_by(variable) %>%
  mutate(mean_x = mean(value))
c = df%>%
  ggplot(aes(x = value, fill=variable)) +
  geom_density(alpha = .5)+
  theme_minimal(base_size = 21)+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  ylab("Density") +
  xlab("Size of the set")  +
  geom_vline(data=cdat, aes(xintercept=mean_x,  colour=variable),
                                          linetype="dashed", size=1)+
  labs(fill = "Positive set") +
  guides(colour="none")+
  theme(panel.grid = element_line(size = 0.5))



library(Cairo)

Cairo(file='Figure_S9I_positives.pdf', type="pdf", width=300, height=100, units="mm")
c
dev.off()


library(reshape)
xxyy = data_CHS %>% merge(data_GHTS, by="TF_name") %>% dplyr::select(c("TF_name", "Seq_count_random.x", "Seq_count_random.y"))
colnames(xxyy) = c("TF_name", "CHS", "GHTS")

df = xxyy %>% melt(id.vars = c("TF_name"),
                   measure.vars = c("CHS", "GHTS"))



cdat = df %>% 
  group_by(variable) %>% 
  mutate(mean_x = mean(value)) 
c = df%>% 
  ggplot(aes(x = value, fill=variable)) +
  geom_density(alpha = .5)+
  theme_minimal(base_size = 21)+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  ylab("Density") +
  xlab("Size of the set")  +
  geom_vline(data=cdat, aes(xintercept=mean_x,  colour=variable),
             linetype="dashed", size=1)+
  labs(fill = "Random set") +
  guides(colour="none")



library(Cairo)

Cairo(file='Figure_S9I_randoms.pdf', type="pdf", width=300, height=100, units="mm")
c
dev.off()




