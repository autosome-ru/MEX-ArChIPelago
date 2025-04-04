
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


theme_set(theme_get() + theme(text = element_text(family = 'Arial')))


data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

df_violin_CHS <- read.csv("~/Desktop/MEX_trees/df_violin_CHS_GHTS_roc_15022024.csv")

df_violin_CHS$TF_name <- as.factor(df_violin_CHS$TF_name)


# df_violin_CHS %>%
#   mutate( n=factor(Count,levels=c(2,4,8,16,"all")) ) %>% 
#   ggplot(aes(x=n, y=dLR)) + 
#   geom_violin() + 
#   theme_minimal(base_size = 21) +
#   geom_point(alpha=0.7, shape=21, size=2, position=position_jitter(0.2, seed=666))+
#   #geom_jitter(alpha=0.7, shape=21, size=2, position=position_jitter(0.2, seed=666)) + 
#   geom_line(aes(x=n, y=dLR, group=TF_name), position=position_jitter(0.2, seed=666)) +
#   stat_summary(fun.y=median, geom="point", size=2, color="red") +
#   ylab("ΔauROC to the best PWM") +
#   xlab("Number of features")+
#   geom_hline(yintercept = 0, linetype="dotted")+
#   theme(panel.grid = element_line(size = 0.5)) 
#   
library(MASS) 
library(reshape2) 
library(reshape) 

# c = df_violin_CHS %>% dplyr::select(c("TF_name", "roc_auc_test_H_PWM", "roc_auc_test_H", "PWM")) %>% 
#   dcast(TF_name~PWM, value.var = "roc_auc_test_H") %>% mutate(diff=all-`2`) %>% 
#   merge(df_violin_CHS, by="TF_name") %>% 
#   mutate( n=factor(Count,levels=c(2,4,8,16,"all")) ) %>% 
#   dplyr::arrange(TF_name, n) %>% 
#   dplyr::group_by(TF_name) %>% 
#   mutate(diffc = cut(diff, c(-Inf, 0, 0.05, Inf))) %>% 
#   mutate(diffc_m = replace(diffc, diffc=="(0,0.05]", NA)) %>% 
#   ggplot2::ggplot(., aes(y = dLR, x = n)) +
#   geom_violin() + 
#   geom_path(
#     aes(group = TF_name, colour = diffc_m), 
#     size = 0.5, alpha = 0.5, 
#     position = position_jitter(width = 0.1, seed = 3922)
#   ) + 
#   
#   geom_point(alpha=0.7, shape=21, size=2, position = position_jitter(width = 0.1, seed = 3922), 
#              colour = "black") +  
#   scale_color_manual(name = "diffc_m",
#                      values = c("(-Inf,0]" = "cyan",
#                                 "(0.05, Inf]" = "magenta"),
#                      labels = c("≤ 0", "> 0.05"), na.translate = F)+
#   stat_summary(fun.y=median, geom="point", size=2, color="red") +
#   
#   ylab("ΔauROC to the best PWM") +
#   xlab("Number of features")+
#   geom_hline(yintercept = 0, linetype="dotted")+
#   theme_minimal(base_size = 21) +
#   theme(panel.grid = element_line(size = 0.5)) + 
#   guides(color=guide_legend("all - 2 \nfeatures \nΔauROC"))
# 


c = df_violin_CHS %>% dplyr::select(c("TF_name", "roc_auc_test_M_PWM", "dLR", "PWM")) %>% 
  dcast(TF_name~PWM, value.var = "dLR") %>% 
  merge(df_violin_CHS, by="TF_name") %>% 
  mutate( n=factor(Count,levels=c(2,4,8,16,"all")) ) %>% 
  dplyr::arrange(TF_name, n) %>% 
  dplyr::group_by(TF_name) %>% 
  mutate(diffc = cut(`all`, c(-Inf, 0, 0.1, Inf))) %>% 
  mutate(diffc_m = replace(diffc, diffc=="(0,0.1]", NA)) %>% 
  ggplot2::ggplot(., aes(y = dLR, x = n)) +
  geom_violin() + 
  geom_path(
    aes(group = TF_name, colour = diffc_m), 
    size = 0.7, alpha = 0.5, 
    position = position_jitter(width = 0.1, seed = 3922)
  ) + 
  
  geom_point(alpha=0.7, shape=21, size=2, position = position_jitter(width = 0.1, seed = 3922), 
             colour = "black") +  
  scale_color_manual(name = "diffc_m",
                     values = c("(-Inf,0]" = "cyan",
                                "(0.1, Inf]" = "magenta"),
                     labels = c("≤ 0", "> 0.1"), na.translate = F)+
  stat_summary(fun.y=median, geom="point", size=2, color="red") +
  
  ylab("ΔauROC to the best PWM") +
  xlab("Number of features")+
  geom_hline(yintercept = 0, linetype="dotted")+
  theme_minimal(base_size = 21) +
  theme(panel.grid = element_line(size = 0.5)) + 
  guides(color=guide_legend("ΔauROC"))


c

library(Cairo)

Cairo(file='Figure_4B_CHS_to_GHTS_ROC_15022024.pdf', type="pdf", width=200, height=200, units="mm")
c
dev.off()

