library(ggplot2)
library(patchwork)
library(RColorBrewer)

# load data
data_10 <- read.table("~data_10.txt", header = T, quote="",sep = "\t")
data_11 <- read.table("~data_11.txt", header = T, quote="",sep = "\t")
data_12 <- read.table("~data_12.txt", header = T, quote="",sep = "\t")

# order models
data_10$mType <- factor(data_10$mType, levels = rev(c("logH","expH","linH","linT+linP+L+linH","quadT+L+expH","linT+linP+L+expH","quadT+linH","quadT+L+linH",
                                            "quadT+expH","quadT+L+pH","linT+L+linH","linT+L+expH","pH","quadT+L+logH","linT+linP+L+pH","quadT+pH",
                                            "linT+L+pH","linT+linP+L+logH","linT+L+logH","quadT+logH","quadT+L","linT+linP+L","linT+L","quadT",
                                            "linP","linT","quadT+quadP+L","quadP","linT+linP","quadT+quadP","linP+L","quadP+L")))
data_10$Model <- factor(data_10$Model, levels = c("Climate-only", "Human-only", "Combined"))

data_11$Model <- factor(data_11$Model, levels = c("Climate-only", "Human-only", "Combined"))
data_11$times <- factor(data_11$times, levels = c("0-742 kya", "75-100 kya", "50-75 kya", "25-50 kya", "0-25 kya"))

data_12 <- data_12[order(data_12$midT),]
data_12$Model <- factor(data_12$Model, levels = c("Climate-only", "Human-only", "Combined"))

# set colours
my_colours <- RColorBrewer::brewer.pal(5, "Set1")[c(1,2,5)]

# Fig. 3a
f3a <- ggplot(subset(data_10, mType%in%c("logH","expH","linH","linT+linP+L+linH","quadT+L+expH","linT+linP+L+expH","quadT+linH","pH","quadT+L","linT+linP+L","linT+L","quadT"))) +
  geom_point(aes(x = mType, y = Log, col=Model), size=0.75)+
  geom_segment(aes(x = mType, y=loo_lower, yend=loo_upper, xend=mType, col=Model))+
  geom_hline(yintercept = -128.44422452366314, lty=2,col="#e41a1c")+
  coord_flip()+
  theme_bw()+
  labs(tag = "a")+
  ylab("Log-score")+
  theme(legend.position = "none",
        axis.title.y = element_blank(), axis.text.y = element_text(family = "Times", face = "italic"), 
        axis.title.x = element_text(vjust = 12))+
  scale_color_manual(values = my_colours)
f3a

# Fig. 3b
f3b <- ggplot(data_11, aes(x = as.factor(times), y=log10(MSE), fill=Model)) + 
  geom_boxplot(outlier.size = 0.25, lwd=0.4)+
  theme_classic()+
  labs(tag = "b")+
  ylab("MSE")+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0.2, unit = "cm"),
        legend.key.size = unit(0.5,"line"),
        axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color="black"))+
  scale_y_continuous(name = "MSE", breaks=log10(c(0.1, 0.3, 1, 3)), labels = c(0.1, 0.3, 1, 3))+
  scale_fill_manual(values = my_colours)

f3b

# Fig. 3c
f3c_upper <- ggplot(data_12, aes(x=midT, y=meanPred, group=interaction(Species,Model), fill=Model))+
  geom_line(col="gray", lwd=0.1)+
  geom_smooth(aes(y = logNe, group = NULL), method="loess", col="#ffff33", fill = "#ffff33", lwd=0.5, alpha=1, lty="dashed")+
  geom_ribbon(stat = "smooth", method = "loess", span=0.5, alpha=0.75, lwd=0.5, aes(fill=Model,group=NULL))+
  facet_wrap(~Model, ncol=3)+
  theme_bw()+
  labs(tag = "c")+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0.2, unit = "cm"), legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_text(vjust = -15.5))+
  scale_fill_manual(values = my_colours)+
  scale_color_manual(values = my_colours)+
  scale_x_reverse(name = "Years ago (×100,000)", breaks=c(700000,600000,500000,400000,300000,200000,100000,0), labels = expression(7,6,5,4,3,2,1,0))+
  scale_y_continuous(name = expression(paste("Effective size (", italic(N[e]), ")", sep="")), 
                     breaks=log10(c(100,1000,10000,100000,1000000)), 
                     labels = expression(10^2,10^3, 10^4, 10^5, 10^6))
f3c_upper

f3c_lower <- ggplot(data_12, aes(x=midT, y=(seMean), group=interaction(Species,Model), fill=Model))+
  geom_line(col="gray", lwd=0.075)+
  geom_smooth(aes(group=Model, color=Model), method = "loess", se=FALSE, lwd=0.5)+
  facet_wrap(~Model, ncol=3)+
  theme_bw()+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0.2, unit = "cm"), legend.position = "none", strip.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.title.y = element_text(vjust = -15.5))+
  scale_fill_manual(values = my_colours)+
  scale_color_manual(values = my_colours)+
  scale_x_reverse(name = "Years ago (×100,000)", breaks=c(700000,600000,500000,400000,300000,200000,100000,0), 
                  labels = expression(7,6,5,4,3,2,1,0))+
  scale_y_log10(name = "MSE", breaks=c(0.1, 0.3, 1))
f3c_lower

# Fig. 3
f3 <- (((f3a|f3b) + plot_layout(widths = c(1.5,2)))/plot_spacer()/
         ((f3c_upper/f3c_lower)+ plot_layout(heights = c(3,2))))+
  plot_layout(heights = c(1.2,-0.35,2))&
  theme(plot.tag = element_text(face = "bold"))
f3