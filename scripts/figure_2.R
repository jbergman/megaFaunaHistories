library(ggplot2)
library(patchwork)

# load data
data_5 <- read.table("~data_5.txt", header = T, quote="",sep = "\t")
data_6 <- read.table("~data_6.txt", header = T, quote="",sep = "\t")
data_7 <- read.table("~data_7.txt", header = T, quote="",sep = "\t")
data_8 <- read.table("~data_8.txt", header = T, quote="",sep = "\t")
data_9 <- read.table("~data_9.txt", header = T, quote="",sep = "\t")

# Fig. 2a
f2a_upper <- ggplot(data_5, aes(x=midT, y=log10(Ne), group=Species)) +
  annotate("rect", xmin=12000, xmax=0, ymin=-Inf, ymax=Inf, fill = "#e41a1c") +
  annotate("rect", xmin=115000, xmax=130000, ymin=-Inf, ymax=Inf, fill="#e41a1c") +
  annotate("rect", xmin=245000, xmax=235000, ymin=-Inf, ymax=Inf, fill="#e41a1c") +
  annotate("rect", xmin=337000, xmax=320000, ymin=-Inf, ymax=Inf, fill="#e41a1c") +
  annotate("rect", xmin=424000, xmax=385000, ymin=-Inf, ymax=Inf, fill="#e41a1c") + 
  annotate("rect", fill = "#377EB8", xmin = (31998.73), xmax = (75682.07),ymin = -Inf, ymax = Inf)+
  geom_line(col="gray", size=0.5)+
  theme_bw()+
  labs(tag = "a")+
  geom_smooth(aes(group = NULL), method="loess", col="black", fill = "#ffff33", lwd=0.7, alpha=1, lty=2)+
  xlab("Years ago (log10 scale)") + ylab(expression(italic(N[e]))) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0.2, unit = "cm"), legend.key.size = unit(0.5,"line"),
        axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())+
  scale_x_reverse(name = "Years ago (×100,000)", breaks=c(700000,600000,500000,400000,300000,200000,100000,0), 
                  labels = expression(7,6,5,4,3,2,1,0), limits=c(742419,0))+
  scale_y_continuous(name = expression(paste("Effective size (", italic(N[e]), ")", sep="")), 
                     breaks=log10(c(100,1000,10000,100000,1000000)), 
                     labels = expression(10^2,10^3, 10^4, 10^5, 10^6), limits=log10(c(1000,1100000))) 

f2a_upper_inset <- ggplot(data_6, aes(x = time, y = log10(ne))) +
  geom_boxplot(outlier.size = 0.3)+
  theme_bw()+
  ylab(expression(italic(N[e])))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust=1, color="black"),
        axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 7), plot.background = element_rect(colour = "black"))+
  scale_y_continuous(name = expression(italic(N[e])), 
                     breaks=log10(c(100,1000,10000,100000,1000000)), 
                     labels = expression(10^2,10^3, 10^4, 10^5, 10^6))

f2a_upper <- f2a_upper + annotation_custom(ggplotGrob(f2a_upper_inset), xmin = -750000, xmax = -550000, ymin = 3, ymax = 4.5)
f2a_upper

f2a_lower <- ggplot(data_7, aes(x=Topage.yrBP-38, y=deltaD.permill)) +
  annotate("rect", xmin=12000, xmax=0, ymin=-Inf, ymax=Inf, fill = "#e41a1c") +
  annotate("rect", xmin=115000, xmax=130000, ymin=-Inf, ymax=Inf, fill="#e41a1c") +
  annotate("rect", xmin=245000, xmax=235000, ymin=-Inf, ymax=Inf, fill="#e41a1c") +
  annotate("rect", xmin=337000, xmax=320000, ymin=-Inf, ymax=Inf, fill="#e41a1c") +
  annotate("rect", xmin=424000, xmax=385000, ymin=-Inf, ymax=Inf, fill="#e41a1c") +
  annotate("rect", fill = "#377EB8", xmin = (31998.73), xmax = (75682.07),ymin = -Inf, ymax = Inf)+
  geom_line(size=0.5) +
  theme_bw() +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0.2, unit = "cm"), legend.key.size = unit(0.5,"line"),
        axis.title.x = element_text(vjust = 12))+
  scale_x_reverse(name = "Years ago (×100,000)", breaks=c(700000,600000,500000,400000,300000,200000,100000,0), labels = expression(7,6,5,4,3,2,1,0), limits=c(742419,0))+
  ylab(expression(paste(italic(delta),italic(D)," (‰)")))
f2a_lower

# Fig. 2b
f2b_upper <- ggplot(data_8, aes(x=midT, y=logNe, group=interaction(midT,nType), fill=nType)) +
  geom_boxplot(outlier.size = 0.3)+
  theme_classic()+
  labs(tag = "b")+
  xlab("Years ago (log10 scale)") + ylab(expression(italic(N[e]))) +
  theme(legend.key.size = unit(0.5,"line"), 
        legend.title = element_blank(), legend.margin=margin(0,0,0,0),
        axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())+
  scale_x_reverse(name = "Years ago (×100,000)", breaks=c(100000,50000,0), 
                  labels = expression(1,0.5,0), limits=c(100000,0))+
  scale_y_continuous(name = expression(paste("Effective size (", italic(N[e]), ")", sep="")), 
                     breaks=log10(c(100,1000,10000,100000,1000000,10000000)), 
                     labels = expression(10^2,10^3, 10^4, 10^5, 10^6, 10^7))+
  scale_fill_brewer(palette="Set1")
f2b_upper

f2b_lower <- ggplot(data_9, aes(x=midT, y=log10(meanSE), group=midT)) +
  geom_boxplot(outlier.size = 0.3)+
  theme_classic()+
  ylab("MSE")+
  theme(legend.key.size = unit(0.5,"line"), 
        axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color="black"),
        axis.title.y = element_text(vjust = 0)) +
  scale_x_reverse(name = "Years ago (×100,000)", breaks=c(87500, 62500, 37500, 12500), 
                  labels = c("75-100 kya", "50-75 kya", "25-50 kya", "0-25 kya"), limits=c(100000,0))+
  scale_y_continuous(name = "MSE", breaks=log10(c(0.1, 0.3, 1, 3)), labels = c(0.1, 0.3, 1, 3)) 
f2b_lower

# Fig. 2
f2 <- ((f2a_upper/f2a_lower +  plot_layout(heights = c(5,1))) | (f2b_upper/f2b_lower +  plot_layout(heights = c(1.5,1.5)))) +
  plot_layout(widths = c(2,1))&
  theme(plot.tag = element_text(face = 'bold'))
f2