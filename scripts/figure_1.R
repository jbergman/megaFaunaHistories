library(ggplot2)
library(patchwork)

# load data
data_1 <- read.table("~data_1.txt", header = T, quote="",sep = "\t")
data_2 <- read.table("~data_2.txt", header = T, quote="",sep = "\t")
data_3 <- read.table("~data_3.txt", header = T, quote="",sep = "\t")
data_4 <- read.table("~data_4.txt", header = T, quote="",sep = "\t")

# order species by mass
data_1 <- data_1[order(data_1$Mass.g),]

# set species as factor
data_1$Species <- factor(data_1$Species, levels = unique(data_1$Species))

# exclude t = 0 for plotting purposes
data_1 <- subset(data_1, t > 0)

# Fig. 1a
f1a <- ggplot() +
  geom_step(aes(x=log10(data_1$midT), y=data_1$logNe, group=data_1$Species, col=log10(data_1$Mass.g)), size=0.35)+
  theme_classic()+
  annotate("rect", fill = "#377EB8", alpha = 0.5,
           xmin = log10(31998.73), xmax = log10(75682.07),
           ymin = -Inf, ymax = Inf)+
  xlab("Years ago (log10 scale)") +
  labs(tag = "a")+
  theme(legend.key.size = unit(0.5,"line"), legend.position = c(0.15,0.2), 
        legend.text=element_text(size=9),legend.title=element_text(size=11),
        legend.background = element_rect(colour = "black", size=0.4))+
  scale_color_continuous(name  = "Mass (kg)", 
                         breaks = log10(c(30000,100000,300000,1000000,3000000)), 
                         labels = c("30", "100", "300", "1000", "3000"), 
                         low = "#ffff33", high = "#e41a1c")+
  geom_ribbon(aes(x = log10(data_2$midT), ymin = data_2$lwr, ymax = data_2$upr), alpha = .15) +
  geom_line(aes(x = log10(data_2$midT), y = data_2$fit), lty=2)+
  scale_x_reverse(name = "Years ago (×100,000)", breaks=c(7,6,5,4,3), 
                  labels = c("100", "10", "1", "0.1", "0.01"))+
  scale_y_continuous(name = expression(paste("Effective size (", italic(N[e]), ")", sep="")), 
                     breaks=c(1,2,3,4,5,6), labels = expression(10^1,10^2,10^3,10^4,10^5,10^6))
f1a

# Fig. 1b
f1b <- ggplot(data_3) +
  geom_point(aes(x = mass, y = med, col=mass), size=0.75)+
  geom_segment(aes(x = mass, y=q025, yend=q975, xend=mass, col=mass))+
  geom_ribbon(aes(x=mass, ymin=q025Pred, ymax=q975Pred), fill = "black", alpha=0.25)+
  geom_line(aes(x=mass, y=medPred), lty = 2)+
  ylab("Slope") + 
  theme_classic()+
  labs(tag = "b")+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0.2, unit = "cm"), legend.key.size = unit(0.5,"line"))+
  geom_hline(yintercept = 0, lty=2)+ xlab("Mass (kg)")+
  scale_color_continuous(name = "Mass (kg)", low = "#ffff33", high = "#e41a1c",
                         breaks = log10(c(30000,100000,300000,1000000, 3000000)), 
                         labels = c("30", "100", "300", "1000", "3000"))+
  scale_x_continuous(name  = "Mass (kg)", breaks = log10(c(30000,100000,300000,1000000, 3000000)), 
                     labels = c("30", "100", "300", "1000", "3000"))+
  theme(legend.position = "none")
f1b

# Fig. 1c
f1c <- ggplot(data_4, aes(x=decSev*100)) + 
  geom_histogram()+
  xlab("Decline severity (%)")+
  ylab("No. species")+
  labs(tag = "c")+
  theme_classic()
f1c

# Fig. 1 
f1 <- ((f1a)/((f1b|f1c)+plot_layout(widths = c(1,1))))+
  plot_layout(heights = c(2,1))&
  theme(plot.tag = element_text(face = 'bold'))
f1