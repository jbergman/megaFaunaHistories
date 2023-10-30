library(ggplot2)
library(patchwork)

# load data
data_13 <- read.table("~data_13.txt", header = T, quote="",sep = "\t")
data_14 <- read.table("~data_14.txt", header = T, quote="",sep = "\t")

# sort data.frames
data_13$pType <- factor(data_13$pType, levels = c("Census size", "Biomass", "Energy turnover"))
data_13$period <- factor(data_13$period, levels = c("Baseline", "75-100 kya", "50-75 kya", "25-50 kya", "0-25 kya", "Current"))

data_14$pType <- factor(data_14$pType, levels = c("Census size", "Biomass", "Energy turnover"))
data_14$period <- factor(data_14$period, levels = c("Baseline (included)", "Baseline (missing)", "Baseline (extinct)", "Current (included)", "Current (missing)"))

# Fig. 4a
f4a <- ggplot(data_13, aes(x=pType, y=(med_norm)*100, group=pType))+
  geom_bar(stat="identity", position = "dodge", fill="#377eb8")+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title = element_blank(), legend.text.align = 0,
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color="black"),
        strip.text = element_text(size = 7.25))+
  labs(tag = "a")+
  ylab("Baseline proportion (%)")+
  facet_grid(~period)+
  scale_alpha_manual(values=c(1,1,1,1,1,1), guide = "none")
f4a

# Fig. 4b
f4b <- ggplot(data_14, aes(x = pType, y=med_norm*100, fill=period3))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.title.x = element_blank(), legend.title = element_blank(), legend.text.align = 0, 
        legend.key.size = unit(0.5,"line"), 
        strip.text = element_text(size = 7.25),
        legend.position = c(1,0.75),
        plot.margin = margin(0,37,0,-10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color="black")) +
  labs(tag = "b")+
  ylab("Baseline proportion (%)")+
  facet_grid(~period2)+
  ylim(0,105)+
  scale_fill_manual(values=c("#377eb8", "#a6cee3", "gray"))
f4b

# Fig. 4
f4 <- ((f4a|f4b)+plot_layout(widths = c(3, 1)))&
  theme(plot.tag = element_text(face = 'bold'))
f4