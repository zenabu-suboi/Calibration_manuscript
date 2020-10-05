require(ggthemes)
require(ggplot2)
require(ggpubr)

# Change box plot colors by groups
p1 <-ggplot(x, aes(x=Parameter_search_strategy2, y=CALIB, fill=Parameter_search_strategy)) +
  geom_boxplot()

#mycomparison <- list(c("Unidentifiable", "Sampling"))

p1 <- p1 + scale_fill_manual(values=c( "#fdae61", "#abd9e9", "#2c7bb6","#d7191c"))+
  coord_fixed(ratio = 0.05) +
  xlab("Parameter search strategy") +
  ylab("# calibrated parameters") +
  theme_economist()+
  theme(axis.text=element_text(size=6),
        axis.title.x= element_text(size=12),
        axis.title.y= element_text(size=12),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
        legend.position="none")+
  stat_compare_means(size=3, label.x= 1.2)+
  geom_signif(comparisons=list(c("Unidentifiable", "Sampling")), annotations="*",
              y_position = 78, tip_length = 0.01, vjust=0.4,
              textsize=6) 
#stat_compare_means(comparisons = mycomparison)


# Change box plot colors by groups
p2 <-ggplot(x, aes(x=Parameter_search_strategy2, y=TARGETS, fill=Parameter_search_strategy)) +
  geom_boxplot()

p2 <- p2 + scale_fill_manual(values=c( "#fdae61", "#abd9e9", "#2c7bb6","#d7191c"))+
  coord_fixed(ratio = 0.0145) +
  theme_economist()+
  theme(axis.text=element_text(size=6),
        axis.title.x= element_text(size=12),
        axis.title.y= element_text(size=12),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
        legend.position="none")+
  xlab("Parameter search strategy") +
  ylab("# target statistics") +
  stat_compare_means(size=3, label.x= 1.2)+
  geom_signif(comparisons=list(c("Unidentifiable", "Sampling")), annotations="*",
              y_position = 260, tip_length = 0.01, vjust=0.4,
              textsize=6) 


#ggarrange(p1,p2, align="hv", labels = c("A", "B"), hjust=-2, ncol = 2)
p1 + p2
p1 + labs(tag = 'A') + p2 + labs(tag = 'B')

ggsave(filename = "TARGETSandCALIBbyPSS_actual.tiff", device="tiff" , path = "C:\\Users\\marijnhazelbag\\OneDrive - Stellenbosch University\\Review calibration\\FINAL PLAN\\Manuscript\\Results extra google form numbers targets etc\\", )

### change size to smaller then 10mb
ggsave(filename = "TARGETSandCALIBbyPSS_actual_S.tiff", device="tiff" ,dpi = 300 ,width= 19.05,units="cm", path = "C:\\Users\\marijnhazelbag\\OneDrive - Stellenbosch University\\Review calibration\\FINAL PLAN\\Manuscript\\Results extra google form numbers targets etc\\", )

