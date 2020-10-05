
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(viridis)
library(ggpubr)
library(reshape2)
library(cowplot)
#######################################################

rej_ref3_plot <- ggplot(mydat_3targets_rej,
                        aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5, alpha = 0.1) +
  
  ggtitle("Rejection ABC") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=8),
    plot.background = element_rect(fill = "#E8E8E8")
  ) +
  ylab('') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_3targets_ref,
             colour = "blue", size = 0.5, alpha = 0.02)+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "gold")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "gold")

annotation_b <- data.frame(
  x = c(0.17),
  y = c(0.13),
  label = "0.2"
)

rej_ref3_plot <- rej_ref3_plot + geom_text(data=annotation_b,
                                           aes( x=x, y=y, label=label),                 , 
                                           color="brown", 
                                           size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.015),
  y = c(0.03),
  label = "0.02"
)

rej_ref3_plot <- rej_ref3_plot + geom_text(data=annotation_g,
                                           aes( x=x, y=y, label=label),                 , 
                                           color="brown", 
                                           size=3, angle=0, fontface="bold")+


  theme_economist()+
  theme(axis.text=element_text(size=6),
        axis.title.x= element_text(size=10),
        axis.title.y= element_text(size=10),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.background = element_rect(fill = "#E8E8E8"),
        legend.position="none")
##################################################################  
# seq3

seq_ref3_plot <- ggplot(mydat_3targets_seq,
                        aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5, alpha = 0.1) +
  
  ggtitle("Seq ABC") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=8)
  ) +
  ylab('') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_3targets_ref,
             colour = "blue", size = 0.5, alpha = 0.02)+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "gold")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "gold")

annotation_b <- data.frame(
  x = c(0.17),
  y = c(0.13),
  label = "0.2"
)

seq_ref3_plot <- seq_ref3_plot + geom_text(data=annotation_b,
                                           aes( x=x, y=y, label=label),                 , 
                                           color="brown", 
                                           size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.015),
  y = c(0.03),
  label = "0.02"
)

seq_ref3_plot <- seq_ref3_plot + geom_text(data=annotation_g,
                                           aes( x=x, y=y, label=label),                 , 
                                           color="brown", 
                                           size=3, angle=0, fontface="bold")+


  theme_economist()+
  theme(axis.text=element_text(size=6),
        axis.title.x= element_text(size=10),
        axis.title.y= element_text(size=10),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.background = element_rect(fill = "#E8E8E8"),
        legend.position="none")
#############################################################
#  abcsmc 3
abcsmc_ref3_plot <- ggplot(mydat_3targets_abcsmc,
                           aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5, alpha = 0.1) +
  
  ggtitle("AbcSmc") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=8)
  ) +
  ylab('Gamma') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_3targets_ref,
             colour = "blue", size = 0.5, alpha = 0.02)+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "gold")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "gold")

annotation_b <- data.frame(
  x = c(0.17),
  y = c(0.13),
  label = "0.2"
)

abcsmc_ref3_plot <- abcsmc_ref3_plot + geom_text(data=annotation_b,
                                                 aes( x=x, y=y, label=label),                 , 
                                                 color="brown", 
                                                 size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.015),
  y = c(0.03),
  label = "0.02"
)

abcsmc_ref3_plot <- abcsmc_ref3_plot + geom_text(data=annotation_g,
                                                 aes( x=x, y=y, label=label),                 , 
                                                 color="brown", 
                                                 size=3, angle=0, fontface="bold")+
  theme_economist()+
  theme(axis.text=element_text(size=6),
        axis.title.x= element_text(size=10),
        axis.title.y= element_text(size=10),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.background = element_rect(fill = "#E8E8E8"),
        legend.position="none")
###############################################################

# BC_SIR3

BC_SIR_ref3_plot <- ggplot(mydat_3targets_BC_SIR,
                           aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5, alpha = 0.1) +
  
  ggtitle("BcSIR") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=8)
  ) +
  ylab('') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_3targets_ref,
             colour = "blue", size = 0.5, alpha = 0.02)+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "gold")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "gold")

annotation_b <- data.frame(
  x = c(0.17),
  y = c(0.13),
  label = "0.2"
)

BC_SIR_ref3_plot <- BC_SIR_ref3_plot + geom_text(data=annotation_b,
                                                 aes( x=x, y=y, label=label),                 , 
                                                 color="brown", 
                                                 size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.015),
  y = c(0.03),
  label = "0.02"
)

BC_SIR_ref3_plot <- BC_SIR_ref3_plot + geom_text(data=annotation_g,
                                                 aes( x=x, y=y, label=label),                 , 
                                                 color="brown", 
                                                 size=3, angle=0, fontface="bold")+

  theme_economist()+
  theme(axis.text=element_text(size=6),
        axis.title.x= element_text(size=10),
        axis.title.y= element_text(size=10),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.background = element_rect(fill = "#E8E8E8"),
        legend.position="none")
############################################################
# imis3

imis_ref3_plot <- ggplot(mydat_3targets_imis,
                         aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5, alpha = 0.1) +
  
  ggtitle("BcIMIS") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=8)
  ) +
  ylab('') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_3targets_ref,
             colour = "blue", size = 0.5, alpha = 0.02)+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "gold")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "gold")

annotation_b <- data.frame(
  x = c(0.17),
  y = c(0.13),
  label = "0.2"
)

imis_ref3_plot <- imis_ref3_plot + geom_text(data=annotation_b,
                                             aes( x=x, y=y, label=label),                 , 
                                             color="brown", 
                                             size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.015),
  y = c(0.03),
  label = "0.02"
)

imis_ref3_plot <- imis_ref3_plot + geom_text(data=annotation_g,
                                             aes( x=x, y=y, label=label),                 , 
                                             color="brown", 
                                             size=3, angle=0, fontface="bold")+
  theme_economist()+
  theme(axis.text=element_text(size=6),
        axis.title.x= element_text(size=10),
        axis.title.y= element_text(size=10),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.background = element_rect(fill = "#E8E8E8"),
        legend.position="none")
###########################################################

#######################################
# refs 
ref3_plot <- ggplot(mydat_3targets_ref,
                    aes(x=mydat_3targets_ref$beta, 
                        y=mydat_3targets_ref$gamma)) +
  
  geom_point( size = 0.5, alpha = 0.1, col = "blue") +
  
  ggtitle("Reference") +
  
  theme(
    plot.title = element_text(size=8)
  ) +
  ylab('Gamma') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_hline(yintercept=0.02, alpha = 0.7,color = "gold")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "gold")

annotation_b <- data.frame(
  x = c(0.17),
  y = c(0.13),
  label = "0.2"
)

ref3_plot <- ref3_plot + geom_text(data=annotation_b,
                                   aes( x=x, y=y, label=label),                 , 
                                   color="brown", 
                                   size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.015),
  y = c(0.03),
  label = "0.02"
)

ref3_plot <- ref3_plot + geom_text(data=annotation_g,
                                   aes( x=x, y=y, label=label),                 , 
                                   color="brown", 
                                   size=3, angle=0, fontface="bold")+

  
  theme_economist()+
  theme(axis.text=element_text(size=6),
        axis.title.x= element_text(size=10),
        axis.title.y= element_text(size=10),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.background = element_rect(fill = "#E8E8E8"),
        legend.position="none")

############################################################

# put plots together
Posterior_plots_S2 <- plot_grid(ref3_plot,
          rej_ref3_plot,
          BC_SIR_ref3_plot,
          abcsmc_ref3_plot,
          seq_ref3_plot,
          imis_ref3_plot,
          #labels = c("A", "B", "C"),
          ncol = 3, nrow = 2)



# save figures
### change size to smaller than 10mb
ggsave(filename = "Posterior_plots_S2.tiff",
       device="tiff", dpi = 300, width= 19.05, height = 13, units="cm", 
       path = "C:\\Users\\Zee\\Documents\\GitHub\\Calibration_manuscript\\Plots", )

