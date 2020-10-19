
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(viridis)
library(ggpubr)
library(reshape2)
library(cowplot)
#######################################################

rej_ref2_plot <- ggplot(mydat_2targets_rej,
                        aes(x=beta, y=gamma)) +
  
  geom_point( size = 0.5, alpha = 0.1) +
  
  labs(#title = "Posterior plots for scenario 1",
       title = "Rejection ABC",
       x = "Beta",
       y = "")+
  
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_2targets_ref,
             color = "red",size = 0.5,alpha = 0.02)+
  
  geom_hline(yintercept=0.02, alpha = 0.7,color = "gold")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "gold")

annotation_b <- data.frame(
  x = c(0.17),
  y = c(0.13),
  label = "0.2"
)

rej_ref2_plot <- rej_ref2_plot + geom_text(data=annotation_b,
                                           aes( x=x, y=y, label=label),                 , 
                                           color="brown", 
                                           size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.015),
  y = c(0.03),
  label = "0.02"
)

rej_ref2_plot <- rej_ref2_plot + geom_text(data=annotation_g,
                                           aes( x=x, y=y, label=label),                 , 
                                           color="brown", 
                                           size=3, angle=0, fontface="bold")+

  theme_economist()+
  theme(axis.text=element_text(size=8),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8),
        axis.title.x= element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.background = element_rect(fill = "gray93"),
        legend.position="none")
##################################################################  
# seq2

seq_ref2_plot <- ggplot(mydat_2targets_seq,
                        aes(x=beta, y=gamma)) +
  geom_point( size = 0.5, alpha = 0.1) +
  
  
  labs(#title = "",
       title = "Seq ABC",
       x = "Beta",
       y = "")+
  
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_2targets_ref,
             colour = "red", size = 0.5,alpha = 0.02)+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "gold")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "gold")

annotation_b <- data.frame(
  x = c(0.17),
  y = c(0.13),
  label = "0.2"
)

seq_ref2_plot <- seq_ref2_plot + geom_text(data=annotation_b,
                                           aes( x=x, y=y, label=label),                 , 
                                           color="brown", 
                                           size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.015),
  y = c(0.03),
  label = "0.02"
)

seq_ref2_plot <- seq_ref2_plot + geom_text(data=annotation_g,
                                           aes( x=x, y=y, label=label),                 , 
                                           color="brown", 
                                           size=3, angle=0, fontface="bold")+

theme_economist()+
  theme(axis.text=element_text(size=8),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8),
        axis.title.x= element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.background = element_rect(fill = "gray93"),
        legend.position="none")
#############################################################
#  abcsmc 2
abcsmc_ref2_plot <- ggplot(mydat_2targets_abcsmc,
                           aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5, alpha = 0.1) +
  
  
  labs(#title = "",
       title = "AbcSmc",
       x = "Beta",
       y = "Gamma")+
  
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_2targets_ref,
             colour = "red", size = 0.5, alpha = 0.02)+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "gold")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "gold")

annotation_b <- data.frame(
  x = c(0.17),
  y = c(0.13),
  label = "0.2"
)

abcsmc_ref2_plot <- abcsmc_ref2_plot + geom_text(data=annotation_b,
                                                 aes( x=x, y=y, label=label),                 , 
                                                 color="brown", 
                                                 size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.015),
  y = c(0.03),
  label = "0.02"
)

abcsmc_ref2_plot <- abcsmc_ref2_plot + geom_text(data=annotation_g,
                                                 aes( x=x, y=y, label=label),                 , 
                                                 color="brown", 
                                                 size=3, angle=0, fontface="bold")+

theme_economist()+
  theme(axis.text=element_text(size=8),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8),
        axis.title.x= element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.background = element_rect(fill = "gray93"),
        legend.position="none")
###############################################################

# BC_SIR2

BC_SIR_ref2_plot <- ggplot(mydat_2targets_BC_SIR,
                           aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5, alpha = 0.1) +
  
  
  labs(#title = "",
       title = "BcSIR",
       x = "Beta",
       y = "")+
 
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_2targets_ref,
             colour = "red", size = 0.5, alpha = 0.02)+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "gold")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "gold")

annotation_b <- data.frame(
  x = c(0.17),
  y = c(0.13),
  label = "0.2"
)

BC_SIR_ref2_plot <- BC_SIR_ref2_plot + geom_text(data=annotation_b,
                                                 aes( x=x, y=y, label=label),                 , 
                                                 color="brown", 
                                                 size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.015),
  y = c(0.03),
  label = " 0.02"
)

BC_SIR_ref2_plot <- BC_SIR_ref2_plot + geom_text(data=annotation_g,
                                                 aes( x=x, y=y, label=label),                 , 
                                                 color="brown", 
                                                 size=3, angle=0, fontface="bold")+


theme_economist()+
  theme(axis.text=element_text(size=8),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8),
        axis.title.x= element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.background = element_rect(fill = "gray93"),
        legend.position="none")
############################################################
# imis

imis_ref2_plot <- ggplot(mydat_2targets_imis,
                         aes(x=beta, y=gamma)) +
  geom_point( size = 0.5, alpha = 0.1) +
  
  
  labs(#title = "",
       title = "BcIMIS",
       x = "Beta",
       y = "")+
 
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_2targets_ref,
             colour = "red", size = 0.5,alpha = 0.02)+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "gold")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "gold")

annotation_b <- data.frame(
  x = c(0.17),
  y = c(0.13),
  label = "0.2"
)

imis_ref2_plot <- imis_ref2_plot + geom_text(data=annotation_b,
                                             aes( x=x, y=y, label=label),                 , 
                                             color="brown", 
                                             size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.015),
  y = c(0.03),
  label = "0.02"
)

imis_ref2_plot <- imis_ref2_plot + geom_text(data=annotation_g,
                                             aes( x=x, y=y, label=label),                 , 
                                             color="brown", 
                                             size=3, angle=0, fontface="bold")+

theme_economist()+
  theme(axis.text=element_text(size=8),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8),
        axis.title.x= element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.background = element_rect(fill = "gray93"),
        legend.position="none")
###########################################################

#######################################
# refs 
ref2_plot <- ggplot(mydat_2targets_ref,
                    aes(x=mydat_2targets_ref$beta, 
                        y=mydat_2targets_ref$gamma)) +
  
  geom_point( size = 0.5, alpha = 0.1, col = "red") +
  
 
  labs(#title = "",
       title = "Reference",
       x = "Beta",
       y = "Gamma")+

  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_hline(yintercept=0.02, alpha = 0.7,color = "gold")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "gold")

annotation_b <- data.frame(
  x = c(0.17),
  y = c(0.13),
  label = "0.2"
)

ref2_plot <- ref2_plot + geom_text(data=annotation_b,
                                   aes( x=x, y=y, label=label),                 , 
                                   color="brown", 
                                   size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.015),
  y = c(0.03),
  label = "0.02"
)

ref2_plot <- ref2_plot + geom_text(data=annotation_g,
                                   aes( x=x, y=y, label=label),                 , 
                                   color="brown", 
                                   size=3, angle=0, fontface="bold")+

  
  theme_economist()+
  theme(axis.text=element_text(size=8),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8),
        axis.title.x= element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.background = element_rect(fill = "gray93"),
        legend.position="none")

############################################################

# put plots together using plotgrid
Posterior_plots_S1 <- plot_grid(ref2_plot,
                      rej_ref2_plot,
                      BC_SIR_ref2_plot,
                      abcsmc_ref2_plot,
                      seq_ref2_plot,
                      imis_ref2_plot,
                      #labels = c("A", "B", "C"),
                      ncol = 3, nrow = 2)



# save figures
### change size to smaller than 10mb
ggsave(filename = "Posterior_plots_S1.tiff",
       device="tiff", dpi = 300, width= 19.05, height = 13, units="cm", 
       path = "C:\\Users\\Zee\\Documents\\GitHub\\Calibration_manuscript\\Plots", )

