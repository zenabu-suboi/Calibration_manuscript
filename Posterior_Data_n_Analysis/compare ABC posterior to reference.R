### COMPARE ABC POSTERIOR TO reference POSTERIOR
library(ggpubr)
library(ggplot2)
library(cowplot)
library(scales)

### scenario 1

### percentage overlap:
(1-(sum(abs(ras.rej2.mat-ras.ref2.mat)))/ (2*5000))*100 # 2.3%

(1-(sum(abs(ras.seq2.mat-ras.ref2.mat)))/ (2*5000))*100 # 21.6%

(1-(sum(abs(ras.abcsmc2.mat-ras.ref2.mat)))/ (2*5000))*100 # 53.9%

(1-(sum(abs(ras.BC_SIR2.mat-ras.ref2.mat)))/ (2*5000))*100 # 34.8%

(1-(sum(abs(ras.imis2.mat-ras.ref2.mat)))/ (2*5000))*100 # 72.7%

###################################################################
### scenario 2

### percentage overlap:
(1-(sum(abs(ras.rej3.mat-ras.ref3.mat)))/ (2*5000))*100 # 1.8%

(1-(sum(abs(ras.seq3.mat-ras.ref3.mat)))/ (2*5000))*100 # 62.7%

(1-(sum(abs(ras.BC_SIR3.mat-ras.ref3.mat)))/ (2*5000))*100 # 39.8%

(1-(sum(abs(ras.imis3.mat-ras.ref3.mat)))/ (2*5000))*100 # 85.5%

(1-(sum(abs(ras.abcsmc3.mat-ras.ref3.mat)))/ (2*5000))*100 # 72.9%


#############################################################################


#install.packages("ggpubr")

############################################

Rej <- c(2.3, 1.8) # percentage overlap for rej in both scenarios
Seq <- c(21.6, 62.7) # percentage overlap for seq in both scenarios
BC_SIR <- c(34.8, 39.8) # percentage overlap for bmle in both scenarios
IMIS <- c(72.7, 85.5) 
abcsmc <- c(53.9, 72.9)# percentage overlap for tom in both scenarios

Scenario <- c(1, 2) # scenarios considered

################################################################
#######################################################

toplot = data.frame(Scenario = rep(Scenario, 5),
                    Percentage_overlap = c(Rej, Seq, abcsmc, BC_SIR, IMIS),
                    class = c(rep('Rejection ABC',2),
                              rep('Seq ABC',2),
                              rep('AbcSmc',2),
                              rep('BcSIR',2),
                              rep('BcIMIS',2)))

toplot$class <- factor(toplot$class, levels = c("Rejection ABC", "BcSIR",
                                                "AbcSmc", "Seq ABC",
                                                "BcIMIS"))


#View(toplot)
q = ggdotplot(toplot, x = 'Scenario',
              y = 'Percentage_overlap',
              color = "Scenario",
              palette =  c("red","blue"),
              binwidth = 2, fill = 'Scenario')+
  facet_wrap(~class,ncol = 5)
 # ggtitle("Percentage overlap plot")
p_overlap <- q + #theme_economist()+
  theme( panel.background = element_rect(fill = "gray93", 
                                                        colour = "white",
                                           size = 0.3, linetype = "solid"),
           panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                           colour = "white"), 
           panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                           colour = "white", ),
         
         
        axis.text=element_text(size=10),
         #plot.title = element_text(size=10),
        # plot.subtitle = element_text(size=8),
         axis.title.x= element_text(size=11, face = "bold"),
         axis.title.y= element_text(size=11, face = "bold"),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
         #plot.background = element_rect(fill = "gray93")
         #legend.position="none"
        )+

           scale_y_continuous(name = "Percentage Overlap", breaks=seq(0,100,20),
                               labels = function(x) paste0(x,"%"))
                              
                               


# save figures
### change size to smaller than 10mb
ggsave(filename = "Percentage_overlap_plot.tiff",
       device="tiff", dpi = 300, width= 19.05, height = 12, units="cm", 
       path = "C:\\Users\\Zee\\Documents\\GitHub\\Calibration_manuscript\\Plots", )

