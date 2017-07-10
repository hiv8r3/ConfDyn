require(ggplot2)


grandavg = read.delim("./GrandAvg_forPlotting.txt")


### make plots

top = 7.5
bottom = -6

N2box = annotate("rect",  
                 xmin=220, xmax=320, ymin=bottom+.05, ymax=top-.05, alpha=.04,
                 color="black") 

FSWbox =  annotate("rect",
                   xmin=600, xmax=1150, ymin=bottom+.05, ymax=top-.05,
                   alpha=.04,
                   color="black")

ERPline = geom_line(lwd=1.1,
                    aes(color = Cond, linetype = Cond))

condLinetype = c("longdash", "solid", "longdash", "solid")

condColors = c("navy", "navy", "magenta1", "magenta1")

none = element_blank() 



# Individual electrodes ----------------------------------------------------------------

ggplot(data=grandavg, aes(Time, FZ, group = Cond)) + 
  ERPline + 
  N2box +
  FSWbox +
  # add label for electrode
  annotate("text", label = "FZ", x = -50, y = 6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1200), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200)) +
  geom_hline(yintercept=0) + # adds x axis
  scale_y_continuous(limits =c(-7.5, 7.5)) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values=condColors) +
  scale_linetype_manual(values=condLinetype) +
  guides(color=FALSE) # Remove legend 




# Average electrode -------------------------------------------------------

ggplot(data=grandavg, aes(Time, avgElec9, group = Cond)) + 
  ERPline + 
  N2box +
  FSWbox +
  # add label for electrode
  #annotate("text", label = "All electrodes", x = 150, y = 6.5, size = 8, colour = "black") +
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1200), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(limits =c(bottom, top), expand=c(0,0)) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values=condColors) +
  #ggtitle("All subjects") +
  scale_linetype_manual(values=condLinetype) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
#  guides(color=FALSE) # Remove legend 

ggsave("./Figures/GrandAverage_allElec_allSubjects.tiff", width = 14, height = 8)



