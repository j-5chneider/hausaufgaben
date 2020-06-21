### Zeit 1 ##################################################################################################
source("R_rainclouds.R")

# data_summary <- function(data, varname, groupnames){
#     require(plyr)
#     summary_func <- function(x, col){
#         c(mean = mean(x[[col]], na.rm=TRUE),
#           sd = sd(x[[col]], na.rm=TRUE))
#     }
#     data_sum<-ddply(data, groupnames, .fun=summary_func,
#                     varname)
#     data_sum <- rename(data_sum, c("mean" = varname))
#     return(data_sum)
# }

######### BEGINN HA #######################
#### 45 Minuten
library(tidyverse)
beg_45_p <-
    ggplot(p_data%>%dplyr::filter(stunde == 45), aes(x="", y = beg_hw_min, fill = Schulart, colour = Schulart)) +
    geom_flat_violin(position = position_nudge(x = 0, y = 0), adjust = 1.6, trim = F, alpha = .3) +
    geom_boxplot(aes(x=""), position = position_nudge(x = c(.49, .54), y = 0), 
                 outlier.shape = NA, alpha = .5, width = .04, colour = "black") +
    geom_hline(yintercept = 45, linetype = "dashed", colour = "#696f71", size = 1) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(0, 10,20,30,40,45,50,60), limits = c(0, 65)) +
    scale_x_discrete(expand = c(0, 0)) +
    ylab("Minuten seit geplantem Stundenbeginn bei Vergabe der HA") +
    xlab("Dichte") +
    ggtitle("Einzelstunden") +
    theme_light() +
    coord_flip()

beg_90_p <-
    ggplot(p_data%>%dplyr::filter(stunde == 90), aes(x="", y = beg_hw_min, fill = Schulart, colour = Schulart)) +
    geom_flat_violin(position = position_nudge(x = 0, y = 0), adjust = 1.6, trim = F, alpha = .3) +
    geom_boxplot(aes(x=""), position = position_nudge(x = c(.49, .54), y = 0), 
                 outlier.shape = NA, alpha = .5, width = .04, colour = "black") +
    geom_hline(yintercept = 90, linetype = "dashed", colour = "#696f71", size = 1) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(0, 10,20,30,40,50,60,70,80,90), limits = c(0, 95)) +
    scale_x_discrete(expand = c(0, 0)) +
    ylab("Minuten seit geplantem Stundenbeginn bei Vergabe der HA") +
    # xlab("Dichte") +
    xlab("") +
    ggtitle("Doppelstunden") +
    theme_light() +
    # theme(axis.line = element_line(colour = "#696f71"),
    #       panel.grid.major = element_blank(),
    #       panel.grid.minor = element_blank(),
    #       panel.border = element_blank(),
    #       panel.background = element_rect(fill = "#f6f7f7")) +
    coord_flip()

library(ggpubr)
ggarrange(beg_45_p, beg_90_p,
          ncol = 2,
          nrow = 1,
          common.legend = T,
          legend = "bottom")


dau_45_p <-
    ggplot(p_data%>%dplyr::filter(stunde == 45), aes(x="", y = dau_hw_min, fill = Schulart, colour = Schulart)) +
    geom_flat_violin(position = position_nudge(x = 0, y = 0), adjust = 1.6, trim = F, alpha = .3) +
    geom_boxplot(aes(x=""), position = position_nudge(x = c(.49, .54), y = 0), 
                 outlier.shape = NA, alpha = .5, width = .04, colour = "black") +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(0,5,10,15,20,25,30), limits = c(0, 30)) +
    scale_x_discrete(expand = c(0, 0)) +
    ylab("Dauer Hausaufgabenvergabe [Min]") +
    xlab("Dichte") +
    ggtitle("Einzelstunden") +
    theme_light() +
    # theme(axis.line = element_line(colour = "#696f71"),
    #       panel.grid.major = element_blank(),
    #       panel.grid.minor = element_blank(),
    #       panel.border = element_blank(),
    #       panel.background = element_rect(fill = "#f6f7f7")) +
    coord_flip()

dau_90_p <-
    ggplot(p_data%>%dplyr::filter(stunde == 90), aes(x="", y = dau_hw_min, fill = Schulart, colour = Schulart)) +
    geom_flat_violin(position = position_nudge(x = 0, y = 0), adjust = 1.6, trim = F, alpha = .3) +
    geom_boxplot(aes(x=""), position = position_nudge(x = c(.49, .54), y = 0), 
                 outlier.shape = NA, alpha = .5, width = .04, colour = "black") +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(0,5,10,15,20,25,30,35,40), limits = c(0, 40)) +
    scale_x_discrete(expand = c(0, 0)) +
    ylab("Dauer Hausaufgabenvergabe [Min]") +
    # xlab("Dichte") +
    xlab("") +
    ggtitle("Doppelstunden") +
    theme_light() +
    coord_flip()

ggarrange(dau_45_p, dau_90_p,
          ncol = 2,
          nrow = 1,
          common.legend = T,
          legend = "bottom")


# library(ggridges)
# p_data_sum45 <- data_summary(p_data%>%dplyr::filter(stunde == 45), c("beg_hw_min"), c("Klassenstufe"))
# 
# ggplot(p_data_sum45, aes(x=Klassenstufe, y=beg_hw_min)) +
#     geom_line()+
#     geom_pointrange(aes(ymin=beg_hw_min-sd, ymax=beg_hw_min+sd), color = "darkgrey") +
#     geom_point() +
#     scale_x_continuous(breaks = 1:12) +
#     scale_y_continuous(limits = c(0,50)) +
#     theme_minimal()
# 
# 
# ### RIDGES #####
# p_data_ridges <- p_data %>%
#     dplyr::filter(!is.na(Klassenstufe)) %>%
#     mutate(Klassenstufe = as.factor(Klassenstufe))
# 
# ggplot(p_data_ridges%>%dplyr::filter(stunde == 45), aes(x = beg_hw_min, y = as.factor(Klassenstufe), fill = Schulart)) +
#     geom_density_ridges(aes(height = ..count..), scale = 1.2, stat = "density", alpha = .7, bw = 2.5) +
#     scale_x_continuous(limits = c(0,55), breaks = c(0,5,10,15,20,25,30,35,40,45,50,55), expand = c(0, 0)) +
#     # geom_segment(data = p_mean_ridges, aes(x = beg_hw_min, xend = beg_hw_min, y = as.factor(Klassenstufe), yend = as.factor(Klassenstufe))) +
#     # geom_point(data = p_mean_ridges, aes(x=beg_hw_min, y=as.factor(Klassenstufe))) +
#     coord_flip() +
#     theme_minimal()
# 
# 
# #### 90 Minuten
# p_data_sum90 <- data_summary(p_data%>%dplyr::filter(stunde == 90), c("beg_hw_min"), c("Klassenstufe"))
# 
# ggplot(p_data_sum90, aes(x=Klassenstufe, y=beg_hw_min)) +
#     geom_line()+
#     geom_pointrange(aes(ymin=beg_hw_min-sd, ymax=beg_hw_min+sd), color = "darkgrey") +
#     geom_point() +
#     scale_x_continuous(breaks = 1:12) +
#     scale_y_continuous(limits = c(0,100)) +
#     theme_minimal()
# 
# 
# ######### Dauer HA #######################
# #### 45 Minuten
# p_data_sum45 <- data_summary(p_data%>%dplyr::filter(stunde == 45), c("dau_hw_min"), c("Klassenstufe"))
# 
# ggplot(p_data_sum45, aes(x=Klassenstufe, y=dau_hw_min)) + 
#     geom_line()+
#     geom_pointrange(aes(ymin=dau_hw_min-sd, ymax=dau_hw_min+sd), color = "darkgrey") + 
#     geom_point() +
#     scale_x_continuous(breaks = 1:12) +
#     scale_y_continuous(limits = c(0,10)) +
#     theme_minimal()
# 
# 
# #### 90 Minuten
# p_data_sum90 <- data_summary(p_data%>%dplyr::filter(stunde == 90), c("dau_hw_min"), c("Klassenstufe"))
# 
# ggplot(p_data_sum90, aes(x=Klassenstufe, y=dau_hw_min)) + 
#     geom_line()+
#     geom_pointrange(aes(ymin=dau_hw_min-sd, ymax=dau_hw_min+sd), color = "darkgrey") + 
#     geom_point() +
#     scale_x_continuous(breaks = 1:12) +
#     scale_y_continuous(limits = c(0,25)) +
#     theme_minimal()
    


### Zeit 2 ##################################################################################################
p_data_ridges <- p_data %>%
    dplyr::filter(!is.na(Klassenstufe)) %>%
    mutate(Klassenstufe = factor(Klassenstufe, levels = c(1,2,3,4,5,6,7,8,9,10,11,12)))

ridges_length45 <- p_data_ridges %>%
    dplyr::filter(stunde == 45) %>%
    group_by(Klassenstufe) %>%
    summarize_all(length) 

names(ridges_length45) <- paste(names(ridges_length45), "_n45", sep="")
names(ridges_length45)[1] <- "Klassenstufe"

ridges_length90 <- p_data_ridges %>%
    dplyr::filter(stunde == 90) %>%
    group_by(Klassenstufe) %>%
    summarize_all(length) 

names(ridges_length90) <- paste(names(ridges_length90), "_n90", sep="")
names(ridges_length90)[1] <- "Klassenstufe"

ridges_length <- p_data_ridges %>%
    group_by(Klassenstufe) %>%
    summarize_all(length) 

names(ridges_length) <- paste(names(ridges_length), "_n", sep="")
names(ridges_length)[1] <- "Klassenstufe"

p_data_ridges <- left_join(p_data_ridges, ridges_length45, by="Klassenstufe")
p_data_ridges <- left_join(p_data_ridges, ridges_length90, by="Klassenstufe")
p_data_ridges <- left_join(p_data_ridges, ridges_length, by="Klassenstufe")


beg_45_p <-
ggplot(p_data_ridges%>%dplyr::filter(stunde == 45), aes(x=as.factor(Klassenstufe), y = beg_hw_min, fill = Schulart, 
                                                        colour = Schulart, group = as.factor(Klassenstufe))) +
    geom_flat_violin(adjust = 2, 
                     trim = F, 
                     alpha = .3, 
                     # scale = "count", 
                     width = 3) +
    geom_hline(yintercept = 45, linetype = "dashed", colour = "#696f71", size = 1) +
    stat_summary(fun.y = mean, geom = "line", aes(group = 1), position = position_nudge(x=.1), size = 1, color = "grey") +
    stat_summary(aes(size = beg_hw_min_n45), fun.y = mean, geom = "point",  
                 alpha = .85, position = position_nudge(x=.1)) +
    scale_size(range = c(.3,7.7)) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(0, 10,20,30,40,50), limits = c(0, 55)) +
    scale_x_discrete(expand = c(0, 0)) +
    ylab("Minuten seit Stundenbeginn bei Vergabe der HA") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    xlab("Klassenstufe") +
    ggtitle("Einzelstunden") +
    theme_light() 


beg_90_p <-
ggplot(p_data_ridges%>%dplyr::filter(stunde == 90), aes(x=as.factor(Klassenstufe), y = beg_hw_min, fill = Schulart, 
                                                            colour = Schulart, group = as.factor(Klassenstufe))) +
    geom_flat_violin(adjust = 2, 
                     trim = F, 
                     alpha = .3, 
                     # scale = "count", 
                     width = 3) +
    geom_hline(yintercept = 90, linetype = "dashed", colour = "#696f71", size = 1) +
    stat_summary(fun.y = mean, geom = "line", aes(group = 1), position = position_nudge(x=.1), size = 1, color = "grey") +
    stat_summary(aes(size = beg_hw_min_n90), fun.y = mean, geom = "point",  
                 alpha = .85, position = position_nudge(x=.1)) +
    scale_size(range = c(.1,1.2)) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(0, 10,20,30,40,50,60, 70, 80, 90, 100), limits = c(0, 100)) +
    scale_x_discrete(expand = c(0, 0), breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                     limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) +
    ylab("") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    xlab("Klassenstufe") +
    ggtitle("Doppelstunden") +
    theme_light() 


dau_45_p <-
    ggplot(p_data_ridges%>%dplyr::filter(stunde == 45), aes(x=as.factor(Klassenstufe), y = dau_hw_min, fill = Schulart, 
                                                            colour = Schulart, group = as.factor(Klassenstufe))) +
    geom_flat_violin(adjust = 2, 
                     trim = F, 
                     alpha = .3, 
                     # scale = "count", 
                     width = 3) +
    stat_summary(fun.y = mean, geom = "line", aes(group = 1), position = position_nudge(x=.1), size = 1, color = "grey") +
    stat_summary(aes(size = dau_hw_min_n45), fun.y = mean, geom = "point",  
                 alpha = .85, position = position_nudge(x=.1)) +
    scale_size(range = c(.3,7.7)) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(0,5,10,15,20,25,30), limits = c(0, 30)) +
    scale_x_discrete(expand = c(0, 0)) +
    ylab("Dauer der Hausaufgabenvergabe") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    xlab("Klassenstufe") +
    ggtitle("Einzelstunden") +
    theme_light() 


dau_90_p <-
    ggplot(p_data_ridges%>%dplyr::filter(stunde == 90), aes(x=as.factor(Klassenstufe), y = dau_hw_min, fill = Schulart, 
                                                            colour = Schulart, group = as.factor(Klassenstufe))) +
    geom_flat_violin(adjust = 2, 
                     trim = F, 
                     alpha = .3, 
                     # scale = "count", 
                     width = 3) +
    stat_summary(fun.y = mean, geom = "line", aes(group = 1), position = position_nudge(x=.1), size = 1, color = "grey") +
    stat_summary(aes(size = dau_hw_min_n90), fun.y = mean, geom = "point",  
                 alpha = .85, position = position_nudge(x=.1)) +
    scale_size(range = c(.1,1.2)) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(0,5,10,15,20,25,30,35,40,45), limits = c(0, 45)) +
    scale_x_discrete(expand = c(0, 0), breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                     limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) +
    ylab("") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    xlab("Klassenstufe") +
    ggtitle("Doppelstunden") +
    theme_light()


library(ggpubr)
ggarrange(beg_45_p, beg_90_p,
          ncol = 2,
          nrow = 1,
          common.legend = T,
          legend = "bottom")


ggarrange(dau_45_p, dau_90_p,
          ncol = 2,
          nrow = 1,
          common.legend = T,
          legend = "bottom")




### Angebotsseite ##########################################################################################
ank_p <-
ggplot(lh_ank_p, aes(x = Klassenstufe, y = lh_ank, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n), alpha = .9) +
    geom_text(aes(label = paste(round(lh_ank*100,0), sep = ""), vjust = 3)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = c("0", "25", "50", "75", "100")) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    xlab("") +
    # xlab("Klassenstufe") +
    ggtitle("Kündigt HA an") +
    # ylab("") +
    ylab("% Häufigkeit") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() 

nen_p <-
ggplot(lh_nen_p, aes(x = Klassenstufe, y = lh_nen, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n), alpha = .9) +
    geom_text(aes(label = paste(round(lh_nen*100,0), sep = ""), vjust = 3)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = c("0", "25", "50", "75", "100")) +
    scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    xlab("") +
    # xlab("Klassenstufe") +
    ggtitle("Nennt HA") +
    # ylab("% Häufigkeit") +
    ylab("") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() 

auf_p <- 
ggplot(lh_auf_p, aes(x = Klassenstufe, y = lh_auf, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n), alpha = .9) +
    geom_text(aes(label = paste(round(lh_auf*100,0), sep = ""), vjust = 3)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = c("0", "25", "50", "75", "100")) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    xlab("") +
    # xlab("Klassenstufe") +
    ggtitle("Fordert Aufmerksamkeit") +
    # ylab("") +
    ylab("% Häufigkeit") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() 

sch_p <- 
ggplot(lh_sch_p, aes(x = Klassenstufe, y = lh_sch, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n), alpha = .9) +
    geom_text(aes(label = paste(round(lh_sch*100,0), sep = ""), vjust = 3)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = c("0", "25", "50", "75", "100")) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    xlab("") +
    # xlab("Klassenstufe") +
    ggtitle("Schreibt HA an") +
    ylab("") +
    # ylab("% Häufigkeit") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() 

erl_p <- 
ggplot(lh_erl_p, aes(x = Klassenstufe, y = lh_erl, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n), alpha = .9) +
    geom_text(aes(label = paste(round(lh_erl*100,0), sep = ""), vjust = 3)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = c("0", "25", "50", "75", "100")) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    xlab("") +
    # xlab("Klassenstufe") +
    ggtitle("Erläutert HA") +
    # ylab("") +
    ylab("% Häufigkeit") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() 

wfr_p <-
ggplot(lh_wfr_p, aes(x = Klassenstufe, y = lh_wfr, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n), alpha = .9) +
    geom_text(aes(label = paste(round(lh_wfr*100,0), sep = ""), vjust = -3)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = c("0", "25", "50", "75", "100")) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    xlab("") +
    # xlab("Klassenstufe") +
    ggtitle("Will Fragen") +
    ylab("") +
    # ylab("% Häufigkeit") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() 

wno_p <- 
ggplot(lh_wno_p, aes(x = Klassenstufe, y = lh_wno, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n), alpha = .9) +
    geom_text(aes(label = paste(round(lh_wno*100,0), sep = ""), vjust = 2)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = c("0", "25", "50", "75", "100")) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    xlab("") +
    # xlab("Klassenstufe") +
    ggtitle("Will Notation") +
    # ylab("") +
    ylab("% Häufigkeit") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() 

ano_p <-
ggplot(lh_ano_p, aes(x = Klassenstufe, y = lh_ano, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n), alpha = .9) +
    geom_text(aes(label = paste(round(lh_ano*100,0), sep = ""), vjust = -2)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = c("0", "25", "50", "75", "100")) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    # xlab("") +
    xlab("Klassenstufe") +
    ggtitle("Achtet auf Notation") +
    ylab("") +
    # ylab("% Häufigkeit") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() 


bfr_p <-
ggplot(lh_bfr_p, aes(x = Klassenstufe, y = lh_bfr, color = Schulart)) +
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n), alpha = .9) +
    geom_text(aes(label = sub("^(-?)0.", "\\1.", round(lh_bfr, 2)), vjust = 2)) +
    scale_y_continuous(limits = c(0,(max(lh_bfr_p$lh_bfr)*1.1)), expand = c(0,0)) +
    scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    # xlab("") +
    xlab("Klassenstufe") +
    ylab("∅ absolute Häufigkeit") +
    ggtitle("Anzahl beantworteter Fragen durch Lehrperson") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light()

# bfr_p <-
# ggplot(p_data_ridges, aes(x=as.factor(Klassenstufe), y = lh_bfr, fill = Schulart, 
#                           colour = Schulart, group = as.factor(Klassenstufe))) +
#     geom_flat_violin(adjust = 2, 
#                      trim = F, 
#                      alpha = .3, 
#                      # scale = "count", 
#                      width = 3) +
#     geom_text(data = lh_bfr_p, aes(label = round(lh_bfr, 2), vjust = -6, hjust = .1), position = position_dodge(width = 1)) +
#     stat_summary(fun.y = mean, geom = "line", aes(group = 1), position = position_nudge(x=.1), size = 1, color = "grey") +
#     stat_summary(aes(size = lh_bfr_n), fun.y = mean, geom = "point",  
#                  alpha = .85, position = position_nudge(x=.1)) +
#     scale_colour_brewer(palette = "Set1")+
#     scale_fill_brewer(palette = "Set1") +
#     scale_y_continuous(expand = c(0, 0), breaks = c(1:7), limits = c(0, 7)) +
#     scale_x_discrete(expand = c(0, 0), breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
#                      limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) +
#     # xlab("") +
#     xlab("Klassenstufe") +
#     ylab("∅ absolute Häufigkeit") +
#     ggtitle("Anzahl beantworteter Fragen durch Lehrperson") +
#     labs(size = "Anzahl\neingegangener\nStunden") +
#     theme_light()



# windows(4000, 6000, pointsize = 72) #opens a separate window with the size you want
ggarrange(ank_p, nen_p, auf_p, sch_p, erl_p, wfr_p, wno_p, ano_p, bfr_p,
          ncol = 2,
          nrow = 5,
          align = "v",
          common.legend = T,
          legend = "bottom"
          )
# save.image(lh_p, file = "lh_p.png")
# savePlot("clipboard", type="wmf")



#### Nutzungsseite ################################################################################

mel_p <-
ggplot(p_data_ridges, aes(x=as.factor(Klassenstufe), y = sh_mel, fill = Schulart,
                          colour = Schulart, group = as.factor(Klassenstufe))) +
    geom_flat_violin(adjust = 1,
                     trim = F,
                     alpha = .3,
                     # scale = "count",
                     width = 2) +
    stat_summary(fun.y = mean, geom = "line", aes(group = 1), position = position_nudge(x=.1), size = 1, color = "grey") +
    stat_summary(aes(size = sh_mel_n), fun.y = mean, geom = "point",
                 alpha = .85, position = position_nudge(x=.1)) +
    geom_text(data = sh_mel_p, aes(label = sub("^(-?)0.", "\\1.", round(sh_mel, 2)), vjust = -0.5, hjust = -.2), position = position_dodge(width = 1), color = "black") +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(0:7), limits = c(0, 3)) +
    scale_x_discrete(expand = c(0, 0), breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                     limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) +
    xlab("") +
    # xlab("Klassenstufe") +
    # ylab("") +
    ylab("absolute Häufigkeit") +
    ggtitle("Anzahl Schülermeldungen") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light()

fra_p <- 
# ggplot(sh_fra_p, aes(x = Klassenstufe, y = sh_fra, color = Schulart)) + 
#     geom_line(aes(group = NA), color = "grey", size = 1) +
#     geom_point(aes(size = length_n)) +
#     geom_text(aes(label = round(sh_fra, 2), vjust = 3)) +
#     scale_y_continuous(limits = c(0,(max(sh_fra_p$sh_fra)*1.1)), expand = c(0,0)) +
#     geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "red", alpha = .01, color = NA) +
#     geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
#     xlab("") +
#     ylab("") +
#     ggtitle("Anzahl Schülerfragen") +
#     labs(size = "Anzahl\neingegangener\nStunden") +
#     theme_light() +
#     theme(axis.line = element_line(colour = "#696f71"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.border = element_blank(),
#           panel.background = element_rect(fill = "#f6f7f7"))

ggplot(p_data_ridges, aes(x=as.factor(Klassenstufe), y = sh_fra, fill = Schulart,
                          colour = Schulart, group = as.factor(Klassenstufe))) +
    geom_flat_violin(adjust = 1,
                     trim = F,
                     alpha = .3,
                     # scale = "count",
                     width = 2) +
    stat_summary(fun.y = mean, geom = "line", aes(group = 1), position = position_nudge(x=.1), size = 1, color = "grey") +
    stat_summary(aes(size = sh_fra_n), fun.y = mean, geom = "point",
                 alpha = .85, position = position_nudge(x=.1)) +
    geom_text(data = sh_fra_p, aes(label = sub("^(-?)0.", "\\1.", round(sh_fra, 2)), vjust = -.5, hjust = -.1), position = position_dodge(width = 1), 
              color = "black") +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(0:3), limits = c(0, 3)) +
    scale_x_discrete(expand = c(0, 0), breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                     limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) +
    xlab("") +
    # xlab("Klassenstufe") +
    ylab("") +
    # ylab("absolute Häufigkeit") +
    ggtitle("Anzahl Schülerfragen") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light()


p_data_ridges <- p_data_ridges %>%
    mutate(sh_not_p = sh_not/100)

sh_not_p <- sh_not_p %>%
    mutate(sh_not_p = sh_not/100)

not_p <-
# ggplot(sh_not_p, aes(x = Klassenstufe, y = sh_not, color = Schulart)) + 
#     geom_line(aes(group = NA), color = "grey", size = 1) +
#     geom_point(aes(size = length_n)) +
#     geom_text(aes(label = paste(round(sh_not,0)), vjust = 3)) +
#     scale_y_continuous(limits = c(0,(max(sh_not_p$sh_not)*1.1)), expand = c(0,0)) +
#     geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "red", alpha = .01, color = NA) +
#     geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
#     xlab("Klassenstufe") +
#     ylab("∅ relative Häufigkeit [%]") +
#     ggtitle("Anteil der notierenden SuS") +
#     labs(size = "Anzahl\neingegangener\nStunden") +
#     theme_light() +
#     theme(axis.line = element_line(colour = "#696f71"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.border = element_blank(),
#           panel.background = element_rect(fill = "#f6f7f7")) 

ggplot(p_data_ridges, aes(x=as.factor(Klassenstufe), y = sh_not_p, fill = Schulart,
                          colour = Schulart, group = as.factor(Klassenstufe))) +
    geom_flat_violin(adjust = 1,
                     trim = F,
                     alpha = .3,
                     # scale = "count",
                     width = 2) +
    stat_summary(fun.y = mean, geom = "line", aes(group = 1), position = position_nudge(x=.1), size = 1, color = "grey") +
    stat_summary(aes(size = sh_not_n), fun.y = mean, geom = "point",
                 alpha = .85, position = position_nudge(x=.1)) +
    geom_text(data = sh_not_p, aes(label = round(sh_not_p, 2)*100, vjust = 2, hjust = -.3),
              position = position_dodge(width = 1), color = "black") +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0, 1),
                       labels = c("0", "20", "40", "60", "80", "100")) +
    scale_x_discrete(expand = c(0, 0), breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                     limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) +
    # xlab("") +
    xlab("Klassenstufe") +
    # ylab("") +
    ylab("∅ relative Häufigkeit [%]") +
    ggtitle("Anteil der notierenden SuS") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light()

auf_p <-
# ggplot(sh_auf_p, aes(x = Klassenstufe, y = sh_auf, color = Schulart)) + 
#     geom_line(aes(group = NA), color = "grey", size = 1) +
#     geom_point(aes(size = length_n)) +
#     geom_text(aes(label = round(sh_auf,1), vjust = 3)) +
#     scale_y_continuous(limits = c(1,5), expand = c(0,0)) +
#     geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "red", alpha = .01, color = NA) +
#     geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
#     xlab("Klassenstufe") +
#     ylab("Zustimmung (Likert Item)") +
#     ggtitle("Eingeschätzte Aufmerksamkeit der SuS") +
#     labs(size = "Anzahl\neingegangener\nStunden") +
#     theme_light() +
#     theme(axis.line = element_line(colour = "#696f71"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.border = element_blank(),
#           panel.background = element_rect(fill = "#f6f7f7")) 

ggplot(p_data_ridges, aes(x=as.factor(Klassenstufe), y = sh_auf, fill = Schulart,
                          colour = Schulart, group = as.factor(Klassenstufe))) +
    geom_flat_violin(adjust = 1,
                     trim = F,
                     alpha = .3,
                     # scale = "count",
                     width = 2) +
    stat_summary(fun.y = mean, geom = "line", aes(group = 1), position = position_nudge(x=.1), size = 1, color = "grey") +
    stat_summary(aes(size = sh_auf_n), fun.y = mean, geom = "point",
                 alpha = .85, position = position_nudge(x=.1)) +
    geom_text(data = sh_auf_p, aes(label = round(sh_auf, 2), vjust = 2, hjust = -.1), position = position_dodge(width = 1), color = "black") +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(1:5), limits = c(1, 5)) +
    scale_x_discrete(expand = c(0, 0), breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                     limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) +
    # xlab("") +
    xlab("Klassenstufe") +
    # ylab("") +
    ylab("Zustimmung (Likert Item)") +
    ggtitle("Eingeschätzte Aufmerksamkeit der SuS") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light()


ggarrange(mel_p, fra_p, not_p, auf_p,
          ncol = 2,
          nrow = 2,
          align = "v",
          common.legend = T,
          legend = "bottom"
)


#### Nutzungsseite ################################################################################

# mel_p <- 
# ggplot(sh_mel_p, aes(x = Klassenstufe, y = sh_mel, color = Schulart)) +
#     geom_line(aes(group = NA), color = "grey", size = 1) +
#     geom_errorbar(aes(ymin=sh_mel-sh_mel_sd, ymax=sh_mel+sh_mel_sd), alpha = .7, width = .2) +
#     geom_point(aes(size = length_n)) +
#     geom_text(aes(label = round(sh_mel, 2), vjust = 3)) +
#     coord_cartesian(ylim=c(0, 2.5)) +
#     scale_y_continuous(expand = c(0,0)) +
#     scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
#                      limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
#     xlab("Klassenstufe") +
#     ylab("∅ absolute Häufigkeit") +
#     ggtitle("Anzahl Schülermeldungen") +
#     labs(size = "Anzahl\neingegangener\nStunden") +
#     theme_light()
