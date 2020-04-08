### Zeit ##################################################################################################
source("R_rainclouds.R")

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
    xlab("Häufigkeit") +
    ggtitle("45min Stunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) +
    coord_flip()


beg_90_p <-
ggplot(p_data%>%dplyr::filter(stunde == 90), aes(x="", y = beg_hw_min, fill = Schulart, colour = Schulart)) +
    geom_flat_violin(position = position_nudge(x = 0, y = 0), adjust = 1.6, trim = F, alpha = .3) +
    geom_boxplot(aes(x=""), position = position_nudge(x = c(.50, .56), y = 0), 
                 outlier.shape = NA, alpha = .5, width = .05, colour = "black") +
    geom_hline(yintercept = 90, linetype = "dashed", colour = "#696f71", size = 1) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), breaks = c(0, 10,20,30,40,50,60,70,80,90), limits = c(0,95)) +
    scale_x_discrete(expand = c(0, 0)) +
    ylab("Minuten seit geplantem Stundenbeginn bei Vergabe der HA") +
    xlab("") +
    ggtitle("90min Stunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) +
    coord_flip()


dau_45_p <-
ggplot(p_data%>%dplyr::filter(stunde == 45), aes(x="", y = dau_hw_min, fill = Schulart, colour = Schulart)) +
    geom_flat_violin(position = position_nudge(x = 0, y = 0), adjust = 1.6, trim = F, alpha = .3) +
    geom_boxplot(aes(x=""), position = position_nudge(x = c(.50, .56), y = 0), 
                 outlier.shape = NA, alpha = .5, width = .05, colour = "black") +
    # geom_hline(yintercept = 90, linetype = "dashed", colour = "#696f71", size = 1) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), limits = c(0,40)) +
    scale_x_discrete(expand = c(0, 0)) +
    ylab("Dauer HA Vergabe [Min]") +
    xlab("Häufigkeit") +
    ggtitle("45min Stunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) +
    coord_flip()

dau_90_p <-
ggplot(p_data%>%dplyr::filter(stunde == 90), aes(x="", y = dau_hw_min, fill = Schulart, colour = Schulart)) +
    geom_flat_violin(position = position_nudge(x = 0, y = 0), adjust = 1.6, trim = F, alpha = .3) +
    geom_boxplot(aes(x=""), position = position_nudge(x = c(.50, .56), y = 0), 
                 outlier.shape = NA, alpha = .5, width = .05, colour = "black") +
    # geom_hline(yintercept = 90, linetype = "dashed", colour = "#696f71", size = 1) +
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(expand = c(0, 0), limits = c(0,40)) +
    scale_x_discrete(expand = c(0, 0)) +
    ylab("Dauer HA Vergabe [Min]") +
    xlab("") +
    ggtitle("90min Stunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) +
    coord_flip()

library(ggpubr)
ggarrange(beg_45_p, beg_90_p, dau_45_p, dau_90_p,
          ncol = 2,
          nrow = 2,
          common.legend = T,
          legend = "bottom")


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
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = paste(round(lh_ank*100,0), "%", sep = ""), vjust = 3)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("") +
    ggtitle("Kündigt HA an") +
    ylab("% Häufigkeit") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) 

nen_p <-
    ggplot(lh_nen_p, aes(x = Klassenstufe, y = lh_nen, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = paste(round(lh_nen*100,0), "%", sep = ""), vjust = 3)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("") +
    ggtitle("Nennt HA") +
    ylab("") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) 

auf_p <- 
ggplot(lh_auf_p, aes(x = Klassenstufe, y = lh_auf, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = paste(round(lh_auf*100,0), "%", sep = ""), vjust = 3)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("") +
    ggtitle("Fordert Aufmerksamkeit") +
    ylab("% Häufigkeit") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) 

sch_p <- 
ggplot(lh_sch_p, aes(x = Klassenstufe, y = lh_sch, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = paste(round(lh_sch*100,0), "%", sep = ""), vjust = 3)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("") +
    ggtitle("Schreibt HA an") +
    ylab("") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) 

erl_p <- 
ggplot(lh_erl_p, aes(x = Klassenstufe, y = lh_erl, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = paste(round(lh_erl*100,0), "%", sep = ""), vjust = 3)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("") +
    ggtitle("Erläutert HA") +
    ylab("% Häufigkeit") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) 

wfr_p <-
ggplot(lh_wfr_p, aes(x = Klassenstufe, y = lh_wfr, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = paste(round(lh_wfr*100,0), "%", sep = ""), vjust = -3)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("") +
    ggtitle("Will Fragen") +
    ylab("") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) 

wno_p <- 
ggplot(lh_wno_p, aes(x = Klassenstufe, y = lh_wno, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = paste(round(lh_wno*100,0), "%", sep = ""), vjust = 2)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("") +
    ggtitle("Will Notation") +
    ylab("% Häufigkeit") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) 

ano_p <-
ggplot(lh_ano_p, aes(x = Klassenstufe, y = lh_ano, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = paste(round(lh_ano*100,0), "%", sep = ""), vjust = -2)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("Klassenstufe") +
    ggtitle("Achtet auf Notation") +
    ylab("") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) 


bfr_p <-
    ggplot(lh_bfr_p, aes(x = Klassenstufe, y = lh_bfr, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = round(lh_bfr, 2), vjust = 2)) +
    scale_y_continuous(limits = c(0,(max(lh_bfr_p$lh_bfr)*1.1)), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("Klassenstufe") +
    ylab("∅ absolute Häufigkeit") +
    ggtitle("Anzahl beantworteter Fragen durch Lehrperson") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7"))



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



#### Nutzungsseite (+1 Angebotsseite) ################################################################################

mel_p <- 
ggplot(sh_mel_p, aes(x = Klassenstufe, y = sh_mel, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = round(sh_mel, 2), vjust = 3)) +
    scale_y_continuous(limits = c(0,(max(sh_mel_p$sh_mel)*1.1)), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("Klassenstufe") +
    ylab("∅ absolute Häufigkeit") +
    ggtitle("Anzahl Schülermeldungen") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) 

fra_p <- 
ggplot(sh_fra_p, aes(x = Klassenstufe, y = sh_fra, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = round(sh_fra, 2), vjust = 3)) +
    scale_y_continuous(limits = c(0,(max(sh_fra_p$sh_fra)*1.1)), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("") +
    ylab("") +
    ggtitle("Anzahl Schülerfragen") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7"))

not_p <-
ggplot(sh_not_p, aes(x = Klassenstufe, y = sh_not, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = paste(round(sh_not,0), "%"), vjust = 3)) +
    scale_y_continuous(limits = c(0,(max(sh_not_p$sh_not)*1.1)), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("Klassenstufe") +
    ylab("∅ relative Häufigkeit") +
    ggtitle("Anteil der notierenden SuS") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) 

auf_p <-
    ggplot(sh_auf_p, aes(x = Klassenstufe, y = sh_auf, color = Schulart)) + 
    geom_line(aes(group = NA), color = "grey", size = 1) +
    geom_point(aes(size = length_n)) +
    geom_text(aes(label = round(sh_auf,1), vjust = 3)) +
    scale_y_continuous(limits = c(1,5), expand = c(0,0)) +
    geom_rect(aes(xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = "pink", alpha = .01, color = NA) +
    geom_rect(aes(xmin = 4.5, xmax = 14, ymin = -Inf, ymax = Inf), fill = "#BFEFFF", alpha = .01, color = NA) +
    xlab("Klassenstufe") +
    ylab("Zustimmung (Likert Item)") +
    ggtitle("Eingeschätzte Aufmerksamkeit der SuS") +
    labs(size = "Anzahl\neingegangener\nStunden") +
    theme_light() +
    theme(axis.line = element_line(colour = "#696f71"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f6f7f7")) 


ggarrange(mel_p, fra_p, not_p, auf_p,
          ncol = 2,
          nrow = 2,
          align = "v",
          common.legend = T,
          legend = "bottom"
)
