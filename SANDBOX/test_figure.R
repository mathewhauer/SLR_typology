


files  <- list.files(path = "DATA-PROCESSED/bg_eae/", pattern = 'rcp85\\.csv')
uncontrolled <- read_csv("DATA-PROCESSED/blkgrp_projections_20202100.csv")
tables <- lapply(files, read_csv)

files <- paste0("DATA-PROCESSED/bg_eae//", list.files(path = "./DATA-PROCESSED/bg_eae/",pattern = 'rcp85\\.csv'))
temp <- lapply(files, read_csv)
z <- rbindlist( temp ) %>%
  dplyr::select(GEOID = GEOID10, everything()) %>%
  gather(Year, EAE, EAE_2020:EAE_2100) %>%
  mutate(YEAR = as.integer(substr(Year, 5,8)),
         EAE = as.numeric(EAE)) %>%
  dplyr::select(-Year) %>%
  left_join(., uncontrolled) %>%
  mutate(per = EAE / Uncontrolled,
         EAE_SSP1 = per*SSP1,
         EAE_SSP2 = per*SSP2,
         EAE_SSP3 = per*SSP3,
         EAE_SSP4 = per*SSP4,
         EAE_SSP5 = per*SSP5) %>%
  na.omit

zz <- z %>%
  # filter(STATE == "12") %>%
  group_by(YEAR) %>%
  dplyr::summarise(
    # EAE = sum(EAE),
                   EAE_SSP1 = sum(EAE_SSP1),
                   EAE_SSP2 = sum(EAE_SSP2),
                   EAE_SSP3 = sum(EAE_SSP3),
                   EAE_SSP4 = sum(EAE_SSP4),
                   EAE_SSP5 = sum(EAE_SSP5)) %>%
  gather(Type, Population, EAE_SSP1:EAE_SSP5) %>%
  mutate(SSP = substr(Type, 5,8),
                      Type = "Directly")

zz3 <- z %>%
  filter(EAE >0) %>%
  group_by(YEAR) %>%
  dplyr::summarise(SSP1 = sum(SSP1) - sum(EAE_SSP1),
                   SSP2 = sum(SSP2) - sum(EAE_SSP2),
                   SSP3 = sum(SSP3) - sum(EAE_SSP3),
                   SSP4 = sum(SSP4) - sum(EAE_SSP4),
                   SSP5 = sum(SSP5) - sum(EAE_SSP5)) %>%
    gather(Type, Population, SSP1:SSP5) %>%
  mutate(SSP = Type,
         Type = "Indirectly")
  
together <- rbind(zz, zz3)

this.ssp <- "SSP2"

plotfunc <- function(this.ssp, scen){
 
  h1 <- max(filter(together, SSP == this.ssp, Type == "Directly")$Population)
  
   h2 <- max(filter(together, SSP == this.ssp, Type == "Indirectly")$Population) + 
     h1

ggplot(data = filter(together, SSP == this.ssp), aes(y=Population, x = YEAR, fill = reorder(Type, desc(Type))), alpha = 0.5) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=comma, 
                     expand=c(0,0), 
                     limits = c(0, max(together$Population*1.3)),
                     breaks = c(seq(0,max(together$Population*1.3), 20000000)),
                     sec.axis = sec_axis(~.,breaks = c(h1,h2),labels=comma)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_manual(values=c("blue", "red", "dark gray")) +
  # geom_hline(yintercept = h1) +
  # geom_hline(yintercept = h2) +
    # geom_text(aes(2025, h, label = paste0(prettyNum(h,big.mark=",",scientific=FALSE)),
    #                vjust = -1), size = 3) +
  guides(fill=guide_legend(title="Exposure Cat.")) +
  labs(x = "Year",
       y = "Population Exposure",
       title = paste0(this.ssp,": ", scen)) +
  theme_bw() +
    NULL}
plotfunc("SSP4", "Middle of the Road")

ssp1 <- plotfunc("SSP1", "Sustainability")
ssp2 <- plotfunc("SSP2", "Middle of the road")
ssp3 <- plotfunc("SSP3", "Regional rivalry")
ssp4 <- plotfunc("SSP4", "Inequality")
ssp5 <- plotfunc("SSP5", "Fossil-fueled development")

leg <- get_legend(ssp1)

top <- plot_grid(ssp5 + theme(legend.position="none"),
                 ssp3 + theme(legend.position="none"),
                 ncol=2, align = 'h')
bot <- plot_grid(ssp1 + theme(legend.position="none"),
                 ssp4 + theme(legend.position="none"),
                 ncol=2, align = 'h')
mid <- plot_grid(leg, ssp2 + theme(legend.position="none"), NULL, ncol=3,rel_widths = c(1,3,1),
                 rel_heights = c(1,5,1))

maps<- plot_grid(top,
                 mid,
                 bot,
                 ncol=1, align = 'h', 
                 # rel_heights = c(2, 1),
                 label_x = 0.3)

fig<- ggplot() + 
  geom_blank() + 
  xlim(0, 10) + 
  ylim(0, 10) +
  theme_classic() + 
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y  = element_line(arrow = arrow()),
        axis.line.x = element_line(arrow = arrow())) +
  labs(x ="Barriers to Adaptation", 
       y = "Barriers to Mitigation") 

countymaps <- ggdraw() +
  draw_plot(fig) +
  draw_plot(maps, scale = 0.91)
  
ggsave("FIGURES/exposure.pdf")