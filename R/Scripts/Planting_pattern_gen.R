library(PlantingPatterns)


# PARAMETERS ----
PLOT_SIZE<-50

# rubber
RUBBER_BIROWDIST<-2.3 # distance between rows
RUBBER_XDIST<-15+RUBBER_BIROWDIST
RUBBER_YDIST<-2.7

# Simple
SIMPLE_LD_YDIST<-5 # light demanding

# Complex
COMPLEX_LD_YDIST<-4 # light demanding
COMPLEX_LD_BIROWDIST<-6 # light demanding; distance between rows
COMPLEX_SH_YDIST<-6 # shade tolerant

# Natural regeneration
REGEN_SH_YDIST<-6 # shade tolerant

# The list of mixes
mix<-read.csv("data/DaftarCampur.csv")
colnames(mix)<-c("system", "mix_name", "type", "species")

# RUBBER ----
rubber_xy<-species_pos_gen(length=50,
                xdist=RUBBER_XDIST,
                ydist=RUBBER_YDIST,
                xoffset=(RUBBER_BIROWDIST / 2) * c(-1,1),
                yoffset=c(0, RUBBER_YDIST/2),
                xcentralise=TRUE, 
                ycentralise=FALSE)
rubber_xy<-select_species(rubber_xy, mix, "DemPlotDasar", species.type = "Karet", 
                             alternate.inrow = FALSE)

mono_xy<-rubber_xy
mono_xy$sistem<-"mono"

# Simple ----
# Simple light demanding
simple_ld_xy<-species_pos_gen(length=50,
                           xdist=RUBBER_XDIST,
                           ydist=SIMPLE_LD_YDIST,
                           xoffset=RUBBER_XDIST / 2,
                           yoffset=0,
                           xcentralise=TRUE, 
                           ycentralise=FALSE)
simple_ld_xy<-select_species(simple_ld_xy, mix, "DemPlotDasar", species.type = "Cahaya", 
                                     alternate.inrow = FALSE)

simple_xy<-rbind(rubber_xy, simple_ld_xy)
simple_xy$sistem<-"dasar"

# Complex ----
# Complex light demanding
complex_ld_xy<-species_pos_gen(length=50,
                              xdist=RUBBER_XDIST,
                              ydist=COMPLEX_LD_YDIST,
                              xoffset=(RUBBER_XDIST / 2) + (COMPLEX_LD_BIROWDIST/2) * c(-1,1),
                              yoffset=c(0, COMPLEX_LD_YDIST/2),
                              xcentralise=TRUE, 
                              ycentralise=FALSE)
complex_ld_xy<-select_species(complex_ld_xy, mix, "DemPlotKompleks", species.type = "Cahaya", 
                             alternate.inrow = FALSE)

# Complex shade tolerating
complex_sh_xy<-species_pos_gen(length=50,
                               xdist=RUBBER_XDIST,
                               ydist=COMPLEX_SH_YDIST,
                               xoffset=RUBBER_XDIST / 2,
                               yoffset=0,
                               xcentralise=TRUE, 
                               ycentralise=FALSE)
complex_sh_xy<-select_species(complex_sh_xy, mix, "DemPlotKompleks", species.type = "Naungan", 
                             alternate.inrow = TRUE)


complex_xy<-rbind(rubber_xy, complex_ld_xy, complex_sh_xy)
complex_xy$sistem<-"kompleks"

# Natural regeneration ----
# Natural regeneration shade tolerant species
regen_sh_xy<-species_pos_gen(length=50,
                               xdist=RUBBER_XDIST,
                               ydist=REGEN_SH_YDIST,
                               xoffset=(RUBBER_XDIST / 2),
                               yoffset=0,
                               xcentralise=TRUE, 
                               ycentralise=TRUE)
regen_sh_xy<-select_species(regen_sh_xy, mix, "DemPlotRegen", species.type = "Naungan", 
                             alternate.inrow = TRUE)

regen_xy<-rbind(rubber_xy, regen_sh_xy)
regen_xy$sistem<-"regen"
  
# Plotting ----

systems_xy<-rbind(mono_xy, simple_xy, complex_xy, regen_xy)
systems_xy$sistem<-fct_relevel(systems_xy$sistem, "mono", "dasar", "regen", "kompleks")
systems_xy$species<-fct_relevel(systems_xy$species, "Karet")

pdf(file = "Figures/PatternAgroForest.pdf", 
    width = 7,
    height = 5, 
    pointsize = 20)
ggplot(systems_xy, aes(x, y, color = species)) +
  geom_point() +
  xlim(0, 50) + ylim(0, 50) +
  facet_grid(sistem~.) +
  coord_fixed()
dev.off()

mono_xy<-dplyr::rename(mono_xy, posisi = row, jalur = col)
simple_xy<-dplyr::rename(simple_xy, posisi = row, jalur = col)
complex_xy<-dplyr::rename(complex_xy, posisi = row, jalur = col)
regen_xy<-dplyr::rename(regen_xy, posisi = row, jalur = col)


write.csv(mono_xy, "data/Pola_tanam_mono.csv")
write.csv(simple_xy, "data/Pola_tanam_simple.csv")
write.csv(complex_xy, "data/Pola_tanam_kompleks.csv")
write.csv(regen_xy, "data/Pola_tanam_regen.csv")
