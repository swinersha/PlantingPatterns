#library(forcats)

#' pos_calc
#' 
#' Calculates planting row position given the length of the row, the distance between
#' plants and an inital offset
#' 
#' @param length The length of the planting row or plot
#' @param sep_dist The distance between plants
#' @param offset The distance from the edge to the first plant 
#' @param centralise Logical indicating whether or not planting positions should be
#' centralised within the row or plot
#' @return A numeric vetor of planting positions
#' @export
#' @author Tom Swinfield
#' @details 
#' 
#' Created 17-08-04

pos_calc<-function(length, sep_dist, offset=0, centralise=FALSE){
  centre_offset<-0
  if(centralise)
    centre_offset<-(length %% sep_dist)/2
  final_offset<-offset+centre_offset
  final_offset<-ifelse(final_offset>sep_dist, final_offset-sep_dist, final_offset)
  #yn<-floor((length-(offset+centre_offset)) / sep_dist)+1
  ypos<-seq(final_offset, by=sep_dist, to=length)
  return(ypos)
}

#' pos_calc
#' 
#' Calculates planting row position given the length of the row, the distance between
#' plants and an inital offset
#' 
#' @param length The length of the planting row or plot
#' @param xdist The distance between planting rows
#' @param ydist The distance between plantins within rows
#' @param xoffset a numeric offset, should be the same length as yoffset
#' indicates an offset relative to the estimated position of the rows. 
#' @param yoffset a numeric offset, should be the same length as xoffset
#' indicates an offset relative to the estimated planting positions. 
#' @param xcentralise Logical indicating whether or not planting row positions should be
#' centralised 
#' @param ycentralise Logical indicating whether or not planting positions should be
#' centralised wihtin the row
#' @return A numeric vetor of planting positions
#' @export
#' @author Tom Swinfield
#' @details 
#' 
#' Created 17-08-04

species_pos_gen<-function(length, xdist, ydist, xoffset=0, yoffset=0, xcentralise=FALSE, ycentralise=FALSE){
  xoffset_length<-length(xoffset)
  yoffset_length<-length(yoffset)
  if(xoffset_length != yoffset_length)
    stop("xoffset and yoffset must be the same.")
  xy<-lapply(1:length(xoffset), function(i){
    xpos <-
      pos_calc(
        length,
        xdist,
        offset = xoffset[i],
        centralise = xcentralise
      )
    ypos <-
      pos_calc(
        length,
        ydist,
        offset = yoffset[i],
        centralise = ycentralise
      )
    xrow<-expand.grid(x=xpos, row=1:length(ypos))
    xy<-expand.grid(x=xpos, y=ypos)
    xy<-cbind(xy, row=xrow$row)
    return(xy)
  })
  xy<-do.call(rbind, xy)
  xy<-xy[order(xy$x),]
  xy$col<-as.integer(as.factor(xy$x))
  rownames(xy)<-NULL
  #unique()
  #xy<-cbind(xy, rowcol)
  return(xy)
}

#' select_species
#' 
#' A funciton which given the mix name returns a repeating list
#' of the species for the planting positions
#' 
#' @param xy A dataframe describing the planting positions
#' @param mix A dataframe describing the mixes
#' @param mix_name The name of the mix
#' @param type The type of species to be selected
#' @param alternate.inrow Logical stating whether or not the species should alternate
#' within plating rows
#' the number of planting positions
#' @return A factor vetor of species the same length as length_out
#' @export
#' @author Tom Swinfield
#' @details 
#' 
#' Created 17-08-10

xy = simple_ld_xy
mix.name = "DemPlotDasar"
species.type = "Cahaya"
alternate.inrow = TRUE

select_species<-function(xy, mix, mix.name, species.type=NULL, alternate.inrow=TRUE){
  if(!is.null(species.type))
    mix<-filter(mix, type == species.type)
  spp<-mix %>% filter(mix_name == mix.name) %>%
    select(species) %>% 
    unlist()
  xy$type<-species.type
  if(alternate.inrow)
    xy$species<-rep_len(spp, length.out=nrow(xy)) # alternates within planting rows
  else
    xy$species<-spp[(xy$col - 1) %% length(spp) +1] # alternates between planting rows
  return(xy)
}
