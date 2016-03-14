## I'm not totally sure how this works.


###
### Plot individual grid cell
###

# Set up a dataframe of corners we want. Take the ones that have the up, left
corners <- code.df[which(apply(code.df,1,function(x) sum(!is.na(x[c(2,4)])))==2),]

# Here's a dataframe that'll just show the grid points rather than
# the c1d1 and c0.5 and whatnot ones
gps.whole <- subset(gps, tg_name %in% code.df[,1])

# Decide where the figures should go.
setwd(figure.wd)

for(i in 1:length(levels(gps$site))){
  site.set <- subset(gps, site == levels(gps$site)[i])
  
  for(j in 1:dim(corners)[1]){
    cor <- as.character(corners$code[j])
    cor <- paste(levels(gps$site)[i], cor)
    
    up <- as.character(corners$up[j])
    up <- paste(levels(gps$site)[i], up)
    
    left <- as.character(corners$left[j])
    left <- paste(levels(gps$site)[i], left)
    
    
    g.cor <- gps[cor,c("point_x","point_y")]
    g.cor <- gps[cor,c("point_x","point_y")]
    
    g.up <- gps[up,c("point_x","point_y")]
    g.up <- gps[up,c("point_x","point_y")]
    
    g.left <- gps[left,c("point_x","point_y")]
    g.left <- gps[left,c("point_x","point_y")]
    
    #
    g.mean <- colSums(rbind(g.up,g.left))/2
    
    # Find the mapping points within a given radius
    rad.val <- 9
    
    map.trees <- mapping_all[which(abs(mapping_all$xcoord - g.mean[1]) < rad.val &
                                     abs(mapping_all$ycoord - g.mean[2]) < rad.val),]
    map.gps <- gps.whole[which(abs(gps.whole$point_x - g.mean[1]) < rad.val &
                                 abs(gps.whole$point_y - g.mean[2]) < rad.val),]
    
    
    
    # Plot
    
    name.file <- paste(levels(gps$site)[i],
                       sort(c(as.character(corners$up[j]),as.character(corners$left[j])))[1],
                       sort(c(as.character(corners$up[j]),as.character(corners$left[j])))[2])
    name.file <- paste(name.file,".pdf",sep="")
    pdf(name.file,width=6,height=6)
    print(ggplot(map.trees) +
      geom_point(aes(xcoord, ycoord)) + 
      geom_text_repel(aes(xcoord, ycoord, label = tag)) +
      theme_bw()+
      theme(#panel.background = element_rect(fill = 'white', colour = 'gray'),
            axis.text.y=element_text(angle=90),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none") + 
      geom_text(data=map.gps, aes(point_x, point_y, label = tg_name), size = 6))
    dev.off()
    
    
  }
}




plot_ebl("anao")


levels(mapping$quaddiag)
















###
### Toy version of mapping based on L and R distances
###


lp <- c(12,6)
rp <- c(10,15)
ldist <- 3
rdist <- 1 #sqrt(9^2+2^2)

lr.dist <- sqrt((lp[1]-rp[1])^2 + (lp[2]-rp[2])^2)
lr.dist

#mapping_all$xprime<-((mapping_all$DistLRKP^2)-(mapping_all$leftdist^2)+(mapping_all$rightdist^2))/(2*mapping_all$DistLRKP)

xprime <- (lr.dist^2 - ldist^2 + rdist^2) / (2 * lr.dist)

#mapping_all$yprime<-sqrt((mapping_all$rightdist^2)-(mapping_all$xprime^2))

sqrs <- rdist^2 - xprime^2
sqrs[which(sqrs < 0)] <- 0
#sqrs <- abs(sqrs)
yprime <- sqrt(sqrs)
#yprime <- sqrt(rdist^2 - xprime^2)


#mapping_all$alpha<-asin((mapping_all$LKPy-mapping_all$RKPy)/mapping_all$DistLRKP)

alpha <- asin((lp[2] - rp[2]) / lr.dist)

# mapping_all$xcoord<-(cos(mapping_all$alpha))*mapping_all$xprime - (sin(mapping_all$alpha))*mapping_all$yprime + mapping_all$RKPx
# mapping_all$ycoord<-(sin(mapping_all$alpha))*mapping_all$xprime - (cos(mapping_all$alpha))*mapping_all$yprime + mapping_all$RKPy

xcoord <- cos(alpha) * xprime - sin(alpha) * yprime + rp[1]
ycoord <- sin(alpha) * xprime - cos(alpha) * yprime + rp[2]


plot(rbind(lp,rp,c(xcoord,ycoord)),ylim=c(0,20),xlim=c(0,20))

text(lp[1],lp[2],labels=c("lp"),pos=3)
text(rp[1],rp[2],labels=c("rp"),pos=3)

alpha
xprime
yprime
