calcMyDists <- function(scoresDF, ord, 
                        referenceVal = "Presettlement", 
                        group = "Topo"){
  
  metaScores <- scoresDF
  
  lt <- ordiellipse(ord, group = metaScores[[group]],
                    kind = "sd", draw = "none",
                    conf = 0.95)
  
  def_ell <- data.frame() #sets up a data frame before running the function.
  for(g in c(referenceVal)){
    
    tmp <- cbind(as.data.frame(with(metaScores[metaScores$group==g,],
                                    veganCovEllipse(lt[[g]]$cov,lt[[g]]$center,lt[[g]]$scale)))
                 ,group=g)
    def_ell <- rbind(def_ell, tmp)
    
    if(g == referenceVal){
      
      
      phPoly <- SpatialPolygons(list(Polygons(srl = list(Polygon(as.matrix(tmp[1:2]), hole = FALSE)), ID = 1)))
      #allPts <- metaScores[metaScores$period != "Pre-Human", c("NMDS1", "NMDS2")]
      allPts2 <- metaScores[, c("NMDS1", "NMDS2")]
      spts <- SpatialPoints(allPts2)
      
      dists <- apply(gDistance(spts, phPoly,byid=TRUE),2,min)
      
      cent <- SpatialPoints(data.frame(NMDS1 = lt[[g]]$center[1], NMDS2 = lt[[g]]$center[2], row.names = NULL))
      
      centDist <- gDistance(spts, cent, byid = TRUE)
      
      distPH <- data.frame(metaScores, distEllipse = dists,
                           disCentroid = as.vector(centDist),
                           centroidNMDS1 =  lt[[g]]$center[1],
                           centroidNMDS2 = lt[[g]]$center[2])
      
    }
    
  }
  distPH$Impact2 <- factor(distPH$Impact,
                           levels = c("Low", "Mod", "High"),
                           ordered = TRUE)
  return(list(distPH, phPoly, spts))
}
