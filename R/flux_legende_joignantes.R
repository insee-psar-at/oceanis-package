flux_legende_joignantes <-
function(x,y,long,large)
{
  l <- list()
  
  xp <- x
  yp <- y
  xg <- x
  yg <- y+large/1.5
  
  vec <- matrix(c(xg,yg, xg+long,yg, xg+long+large*0.655,yg+large*0.655/2, xg+long,yg+large*0.655,   xg,yg+large*0.655,   xg,yg),6,2,byrow=T)
  l[[1]] <- st_polygon(list(vec))
  vec <- matrix(c(xp,yp, xp+long,yp, xp+long+large*0.655,yp+large*0.655/6, xp+long,yp+large*0.655/3, xp,yp+large*0.655/3, xp,yp),6,2,byrow=T)
  l[[2]] <- st_polygon(list(vec))
  
  flux_legWGS84 <- st_sf(geometry=st_sfc(l), crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
  
  pointe1 <- c(xg+long+large*0.655,yg+large*0.655/2)
  pointe2 <- c(xp+long+large*0.655,yp+large*0.655/6)
  
  return(list(flux_legWGS84,pointe1,pointe2))
}
