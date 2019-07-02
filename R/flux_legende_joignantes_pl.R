flux_legende_joignantes_pl <-
function(x,y,long_pl,large_pl,code_epsg)
{
  l_pl <- list()
  
  xp <- x
  yp <- y
  
  xpyp_pl <- st_sf(geometry=st_sfc(st_point(c(xp,yp))), crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
  xpyp_pl <- st_transform(xpyp_pl,paste0("+init=epsg:",code_epsg))
  
  xp_pl <- st_coordinates(xpyp_pl)[1]
  yp_pl <- st_coordinates(xpyp_pl)[2]
  xg_pl <- xp_pl
  yg_pl <- yp_pl+large_pl
  
  vec <- matrix(c(xg_pl,yg_pl, xg_pl+long_pl,yg_pl, xg_pl+long_pl+large_pl/2,yg_pl+large_pl/2, xg_pl+long_pl,yg_pl+large_pl,   xg_pl,yg_pl+large_pl,   xg_pl,yg_pl),6,2,byrow=T)
  l_pl[[1]] <- st_polygon(list(vec))
  vec <- matrix(c(xp_pl,yp_pl, xp_pl+long_pl,yp_pl, xp_pl+long_pl+large_pl/2,yp_pl+large_pl/6, xp_pl+long_pl,yp_pl+large_pl/3, xp_pl,yp_pl+large_pl/3, xp_pl,yp_pl),6,2,byrow=T)
  l_pl[[2]] <- st_polygon(list(vec))
  
  flux_leg_pl <- st_sf(geometry=st_sfc(l_pl), crs=paste0("+init=epsg:",code_epsg))
  
  return(flux_leg_pl)
}
