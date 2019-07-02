construction_ronds_legende <-
function(lon,lat,code_epsg,taille_rond_m)
{
  centres_leg <- t(data_frame(c(lon,lat)))
  
  #On cree les points en WGS84
  ronds_sf_leg <- st_sf(geometry=st_sfc(st_point(centres_leg),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
  
  #On les convertit en projection locale (pl) pour avoir des metres
  ronds_pl_leg <- st_transform(ronds_sf_leg,paste0("+init=epsg:",code_epsg)) # Lambert 93
  
  #On cree le grand cercle en pl
  ronds_pl_leg_1 <- st_buffer(ronds_pl_leg, taille_rond_m)
  
  #que l'on convertit en WGS84
  ronds_sf_leg_1 <- st_transform(ronds_pl_leg_1,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
  
  #On cree le petit cercle en pl
  ronds_pl_leg_2 <- st_buffer(ronds_pl_leg, taille_rond_m/sqrt(3))
  
  #que l'on decale sur le grand cercle
  ronds_pl_leg_3 <- st_sf(geometry=st_sfc(st_geometry(ronds_pl_leg_2)+c(0,(st_bbox(ronds_pl_leg_1)[2]-st_bbox(ronds_pl_leg_2)[2])),crs=paste0("+init=epsg:",code_epsg)))
  
  #On fusionne les 2 cercles en pl
  ronds_pl_leg <- rbind(ronds_pl_leg_1,ronds_pl_leg_3)
  
  #On convertit le petit cercle en WGS84
  ronds_sf_leg_2 <- st_transform(ronds_pl_leg_2,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
  
  #que l'on decale sur le grand cercle
  ronds_sf_leg_3 <- st_sf(geometry=st_sfc(st_geometry(ronds_sf_leg_2)+c(0,(st_bbox(ronds_sf_leg_1)[2]-st_bbox(ronds_sf_leg_2)[2])),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
  
  #On fusionne les 2 cercles en WGS84
  ronds_sf_leg <- rbind(ronds_sf_leg_1,ronds_sf_leg_3)
  
  return(list(ronds_sf_leg,ronds_pl_leg))
}
