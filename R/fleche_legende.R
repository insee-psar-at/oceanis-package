fleche_legende <-
  function(x,y,long,large,vmax,epsg)
  {
    l <- list()
    pt <- st_sfc(st_geometry(st_point(c(x,y))), crs = 4326)
    pt <- st_transform(pt, crs = as.numeric(epsg))
    coord_pt <- st_coordinates(pt)[1:2]

    xp <- coord_pt[1]
    yp <- coord_pt[2]
    xg <- coord_pt[1]
    yg <- coord_pt[2] + large/1.5

    vec <- matrix(c(xg,yg, xg+long,yg, xg+long+large/2,yg+large/2, xg+long,yg+large,   xg,yg+large,   xg,yg),6,2,byrow=T)
    l[[1]] <- st_polygon(list(vec))
    vec <- matrix(c(xp,yp, xp+long,yp, xp+long+large/2,yp+large/6, xp+long,yp+large/3, xp,yp+large/3, xp,yp),6,2,byrow=T)
    l[[2]] <- st_polygon(list(vec))

    flux_leg_pl <- st_sf(geometry=st_sfc(l), crs = as.numeric(epsg))
    
    rectangle <- st_bbox(flux_leg_pl)
    rectangle[1] <- rectangle[1] - large / 3
    rectangle[2] <- rectangle[2] - large / 3
    rectangle[3] <- rectangle[3] + large / 6 * nchar(vmax)
    rectangle[4] <- rectangle[4] + large
    
    vec <- matrix(c(rectangle[1],rectangle[2],   rectangle[3],rectangle[2],   rectangle[3],rectangle[4],   rectangle[1],rectangle[4],   rectangle[1],rectangle[2]),5,2,byrow=T)
    rectangle <- st_sfc(st_polygon(list(vec)), crs = as.numeric(epsg))
    
    flux_legWGS84 <- st_transform(flux_leg_pl, crs = 4326)
    rectangle <- st_transform(rectangle, crs = 4326)

    pointe1 <- st_sfc(st_geometry(st_point(c(xg+long+large/2, yg+large/2))), crs = as.numeric(epsg))
    pointe1 <- st_coordinates(st_transform(pointe1, crs = 4326))
    pointe2 <- st_sfc(st_geometry(st_point(c(xp+long+large/2, yp+large/6))), crs = as.numeric(epsg))
    pointe2 <- st_coordinates(st_transform(pointe2, crs = 4326))
    
    return(list(flux_legWGS84,pointe1,pointe2,rectangle,flux_leg_pl))
  }
