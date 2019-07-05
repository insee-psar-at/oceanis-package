export_projet_qgis_fleches_joignantes <-
function(liste_fonds,chemin_fonds,nom_projet,titre,titre2,sourc,colFleche,colBorder,annee)
  {
    chemin_fonds <- paste0(chemin_fonds,"/")
    
    fond_maille <- read_sf(paste0(chemin_fonds,"fond_maille.shp"))
    xmin=st_bbox(fond_maille)[1]-0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    xmax=st_bbox(fond_maille)[3]+0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    ymin=st_bbox(fond_maille)[2]-0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    ymax=st_bbox(fond_maille)[4]+0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    
    #on reajuste la fenetre pour que (xmax-xmin)=1.65(ymax-ymin)
    dif_x=xmax-xmin
    dif_y=ymax-ymin
    
    if (dif_x>dif_y)
    {
      if (dif_x/dif_y<1.65)
      {
        xmin=xmin-((1.65*dif_y-dif_x)/2)
        xmax=xmax+((1.65*dif_y-dif_x)/2)
      }else
      {
        ymin=ymin-((dif_x/1.65)-dif_y)/2
        ymax=ymax+((dif_x/1.65)-dif_y)/2
      }   
    }else
    {
      xmin=xmin-((1.65*dif_y-dif_x)/2)
      xmax=xmax+((1.65*dif_y-dif_x)/2)
    }
    
    BLOCCOMPOSER=modif_canevas_MEP(nom_projet,str_sub(titre,end=-2),titre2,annee,sourc,xmin,xmax,ymin,ymax)
    
    BLOCLEG=data.frame()
    BLOCPROJECT=data.frame()
    BLOCLAYERITEM=data.frame()
    
    l <- liste_fonds
    for (i in 1:length(l))
    {
      #BLOCLEG 
      if(nchar(l[i])<11)
      {
        idcouche=paste0(l[i],str_c(rep("0",11-nchar(l[i])), collapse = ""))
      }else
      {
        idcouche=l[i]
      }
      
      toto=modif_blocleg(l[i],idcouche)  
      BLOCLEG=rbind(BLOCLEG,toto)
      
      #BLOCLAYERITEM                  
      if(str_sub(l[i][length(l[i])],start=-5)!="carte")
      {
        bloclayeritem=modif_bloclayeritem(l[i],idcouche,"hidden")
      }
      
      #BLOCPROJECT
      #param idcouche, chemincouche, nomcouche
      nomcouche=l[i]
      chemincouche=paste0(chemin_fonds,nomcouche,".shp")
      BLOCCATEGORIES=data.frame()      
      
      attr=""
      name="0"
      typeanalyse="singleSymbol"
      couleurfond="255,255,255"
      
      if (l[i] %in% c("fond_flux","fond_flux_leg"))
      {
        couleurfond=colFleche
        remplissagefond="yes"
      }else if(l[i]=="fond_flux_rupt")
      {
        couleurfond="100,149,237"
        remplissagefond="yes"
      }else
      {
        remplissagefond="no"
      }
      
      if (l[i] %in% c("fond_maille","fond_departement","fond_region","fond_pays"))
      {
        couleurbordure="128,128,128"
      }else if (l[i]=="fond_territoire")
      {
        couleurbordure="191,191,191"
      }else if (l[i] %in% c("fond_flux","fond_flux_leg"))
      {
        couleurbordure=colBorder
      }else
      {
        couleurbordure="0,0,0"
      }
      
      stylebordure="solid"
      if (l[i] %in% c("fond_flux","fond_flux_leg","fond_flux_rupt","fond_maille","fond_departement","fond_pays","fond_territoire"))
      {
        epaisseurbordure=0.26
      }else
      {
        epaisseurbordure=0.5
      }
      
      analyse_fj_fs <- read_sf(chemincouche)
      projcouche=st_crs(analyse_fj_fs)$proj4string
      geometrie=attr(analyse_fj_fs$geometry[[1]],"class")[2]
      
      if(is.null(geometrie))
        geometrie=attr(analyse_fj_fs[[1]][[1]],"class")[2]
      
      if(is.null(geometrie))
        geometrie=attr(analyse_fj_fs[[1]],"class")[2]
      
      if (geometrie %in% c("POLYGON","MULTIPOLYGON"))
      {
        BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
      }else
      {
        if (geometrie %in% c("LINESTRING","MULTILINESTRING"))
        {
          BLOCSYMBOLS=modif_blocsymbolsLine(couleurbordure,stylebordure,epaisseurbordure,name)
        }
      }
      
      blocvector=modif_blocvectorClassification(nomcouche)
      blocvector=data.frame(V1=c(blocvector[1:2,],BLOCSYMBOLS[,1],blocvector[4:5,]))
      bloclayeritem=data.frame(V1=c(bloclayeritem[1,],blocvector[,1],bloclayeritem[3,]))
      BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
      
      toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
      toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
      BLOCPROJECT=rbind(BLOCPROJECT,toto)
    }
    projproj=projcouche
    qgs1=modif_canevas(xmin,xmax,ymin,ymax,projproj,length(l))
    #etape finale
    BLOCCOMPOSER=data.frame(V1=c(BLOCCOMPOSER[1:43,],BLOCLAYERITEM[,1],BLOCCOMPOSER[45:94,]))
    canevas_final=data.frame(V1=c(qgs1[1:19,],BLOCLEG[,1],qgs1[21,],BLOCCOMPOSER[,1],qgs1[23,],BLOCPROJECT[,1],qgs1[25:26,]))
    colnames(canevas_final)=NULL
    write.csv(canevas_final,paste0(chemin_fonds,nom_projet,".qgs"),row.names = F, quote = F, fileEncoding = "UTF-8")
  }
