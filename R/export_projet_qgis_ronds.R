export_projet_qgis_ronds <-
function(liste_fonds,chemin_fonds,nom_projet,titre,titre2,sourc,annee)
  {
    chemin_fonds <- paste0(chemin_fonds,"/layers/")
    
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
    
    BLOCCOMPOSER=modif_canevas_MEP(nom_projet,str_sub(titre,end=-2),titre2,annee,sourc,xmin,xmax,ymin,ymax) #str_sub(unlist(str_split(sortie,"/"))[length(unlist(str_split(sortie,"/")))],end=-5)
    
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
      chemincoucherelatif=paste0("./layers/",nomcouche,".shp")
      
      BLOCCATEGORIES=data.frame()      
      #cas ou le fond selectionne est l'analyse en ronds ou les ronds de la legende
      if(str_sub(l[i][length(l[i])],start=-5)=="carte")
      {
        analyse_ronds <- read_sf(chemincouche)
        geometrie=attr(analyse_ronds$geometry[[1]],"class")[2]
        projcouche=st_crs(analyse_ronds$geometry)$proj4string
        attr=""
        name="0"
        typeanalyse="singleSymbol"
        #preparation des param du BLOCSYMBOL
        if(str_sub(l[i][length(l[i])],start=-9) %in% c("pos_carte","neg_carte")) #par les fonctions autres que shiny_
        {
          couleurbordure=unique(analyse_ronds$COL_BOR)# par defaut : "255,255,255" white
          stylebordure="solid"
          epaisseurbordure=0.26
          if(str_sub(l[i][length(l[i])],start=-9)=="pos_carte") couleurfond=unique(analyse_ronds$COL)# par defaut : "205,133,63" orange peru
          if(str_sub(l[i][length(l[i])],start=-9)=="neg_carte") couleurfond=unique(analyse_ronds$COL)# par defaut : "100,149,237" cornflowerblue
        }else if(str_sub(l[i][length(l[i])],start=-16) %in% c("pos_elargi_carte","neg_elargi_carte"))
        {
          couleurbordure=unique(analyse_ronds$COL_BOR)# par defaut : "255,255,255" white
          stylebordure="solid"
          epaisseurbordure=0.26
          if(str_sub(l[i][length(l[i])],start=-16)=="pos_elargi_carte") couleurfond=unique(analyse_ronds$COL)# par defaut : "205,133,63" orange peru
          if(str_sub(l[i][length(l[i])],start=-16)=="neg_elargi_carte") couleurfond=unique(analyse_ronds$COL)# par defaut : "100,149,237" cornflowerblue
        }else #par les fonctions shiny_
        {
          couleurbordure="255,255,255"
          stylebordure="solid"
          epaisseurbordure=0.26
          couleurfond="235,97,127"
          if(str_sub(l[i][length(l[i])],start=-10)=="rupt_carte") couleurfond="40,106,199"
        }
        remplissagefond="solid"
        BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
        
        toto=modif_blocprojectlayers(geometrie,idcouche,chemincoucherelatif,nomcouche,projcouche,attr,typeanalyse)
        toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
        BLOCPROJECT=rbind(BLOCPROJECT,toto)
      }else
      {
        #cas ou le fond selectionne n'est pas l'analyse en ronds ni les ronds de la legende
        attr=""
        name="0"
        typeanalyse="singleSymbol"
        couleurfond="255,255,255"
        
        if(l[i]=="fond_ronds")
        {
          couleurfond="255,255,255"
          remplissagefond="yes"
        }else
        {
          remplissagefond="no"
        }
        
        if (l[i] %in% c("fond_maille","fond_departement","fond_region","fond_pays","fond_etranger"))
        {
          couleurbordure="128,128,128"
        }else if (l[i]=="fond_maille_elargi")
        {
          couleurbordure="200,200,200"
        }else if (l[i]=="fond_territoire")
        {
          couleurbordure="191,191,191"
        }else
        {
          couleurbordure="0,0,0"
        }
        
        stylebordure="solid"
        if (l[i] %in% c("fond_maille","fond_maille_elargi","fond_ronds","fond_departement","fond_pays","fond_etranger","fond_territoire","fond_lignes_leg"))
        {
          epaisseurbordure=0.26
        }else
        {
          epaisseurbordure=0.5
        }
        
        assign(paste0(l[i]),read_sf(chemincouche))
        projcouche=st_crs(get(l[i]))$proj4string
        geometrie=attr(get(l[i])$geometry[[1]],"class")[2]
        
        if(is.null(geometrie))
          geometrie=attr(get(l[i])[[1]][[1]],"class")[2]
        
        if(is.null(geometrie))
          geometrie=attr(get(l[i])[[1]],"class")[2]
        
        if (geometrie %in% c("POLYGON","MULTIPOLYGON"))
        {
          BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
        }else
        {
          if (geometrie %in% c("MULTILINESTRING"))
          {
            BLOCSYMBOLS=modif_blocsymbolsLine(couleurbordure,stylebordure,epaisseurbordure,name)
          }
        }
        
        blocvector=modif_blocvectorClassification(nomcouche)
        blocvector=data.frame(V1=c(blocvector[1:2,],BLOCSYMBOLS[,1],blocvector[4:5,]))
        bloclayeritem=data.frame(V1=c(bloclayeritem[1,],blocvector[,1],bloclayeritem[3,]))
        BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
        
        if (l[i]=="fond_ronds_leg")
        {
          BLOCSYMBOLSGENERATOR <- balises_qgis()[[11]]
          BLOCLABELING <- balises_qgis()[[12]]
        }else
        {
          BLOCSYMBOLSGENERATOR <- data.frame()
          BLOCLABELING <- data.frame()
        }
        
        toto=modif_blocprojectlayers(geometrie,idcouche,chemincoucherelatif,nomcouche,projcouche,attr,typeanalyse)
        toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),data.frame(V1=BLOCSYMBOLS[1:11,]),BLOCSYMBOLSGENERATOR,data.frame(V1=BLOCSYMBOLS[12,]),data.frame(V1=toto[17:20,]),BLOCLABELING,data.frame(V1=toto[21:23,]))
        BLOCPROJECT=rbind(BLOCPROJECT,toto)
      }
    }
    projproj=projcouche
    qgs1=modif_canevas(xmin,xmax,ymin,ymax,projproj,length(l))
    #etape finale
    blocproperties <- balises_qgis()[[13]]
    BLOCCOMPOSER=data.frame(V1=c(BLOCCOMPOSER[1:43,],BLOCLAYERITEM[,1],BLOCCOMPOSER[45:94,]))
    canevas_final=data.frame(V1=c(qgs1[1:19,],BLOCLEG[,1],qgs1[21,],BLOCCOMPOSER[,1],qgs1[23,],BLOCPROJECT[,1],qgs1[25,],blocproperties[,1],qgs1[26,]))
    colnames(canevas_final)=NULL
    write.csv(canevas_final,paste0(substr(chemin_fonds,1,nchar(chemin_fonds)-7),nom_projet,".qgs"),row.names = F, quote = F, fileEncoding = "UTF-8")
  }
