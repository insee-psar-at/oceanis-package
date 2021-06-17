shiny_saphirs <-
function(data,fondMaille,typeMaille,fondContour,fondSuppl=NULL,idDataDepart,idDataArrivee,varFlux,direction="Ent",emprise="FRM",fondEtranger=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20<-msg_error21<-msg_error22 <- NULL

    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error3 <- "Le fond de contour doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error4 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idDataDepart)!="character")) msg_error5 <- "Le nom de la variable de depart doit etre de type caractere / "
    if(any(class(idDataArrivee)!="character")) msg_error6 <- "Le nom de la variable d'arrivee doit etre de type caractere / "
    if(any(class(varFlux)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(typeMaille)!="character")) msg_error8 <- "La valeur doit etre de type caractere ('REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'DEPCOM') / "
    if(any(class(direction)!="character")) msg_error9 <- "La valeur doit etre de type caractere ('Ent', 'Sor' ou 'Sol') / "
    if(any(class(emprise)!="character")) msg_error10 <- "La valeur doit etre de type caractere ('FRM', '971', '972', '973', '974', '976' ou '999') / "

    if(length(names(data))<3) msg_error11 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant de depart, une variable identifiant d'arrivee et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error12 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error13 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error14 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

    if(!any(names(data) %in% idDataDepart))  msg_error15 <- "La variable identifiant de depart n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% idDataArrivee))  msg_error16 <- "La variable identifiant d'arrivee n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varFlux))  msg_error17 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!typeMaille %in% c("REG", "DEP", "ZE", "AU", "BV", "UU", "EPCI", "DEPCOM")) msg_error18 <- "La variable typeMaille doit etre 'REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'DEPCOM' / "
    if(!direction %in% c("Ent", "Sor", "Sol")) msg_error19 <- "La variable direction doit etre 'Ent', 'Sor' ou 'Sol' / "
    if(!emprise %in% c("FRM","971","972","973","974","976","999")) msg_error20 <- "La variable emprise doit etre 'FRM', '971', '972', '973', '974', '976' ou '999' / "
    if(!is.null(fondEtranger)) if(any(!any(class(fondEtranger) %in% "sf"),!any(class(fondEtranger) %in% "data.frame"))) msg_error21 <- "Le fond etranger doit etre un objet sf / "
    if(!is.null(fondEtranger)) if(length(names(fondEtranger))<3) msg_error22 <- "Le fond etranger n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),!is.null(msg_error13),
           !is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),!is.null(msg_error17),
           !is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20),!is.null(msg_error21),!is.null(msg_error22)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,
                              msg_error17,msg_error18,msg_error19,msg_error20,msg_error21,msg_error22)))
    }

    nb_up <- reactiveValues(a=0)
    nb_down <- reactiveValues(a=0)
    ordre_analyse <- reactiveValues(a=1,b=2)
    insert_save <- reactiveValues(a=0)
    remove_carte <- reactiveValues(a=0)
    liste_fonds <- reactiveValues(a=c("analyse","maille","contour"))
    m_save_fs <- reactiveValues(a=0)

    flux_min <- reactiveValues(a=100)
    distance_max <- reactiveValues(a=300)
    flux_majeur <- reactiveValues(a=10)
    largeur <- reactiveValues(a=0)
    longueur <- reactiveValues(a=3000)

    if (typeMaille=="REG") largeur$a<-100
    if (typeMaille=="DEP") largeur$a<-30
    if (substr(typeMaille,1,2)=="ZE") largeur$a<-10
    if (substr(typeMaille,1,2)=="AU") largeur$a<-6
    if (substr(typeMaille,1,2)=="BV") largeur$a<-6
    if (substr(typeMaille,1,2)=="UU") largeur$a<-6
    if (typeMaille=="EPCI") largeur$a<-4
    if (typeMaille=="DEPCOM") largeur$a<-2

    erreur_maille <- reactiveValues(a=FALSE)

    legende <- reactiveValues(a=NULL)

    sourc <- "Source : Insee"

    names(data)[names(data)==idDataDepart] <- "CODE1"
    names(data)[names(data)==idDataArrivee] <- "CODE2"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    names(fondContour)[1] <- "CODE"
    names(fondContour)[2] <- "LIBELLE"
    epsg_etranger <- NULL
    if(!is.null(fondEtranger))
    {
      names(fondEtranger)[1] <- "CODE"
      names(fondEtranger)[2] <- "LIBGEO"
      fondEtranger$LIBGEO<-iconv(fondEtranger$LIBGEO,"latin1","utf8")

      if(substr(st_crs(fondEtranger)[1]$input,1,5) == "EPSG:")
      {
        epsg_etranger <- substr(st_crs(fondEtranger)[1]$input,6,9)
      }else
      {
        epsg_etranger <- st_crs(fondEtranger)[1]$input
      }

      if(is.na(epsg_etranger) | epsg_etranger=="4326")
      {
        epsg_etranger <- "3395" # Mercator
      }
    }
    if(!is.null(fondSuppl))
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }

    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    fondContour$LIBELLE<-iconv(fondContour$LIBELLE,"latin1","utf8")

    ui <- navbarPage("OCEANIS", id="menu",

                     theme = shinytheme("superhero"),

                     tabPanel("Carte",value="carte",
                              sidebarLayout(

                                sidebarPanel(width = 3,
                                             style = "overflow-y:scroll; min-height: 840px; max-height: 840px",
                                             h4(HTML("<b><font color=#95BAE2>VARIABLES</font></b>")),
                                             uiOutput("variable_flux_fs"),
                                             tags$hr(style="border: 5px solid #5182B6"), #337ab7
                                             h4(HTML("<b><font color=#95BAE2>FONDS</font></b>")),
                                             fluidRow(
                                               column(width=9, offset=0.5,
                                                      uiOutput("ordre_fonds_fs")
                                               ),
                                               column(width=1,
                                                      br(),
                                                      br(),
                                                      htmlOutput("monter_fond_fs", inline=FALSE),
                                                      htmlOutput("descendre_fond_fs", inline=FALSE)
                                               )
                                             ),
                                             uiOutput("ajout_territoire_fs"),
                                             uiOutput("ajout_reg_fs"),
                                             uiOutput("ajout_dep_fs"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>FLUX</font></b>")),
                                             uiOutput("flux_min_fs"),
                                             uiOutput("largeur_fs"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>L\u00c9GENDE</font></b>")),
                                             uiOutput("titre_flux_legende_fs"),
                                             br(),
                                             uiOutput("affiche_legende_fs"),
                                             br(),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>SAUVEGARDE</font></b>")),
                                             uiOutput("save_carte_fs"),
                                             br(),
                                             conditionalPanel(condition = 'input.mymap_fs_click',
                                                              tags$div(class="dropup",
                                                                       HTML('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                                                            Exporter en projet Qgis
                                                                            <span class="caret"></span>
                                                                            </button>'),
                                                                       tags$ul(class="dropdown-menu",
                                                                               wellPanel(
                                                                                 style="background: #2B3E50; width:340px",
                                                                                 h4("Export de la carte en projet Qgis"),
                                                                                 br(),
                                                                                 uiOutput("sortie_qgis_fs"),
                                                                                 br(),
                                                                                 uiOutput("titre1_qgis_fs"),
                                                                                 uiOutput("titre2_qgis_fs"),
                                                                                 uiOutput("source_qgis_fs"),
                                                                                 tags$head(tags$style(HTML('#export_qgis_fs_id{background-color:#337ab7}'))),
                                                                                 uiOutput("export_qgis_fs")
                                                                               )
                                                                       )
                                                                       )
                                             ),
                                             br(),
                                             uiOutput("aide_image_fs"),
                                             br()
                              ),
                              mainPanel(
                                tags$head(
                                  tags$style(HTML(".leaflet-container { background: #AFC9E0; }"))
                                ),
                                tabsetPanel(id="onglets_fs",
                                            tabPanel(title=HTML("<b>Carte</b>"),value="carte",
                                                     leafletOutput("mymap_fs",width="100%",height = 800)
                                            ),
                                            tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es</b>")),value="donnees",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mydonnees_fs",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Maille</b>"),value="maille",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mymaille_fs",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mycontour_fs",width="100%",height = 800))
                                )
                              )
    )
    )
    )

    server <- function(input, output, session) {

      #################
      #Onglet Carte
      #################

      #Charge les donnees et fonds en memoire et affiche les widgets adequats dans le sidePanel de l'onglet "Carte"
      observe({

        # VARIABLES

        output$variable_flux_fs <- renderUI({
          selectInput("variable_flux_fs_id", label=h5("Variable des flux (en volume)"), choices = varFlux, selected = varFlux)
        })

        # FONDS

        output$ordre_fonds_fs <- renderUI({
          selectInput("ordre_fonds_fs_id", label=h5("Modifier l'ordre des fonds"), choices = liste_fonds$a, multiple=TRUE, selectize=FALSE, selected = "analyse")
        })
        output$monter_fond_fs <- renderUI({
          actionButton("monter_fond_fs_id", label="", icon=icon("arrow-up"))
        })
        output$descendre_fond_fs <- renderUI({
          actionButton("descendre_fond_fs_id", label="", icon=icon("arrow-down"))
        })

        output$ajout_territoire_fs <- renderUI({
          checkboxInput("ajout_territoire_fs_id", label = "Afficher le fond des territoires",
                        value = if(is.null(fondSuppl)) FALSE else TRUE)
        })
        output$ajout_reg_fs <- renderUI({
          checkboxInput("ajout_reg_fs_id", label = "Afficher le fond des r\u00e9gions",
                        value = FALSE)
        })
        output$ajout_dep_fs <- renderUI({
          checkboxInput("ajout_dep_fs_id", label = "Afficher le fond des d\u00e9partements",
                        value = FALSE)
        })

        # FLUX

        if(direction!="Sol")
        {
          output$flux_min_fs <- renderUI({
            numericInput("flux_min_fs_id", label = h5("Seuil de flux minimal"), value=flux_min$a, step=10)
          })
        }

        output$largeur_fs <- renderUI({
          numericInput("largeur_fs_id", label = h5("Largeur de la fleche la plus large (km)"), value=largeur$a, step=10)
        })

        # LEGENDE

        output$titre_flux_legende_fs <- renderUI({
          textInput("titre_flux_legende_fs_id", label = h5("Titre de la l\u00e9gende des flux"), value = "")
        })

        output$affiche_legende_fs <- renderUI({
          checkboxInput("affiche_legende_fs_id", label = "Activer le d\u00e9placement de la l\u00e9gende au clic",
                        value = TRUE)
        })

        # SAUVEGARDE

        output$save_carte_fs <- renderUI({
          actionButton("save_carte_fs_id", label=HTML("<font size=3>Sauvegarder la carte dans un onglet</font>"), style="color:#FFFFFF; background-color:#DF691A")
        })

        output$entrees_qgis_fs <- renderUI({
          actionButton("entrees_qgis_fs_id", label="Exporter en projet Qgis")
        })

        output$sortie_qgis_fs <- renderUI({
          tags$div(class="input-group",
                   HTML('<input type="text" id="sortie_qgis_fs_id" class="form-control" placeholder="Nom du projet" aria-describedby="sortie_qgis_fs_id">
                        <span class="input-group-addon" id="sortie_qgis_fs_id">.qgs</span>'))
        })

        output$titre1_qgis_fs <- renderUI({
          textInput("titre1_qgis_fs_id", label = h5("Titre informatif"), value = "", placeholder= "Facultatif")
        })

        output$titre2_qgis_fs <- renderUI({
          textInput("titre2_qgis_fs_id", label = h5("Titre descriptif"), value = "", placeholder= "Facultatif")
        })

        output$source_qgis_fs <- renderUI({
          textInput("source_qgis_fs_id", label = h5("Source de la carte"), value = sourc)
        })

        output$aide_image_fs <- renderUI({
          tags$div(class="dropup",
                   HTML(paste0('<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">
                               <i class="fa fa-book fa-fw" aria-hidden="true"></i>
                               Proc','\u00e9','dure pour capture d\'','\u00e9','cran
                               <span class="caret"></span>
                               </button>')),
                   tags$ul(class="dropdown-menu",
                           wellPanel(
                             style="background: #2B3E50; width:340px",
                             div(
                               HTML("<font size=2>Deux possibilit\u00e9s :</font>"),
                               br(),
                               br(),
                               strong(HTML("<font size=3>Par l'Outil Capture</font>")),
                               br(),
                               HTML("<font size=2>1- Ouvrir un logiciel de capture (Outil Capture de Windows par exemple).</font>"),
                               br(),
                               HTML(paste0("<font size=2>2- S\u00e9lectionner la zone \u00e0 capturer.</font>")),
                               br(),
                               HTML("<font size=2>3- Enregistrer l'image ou copier la dans le presse-papier.</font>"),
                               br(),
                               br(),
                               strong(HTML(paste0("<font size=3>Par impression d'","\u00e9","cran</font>"))),
                               br(),
                               HTML("<font size=2>1- Appuyer sur la touche clavier \"Impr ecran\".</font>"),
                               br(),
                               HTML("<font size=2>2- Ouvrir un logiciel de retouche image (Paint par exemple).</font>"),
                               br(),
                               HTML("<font size=2>3- Coller l'image et l'enregistrer au format voulu (.jpg, .png, .bmp).</font>")
                             )
                           )
                   )
                   )
        })
        })

      # Pour modifier l'ordre des fonds
      observeEvent(list(input$monter_fond_fs_id,input$descendre_fond_fs_id),{

        if(as.numeric(input$monter_fond_fs_id)==0 & as.numeric(input$descendre_fond_fs_id)==0) return(NULL)
        ordre <- c()
        if(as.numeric(input$monter_fond_fs_id)>nb_up$a)
        {
          ordre <- c(2,3)
          nb_up$a <- nb_up$a+1
        }
        if(as.numeric(input$descendre_fond_fs_id)>nb_down$a)
        {
          ordre <- c(1,2)
          nb_down$a <- nb_down$a+1
        }

        if(is.null(input$ordre_fonds_fs_id)) pos_select <- 0 else pos_select <- which(liste_fonds$a==input$ordre_fonds_fs_id)

        if(pos_select>0)
        {
          if(pos_select==ordre[1]) liste_fonds$a <- liste_fonds$a[c(2,1,3)]
          if(pos_select==ordre[2]) liste_fonds$a <- liste_fonds$a[c(1,3,2)]

          updateSelectInput(session, "ordre_fonds_fs_id",
                            choices = liste_fonds$a,
                            selected = input$ordre_fonds_fs_id
          )
        }
      },ignoreInit = TRUE)

      flux_min_fs <- reactive({

        if(is.null(input$flux_min_fs_id)) return(flux_min$a)

        return(input$flux_min_fs_id)
      })

      # Pour exporter la carte en projet Qgis

      output$export_qgis_fs <- renderUI({
        downloadButton("downloadProjetQgis_fs", label="Exporter")
      })

      output$downloadProjetQgis_fs <- downloadHandler(contentType = "zip",
                                                      filename = function(){
                                                        paste0(input$sortie_qgis_fs_id,".zip")
                                                      },
                                                      content = function(file){
                                                        files <- EXPORT_PROJET_QGIS_FS(file)

                                                        zip(file,files, flags = "-j9X")
                                                      }
      )

      # Pour exporter la carte en projet Qgis
      EXPORT_PROJET_QGIS_FS <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        sortie <- input$sortie_qgis_fs_id
        rep_sortie <- dirname(file)
        files <- c(paste0(rep_sortie,"/",sortie,".qgs"))

        rupture <- FALSE

        donnees <- analyse_fs()[[1]]$save
        if(max(donnees)>0 & min(donnees)<0) rupture <- TRUE

        if(rupture)
        {
          fond_flux_sortie <- st_sf(analyse_fs()[[1]][analyse_fs()[[1]]$save<0,])
          fond_flux_entree <- st_sf(analyse_fs()[[1]][analyse_fs()[[1]]$save>=0,])

          fond_flux_sortie <- st_transform(fond_flux_sortie,crs = as.numeric(code_epsg_fs()))
          fond_flux_entree <- st_transform(fond_flux_entree,crs = as.numeric(code_epsg_fs()))

          suppressWarnings(write_fond_flux_sortie <- try(st_write(fond_flux_sortie, paste0(rep_sortie,"fond_flux_sortie.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
          suppressWarnings(write_fond_flux_entree <- try(st_write(fond_flux_entree, paste0(rep_sortie,"fond_flux_entree.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
        }
        if(min(donnees)>=0)
        {
          fond_flux_entree <- st_sf(analyse_fs()[[1]])
          fond_flux_entree <- st_transform(fond_flux_entree,crs = as.numeric(code_epsg_fs()))
          suppressWarnings(write_fond_flux_entree <- try(st_write(fond_flux_entree, paste0(rep_sortie,"fond_flux_entree.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
        }
        if(max(donnees)<0)
        {
          fond_flux_sortie <- st_sf(analyse_fs()[[1]])
          fond_flux_sortie <- st_transform(fond_flux_sortie,crs = as.numeric(code_epsg_fs()))
          suppressWarnings(write_fond_flux_sortie <- try(st_write(fond_flux_sortie, paste0(rep_sortie,"fond_flux_sortie.shp"), delete_dsn = TRUE, quiet = TRUE),silent=TRUE))
        }

        vmax <- max(abs(data.frame(analyse_fs()[[2]])[,varFlux]))

        coord_fleche_max_pl <- st_coordinates(analyse_fs()[[1]][abs(data.frame(analyse_fs()[[1]])[,varFlux])==vmax,])
        large_pl <- max(st_distance(st_sfc(st_point(c(coord_fleche_max_pl[5,1],coord_fleche_max_pl[5,2])),st_point(c(coord_fleche_max_pl[6,1],coord_fleche_max_pl[6,2])))))
        long_pl <- large_pl*1.5

        if(large_pl>100000) long_pl <- 100000
        if(large_pl<1000) long_pl <- 1000

        flux_leg <- flux_legende_saphirs_pl(lon_lat_fs()[[1]],lon_lat_fs()[[2]],long_pl,large_pl,code_epsg_fs())
        flux_leg <- cbind(flux_leg,VALEUR=c(max(data[,varFlux]),max(data[,varFlux])/3))
        fond_flux_leg <- flux_leg

        fond_maille <- st_transform(fondMaille,crs = as.numeric(code_epsg_fs()))
        fond_contour <- st_transform(fondContour,crs = as.numeric(code_epsg_fs()))
        if(!is.null(fondSuppl) && input$ajout_territoire_fs_id) fond_territoire <- st_transform(fond_territoire_fs(),crs = as.numeric(code_epsg_fs()))
        if(input$ajout_dep_fs_id) fond_departement <- st_transform(fond_departement_fs(),crs = as.numeric(code_epsg_fs()))
        if(input$ajout_reg_fs_id) fond_region <- st_transform(fond_region_fs(),crs = as.numeric(code_epsg_fs()))
        fond_france <- st_transform(fond_habillage_fs()[[1]],crs = as.numeric(code_epsg_fs()))
        fond_pays <- st_transform(fond_habillage_fs()[[2]],crs = as.numeric(code_epsg_fs()))

        if(rupture | min(donnees)>=0) st_write(fond_flux_entree, paste0(rep_sortie,"/fond_flux_entree.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(rupture | max(donnees)<0) st_write(fond_flux_sortie, paste0(rep_sortie,"/fond_flux_sortie.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_flux_leg, paste0(rep_sortie,"/fond_flux_leg.shp"), delete_dsn = TRUE, quiet = TRUE)
        suppressWarnings(st_write(fond_maille, paste0(rep_sortie,"/fond_maille.shp"), delete_dsn = TRUE, quiet = TRUE))
        suppressWarnings(st_write(fond_contour,paste0(rep_sortie,"/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE))
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) st_write(fond_territoire, paste0(rep_sortie,"/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_departement")) if(!is.null(fond_departement)) st_write(fond_departement, paste0(rep_sortie,"/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_region")) if(!is.null(fond_region)) st_write(fond_region,paste0(rep_sortie,"/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_france,paste0(rep_sortie,"/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_pays")) if(!is.null(fond_pays)) st_write(fond_pays,paste0(rep_sortie,"/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE)

        if(rupture | min(donnees)>=0) files <- c(paste0(rep_sortie,"/fond_flux_entree.shp"),paste0(rep_sortie,"/fond_flux_entree.dbf"),paste0(rep_sortie,"/fond_flux_entree.prj"),paste0(rep_sortie,"/fond_flux_entree.shx"),files)
        if(rupture | max(donnees)<0) files <- c(paste0(rep_sortie,"/fond_flux_sortie.shp"),paste0(rep_sortie,"/fond_flux_sortie.dbf"),paste0(rep_sortie,"/fond_flux_sortie.prj"),paste0(rep_sortie,"/fond_flux_sortie.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_flux_leg.shp"),paste0(rep_sortie,"/fond_flux_leg.dbf"),paste0(rep_sortie,"/fond_flux_leg.prj"),paste0(rep_sortie,"/fond_flux_leg.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_maille.shp"),paste0(rep_sortie,"/fond_maille.dbf"),paste0(rep_sortie,"/fond_maille.prj"),paste0(rep_sortie,"/fond_maille.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_contour.shp"),paste0(rep_sortie,"/fond_contour.dbf"),paste0(rep_sortie,"/fond_contour.prj"),paste0(rep_sortie,"/fond_contour.shx"),files)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) files <- c(paste0(rep_sortie,"/fond_territoire.shp"),paste0(rep_sortie,"/fond_territoire.dbf"),paste0(rep_sortie,"/fond_territoire.prj"),paste0(rep_sortie,"/fond_territoire.shx"),files)
        if(exists("fond_departement")) if(!is.null(fond_departement)) files <- c(paste0(rep_sortie,"/fond_departement.shp"),paste0(rep_sortie,"/fond_departement.dbf"),paste0(rep_sortie,"/fond_departement.prj"),paste0(rep_sortie,"/fond_departement.shx"),files)
        if(exists("fond_region")) if(!is.null(fond_region)) files <- c(paste0(rep_sortie,"/fond_region.shp"),paste0(rep_sortie,"/fond_region.dbf"),paste0(rep_sortie,"/fond_region.prj"),paste0(rep_sortie,"/fond_region.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_france.shp"),paste0(rep_sortie,"/fond_france.dbf"),paste0(rep_sortie,"/fond_france.prj"),paste0(rep_sortie,"/fond_france.shx"),files)
        if(exists("fond_pays")) if(!is.null(fond_pays)) files <- c(paste0(rep_sortie,"/fond_pays.shp"),paste0(rep_sortie,"/fond_pays.dbf"),paste0(rep_sortie,"/fond_pays.prj"),paste0(rep_sortie,"/fond_pays.shx"),files)

        chemin_fonds <- rep_sortie
        titre1 <- paste0(input$titre1_qgis_fs_id,"\n")
        titre2 <- input$titre2_qgis_fs_id
        source <- input$source_qgis_fs_id
        annee <- format(Sys.time(), format = "%Y")
        variable_a_representer <- varFlux

        l <- c()
        if(exists("fond_territoire")) l <- "fond_territoire"
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")

        l=c("fond_france",
            "fond_contour",
            "fond_maille",
            l)

        if(rupture | min(donnees)>=0) l=c(l,"fond_flux_entree")
        if(rupture | max(donnees)<0) l=c(l,"fond_flux_sortie")

        l <- c(l,"fond_flux_leg")

        if(exists("fond_pays")) l <- c(l,"fond_pays")

        export_projet_qgis_fleches_saphirs(l,rep_sortie,sortie,titre1,titre2,source,"#FFC300","#286AC7","#303030",annee)

        removeModal()

        showModal(modalDialog(HTML(paste0("<font size=+1>Le projet Qgis a \u00e9t\u00e9 cr","\u00e9","ee.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        return(files)
      }

      code_epsg_fs <- reactive({
        code_epsg <- switch(emprise,
                            "FRM"="2154",# Lambert 93
                            "971"="5490",# UTM 20 N
                            "972"="5490",# UTM 20 N
                            "973"="2972",# UTM 22 N
                            "974"="2975",# UTM 40 S
                            "976"="4471",# UTM 38 S
                            "999"=epsg_etranger)
        return(code_epsg)
      })

      largeur_fs <- reactive({

        if(is.null(input$largeur_fs_id))
        {
          largeur <- largeur$a
        }else if(is.na(input$largeur_fs_id))
        {
          largeur <- largeur$a
        }else
        {
          largeur <- input$largeur_fs_id
        }
        return(largeur)
      })

      largeur_max_fs <- reactive({

        if(is.null(input$largeur_fs_id)) return(NULL)

        if (is.na(input$largeur_fs_id)) largeur$a<-1

        if (typeMaille=="REG") largeur$a<-100
        if (typeMaille=="DEP") largeur$a<-30
        if (substr(typeMaille,1,2)=="ZE") largeur$a<-10
        if (substr(typeMaille,1,2)=="AU") largeur$a<-6
        if (substr(typeMaille,1,2)=="BV") largeur$a<-6
        if (substr(typeMaille,1,2)=="UU") largeur$a<-6
        if (typeMaille=="EPCI") largeur$a<-4
        if (typeMaille=="DEPCOM") largeur$a<-2

        if(!is.na(input$largeur_fs_id))
        {
          if(input$largeur_fs_id==0)
          {
            largeur_max <- largeur$a*1000
          }else
          {
            largeur_max <- input$largeur_fs_id*1000
          }
        }else
        {
          largeur_max <- 1
        }

        return(largeur_max)
      })

      analyse_fs <- reactive({

        if(is.null(largeur_max_fs())) return(NULL)

        if(typeMaille=="REG") longueur$a<-100000
        if(typeMaille=="DEP") longueur$a<-30000
        if(!typeMaille %in% c("France metro","REG","DEP")) longueur$a<-10000

        suppressWarnings(test_k_saphir <- try(k_saphir(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,largeur_max_fs(),longueur$a,direction),silent=TRUE))

        if(!class(test_k_saphir) %in% "try-error")
        {
          analyse<-k_saphir(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,largeur_max_fs(),longueur$a,direction)
        }else
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }

        if(is.null(analyse))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }

        analyse_WGS84 <- st_transform(analyse[[1]],crs=4326)

        return(list(analyse[[1]],analyse_WGS84))
      })

      fond_habillage_fs <- reactive({

        if(emprise=="FRM")
        {
          fond_pays <- st_transform(sf_paysm(),crs=4326)
          fond_france <- st_transform(sf_fram(),crs=4326)
        }else if(emprise!="999")
        {
          if(emprise=="971")
          {
            fond_france <- st_transform(sf_reg01(),crs=4326)
            fond_pays <- fond_france
          }
          if(emprise=="972")
          {
            fond_france <- st_transform(sf_reg02(),crs=4326)
            fond_pays <- fond_france
          }
          if(emprise=="973")
          {
            fond_france <- st_transform(sf_reg03(),crs=4326)
            fond_pays <- st_transform(sf_pays973(),crs=4326)
          }
          if(emprise=="974")
          {
            fond_france <- st_transform(sf_reg04(),crs=4326)
            fond_pays <- fond_france
          }
          if(emprise=="976")
          {
            fond_france <- st_transform(sf_reg06(),crs=4326)
            fond_pays <- fond_france
          }
        }else if(emprise=="999")
        {
          fond_france <- st_transform(fondEtranger,crs=4326)
          fond_pays <- fond_france
        }else{}

        return(list(fond_france,fond_pays))
      })

      fond_contour_maille_fs <- reactive({

        test_contour <- try(st_transform(fondContour,crs=4326), silent = TRUE)
        test_maille <- try(st_transform(fondMaille,crs=4326), silent = TRUE)

        if(any(list(class(test_contour),class(test_maille)) %in% "try-error"))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>Une erreur est survenue dans la cr","\u00e9","ation du territoire.<br><br>Veuillez svp v\u00e9rifier vos donn","\u00e9","es et les variables choisies.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4"))
          erreur_maille$a <- TRUE
          return(NULL)
        }else
        {
          contour_WGS84 <- st_transform(fondContour,crs=4326)
          maille_WGS84 <- st_transform(fondMaille,crs=4326)
        }

        return(list(contour_WGS84,maille_WGS84))
      })

      list_bbox_fs <- reactive({
        req(fond_contour_maille_fs())

        list_bbox <- list(c(st_bbox(fond_contour_maille_fs()[[1]])[1],st_bbox(fond_contour_maille_fs()[[1]])[3]),c(st_bbox(fond_contour_maille_fs()[[1]])[2],st_bbox(fond_contour_maille_fs()[[1]])[4]))
        return(list_bbox)
      })

      fond_territoire_fs <- reactive({
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,crs=4326)
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })

      fond_region_fs <- reactive({
        fond_region <- st_transform(sf_regm(),crs=4326)
        return(fond_region)
      })

      fond_departement_fs <- reactive({
        fond_departement <- st_transform(sf_depm(),crs=4326)
        return(fond_departement)
      })

      fond_select_donnees_fs <- reactive({
        req(analyse_fs())

        fond_donnees <- analyse_fs()[[1]][input$mydonnees_fs_rows_selected,]
        fond_donnees <- st_transform(fond_donnees,crs=4326)
        return(fond_donnees)
      })

      fond_select_maille_fs <- reactive({
        req(fond_contour_maille_fs())

        fond_maille <- fond_contour_maille_fs()[[2]][as.data.frame(fond_contour_maille_fs()[[2]])[,"CODE"] %in% as.data.frame(fondMaille)[input$mymaille_fs_rows_selected,"CODE"],]
        return(fond_maille)
      })

      fond_select_contour_fs <- reactive({
        req(fond_contour_maille_fs())

        fond_contour <- fond_contour_maille_fs()[[1]][as.data.frame(fond_contour_maille_fs()[[1]])[,"CODE"] %in% as.data.frame(fondContour)[input$mycontour_fs_rows_selected,"CODE"],]
        return(fond_contour)
      })

      # Pour construire la map en leaflet
      react_fond_fs <- reactive({

        if(is.null(analyse_fs())) return(NULL)

        if(input$menu=="carte")
        {
          showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>\u00c9laboration de la carte...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

          # Construction de la map par defaut

          m <- leaflet(padding = 0,
                       options = leafletOptions(
                         preferCanvas = TRUE,
                         transition = 2
                       )) %>%

            setMapWidgetStyle(list(background = "#AFC9E0")) %>%

            addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">OCEANIS - \u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%

            fitBounds(lng1 = min(list_bbox_fs()[[1]]),
                      lat1 = min(list_bbox_fs()[[2]]),
                      lng2 = max(list_bbox_fs()[[1]]),
                      lat2 = max(list_bbox_fs()[[2]])
            ) %>%

            # On ajoute une barre d'echelle
            addScaleBar(position = 'bottomright',
                        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
            ) %>%

            # Pour gerer l'ordre des calques
            addMapPane(name = "fond_pays", zIndex = 401) %>%
            addMapPane(name = "fond_france", zIndex = 402) %>%
            addMapPane(name = "fond_habillage", zIndex = 403) %>%
            addMapPane(name = "fond_dep", zIndex = 404) %>%
            addMapPane(name = "fond_reg", zIndex = 405) %>%
            addMapPane(name = "fond_territoire", zIndex = 406) %>%
            addMapPane(name = "fond_trio3", zIndex = 407) %>%
            addMapPane(name = "fond_trio2", zIndex = 408) %>%
            addMapPane(name = "fond_trio1", zIndex = 409) %>%
            addMapPane(name = "selection", zIndex = 410) %>%

            addMapPane(name = "fond_legende", zIndex = 411)

          # AFFICHAGE DES FONDS D'HABILLAGE

          if(emprise %in% c("FRM","973"))
          {
            # fond des pays
            m <- addPolygons(map = m, data = fond_habillage_fs()[[2]][,"LIBGEO"], opacity = 1, # sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1
            )
          }
          # fond de la France metro, DOM ou etranger
          m <- addPolygons(map = m, data = fond_habillage_fs()[[1]][,"LIBGEO"], opacity = 1, # fond_france
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           options = pathOptions(pane = "fond_france", clickable = F),
                           fill = T, fillColor = "white", fillOpacity = 1
          )

          m_save_fs$a <- m

          # AFFICHAGE DU FOND TERRITOIRE

          if(!is.null(fondSuppl))
          {
            m <- addPolygons(map = m, data = fond_territoire_fs(),
                             stroke = TRUE, color = "#BFBFBF", opacity = 1,
                             weight = 0.5,
                             options = pathOptions(pane = "fond_territoire", clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_fs())[,"LIBELLE"], "</font> </b>"),
                             fill = T, fillColor = "white", fillOpacity = 0.001,
                             group = "territoire"
            )
          }

          # AFFICHAGE DES FONDS CONTOUR ET MAILLE

          # fond du contour
          m <- addPolygons(map = m, data = fond_contour_maille_fs()[[1]], opacity = 0.3,
                           stroke = TRUE, color = "black", weight = 3,
                           options = pathOptions(pane = "fond_trio3", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fs()[[1]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.3,
                           group = "maille_contour"
          )

          # fond de la maille
          m <- addPolygons(map = m, data = fond_contour_maille_fs()[[2]], opacity = 1,
                           stroke = TRUE, color = "grey", weight = 1,
                           options = pathOptions(pane = "fond_trio2", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fs()[[2]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.001,
                           group = "maille_contour"
          )

          # AFFICHAGE DE L'ANALYSE

          analyse <- isolate(analyse_fs())
          analyse_WGS84 <- analyse[[2]]

          if(direction!="Sol")
          {
            analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=100,]
            donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
            donnees <- sort(donnees[,varFlux],decreasing = TRUE)
          }else
          {
            donnees <- as.data.frame(analyse_WGS84)[,varFlux]
          }

          m <- addPolylines(map = m,
                            data = analyse_WGS84,
                            stroke = TRUE, color = "#303030",
                            opacity = 1,
                            weight = 1,
                            options = pathOptions(pane = "fond_trio1", clickable = T),
                            popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                            fill = T, fillColor = sapply(donnees, function(x) if(x>0){"#FFC300"}else{"#286AC7"}), fillOpacity = 1,
                            group = "fleche"
          )

          removeModal()

          showModal(modalDialog(HTML("<font size=+1>Veuillez patientez svp, la carte va s'afficher dans quelques secondes...<br><br><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> ensuite sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7

          return(m)
        }
      })

      # MODIFICATION DES FONDS TERRITOIRE D'ETUDES, REGION ET DEPARTEMENT

      observeEvent(input$ajout_territoire_fs_id,{

        proxy <- leafletProxy("mymap_fs")

        clearGroup(map = proxy, group = "territoire")

        # fond du territoire d'etudes
        if(!is.null(fondSuppl))
        {
          if(input$ajout_territoire_fs_id)
          {
            proxy <- addPolygons(map = proxy, data = fond_territoire_fs(),
                                 stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_territoire", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_fs())[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "territoire"
            )
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$ajout_reg_fs_id,{

        proxy <- leafletProxy("mymap_fs")

        clearGroup(map = proxy, group = "region")

        if(emprise=="FRM")
        {
          if(input$ajout_reg_fs_id)
          {
            #fond des regions
            proxy <- addPolygons(map = proxy, data = fond_region_fs(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 1.5,
                                 options = pathOptions(pane = "fond_reg", clickable = F),
                                 fill = F,
                                 group = "region"
            )
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$ajout_dep_fs_id,{

        proxy <- leafletProxy("mymap_fs")

        clearGroup(map = proxy, group = "departement")

        if(emprise=="FRM")
        {
          if(input$ajout_dep_fs_id)
          {
            # fond des departements
            proxy <- addPolygons(map = proxy, data = fond_departement_fs(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_dep", clickable = F),
                                 fill = F,
                                 group = "departement"
            )
          }
        }
      },ignoreInit = TRUE)

      # MODIFICATION DE L'ORDRE DES CALQUES

      observeEvent(list(input$monter_fond_fs_id,input$descendre_fond_fs_id),{

        if(as.numeric(input$monter_fond_fs_id)==0 & as.numeric(input$descendre_fond_fs_id)==0) return(NULL)

        proxy <- leafletProxy("mymap_fs")

        clearGroup(map = proxy, group = "maille_contour")
        clearGroup(map = proxy, group = "fleche")

        i <- 1 #pour gerer l'ordre des fonds dans le pane

        for(fond in liste_fonds$a)
        {
          if(fond=="analyse")
          {
            analyse_WGS84 <- analyse_fs()[[2]]

            if(direction!="Sol")
            {
              analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=flux_min_fs(),]
              donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
              donnees <- sort(donnees[,varFlux],decreasing = TRUE)
            }else
            {
              donnees <- as.data.frame(analyse_WGS84)[,varFlux]
            }

            proxy <- addPolylines(map = proxy,
                                  data = analyse_WGS84,
                                  stroke = TRUE, color = "#303030",
                                  opacity = 1,
                                  weight = 1,
                                  options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                  popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                                  fill = T, fillColor = sapply(donnees, function(x) if(x>0){"#FFC300"}else{"#286AC7"}), fillOpacity = 1,
                                  group = "fleche"
            )

            ordre_analyse$a <- i
          }

          if(fond=="maille")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_fs()[[2]], opacity = 1,
                                 stroke = TRUE, color = "grey", weight = 1,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fs()[[2]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "maille_contour"
            )
          }

          if(fond=="contour")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_fs()[[1]], opacity = 0.3,
                                 stroke = TRUE, color = "black", weight = 3,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fs()[[1]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.3,
                                 group = "maille_contour"
            )
          }

          i <- i + 1
        }
      },ignoreInit = TRUE)

      # MODIFICATION DES FLUX

      observeEvent(list(input$flux_min_fs_id,largeur_fs()),{

        proxy <- leafletProxy("mymap_fs")

        clearGroup(map = proxy, group = "fleche")

        analyse_WGS84 <- analyse_fs()[[2]]

        if(direction!="Sol")
        {
          if(is.na(input$flux_min_fs_id)) flux_min <- 1 else flux_min <- input$flux_min_fs_id

          analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=flux_min,]
          donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
          donnees <- sort(donnees[,varFlux],decreasing = TRUE)
        }else
        {
          donnees <- as.data.frame(analyse_WGS84)[,varFlux]
        }

        proxy <- addPolylines(map = proxy,
                              data = analyse_WGS84,
                              stroke = TRUE, color = "#303030",
                              opacity = 1,
                              weight = 1,
                              options = pathOptions(pane = paste0("fond_trio",ordre_analyse$a), clickable = T),
                              popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                              fill = T, fillColor = sapply(donnees, function(x) if(x>0){"#FFC300"}else{"#286AC7"}), fillOpacity = 1,
                              group = "fleche"
        )
      },ignoreInit = TRUE)

      # MODIFICATION DE LA SELECTION DES OBJETS VIA LES TABLEAUX

      observeEvent(input$onglets_fs,{
        req(input$onglets_fs)

        if(input$onglets_fs == "carte")
        {
          proxy <- leafletProxy("mymap_fs")

          clearGroup(map = proxy, group = "select_donnees")

          if(!is.null(input$mydonnees_fs_rows_selected))
          {
            suppressWarnings(proxy <- addPolylines(map = proxy,
                                                   data = fond_select_donnees_fs(),
                                                   stroke = TRUE, color = "#FFFF00",
                                                   opacity = 1,
                                                   weight = 2,
                                                   options = pathOptions(pane = "selection", clickable = F),
                                                   fill = F,
                                                   group = "select_donnees")
            )
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$onglets_fs,{
        req(input$onglets_fs)

        if(input$onglets_fs == "carte")
        {
          proxy <- leafletProxy("mymap_fs")

          clearGroup(map = proxy, group = "select_maille")

          if(!is.null(input$mymaille_fs_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_maille_fs(),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = T,
                                 fillColor = "#FFFF00",
                                 fillOpacity = 1,
                                 group = "select_maille"
            )
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$onglets_fs,{
        req(input$onglets_fs)

        if(input$onglets_fs == "carte")
        {
          proxy <- leafletProxy("mymap_fs")

          clearGroup(map = proxy, group = "select_contour")

          if(!is.null(input$mycontour_fs_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_contour_fs,
                                 stroke = FALSE,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = T,
                                 fillColor = "#FFFF00",
                                 fillOpacity = 1,
                                 group = "select_contour"
            )
          }
        }
      },ignoreInit = TRUE)

      # CONSTRUCTION DE LA LEGENDE

      lon_lat_fs <- reactive({
        click <- input$mymap_fs_click
        lon <- click$lng
        lat <- click$lat
        return(list(lon,lat))
      })

      observeEvent(list(input$mymap_fs_zoom,input$mymap_fs_click,input$titre_flux_legende_fs_id,input$flux_min_fs_id,input$largeur_fs_id),{
        if(is.null(input$affiche_legende_fs_id)) return(NULL)

        if(input$affiche_legende_fs_id==FALSE) return(NULL)

        if(is.null(lon_lat_fs()[[1]])) return(NULL)

        CONSTRUCTION_LEGENDE_FS()
      })

      CONSTRUCTION_LEGENDE_FS <- function(lon,lat)
      {
        proxy <- leafletProxy("mymap_fs")

        proxy <- clearGroup(map=proxy, group="leg")
        proxy <- clearMarkers(map=proxy)

        zoom <- as.numeric(input$mymap_fs_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude

        vmax <- max(abs(data.frame(analyse_fs()[[2]])[,varFlux]))

        coord_fleche_max <- st_coordinates(analyse_fs()[[2]][abs(data.frame(analyse_fs()[[2]])[,varFlux])==vmax,])
        large <- max(st_distance(st_sfc(st_point(c(coord_fleche_max[5,1],coord_fleche_max[5,2])),st_point(c(coord_fleche_max[6,1],coord_fleche_max[6,2])))))

        long <- coeff*1.5

        flux_legWGS84_s <- flux_legende_saphirs(lon_lat_fs()[[1]],lon_lat_fs()[[2]],long,large)
        flux_legWGS84 <- flux_legWGS84_s[[1]]
        pointe1 <- flux_legWGS84_s[[2]]
        pointe2 <- flux_legWGS84_s[[3]]

        # leaflet du cadre blanc en 1er
        proxy <- addRectangles(map = proxy,
                               lng1 = st_bbox(flux_legWGS84)[1]-coeff/2, lat1 = st_bbox(flux_legWGS84)[2]-coeff/2,
                               lng2 = st_bbox(flux_legWGS84)[3]+coeff*3, lat2 = st_bbox(flux_legWGS84)[4]+coeff*1.2,
                               stroke = FALSE,
                               options = pathOptions(pane = "fond_legende", clickable = F),
                               fill = T,
                               fillColor = "white",
                               fillOpacity = 0.8,
                               group="leg"
        )

        suppressWarnings(proxy <- addPolygons(map = proxy,
                                              data=flux_legWGS84,
                                              stroke = TRUE,
                                              opacity = 1,
                                              color = "#2B3E50",
                                              weight = 2,
                                              options = pathOptions(pane = "fond_legende", clickable = F),
                                              fill = T,
                                              fillColor = "white",
                                              fillOpacity = 1,
                                              group="leg"
        ))

        # leaflet valeur flux
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = pointe1[1], lat = pointe1[2], #grande fleche
                                     label = as.character(format(vmax,big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )

        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = pointe2[1], lat = pointe2[2], #petite fleche
                                     label = as.character(format(round(vmax/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )

        #leaflet titre 1
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = st_bbox(flux_legWGS84)[1]-coeff/3, lat = st_bbox(flux_legWGS84)[4]+coeff/2,
                                     label = input$titre_flux_legende_fs_id,
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "14px"
                                                                 )),
                                     group="leg"
        )
      }

      # AJOUT DES ONGLETS SAUVEGARDE

      observeEvent(input$save_carte_fs_id,{

        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Sauvegarde de la carte en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        insert_save$a <- insert_save$a + 1
        nb_save_carte <- insert_save$a-remove_carte$a

        m_save <- m_save_fs$a

        if(nb_save_carte>6)
        {
          insert_save$a <- insert_save$a - 1
          showModal(modalDialog(HTML("<font size=+1>Vous ne pouvez pas sauvegarger plus de 6 cartes. Veuillez en supprimer avant de continuer.</font> "), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }

        output[[paste0("mymap_save_",insert_save$a,"_fs")]] <- renderLeaflet({

          if(!is.null(fondSuppl))
          {
            if(input$ajout_territoire_fs_id)
            {
              m_save <- addPolygons(map = m_save, data = fond_territoire_fs(),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_fs())[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }

          if(input$ajout_reg_fs_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_region_fs(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }

          if(input$ajout_dep_fs_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_departement_fs(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 0.5,
                                  options = pathOptions(pane = "fond_dep", clickable = F),
                                  fill = F
            )
          }

          i <- 1 #pour gerer l'ordre des fonds dans le pane

          for(fond in liste_fonds$a)
          {
            if(fond=="analyse")
            {
              analyse_WGS84 <- analyse_fs()[[2]]

              if(direction!="Sol")
              {
                analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=flux_min_fs(),]
                donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
                donnees <- sort(donnees[,varFlux],decreasing = TRUE)
              }else
              {
                donnees <- as.data.frame(analyse_WGS84)[,varFlux]
              }

              m_save <- addPolylines(map = m_save,
                                     data = analyse_WGS84,
                                     stroke = TRUE, color = "#303030",
                                     opacity = 1,
                                     weight = 1,
                                     options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                     popup = paste0("<b><font color=#2B3E50>",varFlux," : ",donnees,"</font></b>"),
                                     fill = T, fillColor = sapply(donnees, function(x) if(x>0){"#FFC300"}else{"#286AC7"}), fillOpacity = 1
              )
            }

            if(fond=="maille")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_fs()[[2]], opacity = 1,
                                    stroke = TRUE, color = "grey", weight = 1,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fs()[[2]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }

            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_fs()[[1]], opacity = 0.3,
                                    stroke = TRUE, color = "black", weight = 3,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_fs()[[1]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.3
              )
            }

            i <- i + 1
          }

          zoom <- as.numeric(input$mymap_fs_zoom)
          coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude

          vmax <- max(abs(data.frame(analyse_fs()[[2]])[,varFlux]))

          coord_fleche_max <- st_coordinates(analyse_fs()[[2]][abs(data.frame(analyse_fs()[[2]])[,varFlux])==vmax,])
          large <- max(st_distance(st_sfc(st_point(c(coord_fleche_max[5,1],coord_fleche_max[5,2])),st_point(c(coord_fleche_max[6,1],coord_fleche_max[6,2])))))

          long <- coeff*1.5

          flux_legWGS84_s <- flux_legende_saphirs(lon_lat_fs()[[1]],lon_lat_fs()[[2]],long,large)
          flux_legWGS84 <- flux_legWGS84_s[[1]]
          pointe1 <- flux_legWGS84_s[[2]]
          pointe2 <- flux_legWGS84_s[[3]]

          # leaflet du cadre blanc en 1er
          m_save <- addRectangles(map = m_save,
                                  lng1 = st_bbox(flux_legWGS84)[1]-coeff/2, lat1 = st_bbox(flux_legWGS84)[2]-coeff/2,
                                  lng2 = st_bbox(flux_legWGS84)[3]+coeff*3, lat2 = st_bbox(flux_legWGS84)[4]+coeff*1.2,
                                  stroke = FALSE,
                                  options = pathOptions(pane = "fond_legende", clickable = F),
                                  fill = T,
                                  fillColor = "white",
                                  fillOpacity = 0.8
          )

          suppressWarnings(m_save <- addPolygons(map = m_save,
                                                 data=flux_legWGS84,
                                                 stroke = TRUE,
                                                 opacity = 1,
                                                 color = "#2B3E50",
                                                 weight = 2,
                                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                                 fill = T,
                                                 fillColor = "white",
                                                 fillOpacity = 1
          ))

          # leaflet valeur flux
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = pointe1[1], lat = pointe1[2], #grande fleche
                                        label = as.character(format(vmax,big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )

          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = pointe2[1], lat = pointe2[2], #petite fleche
                                        label = as.character(format(round(vmax/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )

          #leaflet titre 1
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = st_bbox(flux_legWGS84)[1]-coeff/3, lat = st_bbox(flux_legWGS84)[4]+coeff/2,
                                        label = input$titre_flux_legende_fs_id,
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "14px"
                                                                    ))
          )

          removeModal()

          m_save
        })

        output[[paste0("remove_carte_",nb_save_carte,"_fs")]] <- renderUI({
          actionButton(paste0("remove_carte_",nb_save_carte,"_fs_id"),label="X Supprimer la carte", style="color:#FFFFFF; border-color:#FFFFFF; background-color:#2B3E50")
        })
        appendTab(inputId = "onglets_fs",
                  tabPanel(title=HTML(paste0("<font color=#AFC9E0> Carte ",insert_save$a,"</font>")),value=paste0("carte",nb_save_carte),uiOutput(paste0("remove_carte_",nb_save_carte,"_fs")),leafletOutput(paste0("mymap_save_",insert_save$a,"_fs"),width="100%",height = 800)),
                  select = TRUE,
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_1_fs_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fs",
                  target = "carte1",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_2_fs_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fs",
                  target = "carte2",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_3_fs_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fs",
                  target = "carte3",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_4_fs_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fs",
                  target = "carte4",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_5_fs_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fs",
                  target = "carte5",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_6_fs_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_fs",
                  target = "carte6",
                  session = session
        )
      }, ignoreInit = TRUE)

      # TABLEAUX DE DONNEES, MAILLE ET CONTOUR

      output$mydonnees_fs <- DT::renderDataTable(DT::datatable({
        if(direction!="Sol")
        {
          analyse_WGS84 <- analyse_fs()[[2]][as.data.frame(analyse_fs()[[2]])[,varFlux]>=flux_min_fs(),]
          data <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
        }else
        {
          data <- as.data.frame(analyse_fs()[[2]])
        }
        tableau_donnees <- data[rev(order(data[,varFlux])),c("CODE1","CODE2",varFlux)]
      },  style = 'bootstrap'
      ))

      output$mymaille_fs <- DT::renderDataTable(DT::datatable({
        data <- as.data.frame(fondMaille)
        tableau_maille <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))

      output$mycontour_fs <- DT::renderDataTable(DT::datatable({
        data <- as.data.frame(fondContour)
        tableau_contour <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))

      # ENVOI DU LEAFLET A L'UI

      output$mymap_fs <- renderLeaflet({
        react_fond_fs()
      })
    }

    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
    }
