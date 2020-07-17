shiny_typo <-
function(data,fondMaille,fondContour,fondSuppl=NULL,idData,varTypo,emprise="FRM",fondEtranger=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16 <- NULL

    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error3 <- "Le fond de contour doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error4 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varTypo)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(emprise)!="character")) msg_error7 <- "La valeur doit etre de type caractere ('FRM', '971', '972', '973', '974', '976' ou '999') / "

    if(length(names(data))<2) msg_error8 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error9 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error10 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error11 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

    if(!any(names(data) %in% idData))  msg_error12 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varTypo))  msg_error13 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!emprise %in% c("FRM","971","972","973","974","976","999")) msg_error14 <- "La variable emprise doit etre 'FRM', '971', '972', '973', '974', '976' ou '999' / "
    if(!is.null(fondEtranger)) if(any(!any(class(fondEtranger) %in% "sf"),!any(class(fondEtranger) %in% "data.frame"))) msg_error15 <- "Le fond etranger doit etre un objet sf / "
    if(!is.null(fondEtranger)) if(length(names(fondEtranger))<3) msg_error16 <- "Le fond etranger n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),!is.null(msg_error13),
           !is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16)))
    }

    nb_up <- reactiveValues(a=0)
    nb_down <- reactiveValues(a=0)
    ordre_analyse <- reactiveValues(a=1,b=2)
    insert_save <- reactiveValues(a=0)
    remove_carte <- reactiveValues(a=0)
    liste_fonds_ty <- reactiveValues(a=c("analyse/maille","contour"))
    m_save_ty <- reactiveValues(a=0)

    erreur_maille <- reactiveValues(a=FALSE)

    legende <- reactiveValues(a=NULL)

    sourc <- "Source : Insee"

    names(data)[names(data)==idData] <- "CODE"
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
                                             uiOutput("variable_typo_ty"),
                                             tags$hr(style="border: 5px solid #5182B6"), #337ab7
                                             h4(HTML("<b><font color=#95BAE2>FONDS</font></b>")),
                                             fluidRow(
                                               column(width=9, offset=0.5,
                                                      uiOutput("ordre_fonds_ty")
                                               ),
                                               column(width=1,
                                                      br(),
                                                      br(),
                                                      htmlOutput("monter_fond_ty", inline=FALSE),
                                                      htmlOutput("descendre_fond_ty", inline=FALSE)
                                               )
                                             ),
                                             uiOutput("ajout_territoire_ty"),
                                             uiOutput("ajout_reg_ty"),
                                             uiOutput("ajout_dep_ty"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>TYPOLOGIE</font></b>")),
                                             uiOutput("zone_modif_libelle_ty"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>L\u00c9GENDE</font></b>")),
                                             uiOutput("titre_typo_legende_ty"),
                                             br(),
                                             uiOutput("affiche_legende_ty"),
                                             br(),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>SAUVEGARDE</font></b>")),
                                             uiOutput("save_carte_ty"),
                                             br(),
                                             conditionalPanel(condition = 'input.mymap_ty_click',
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
                                                                                 uiOutput("sortie_qgis_ty"),
                                                                                 br(),
                                                                                 uiOutput("titre1_qgis_ty"),
                                                                                 uiOutput("titre2_qgis_ty"),
                                                                                 uiOutput("source_qgis_ty"),
                                                                                 tags$head(tags$style(HTML('#export_qgis_ty_id{background-color:#337ab7}'))),
                                                                                 uiOutput("export_qgis_ty")
                                                                               )
                                                                       )
                                                                       )

                                             ),
                                             br(),
                                             uiOutput("aide_image_ty"),
                                             br()
                              ),
                              mainPanel(
                                tags$head(
                                  tags$style(HTML(".leaflet-container { background: #AFC9E0; }"))
                                ),
                                tabsetPanel(id="onglets_ty",
                                            tabPanel(title=HTML("<b>Carte</b>"),value="carte",
                                                     leafletOutput("mymap_ty",width="100%",height = 800)),
                                            tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es/Maille</b>")),value="donnees",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mydonnees_ty",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mycontour_ty",width="100%",height = 800))
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

        output$variable_typo_ty <- renderUI({
          selectInput("variable_typo_ty_id", label=h5("Variable typologique"), choices = varTypo, selected = varTypo)
        })

        # FONDS

        output$ordre_fonds_ty <- renderUI({
          selectInput("ordre_fonds_ty_id", label=h5("Modifier l'ordre des fonds"), choices = liste_fonds_ty$a, multiple=TRUE, selectize=FALSE, selected = "analyse")
        })
        output$monter_fond_ty <- renderUI({
          actionButton("monter_fond_ty_id", label="", icon=icon("arrow-up"))
        })
        output$descendre_fond_ty <- renderUI({
          actionButton("descendre_fond_ty_id", label="", icon=icon("arrow-down"))
        })

        output$ajout_territoire_ty <- renderUI({
          checkboxInput("ajout_territoire_ty_id", label = "Afficher le fond des territoires",
                        value = if(is.null(fondSuppl)) FALSE else TRUE)
        })
        output$ajout_reg_ty <- renderUI({
          checkboxInput("ajout_reg_ty_id", label = "Afficher le fond des r\u00e9gions",
                        value = FALSE)
        })
        output$ajout_dep_ty <- renderUI({
          checkboxInput("ajout_dep_ty_id", label = "Afficher le fond des d\u00e9partements",
                        value = FALSE)
        })

        # TYPOLOGIE

        observeEvent(list(input$affiche_carte_charge_ty_id,input$affiche_carte_liste_ty_id),{
          output$zone_modif_libelle_ty <- renderUI({
            lapply(1:(as.numeric(length(unique(analyse_ty()[[1]]$classe)))), function(i) {
              textInput(inputId = paste0("lib_typo_", i,"_ty_id"), label = paste("Libelle de la modalite", i),
                        value = sort(unique(analyse_ty()[[1]]$valeur))[i])
            })
          })
        })

        # LEGENDE

        output$titre_typo_legende_ty <- renderUI({
          textInput("titre_typo_legende_ty_id", label = h5("Titre de la l\u00e9gende de la typologie"), value = "")
        })

        output$affiche_legende_ty <- renderUI({
          checkboxInput("affiche_legende_ty_id", label = "Activer le d\u00e9placement de la l\u00e9gende au clic",
                        value = TRUE)
        })

        # SAUVEGARDE

        output$save_carte_ty <- renderUI({
          actionButton("save_carte_ty_id", label=HTML("<font size=3>Sauvegarder la carte dans un onglet</font>"), style="color:#FFFFFF; background-color:#DF691A")
        })

        output$entrees_qgis_ty <- renderUI({
          actionButton("entrees_qgis_ty_id", label="Exporter en projet Qgis")
        })

        output$sortie_qgis_ty <- renderUI({
          tags$div(class="input-group",
                   HTML('<input type="text" id="sortie_qgis_ty_id" class="form-control" placeholder="Nom du projet" aria-describedby="sortie_qgis_ty_id">
                        <span class="input-group-addon" id="sortie_qgis_ty_id">.qgs</span>'))
        })

        output$titre1_qgis_ty <- renderUI({
          textInput("titre1_qgis_ty_id", label = h5("Titre informatif"), value = "", placeholder= "Facultatif")
        })

        output$titre2_qgis_ty <- renderUI({
          textInput("titre2_qgis_ty_id", label = h5("Titre descriptif"), value = "", placeholder= "Facultatif")
        })

        output$source_qgis_ty <- renderUI({
          textInput("source_qgis_ty_id", label = h5("Source de la carte"), value = sourc)
        })

        output$aide_image_ty <- renderUI({
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
      observeEvent(list(input$monter_fond_ty_id,input$descendre_fond_ty_id),{

        if(as.numeric(input$monter_fond_ty_id)==0 & as.numeric(input$descendre_fond_ty_id)==0) return(NULL)
        ordre <- c()
        if(as.numeric(input$monter_fond_ty_id)>nb_up$a)
        {
          ordre <- 2
          nb_up$a <- nb_up$a+1
        }
        if(as.numeric(input$descendre_fond_ty_id)>nb_down$a)
        {
          ordre <- 1
          nb_down$a <- nb_down$a+1
        }

        if(is.null(input$ordre_fonds_ty_id)) pos_select <- 0 else pos_select <- which(liste_fonds_ty$a==input$ordre_fonds_ty_id)

        if(pos_select>0)
        {
          if(pos_select==ordre) liste_fonds_ty$a <- liste_fonds_ty$a[c(2,1)]

          updateSelectInput(session, "ordre_fonds_ty_id",
                            choices = liste_fonds_ty$a,
                            selected = input$ordre_fonds_ty_id
          )
        }
      },ignoreInit = TRUE)

      # Pour exporter la carte en projet Qgis

      output$export_qgis_ty <- renderUI({
        downloadButton("downloadProjetQgis_ty", label="Exporter")
      })

      output$downloadProjetQgis_ty <- downloadHandler(contentType = "zip",
                                                      filename = function(){
                                                        paste0(input$sortie_qgis_ty_id,".zip")
                                                      },
                                                      content = function(file){
                                                        files <- EXPORT_PROJET_QGIS_TY(file)

                                                        zip(file,files, flags = "-j9X")
                                                      }
      )

      # Pour exporter la carte en projet Qgis
      EXPORT_PROJET_QGIS_TY <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        sortie <- input$sortie_qgis_ty_id
        rep_sortie <- dirname(file)
        files <- c(paste0(rep_sortie,"/",sortie,".qgs"))

        nb_typo <- length(unique(analyse_ty()[[1]]$classe))

        if(!is.null(lon_lat_ty()[[1]]))
        {
          suppressWarnings(test_affiche_leg <- try(table_typo <- data.frame(classe=c(nb_typo:1),label=unique(as.data.frame(analyse_ty()[[1]])[,varTypo]),couleurs=unique(palette_ty()$col), stringsAsFactors = F),silent=TRUE))
          if(class(test_affiche_leg) %in% "try-error")
          {
            showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
            return(NULL)
          }else
          {
            table_typo <- data.frame(classe=c(nb_typo:1),label=unique(as.data.frame(analyse_ty()[[1]])[,varTypo]),couleurs=unique(palette_ty()$col), stringsAsFactors = F)
          }
        }else
        {
          showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }

        fond_typo <- st_sf(analyse_ty()[[1]])

        fond_contour <- st_transform(fondContour,crs = as.numeric(code_epsg_ty()))
        if(!is.null(fondSuppl) && input$ajout_territoire_ty_id) fond_territoire <- st_transform(fond_territoire_ty(),crs = as.numeric(code_epsg_ty()))
        if(input$ajout_dep_ty_id) fond_departement <- st_transform(fond_departement_ty(),crs = as.numeric(code_epsg_ty()))
        if(input$ajout_reg_ty_id) fond_region <- st_transform(fond_region_ty(),crs = as.numeric(code_epsg_ty()))
        fond_france <- st_transform(fond_habillage_ty()[[1]],crs = as.numeric(code_epsg_ty()))
        fond_pays <- st_transform(fond_habillage_ty()[[2]],crs = as.numeric(code_epsg_ty()))

        st_write(fond_typo, paste0(rep_sortie,"/fond_maille_typo_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_contour,paste0(rep_sortie,"/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) st_write(fond_territoire, paste0(rep_sortie,"/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_departement")) if(!is.null(fond_departement)) st_write(fond_departement, paste0(rep_sortie,"/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_region")) if(!is.null(fond_region)) st_write(fond_region,paste0(rep_sortie,"/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_france,paste0(rep_sortie,"/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_pays")) if(!is.null(fond_pays)) st_write(fond_pays,paste0(rep_sortie,"/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE)

        files <- c(paste0(rep_sortie,"/fond_maille_typo_carte.shp"),paste0(rep_sortie,"/fond_maille_typo_carte.dbf"),paste0(rep_sortie,"/fond_maille_typo_carte.prj"),paste0(rep_sortie,"/fond_maille_typo_carte.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_contour.shp"),paste0(rep_sortie,"/fond_contour.dbf"),paste0(rep_sortie,"/fond_contour.prj"),paste0(rep_sortie,"/fond_contour.shx"),files)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) files <- c(paste0(rep_sortie,"/fond_territoire.shp"),paste0(rep_sortie,"/fond_territoire.dbf"),paste0(rep_sortie,"/fond_territoire.prj"),paste0(rep_sortie,"/fond_territoire.shx"),files)
        if(exists("fond_departement")) if(!is.null(fond_departement)) files <- c(paste0(rep_sortie,"/fond_departement.shp"),paste0(rep_sortie,"/fond_departement.dbf"),paste0(rep_sortie,"/fond_departement.prj"),paste0(rep_sortie,"/fond_departement.shx"),files)
        if(exists("fond_region")) if(!is.null(fond_region)) files <- c(paste0(rep_sortie,"/fond_region.shp"),paste0(rep_sortie,"/fond_region.dbf"),paste0(rep_sortie,"/fond_region.prj"),paste0(rep_sortie,"/fond_region.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_france.shp"),paste0(rep_sortie,"/fond_france.dbf"),paste0(rep_sortie,"/fond_france.prj"),paste0(rep_sortie,"/fond_france.shx"),files)
        if(exists("fond_pays")) if(!is.null(fond_pays)) files <- c(paste0(rep_sortie,"/fond_pays.shp"),paste0(rep_sortie,"/fond_pays.dbf"),paste0(rep_sortie,"/fond_pays.prj"),paste0(rep_sortie,"/fond_pays.shx"),files)

        chemin_fonds <- rep_sortie
        titre1 <- paste0(input$titre1_qgis_ty_id,"\n")
        titre2 <- input$titre2_qgis_ty_id
        source <- input$source_qgis_ty_id
        annee <- format(Sys.time(), format = "%Y")
        variable_a_representer <- varTypo
        titre_leg_typo <- input$titre_typo_legende_ty_id

        l <- c()
        if(exists("fond_territoire")) l <- "fond_territoire"
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")

        l=c("fond_france",
            "fond_contour",
            l,
            "fond_maille_typo_carte"
        )

        if(exists("fond_pays")) l <- c(l,"fond_pays")

        export_projet_qgis_typo(l,rep_sortie,sortie,titre1,titre2,source,titre_leg_typo,table_typo,variable_a_representer,annee)

        removeModal()

        showModal(modalDialog(HTML(paste0("<font size=+1>Le projet Qgis a \u00e9t\u00e9 cr","\u00e9","ee.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        return(files)
      }

      code_epsg_ty <- reactive({
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

      analyse_ty <- reactive({

        analyse<-k_typo(fondMaille,names(fondMaille)[!sapply(fondMaille[-length(names(fondMaille))],is.numeric)][1],data,"CODE",varTypo)

        if(is.null(analyse))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }

        analyse <- analyse[[1]]
        analyse[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(as.data.frame(analyse)[,varTypo], big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        analyse_WGS84 <- st_transform(analyse,crs=4326)
        return(list(analyse,analyse_WGS84))
      })

      fond_habillage_ty <- reactive({

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

      fond_contour_maille_ty <- reactive({

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

      list_bbox_ty <- reactive({
        req(fond_contour_maille_ty())

        list_bbox <- list(c(st_bbox(fond_contour_maille_ty()[[1]])[1],st_bbox(fond_contour_maille_ty()[[1]])[3]),c(st_bbox(fond_contour_maille_ty()[[1]])[2],st_bbox(fond_contour_maille_ty()[[1]])[4]))
        return(list_bbox)
      })

      palette_ty <- reactive({
        nb_col <- length(unique(as.data.frame(analyse_ty()[[1]])[,"classe"]))
        pal_typo <- substr(rainbow(256)[nb_opposes(256)[1:nb_col]],1,7)
        pal_typo <- data.frame(cbind(pal_typo,unique(as.data.frame(analyse_ty()[[1]])[,"classe"])))
        names(pal_typo) <- c("col","classe")
        analyse <- merge(as.data.frame(analyse_ty()[[1]]),pal_typo,by="classe")
        analyse <- analyse[order(as.data.frame(analyse)[,varTypo]),]
        return(analyse)
      })

      palette_init_ty <- reactive({
        nb_col <- length(unique(as.data.frame(analyse_ty()[[1]])[,"classe"]))
        pal_typo <- substr(rainbow(256)[nb_opposes(256)[1:nb_col]],1,7)
        pal_typo <- data.frame(cbind(pal_typo,unique(as.data.frame(analyse_ty()[[1]])[,"classe"])))
        names(pal_typo) <- c("col","classe")
        analyse <- merge(as.data.frame(analyse_ty()[[1]]),pal_typo,by="classe")
        analyse <- analyse[order(as.data.frame(analyse)[,varTypo]),]
        return(analyse)
      })

      fond_territoire_ty <- reactive({
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,crs=4326)
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })

      fond_region_ty <- reactive({
        fond_region <- st_transform(sf_regm(),crs=4326)
        return(fond_region)
      })

      fond_departement_ty <- reactive({
        fond_departement <- st_transform(sf_depm(),crs=4326)
        return(fond_departement)
      })

      fond_select_donnees_ty <- reactive({
        req(analyse_ty())

        fond_donnees <- analyse_ty()[[1]][input$mydonnees_ty_rows_selected,]
        fond_donnees <- st_transform(fond_donnees,crs=4326)
        return(fond_donnees)
      })

      fond_select_contour_ty <- reactive({
        req(fond_contour_maille_ty())

        fond_contour <- fond_contour_maille_ty()[[1]][as.data.frame(fond_contour_maille_ty()[[1]])[,"CODE"] %in% as.data.frame(fondContour)[input$mycontour_ty_rows_selected,"CODE"],]
        return(fond_contour)
      })

      # CONSTRUCTION DE LA MAP EN LEAFLET

      react_fond_ty <- reactive({

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

            fitBounds(lng1 = min(list_bbox_ty()[[1]]),
                      lat1 = min(list_bbox_ty()[[2]]),
                      lng2 = max(list_bbox_ty()[[1]]),
                      lat2 = max(list_bbox_ty()[[2]])
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
            addMapPane(name = "fond_duo2", zIndex = 407) %>%
            addMapPane(name = "fond_duo1", zIndex = 408) %>%
            addMapPane(name = "selection", zIndex = 409) %>%

            addMapPane(name = "fond_legende", zIndex = 410)

          # AFFICHAGE DES FONDS D'HABILLAGE

          if(emprise %in% c("FRM","973"))
          {
            # fond des pays
            m <- addPolygons(map = m, data = fond_habillage_ty()[[2]][,"LIBGEO"], opacity = 1, # sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1
            )
          }

          # fond de la France metro, DOM ou etranger
          m <- addPolygons(map = m, data = fond_habillage_ty()[[1]][,"LIBGEO"], opacity = 1,
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           options = pathOptions(pane = "fond_france", clickable = F),
                           fill = T, fillColor = "white", fillOpacity = 1
          )

          m_save_ty$a <- m

          # AFFICHAGE DU FOND TERRITOIRE

          if(!is.null(fondSuppl))
          {
            m <- addPolygons(map = m, data = fond_territoire_ty(),
                             stroke = TRUE, color = "#BFBFBF", opacity = 1,
                             weight = 0.5,
                             options = pathOptions(pane = "fond_territoire", clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ty())[,"LIBELLE"], "</font> </b>"),
                             fill = T, fillColor = "white", fillOpacity = 0.001,
                             group = "territoire"
            )
          }
          # AFFICHAGE DU FOND CONTOUR
          m <- addPolygons(map = m, data = fond_contour_maille_ty()[[1]], opacity = 0.3,
                           stroke = TRUE, color = "black", weight = 3,
                           options = pathOptions(pane = "fond_duo2", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ty()[[1]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.3,
                           group = "duo"
          )

          # AFFICHAGE DE LA MAILLE ET DE L'ANALYSE

          m <- addPolygons(map = m, data = analyse_ty()[[2]], opacity = 1,
                           stroke = TRUE, color = "white", weight = 1,
                           options = pathOptions(pane = "fond_duo1", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_ty()[[2]])[,"LIBELLE"], "</font> </b><br><b><font color=#2B3E50>",varTypo," : </font></b>",analyse_ty()[[2]]$TXT1,"<br>"),
                           fill = T,
                           fillColor = palette_init_ty()$col,
                           fillOpacity = 1,
                           group = "duo"
          )

          removeModal()

          showModal(modalDialog(HTML("<font size=+1>Veuillez patientez svp, la carte va s'afficher dans quelques secondes...<br><br><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> ensuite sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7

          return(m)
        }
      })

      # MODIFICATION DES FONDS TERRITOIRE D'ETUDES, REGION ET DEPARTEMENT

      observeEvent(input$ajout_territoire_ty_id,{

        proxy <- leafletProxy("mymap_ty")

        clearGroup(map = proxy, group = "territoire")

        #fond du territoire d'etudes
        if(!is.null(fondSuppl))
        {
          if(input$ajout_territoire_ty_id)
          {
            proxy <- addPolygons(map = proxy, data = fond_territoire_ty(),
                                 stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_territoire", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ty())[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "territoire"
            )
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$ajout_reg_ty_id,{

        proxy <- leafletProxy("mymap_ty")

        clearGroup(map = proxy, group = "region")

        if(emprise=="FRM")
        {
          if(input$ajout_reg_ty_id)
          {
            #fond des regions
            proxy <- addPolygons(map = proxy, data = fond_region_ty(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 1.5,
                                 options = pathOptions(pane = "fond_reg", clickable = F),
                                 fill = F,
                                 group = "region"
            )
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$ajout_dep_ty_id,{

        proxy <- leafletProxy("mymap_ty")

        clearGroup(map = proxy, group = "departement")

        if(emprise=="FRM")
        {
          if(input$ajout_dep_ty_id)
          {
            #fond des departements
            proxy <- addPolygons(map = proxy, data = fond_departement_ty(),
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

      observeEvent(list(input$monter_fond_ty_id,input$descendre_fond_ty_id),{

        if(as.numeric(input$monter_fond_ty_id)==0 & as.numeric(input$descendre_fond_ty_id)==0) return(NULL)

        proxy <- leafletProxy("mymap_ty")

        clearGroup(map = proxy, group = "contour")
        clearGroup(map = proxy, group = "classe")

        i <- 1 #pour gerer l'ordre des fonds dans le pane

        for(fond in liste_fonds_ty$a)
        {
          if(fond=="analyse/maille")
          {
            proxy <- addPolygons(map = proxy, data = analyse_ty()[[2]], opacity = 1,
                                 stroke = TRUE, color = "white", weight = 1,
                                 options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_ty()[[2]])[,"LIBELLE"], "</font> </b><br><b><font color=#2B3E50>",varTypo," : </font></b>",analyse_ty()[[2]]$TXT1,"<br>"),
                                 fill = T,
                                 fillColor = palette_ty()$col,
                                 fillOpacity = 1,
                                 group = "classe"
            )

            ordre_analyse$a <- i
          }

          if(fond=="contour")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_ty()[[1]], opacity = 0.3,
                                 stroke = TRUE, color = "black", weight = 3,
                                 options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ty()[[1]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.3,
                                 group = "contour"
            )
          }

          i <- i + 1
        }
      },ignoreInit = TRUE)

      # MODIFICATION DE LA SELECTION DES OBJETS VIA LES TABLEAUX

      observeEvent(input$onglets_ty,{

        if(input$onglets_ty == "carte")
        {
          proxy <- leafletProxy("mymap_ty")

          clearGroup(map = proxy, group = "select_donnees")

          if(!is.null(input$mydonnees_ty_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_donnees_ty(),
                                 stroke = TRUE, weight = 3,
                                 color="black", opacity = 1,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = F,
                                 group = "select_donnees"
            )
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$onglets_ty,{

        if(input$onglets_ty == "carte")
        {
          proxy <- leafletProxy("mymap_ty")

          clearGroup(map = proxy, group = "select_contour")

          if(!is.null(input$mycontour_ty_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_contour_ty(),
                                 stroke = TRUE, weight = 3,
                                 color="#FFFF00",
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = F,
                                 group = "select_contour"
            )
          }
        }
      },ignoreInit = TRUE)

      # CONSTRUCTION DE LA LEGENDE

      lon_lat_ty <- reactive({
        click <- input$mymap_ty_click
        lon <- click$lng
        lat <- click$lat
        return(list(lon,lat))
      })

      observeEvent(list(input$mymap_ty_zoom,input$mymap_ty_click,input$titre_typo_legende_ty_id),{
        if(is.null(input$affiche_legende_ty_id)) return(NULL)

        if(input$affiche_legende_ty_id==FALSE) return(NULL)

        if(is.null(lon_lat_ty()[[1]])) return(NULL)

        proxy <- leafletProxy("mymap_ty")
        proxy <- clearGroup(map=proxy, group="leg")
        proxy <- clearMarkers(map=proxy)

        zoom <- as.numeric(input$mymap_ty_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude

        position_leg <- t(data_frame(c(lon_lat_ty()[[1]],lon_lat_ty()[[2]])))

        # On cree les rectangles
        nb_typo <- length(unique(analyse_ty()[[1]]$classe))

        for(i in 1:nb_typo)
        {
          # Coordonnees du point haut/gauche des rectangles de la legende
          x_coord_rectangle <- position_leg[1]
          if(i==1) #1er rectangle
          {
            y_coord_rectangle <- position_leg[2]-coeff
          }else
          {
            y_coord_rectangle <- y_coord_rectangle-coeff*0.7
          }
          assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
        }

        # On ajoute un cadre blanc autour de la legende
        y_coord_rectangle <- min(get(paste0("rectangle_",nb_typo))[[1]][,2])

        # leaflet du cadre blanc en 1er
        proxy <- addRectangles(map = proxy,
                               lng1 = position_leg[1]-coeff*0.5, lat1 = position_leg[2]+coeff*0.5, #x_titre_1 et y_titre_2
                               lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                               stroke = TRUE,
                               color = paste0("#2B3E50", ";background: #ffffff;
                                              border-left:2px solid #2B3E50;
                                              border-right:2px solid #2B3E50;
                                              border-top:2px solid #2B3E50;
                                              border-bottom:2px solid #2B3E50;
                                              border-radius: 5%"),
                               weight = 1,
                               options = pathOptions(pane = "fond_legende", clickable = F),
                               fill = T,
                               fillColor = "white",
                               fillOpacity = 0.8,
                               group="leg"
                               )

        palette <- unique(palette_ty()[order(palette_ty()$valeur),"col"])

        for(i in 1: nb_typo)
        {
          if(is.null(input$lib_typo_1_ty_id))
          {
            typo_leg_texte <- sort(unique(analyse_ty()[[1]]$valeur))[i]
          }else
          {
            typo_leg_texte <- input[[paste0("lib_typo_", i,"_ty_id")]]
          }

          proxy <- addPolygons(map = proxy, data = st_polygon(get(paste0("rectangle_",i))),
                               stroke = FALSE,
                               options = pathOptions(pane = "fond_legende", clickable = F),
                               fill = T,
                               fillColor = palette[i],
                               fillOpacity = 1,
                               group="leg"
          )

          proxy <- addLabelOnlyMarkers(map = proxy,
                                       lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                       label = typo_leg_texte,
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "12px"
                                                                   )),
                                       group="leg"
          )
        }

        # leaflet titre
        x_titre_2 <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
        y_titre_2 <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.2

        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = x_titre_2, lat = y_titre_2,
                                     label = input$titre_typo_legende_ty_id,
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "14px"
                                                                 )),
                                     group="leg"
        )
      })

      # AJOUT DES ONGLETS SAUVEGARDE

      observeEvent(input$save_carte_ty_id,{

        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Sauvegarde de la carte en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        insert_save$a <- insert_save$a + 1
        nb_save_carte <- insert_save$a-remove_carte$a

        m_save <- m_save_ty$a

        if(nb_save_carte>6)
        {
          insert_save$a <- insert_save$a - 1
          showModal(modalDialog(HTML("<font size=+1>Vous ne pouvez pas sauvegarger plus de 6 cartes. Veuillez en supprimer avant de continuer.</font> "), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }

        output[[paste0("mymap_save_",insert_save$a,"_ty")]] <- renderLeaflet({

          if(!is.null(fondSuppl))
          {
            if(input$ajout_territoire_ty_id)
            {
              m_save <- addPolygons(map = m_save, data = fond_territoire_ty(),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ty())[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }

          if(input$ajout_reg_ty_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_region_ty(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }

          if(input$ajout_dep_ty_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_departement_ty(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 0.5,
                                  options = pathOptions(pane = "fond_dep", clickable = F),
                                  fill = F
            )
          }

          i <- 1 #pour gerer l'ordre des fonds dans le pane

          for(fond in liste_fonds_ty$a)
          {
            if(fond=="analyse/maille")
            {
              m_save <- addPolygons(map = m_save, data = analyse_ty()[[2]], opacity = 1,
                                    stroke = TRUE, color = "white", weight = 1,
                                    options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_ty()[[2]])[,"LIBELLE"], "</font> </b><br><b><font color=#2B3E50>",varTypo," : </font></b>",analyse_ty()[[2]]$TXT1,"<br>"),
                                    fill = T,
                                    fillColor = palette_ty()$col,
                                    fillOpacity = 1
              )
            }

            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = fond_select_contour_ty(),
                                    stroke = TRUE, weight = 5,
                                    color="black", opacity = 1,
                                    options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                    fill = F
              )
            }

            i <- i + 1
          }

          zoom <- as.numeric(input$mymap_ty_zoom)
          coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude

          position_leg <- t(data_frame(c(lon_lat_ty()[[1]],lon_lat_ty()[[2]])))

          # On cree les rectangles
          nb_typo <- length(unique(analyse_ty()[[1]]$classe))

          for(i in 1:nb_typo)
          {
            # Coordonnees du point haut/gauche des rectangles de la legende
            x_coord_rectangle <- position_leg[1]
            if(i==1) #1er rectangle
            {
              y_coord_rectangle <- position_leg[2]-coeff
            }else
            {
              y_coord_rectangle <- y_coord_rectangle-coeff*0.7
            }
            assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
          }

          # On ajoute un cadre blanc autour de la legende
          y_coord_rectangle <- min(get(paste0("rectangle_",nb_typo))[[1]][,2])

          # leaflet du cadre blanc en 1er
          m_save <- addRectangles(map = m_save,
                                  lng1 = position_leg[1]-coeff*0.5, lat1 = position_leg[2]+coeff*0.5, #x_titre_1 et y_titre_2
                                  lng2 = x_coord_rectangle+coeff*6, lat2 = y_coord_rectangle-coeff*0.8,
                                  stroke = TRUE,
                                  color = paste0("#2B3E50", ";background: #ffffff;
                                                 border-left:2px solid #2B3E50;
                                                 border-right:2px solid #2B3E50;
                                                 border-top:2px solid #2B3E50;
                                                 border-bottom:2px solid #2B3E50;
                                                 border-radius: 5%"),
                                  weight = 1,
                                  options = pathOptions(pane = "fond_legende", clickable = F),
                                  fill = T,
                                  fillColor = "white",
                                  fillOpacity = 0.8
                                  )

          palette <- unique(palette_ty()[order(palette_ty()$valeur),"col"])

          for(i in 1: nb_typo)
          {
            if(is.null(input$lib_typo_1_ty_id))
            {
              typo_leg_texte <- sort(unique(analyse_ty()[[1]]$valeur))[i]
            }else
            {
              typo_leg_texte <- input[[paste0("lib_typo_", i,"_ty_id")]]
            }

            m_save <- addPolygons(map = m_save, data = st_polygon(get(paste0("rectangle_",i))),
                                  stroke = FALSE,
                                  options = pathOptions(pane = "fond_legende", clickable = F),
                                  fill = T,
                                  fillColor = palette[i],
                                  fillOpacity = 1
            )

            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                          label = typo_leg_texte,
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "12px"
                                                                      ))
            )
          }

          # leaflet titre
          x_titre_2 <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
          y_titre_2 <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.2

          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = x_titre_2, lat = y_titre_2,
                                        label = input$titre_typo_legende_ty_id,
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "14px"
                                                                    ))
          )

          removeModal()

          m_save
        })

        output[[paste0("remove_carte_",nb_save_carte,"_ty")]] <- renderUI({
          actionButton(paste0("remove_carte_",nb_save_carte,"_ty_id"),label="X Supprimer la carte", style="color:#FFFFFF; border-color:#FFFFFF; background-color:#2B3E50")
        })
        appendTab(inputId = "onglets_ty",
                  tabPanel(title=HTML(paste0("<font color=#AFC9E0> Carte ",insert_save$a,"</font>")),value=paste0("carte",nb_save_carte),uiOutput(paste0("remove_carte_",nb_save_carte,"_ty")),leafletOutput(paste0("mymap_save_",insert_save$a,"_ty"),width="100%",height = 800)),
                  select = TRUE,
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_1_ty_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ty",
                  target = "carte1",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_2_ty_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ty",
                  target = "carte2",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_3_ty_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ty",
                  target = "carte3",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_4_ty_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ty",
                  target = "carte4",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_5_ty_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ty",
                  target = "carte5",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_6_ty_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ty",
                  target = "carte6",
                  session = session
        )
      }, ignoreInit = TRUE)

      # TABLEAUX DE DONNEES, MAILLE ET CONTOUR

      output$mydonnees_ty <- DT::renderDataTable(DT::datatable({
        data <- as.data.frame(analyse_ty()[[1]])
        tableau_donnees <- data[,c("CODE","LIBELLE",varTypo)]
      },  style = 'bootstrap'
      ))

      output$mycontour_ty <- DT::renderDataTable(DT::datatable({
        data <- as.data.frame(fondContour)
        tableau_contour <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))

      # ENVOI DU LEAFLET A L'UI

      output$mymap_ty <- renderLeaflet({
        react_fond_ty()
      })

    }

    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
    }
