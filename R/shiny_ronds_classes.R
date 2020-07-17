shiny_ronds_classes <-
function(data,fondMaille,fondMailleElargi=NULL,fondContour,fondSuppl=NULL,idData,varVolume,varRatio,emprise="FRM",fondEtranger=NULL,fondChx=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20<-msg_error21 <- NULL

    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondMailleElargi)) if(any(!any(class(fondMailleElargi) %in% "sf"),!any(class(fondMailleElargi) %in% "data.frame"))) msg_error3 <- "Le fond de maille elargie doit etre un objet sf / "
    if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error4 <- "Le fond de contour doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error5 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varVolume)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varRatio)!="character")) msg_error8 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(emprise)!="character")) msg_error9 <- "La valeur doit etre de type caractere ('FRM', '971', '972', '973', '974', '976' ou '999') / "
    if(!is.null(fondChx)) if(any(!any(class(fondChx) %in% "sf"),!any(class(fondChx) %in% "data.frame"))) msg_error10 <- "Le fond des chx doit etre un objet sf / "

    if(length(names(data))<3) msg_error11 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et les 2 variables a representer / "
    if(length(names(fondMaille))<3) msg_error12 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondMailleElargi)) if(length(names(fondMailleElargi))<3) msg_error13 <- "Le fond de maille elargie n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error14 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error15 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

    if(!any(names(data) %in% idData))  msg_error16 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varVolume))  msg_error17 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varRatio))  msg_error18 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!emprise %in% c("FRM","971","972","973","974","976","999")) msg_error19 <- "La variable emprise doit etre 'FRM', '971', '972', '973', '974', '976' ou '999' / "
    if(!is.null(fondEtranger)) if(any(!any(class(fondEtranger) %in% "sf"),!any(class(fondEtranger) %in% "data.frame"))) msg_error20 <- "Le fond etranger doit etre un objet sf / "
    if(!is.null(fondEtranger)) if(length(names(fondEtranger))<3) msg_error21 <- "Le fond etranger n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error18),!is.null(msg_error19),
           !is.null(msg_error20),!is.null(msg_error21)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,
                              msg_error16,msg_error17,msg_error18,msg_error19,msg_error20,msg_error21)))
    }

    nb_up <- reactiveValues(a=0)
    nb_down <- reactiveValues(a=0)
    ordre_analyse <- reactiveValues(a=1,b=2)
    insert_save <- reactiveValues(a=0)
    remove_carte <- reactiveValues(a=0)
    liste_fonds <- reactiveValues(a=c("analyse","maille","contour"))
    m_save_rp_ac <- reactiveValues(a=0)

    erreur_maille <- reactiveValues(a=FALSE)

    max_classes <-  reactiveValues(a=4)
    methode_calcul <- c("fisher","jenks","kmeans","quantile","manuel")

    legende <- reactiveValues(a=NULL)

    sourc <- "Source : Insee"

    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    names(fondContour)[1] <- "CODE"
    names(fondContour)[2] <- "LIBELLE"
    if(!is.null(fondMailleElargi))
    {
      names(fondMailleElargi)[1] <- "CODE"
      names(fondMailleElargi)[2] <- "LIBELLE"
      fondMailleElargi$LIBELLE<-iconv(fondMailleElargi$LIBELLE,"latin1","utf8")
    }
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
                                             uiOutput("variable_rond_rp_ac"),
                                             uiOutput("variable_classe_rp_ac"),
                                             tags$hr(style="border: 5px solid #5182B6"), #337ab7
                                             h4(HTML("<b><font color=#95BAE2>FONDS</font></b>")),
                                             fluidRow(
                                               column(width=9, offset=0.5,
                                                      uiOutput("ordre_fonds_rp_ac")
                                               ),
                                               column(width=1,
                                                      br(),
                                                      br(),
                                                      htmlOutput("monter_fond_rp_ac", inline=FALSE),
                                                      htmlOutput("descendre_fond_rp_ac", inline=FALSE)
                                               )
                                             ),
                                             uiOutput("elargi_rp_ac"),
                                             conditionalPanel(condition = 'input.elargi_rp_ac_id',
                                                              uiOutput("opacite_elargi_rp_ac")
                                             ),
                                             uiOutput("ajout_territoire_rp_ac"),
                                             uiOutput("ajout_reg_rp_ac"),
                                             uiOutput("ajout_dep_rp_ac"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>RONDS</font></b>")),
                                             uiOutput("taille_rond_rp_ac"),
                                             htmlOutput("info_taille_max_rond_rp_ac"),
                                             htmlOutput("info_rapport_rond_rp_ac"),
                                             uiOutput("rapport_rond_rp_ac"),
                                             conditionalPanel(condition = 'input.rapport_rond_rp_ac_id',
                                                              uiOutput("valeur_rapport_rond_rp_ac"),
                                                              htmlOutput("info_rapport_max_rond_rp_ac")
                                             ),
                                             uiOutput("choix_centroid_rp_ac"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>CLASSES</font></b>")),
                                             uiOutput("liste_classes_rp_ac"),
                                             uiOutput("methode_rp_ac"),
                                             uiOutput("distribution_variable_rp_ac"),
                                             conditionalPanel(condition = 'input.distribution_variable_rp_ac_id',
                                                              verticalLayout(
                                                                wellPanel(
                                                                  style="background: #EDF2F9; width:340px",
                                                                  plotOutput("distribution_rp_ac"),
                                                                  br(),
                                                                  uiOutput("slider_bornes_rp_ac"),
                                                                  uiOutput("valid_slider_bornes_rp_ac")
                                                                )
                                                              )
                                             ),
                                             conditionalPanel(condition = 'input.methode_rp_ac_id=="manuel"',
                                                              uiOutput("zone_bornes_rp_ac"),
                                                              uiOutput("valid_bornes_rp_ac")
                                             ),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>L\u00c9GENDE</font></b>")),
                                             uiOutput("titre_ronds_legende_rp_ac"),
                                             uiOutput("titre_classes_legende_rp_ac"),
                                             br(),
                                             uiOutput("affiche_legende_rp_ac"),
                                             uiOutput("type_legende_rp_ac"),
                                             br(),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>SAUVEGARDE</font></b>")),
                                             uiOutput("save_carte_rp_ac"),
                                             br(),
                                             conditionalPanel(condition = 'input.mymap_rp_ac_click',
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
                                                                                 uiOutput("sortie_qgis_rp_ac"),
                                                                                 br(),
                                                                                 uiOutput("titre1_qgis_rp_ac"),
                                                                                 uiOutput("titre2_qgis_rp_ac"),
                                                                                 uiOutput("source_qgis_rp_ac"),
                                                                                 tags$head(tags$style(HTML('#export_qgis_rp_ac_id{background-color:#337ab7}'))),
                                                                                 uiOutput("export_qgis_rp_ac")
                                                                               )
                                                                       )
                                                                       )

                                             ),
                                             br(),
                                             uiOutput("aide_image_rp_ac"),
                                             br()
                              ),
                              mainPanel(
                                tags$head(
                                  tags$style(HTML(".leaflet-container { background: #AFC9E0; }"))
                                ),
                                tabsetPanel(id="onglets_rp_ac",
                                            tabPanel(title=HTML("<b>Carte</b>"),value="carte",
                                                     leafletOutput("mymap_rp_ac",width="100%",height = 800)
                                            ),
                                            tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es</b>")),value="donnees",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mydonnees_rp_ac",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Maille</b>"),value="maille",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mymaille_rp_ac",width="100%",height = 800)),
                                            tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mycontour_rp_ac",width="100%",height = 800))
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

        output$variable_rond_rp_ac <- renderUI({
          selectInput("variable_rond_rp_ac_id", label=h5("Variable des ronds (en volume)"), choices = varVolume, selected = varVolume)
        })
        output$variable_classe_rp_ac <- renderUI({
          selectInput("variable_classe_rp_ac_id", label=h5("Variable des classes (en ratio)"), choices = varRatio, selected = varRatio)
        })

        # FONDS

        output$ordre_fonds_rp_ac <- renderUI({
          selectInput("ordre_fonds_rp_ac_id", label=h5("Modifier l'ordre des fonds"), choices = liste_fonds$a, multiple=TRUE, selectize=FALSE, selected = NULL)
        })
        output$monter_fond_rp_ac <- renderUI({
          actionButton("monter_fond_rp_ac_id", label="", icon=icon("arrow-up"))
        })
        output$descendre_fond_rp_ac <- renderUI({
          actionButton("descendre_fond_rp_ac_id", label="", icon=icon("arrow-down"))
        })

        if(!is.null(fondMailleElargi))
        {
          output$elargi_rp_ac <- renderUI({
            checkboxInput("elargi_rp_ac_id", label = HTML("Afficher une repr\u00e9sentation \u00e9largie de l'analyse<br>(parfois long)"),
                          value = if(is.null(fondMailleElargi)) FALSE else TRUE)
          })
          output$opacite_elargi_rp_ac <- renderUI({
            sliderInput("opacite_elargi_rp_ac_id", label = h5("Opacit\u00e9 de l'analyse \u00e9largie"), value=60, min=0, max=100, step=5, ticks=FALSE)
          })
        }

        output$ajout_territoire_rp_ac <- renderUI({
          checkboxInput("ajout_territoire_rp_ac_id", label = "Afficher le fond des territoires",
                        value = if(is.null(fondSuppl)) FALSE else TRUE)
        })
        output$ajout_reg_rp_ac <- renderUI({
          checkboxInput("ajout_reg_rp_ac_id", label = "Afficher le fond des r\u00e9gions",
                        value = FALSE)
        })
        output$ajout_dep_rp_ac <- renderUI({
          checkboxInput("ajout_dep_rp_ac_id", label = "Afficher le fond des d\u00e9partements",
                        value = FALSE)
        })

        # RONDS

        output$taille_rond_rp_ac <- renderUI({
          numericInput("taille_rond_rp_ac_id", label = h5("Rayon du rond le plus grand (en m\u00e8tres)"), value=round(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]])/1.25,0), min=0, max=round(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]]),0), step=1000)
        })

        output$info_taille_max_rond_rp_ac <- renderText({
          HTML(paste0("<font size=2 color=white>Valeur max du rayon le plus grand = ", round(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]]),0)," m</font>"))
        })

        output$info_rapport_rond_rp_ac <- renderText({
          HTML(paste0("<font size=2 color=white>Rapport Surface rond / Volume = ", (pi*(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]])/1.25)^2)/as.numeric(calcul_max_rayon_metres_rp_ac()[[2]]),"</font>"))
        })

        output$rapport_rond_rp_ac <- renderUI({
          checkboxInput("rapport_rond_rp_ac_id", label = "Modifier la valeur du rapport (permet la comparaison entre cartes)", value=FALSE)
        })

        output$valeur_rapport_rond_rp_ac <- renderUI({
          numericInput("valeur_rapport_rond_rp_ac_id", label = h5("Nouvelle valeur du rapport Surface rond / Volume"), value=(pi*(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]])/1.25)^2)/as.numeric(calcul_max_rayon_metres_rp_ac()[[2]]), min=0.1, max=(pi*(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]]))^2)/as.numeric(calcul_max_rayon_metres_rp_ac()[[2]]), step=0.1)
        })

        output$info_rapport_max_rond_rp_ac <- renderText({
          HTML(paste0("<font size=2 color=white>Valeur max du rapport = ", (pi*(as.numeric(calcul_max_rayon_metres_rp_ac()[[1]]))^2)/as.numeric(calcul_max_rayon_metres_rp_ac()[[2]]),"</font>"))
        })

        if(!is.null(fondChx))
        {
          output$choix_centroid_rp_ac <- renderUI({
            radioButtons("choix_centroid_rp_ac_id", label = h5("Les ronds sont centres sur"), choices=c("les centroides des communes"="centroid","les chx des communes"="chx"), selected = if(!is.null(fondChx)) "chx" else "centroid")
          })
        }else
        {
          output$choix_centroid_rp_ac <- renderUI({
          })
        }

        # CLASSES

        output$liste_classes_rp_ac <- renderUI({
          selectInput("nb_classes_rp_ac_id", label = h5("Nombre de classes"),
                      choices = nb_classes_rp_ac(), selected = 4)
        })
        output$methode_rp_ac <- renderUI({
          selectInput("methode_rp_ac_id", label = h5("M\u00e9thode de calcul des classes"),
                      choices = methode_calcul, selected="kmeans")
        })

        output$distribution_variable_rp_ac <- renderUI({
          bsButton("distribution_variable_rp_ac_id",label="Distribution de la variable", style="btn btn-info", icon = icon("bar-chart-o"),
                   type = "toggle", block = FALSE, disabled = FALSE,
                   value = FALSE)
        })

        observeEvent(input$distribution_variable_rp_ac_id,{
          if(!input$distribution_variable_rp_ac_id) return()
          updateButton(session, "distribution_variable_rp_ac_id", value = TRUE)
        }, ignoreInit = TRUE)

        observeEvent(input$distribution_variable_rp_ac_id,{

          output$distribution_rp_ac <- renderPlot({
            dt_donnees <- data.frame(VAR=as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]))
            ggplot(dt_donnees, aes(x=dt_donnees$VAR)) +
              stat_bin(breaks=unique(sort(c(min(dt_donnees$VAR),new_bornes_rp_ac(),max(dt_donnees$VAR)))), closed = "left", fill="#5182B6", col="white") +
              scale_x_continuous(breaks=unique(sort(c(min(dt_donnees$VAR),new_bornes_rp_ac(),max(dt_donnees$VAR)))), labels = round(unique(sort(c(min(dt_donnees$VAR),new_bornes_rp_ac(),max(dt_donnees$VAR)))),2)) +
              ggtitle(label=paste0("Distribution de la variable  : ",varRatio)) +
              xlab(label = varRatio)
          })

          output$slider_bornes_rp_ac <- renderUI({
            lapply(1:(as.numeric(input$nb_classes_rp_ac_id)-1)+1, function(i) {
              sliderInput(inputId = paste0("slider_bornes_", i,"_rp_ac_id"), label = NULL,
                          value = rev(react_bornes_rp_ac()[[1]])[i], min = min(react_bornes_rp_ac()[[1]]), max = max(react_bornes_rp_ac()[[1]]), step = 0.001) #min = rev(react_bornes_rp_ac()[[1]])[i-1], max = rev(react_bornes_rp_ac()[[1]])[i+1]
            })
          })

          output$valid_slider_bornes_rp_ac <- renderUI({
            actionButton("valid_slider_bornes_rp_ac_id",label=label_bouton_rp_ac(), icon=icon("refresh"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          })
        },ignoreInit = TRUE)

        label_bouton_rp_ac <- eventReactive(input$methode_rp_ac_id,{
          if(input$methode_rp_ac_id=="manuel")
          {
            label_bouton <- "Valider les bornes manuelles"
          }else
          {
            label_bouton <- "Basculer en mode manuel"
          }
          return(label_bouton)
        })

        new_bornes_rp_ac <- reactive({
          bornes <- vector()
          for (i in 2:(as.numeric(input$nb_classes_rp_ac_id))) {
            bornes<-c(bornes,input[[paste0("slider_bornes_", i,"_rp_ac_id")]])
          }
          return(bornes)
        })

        output$zone_bornes_rp_ac <- renderUI({

          if(!is.null(input$methode_rp_ac_id))
          {
            if(input$methode_rp_ac_id=="manuel")
              suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),as.numeric(input$nb_classes_rp_ac_id),style="kmeans",rtimes=10,intervalClosure="left"))
            else
              suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),input$nb_classes_rp_ac_id,style=input$methode_rp_ac_id,rtimes=10,intervalClosure="left"))

            carac_bornes <- calcul_bornes(analyse_rp_ac()[[1]]$donnees,bornes_analyse,varRatio,input$nb_classes_rp_ac_id,input$methode_rp_ac_id)

            if(!is.null(input$nb_classes_rp_ac_id))
            {
              if(input$methode_rp_ac_id=="manuel")
              {
                lapply(1:(as.numeric(input$nb_classes_rp_ac_id)-1)+1, function(i) {
                  numericInput(inputId = paste0("bornes_", i,"_rp_ac_id"), label = paste("Choix de la borne ", i-1),
                               value = carac_bornes[[1]][i])
                })
              }
            }
          }
        })

        output$valid_bornes_rp_ac <- renderUI({
          actionButton("valid_bornes_rp_ac_id",label="Rafraichir la carte", icon=icon("refresh"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        })

        # LEGENDE

        output$titre_ronds_legende_rp_ac <- renderUI({
          textInput("titre_ronds_legende_rp_ac_id", label = h5("Titre de la l\u00e9gende des ronds"), value = "")
        })

        output$titre_classes_legende_rp_ac <- renderUI({
          textInput("titre_classes_legende_rp_ac_id", label = h5("Titre de la l\u00e9gende des classes"), value = "")
        })

        output$affiche_legende_rp_ac <- renderUI({
          checkboxInput("affiche_legende_rp_ac_id", label = "Activer le d\u00e9placement de la l\u00e9gende au clic",
                        value = TRUE)
        })

        output$type_legende_rp_ac <- renderUI({
          radioButtons("type_legende_rp_ac_id", label = h5("Type de l\u00e9gende"),
                       choices = list("Litterale" = 1, "En echelle" = 2),
                       selected = 1, inline = TRUE)
        })

        # SAUVEGARDE

        output$save_carte_rp_ac <- renderUI({
          actionButton("save_carte_rp_ac_id", label=HTML("<font size=3>Sauvegarder la carte dans un onglet</font>"), style="color:#FFFFFF; background-color:#DF691A")
        })

        output$entrees_qgis_rp_ac <- renderUI({
          actionButton("entrees_qgis_rp_ac_id", label="Exporter en projet Qgis")
        })

        output$sortie_qgis_rp_ac <- renderUI({
          tags$div(class="input-group",
                   HTML('<input type="text" id="sortie_qgis_rp_ac_id" class="form-control" placeholder="Nom du projet" aria-describedby="sortie_qgis_rp_ac_id">
                        <span class="input-group-addon" id="sortie_qgis_rp_ac_id">.qgs</span>'))
        })

        output$titre1_qgis_rp_ac <- renderUI({
          textInput("titre1_qgis_rp_ac_id", label = h5("Titre informatif"), value = "", placeholder= "Facultatif")
        })

        output$titre2_qgis_rp_ac <- renderUI({
          textInput("titre2_qgis_rp_ac_id", label = h5("Titre descriptif"), value = "", placeholder= "Facultatif")
        })

        output$source_qgis_rp_ac <- renderUI({
          textInput("source_qgis_rp_ac_id", label = h5("Source de la carte"), value = sourc)
        })

        output$aide_image_rp_ac <- renderUI({
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
      observeEvent(list(input$monter_fond_rp_ac_id,input$descendre_fond_rp_ac_id),{

        ordre <- c()
        if(as.numeric(input$monter_fond_rp_ac_id)>nb_up$a)
        {
          ordre <- c(2,3)
          nb_up$a <- nb_up$a+1
        }
        if(as.numeric(input$descendre_fond_rp_ac_id)>nb_down$a)
        {
          ordre <- c(1,2)
          nb_down$a <- nb_down$a+1
        }

        if(is.null(input$ordre_fonds_rp_ac_id)) pos_select <- 0 else pos_select <- which(liste_fonds$a==input$ordre_fonds_rp_ac_id)

        if(pos_select>0)
        {
          if(pos_select==ordre[1]) liste_fonds$a <- liste_fonds$a[c(2,1,3)]
          if(pos_select==ordre[2]) liste_fonds$a <- liste_fonds$a[c(1,3,2)]

          updateSelectInput(session, "ordre_fonds_rp_ac_id",
                            choices = liste_fonds$a,
                            selected = input$ordre_fonds_rp_ac_id
          )
        }
      },ignoreInit = TRUE)

      # Pour la semio, on calcul le rayon maximal d'un rond de facon a ce que la somme des superficies des ronds ne depasse pas 1/7eme de la superficie du territoire.
      calcul_max_rayon_metres_rp_ac <- reactive({
        #Aire totale du territoire d'etude
        aire_territoire <- as.numeric(sum(st_area(fondMaille[fondMaille$CODE %in% data[,"CODE"],]))) #Superficie du territoire
        #valeur max de la serie de donnees
        suppressWarnings(max_var <- max(data[data[,"CODE"] %in% fondMaille$CODE,varVolume], na.rm = TRUE))

        serie <- data[data[,"CODE"] %in% fondMaille$CODE,varVolume]
        serie <- serie[!is.na(serie)]
        #on ramene la serie a un quotient fonction de la valeur max
        quotient <- serie/max_var
        #somme des carres
        somme_quotient <- sum(quotient^2)
        #calcul de la surface max du rond le plus grand
        max_surface_rond <- (aire_territoire/(7*somme_quotient))
        #calcul du rayon max du rond le plus grand
        max_rayon_metres <- sqrt(max_surface_rond/pi)

        return(list(max_rayon_metres,max_var))
      })

      rayon_rp_ac <- reactive({
        req(input$valeur_rapport_rond_rp_ac_id)
        Sys.sleep(3)
        val <- round(sqrt((input$valeur_rapport_rond_rp_ac_id*isolate(calcul_max_rayon_metres_rp_ac())[[2]])/pi),0)
        return(val)
      })

      rayon_react_rp_ac <- rayon_rp_ac %>% debounce(1000)

      observeEvent(rayon_react_rp_ac(),{
        req(rayon_react_rp_ac())

        if(length(rayon_react_rp_ac())==0) return(NULL)
        if(rayon_react_rp_ac()==0 | is.na(rayon_react_rp_ac())) return(NULL)

        isolate(updateNumericInput(session,"taille_rond_rp_ac_id", value=rayon_react_rp_ac()))

        isolate(output$info_rapport_rond_rp_ac <- renderText({
          HTML(paste0("<font size=2 color=white>Rapport Surface rond / Volume = ", (pi*(rayon_react_rp_ac())^2)/isolate(calcul_max_rayon_metres_rp_ac())[[2]],"</font>"))
        }))
      })

      rapport_rp_ac <- reactive({
        req(input$taille_rond_rp_ac_id)

        val <- (pi*(input$taille_rond_rp_ac_id)^2)/isolate(calcul_max_rayon_metres_rp_ac())[[2]]
        max <- (pi*(isolate(calcul_max_rayon_metres_rp_ac())[[1]])^2)/isolate(calcul_max_rayon_metres_rp_ac())[[2]]

        return(list(val=val,max=max))
      })

      rapport_react_rp_ac <- rapport_rp_ac %>% debounce(1000)

      observeEvent(rapport_react_rp_ac(),{
        req(rapport_react_rp_ac())

        if(length(rapport_react_rp_ac()$val)==0) return(NULL)
        if(rapport_react_rp_ac()$val==0 | is.na(rapport_react_rp_ac()$val)) return(NULL)

        isolate(updateNumericInput(session,"valeur_rapport_rond_rp_ac_id", value=rapport_react_rp_ac()$val))

        isolate(output$info_rapport_rond_rp_ac <- renderText({
          HTML(paste0("<font size=2 color=white>Rapport Surface rond / Volume = ", rapport_react_rp_ac()$val,"</font>"))
        }))
      })

      choix_centroid_rp_ac <- reactive({
        if(is.null(input$choix_centroid_rp_ac_id))
        {
          centroid <- "centroid"
        }else
        {
          centroid <- input$choix_centroid_rp_ac_id
        }
        return(centroid)
      })

      # Pour calculer les bornes des classes
      react_bornes_rp_ac <- reactive({

        if(is.null(input$nb_classes_rp_ac_id) | is.null(input$methode_rp_ac_id))
        {
          max_classes$a <- 4
          methode <- "kmeans"
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
        }else if(input$nb_classes_rp_ac_id=="" | input$methode_rp_ac_id=="")
        {
          max_classes$a <- 4
          methode <- "kmeans"
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
        }else
        {
          max_classes$a <- as.numeric(input$nb_classes_rp_ac_id)
          if(is.na(max_classes$a)) return(NULL)
          methode <- as.character(input$methode_rp_ac_id)

          if(!methode %in% c("manuel"))
          {
            suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
          }
        }
        if(methode!="manuel")
        {
          carac_bornes <- calcul_bornes(analyse_rp_ac()[[1]]$donnees,bornes_analyse,varRatio,max_classes$a,methode)
        }else if(methode=="manuel")
        {
          carac_bornes <- react_bornes_manuel_1_rp_ac()
        }

        return(carac_bornes) #list(bornes=carac_bornes[[1]],pal_classes=carac_bornes[[2]])
      })

      # Pour calculer les bornes des classes
      react_bornes_init_rp_ac <- reactive({

        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(data[,varRatio]),4,style="kmeans",rtimes=10,intervalClosure="left"))
        carac_bornes <- calcul_bornes(data,bornes_analyse,varRatio,4,"kmeans")
        return(carac_bornes) #list(bornes=carac_bornes[[1]],pal_classes=carac_bornes[[2]])
      })

      # Pour mettre a jour les bornes en mode manuel
      react_bornes_manuel_1_rp_ac <- eventReactive(input$valid_bornes_rp_ac_id,{

        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),max_classes$a,style="kmeans",rtimes=10,intervalClosure="left"))

        carac_bornes <- calcul_bornes(analyse_rp_ac()[[1]]$donnees,bornes_analyse,varRatio,input$nb_classes_rp_ac_id,input$methode_rp_ac_id)

        bornes <- vector()
        for (i in 0:(as.numeric(input$nb_classes_rp_ac_id))+1) {
          bornes<-c(bornes,input[[paste0("bornes_", i,"_rp_ac_id")]])
        }
        bornes <- c(carac_bornes[[1]][1],bornes,carac_bornes[[1]][length(carac_bornes[[1]])])
        bornes <- sort(unique(bornes),decreasing = T)

        carac_bornes[[1]] <- bornes

        return(carac_bornes)
      },ignoreNULL = FALSE)

      # Pour mettre a jour les bornes dans la distribution
      observeEvent(input$valid_slider_bornes_rp_ac_id,{
        updateSelectInput(session, inputId = "methode_rp_ac_id", selected = "manuel")
        for (i in 0:(as.numeric(input$nb_classes_rp_ac_id))+1) {
          updateNumericInput(session, inputId = paste0("bornes_", i,"_rp_ac_id"), value = input[[paste0("slider_bornes_", i,"_rp_ac_id")]])
        }
      },ignoreInit = TRUE)

      # Pour renvoyer la fourchette de classes possible
      nb_classes_rp_ac <- reactive({

        if(elargi_rp_ac())
        {
          donnees <- analyse_rp_ac()[[1]]$donnees_elargi[,varRatio]
        }else
        {
          donnees <- analyse_rp_ac()[[1]]$donnees[,varRatio]
        }

        suppressWarnings(
          if(min(donnees)<0 & max(donnees)>0) # Si + et -
          {
            if(length(donnees)>3 & length(donnees)<9)
            {
              return(c(3:(length(donnees)-1)))
            }else
            {
              return(c(3:9))
            }
          }else # Si tout + ou tout -
          {
            if(length(donnees)>3 & length(donnees)<5)
            {
              return(c(3:(length(donnees)-1)))
            }else
            {
              return(c(3:5))
            }
          }
        )
      })
      observe({nb_classes_rp_ac()})

      # Pour exporter la carte en projet Qgis

      output$export_qgis_rp_ac <- renderUI({
        downloadButton("downloadProjetQgis_rp_ac", label="Exporter")
      })

      output$downloadProjetQgis_rp_ac <- downloadHandler(contentType = "zip",
                                                         filename = function(){
                                                           paste0(input$sortie_qgis_rp_ac_id,".zip")
                                                         },
                                                         content = function(file){
                                                           files <- EXPORT_PROJET_QGIS_RP_AC(file)

                                                           zip(file,files, flags = "-j9X")
                                                         }
      )

      EXPORT_PROJET_QGIS_RP_AC <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        sortie <- input$sortie_qgis_rp_ac_id
        rep_sortie <- dirname(file)
        files <- c(paste0(rep_sortie,"/",sortie,".qgs"))

        if(is.null(input$nb_classes_rp_ac_id))
        {
          max_classes <- 4
        }else
        {
          max_classes <- input$nb_classes_rp_ac_id
        }

        if(!is.null(lon_lat_rp_ac()[[1]]))
        {
          suppressWarnings(test_affiche_leg <- try(table_classe <- data.frame(classe=c(max_classes:1),label=legende$a,couleurs=analyse_leg_rp_ac()$pal_classes, stringsAsFactors = F),silent=TRUE))
          if(class(test_affiche_leg) %in% "try-error")
          {
            showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
            return(NULL)
          }else
          {
            table_classe <- data.frame(classe=c(max_classes:1),label=legende$a,couleurs=analyse_leg_rp_ac()$pal_classes, stringsAsFactors = F)
          }
        }else
        {
          showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }

        if(elargi_rp_ac())
        {
          analyse_donnees_elargi <- analyse_rp_ac()[[1]][[4]] #donnees elargi
          analyse_maille_elargi <- fondMailleElargi #fond maille elargi
          names_donnees_elargi <- names(analyse_donnees_elargi)
          analyse_donnees_elargi <- data.frame(analyse_donnees_elargi,val=analyse_donnees_elargi[,varRatio],classe=palette_rp_ac()[[1]](analyse_donnees_elargi[,varRatio]))
          names(analyse_donnees_elargi) <- c(names_donnees_elargi,"val","classe")
          analyse_classes_elargi <- merge(table_classe,analyse_donnees_elargi,by.x="couleurs",by.y="classe")
          analyse_classes_elargi <- analyse_classes_elargi[,c("CODE","LIBELLE",varVolume,varRatio,"val","classe")]

          analyse_classes_elargi <- analyse_classes_elargi[order(analyse_classes_elargi[,varVolume],decreasing = T),]

          analyse_ronds_elargi <- analyse_ronds_sf_rp_ac()[[2]]

          analyse_ronds_elargi$classe <- analyse_classes_elargi$classe
          analyse_ronds_elargi$COL_BOR <- "white"

          fond_ronds_classes_elargi <- analyse_ronds_elargi

          fond_maille_elargi <- st_transform(fondMailleElargi, crs = as.numeric(code_epsg_rp_ac()))

          st_write(fond_ronds_classes_elargi, paste0(rep_sortie,"/fond_maille_elargi_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
          st_write(fond_maille_elargi, paste0(rep_sortie,"/fond_maille_elargi.shp"), delete_dsn = TRUE, quiet = TRUE)
          files <- c(paste0(rep_sortie,"/fond_maille_elargi_carte.shp"),paste0(rep_sortie,"/fond_maille_elargi_carte.dbf"),paste0(rep_sortie,"/fond_maille_elargi_carte.prj"),paste0(rep_sortie,"/fond_maille_elargi_carte.shx"),files)
          files <- c(paste0(rep_sortie,"/fond_maille_elargi.shp"),paste0(rep_sortie,"/fond_maille_elargi.dbf"),paste0(rep_sortie,"/fond_maille_elargi.prj"),paste0(rep_sortie,"/fond_maille_elargi.shx"),files)
        }

        analyse_donnees <- analyse_rp_ac()[[1]][[2]] #donnees
        analyse_maille <- fondMaille #fond maille

        names_donnees <- names(analyse_donnees)
        analyse_donnees <- data.frame(analyse_donnees,val=analyse_donnees[,varRatio],classe=palette_rp_ac()[[1]](analyse_donnees[,varRatio]))
        names(analyse_donnees) <- c(names_donnees,"val","classe")
        analyse_classes <- merge(table_classe,analyse_donnees,by.x="couleurs",by.y="classe")
        analyse_classes <- analyse_classes[,c("CODE","LIBELLE",varVolume,varRatio,"val","classe")]
        analyse_classes <- analyse_classes[order(analyse_classes[,varVolume],decreasing = T),]
        analyse_classes$COL_BOR <- "white"
        analyse_ronds <- analyse_ronds_sf_rp_ac()[[1]]

        analyse_ronds <- merge(analyse_ronds[,c("CODE","geometry")],analyse_classes[,c("CODE","LIBELLE",varVolume,varRatio,"COL_BOR","val","classe")],by="CODE")
        names(analyse_ronds) <- c("CODE","LIBELLE",varVolume,varRatio,"COL_BOR","val","classe","geometry")
        analyse_ronds <- st_sf(analyse_ronds,stringsAsFactors = FALSE)
        analyse_ronds <- analyse_ronds[order(as.data.frame(analyse_ronds)[,varVolume],decreasing = T),]
        fond_ronds_classes <- analyse_ronds
        fond_ronds_leg <- construction_legende_rp_ac()[[1]][[2]]
        fond_lignes_leg <- construction_legende_rp_ac()[[2]][[7]]

        fond_maille <- st_transform(fondMaille, crs = as.numeric(code_epsg_rp_ac()))
        fond_contour <- st_transform(fondContour, crs = as.numeric(code_epsg_rp_ac()))
        if(!is.null(fondSuppl) && input$ajout_territoire_rp_ac_id) fond_territoire <- st_transform(fond_territoire_rp_ac(), crs = as.numeric(code_epsg_rp_ac()))
        if(input$ajout_dep_rp_ac_id) fond_departement <- st_transform(fond_departement_rp_ac(), crs = as.numeric(code_epsg_rp_ac()))
        if(input$ajout_reg_rp_ac_id) fond_region <- st_transform(fond_region_rp_ac(), crs = as.numeric(code_epsg_rp_ac()))
        fond_france <- st_transform(fond_habillage_rp_ac()[[1]], crs = as.numeric(code_epsg_rp_ac()))
        fond_pays <- st_transform(fond_habillage_rp_ac()[[2]], crs = as.numeric(code_epsg_rp_ac()))

        st_write(fond_ronds_classes, paste0(rep_sortie,"/fond_maille_carte.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_ronds_leg, paste0(rep_sortie,"/fond_ronds_leg.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_lignes_leg, paste0(rep_sortie,"/fond_lignes_leg.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_maille, paste0(rep_sortie,"/fond_maille.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_contour,paste0(rep_sortie,"/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) st_write(fond_territoire, paste0(rep_sortie,"/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_departement")) if(!is.null(fond_departement)) st_write(fond_departement, paste0(rep_sortie,"/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_region")) if(!is.null(fond_region)) st_write(fond_region,paste0(rep_sortie,"/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE)
        st_write(fond_france,paste0(rep_sortie,"/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE)
        if(exists("fond_pays")) if(!is.null(fond_pays)) st_write(fond_pays,paste0(rep_sortie,"/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE)

        files <- c(paste0(rep_sortie,"/fond_maille_carte.shp"),paste0(rep_sortie,"/fond_maille_carte.dbf"),paste0(rep_sortie,"/fond_maille_carte.prj"),paste0(rep_sortie,"/fond_maille_carte.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_ronds_leg.shp"),paste0(rep_sortie,"/fond_ronds_leg.dbf"),paste0(rep_sortie,"/fond_ronds_leg.prj"),paste0(rep_sortie,"/fond_ronds_leg.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_lignes_leg.shp"),paste0(rep_sortie,"/fond_lignes_leg.dbf"),paste0(rep_sortie,"/fond_lignes_leg.prj"),paste0(rep_sortie,"/fond_lignes_leg.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_maille.shp"),paste0(rep_sortie,"/fond_maille.dbf"),paste0(rep_sortie,"/fond_maille.prj"),paste0(rep_sortie,"/fond_maille.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_contour.shp"),paste0(rep_sortie,"/fond_contour.dbf"),paste0(rep_sortie,"/fond_contour.prj"),paste0(rep_sortie,"/fond_contour.shx"),files)
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) files <- c(paste0(rep_sortie,"/fond_territoire.shp"),paste0(rep_sortie,"/fond_territoire.dbf"),paste0(rep_sortie,"/fond_territoire.prj"),paste0(rep_sortie,"/fond_territoire.shx"),files)
        if(exists("fond_departement")) if(!is.null(fond_departement)) files <- c(paste0(rep_sortie,"/fond_departement.shp"),paste0(rep_sortie,"/fond_departement.dbf"),paste0(rep_sortie,"/fond_departement.prj"),paste0(rep_sortie,"/fond_departement.shx"),files)
        if(exists("fond_region")) if(!is.null(fond_region)) files <- c(paste0(rep_sortie,"/fond_region.shp"),paste0(rep_sortie,"/fond_region.dbf"),paste0(rep_sortie,"/fond_region.prj"),paste0(rep_sortie,"/fond_region.shx"),files)
        files <- c(paste0(rep_sortie,"/fond_france.shp"),paste0(rep_sortie,"/fond_france.dbf"),paste0(rep_sortie,"/fond_france.prj"),paste0(rep_sortie,"/fond_france.shx"),files)
        if(exists("fond_pays")) if(!is.null(fond_pays)) files <- c(paste0(rep_sortie,"/fond_pays.shp"),paste0(rep_sortie,"/fond_pays.dbf"),paste0(rep_sortie,"/fond_pays.prj"),paste0(rep_sortie,"/fond_pays.shx"),files)

        titre1 <- paste0(input$titre1_qgis_rp_ac_id,"\n")
        titre2 <- input$titre2_qgis_rp_ac_id
        source <- input$source_qgis_rp_ac_id
        annee <- format(Sys.time(), format = "%Y")
        variable_a_representer <- varRatio
        titre_leg_classes <- input$titre_classes_legende_rp_ac_id

        l <- c()
        if(exists("fond_territoire")) l <- "fond_territoire"
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")

        if(elargi_rp_ac())
        {
          l=c("fond_france",
              "fond_contour",
              "fond_maille",
              l,
              "fond_maille_elargi",
              "fond_ronds_leg",
              "fond_lignes_leg",
              "fond_maille_carte",
              "fond_maille_elargi_carte"
          )
        }else
        {
          l=c("fond_france",
              "fond_contour",
              "fond_maille",
              l,
              "fond_ronds_leg",
              "fond_lignes_leg",
              "fond_maille_carte"
          )
        }

        l <- c(l,"fond_pays")

        export_projet_qgis_ronds_classes(l,rep_sortie,sortie,titre1,titre2,source,titre_leg_classes,table_classe,variable_a_representer,annee)

        removeModal()

        showModal(modalDialog(HTML(paste0("<font size=+1>Le projet Qgis a \u00e9t\u00e9 cr","\u00e9","ee.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        return(files)
      }

      elargi_rp_ac <- reactive({
        if(is.null(input$elargi_rp_ac_id))
        {
          elargi <- FALSE
        }else
        {
          elargi <- input$elargi_rp_ac_id
        }
        return(elargi)
      })

      code_epsg_rp_ac <- reactive({
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

      analyse_rp_ac <- reactive({

        suppressWarnings(test_k_ronds <- try(k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,elargi_rp_ac(),choix_centroid_rp_ac(),fondChx),silent=T))
        if(class(test_k_ronds) %in% "try-error")
        {
          return(NULL)
        }else
        {
          analyse <- k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,elargi_rp_ac(),choix_centroid_rp_ac(),fondChx)
        }

        if(is.null(analyse))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }
        analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        analyse$donnees[,"TXT2"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees[,varRatio], big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        if(elargi_rp_ac())
        {
          analyse$donnees_elargi[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees_elargi$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
          analyse$donnees_elargi[,"TXT2"] <- paste0("<b> <font color=#2B3E50>",format(as.vector(analyse$donnees_elargi[,varRatio]), big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        }
        analyse_WGS84 <- st_transform(analyse$analyse_points,crs=4326)
        return(list(analyse,analyse_WGS84))
      })

      analyse_leg_rp_ac <- reactive({
        analyse <- analyse_rp_ac()[[1]]
        analyse$rupture_classes <- palette_rp_ac()[[2]] #bornes
        analyse$pal_classes <- palette_rp_ac()[[3]] # pal_classes
        return(analyse)
      })

      fond_habillage_rp_ac <- reactive({

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

      fond_contour_maille_rp_ac <- reactive({
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

      fond_elargi_rp_ac <- reactive({
        req(analyse_rp_ac())
        if(elargi_rp_ac())
        {
          analyse_WGS84_elargi <- st_transform(analyse_rp_ac()[[1]]$analyse_points_elargi,crs=4326)
          maille_WGS84_elargi <- st_transform(fondMailleElargi,crs=4326)
          return(list(analyse_WGS84_elargi,maille_WGS84_elargi))
        }else
        {
          return(NULL)
        }
      })

      list_bbox_rp_ac <- reactive({
        req(fond_contour_maille_rp_ac())

        list_bbox <- list(c(st_bbox(fond_contour_maille_rp_ac()[[1]])[1],st_bbox(fond_contour_maille_rp_ac()[[1]])[3]),c(st_bbox(fond_contour_maille_rp_ac()[[1]])[2],st_bbox(fond_contour_maille_rp_ac()[[1]])[4]))
        return(list_bbox)
      })

      calcul_rond_rp_ac <- reactive({
        req(calcul_max_rayon_metres_rp_ac(),input$taille_rond_rp_ac_id)
        if(is.null(input$taille_rond_rp_ac_id)) taille_rond <- 1000

        if(!is.null(input$taille_rond_rp_ac_id))
        {
          if(input$taille_rond_rp_ac_id>calcul_max_rayon_metres_rp_ac()[[1]])
          {
            showModal(modalDialog(HTML(paste0("Le rayon du rond le plus grand est trop \u00e9lev\u00e9 et ne permet pas de respecter la r\u00e8gle s\u00e9miologique des 1/7\u00e8me. Le rayon max conseill\u00e9 est ",round(calcul_max_rayon_metres_rp_ac()[[1]],2)," m\u00e8tres.")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          }
          taille_rond_m <- input$taille_rond_rp_ac_id
        }else
        {
          taille_rond_m <- NULL
        }

        return(taille_rond_m)
      })

      analyse_ronds_sf_rp_ac <- reactive({
        req(analyse_rp_ac(),code_epsg_rp_ac(),calcul_rond_rp_ac())
        # On cree les ronds en projection locale (pl) pour l'export Qgis
        if(elargi_rp_ac())
        {
          req(fond_elargi_rp_ac())
          centres <- rbind(st_coordinates(fond_elargi_rp_ac()[[1]]))
          row.names(centres) <- c(1:(nrow(analyse_rp_ac()[[1]]$donnees_elargi)))
          ronds <- st_sf(geometry=st_sfc(lapply(c(1:nrow(centres)),function(x) st_point(centres[x,])),crs=4326))
          ronds_pl_elargi <- st_buffer(st_transform(ronds, crs = as.numeric(code_epsg_rp_ac())), calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]))

          # On cree les ronds
          dt_ronds_sf <- data.frame(ronds_pl_elargi,stringsAsFactors = F)
          analyse_ronds_sf_elargi <- st_sf(cbind(analyse_rp_ac()[[1]]$donnees_elargi,dt_ronds_sf))
        }else
        {
          analyse_ronds_sf_elargi <- NULL
        }

        centres <- rbind(st_coordinates(analyse_rp_ac()[[2]]))
        row.names(centres) <- c(1:(nrow(analyse_rp_ac()[[1]]$donnees)))
        ronds <- st_sf(geometry=st_sfc(lapply(c(1:nrow(centres)),function(x) st_point(centres[x,])),crs=4326))
        ronds_pl <- st_buffer(st_transform(ronds, crs = as.numeric(code_epsg_rp_ac())), calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]))
        # On cree les ronds
        dt_ronds_sf <- data.frame(ronds_pl,stringsAsFactors = F)
        analyse_ronds_sf <- st_sf(cbind(analyse_rp_ac()[[1]]$donnees,dt_ronds_sf))
        return(list(analyse_ronds_sf,analyse_ronds_sf_elargi))
      })

      palette_rp_ac <- reactive({

        bornes <- react_bornes_rp_ac()[[1]]

        if(is.null(bornes)) return(NULL)

        if(elargi_rp_ac()) # On redefini le min et le max de la serie pour eviter les valeurs en NA
        {
          bornes[length(bornes)] <- min(as.numeric(analyse_rp_ac()[[1]]$donnees_elargi[,varRatio]))
          bornes[1] <- max(as.numeric(analyse_rp_ac()[[1]]$donnees_elargi[,varRatio]))
        }else
        {
          bornes[length(bornes)] <- min(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]))
          bornes[1] <- max(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]))
        }

        if(length(unique(bornes)) != length(bornes))
        {
          removeModal()
          showModal(modalDialog(HTML(paste0("<font size=+1>Les bornes calculees avec la methode '",input$methode_rp_ac_id,"' ne sont pas uniques. La methode kmeans a donc ete retenue.</font>")), size="l", footer=NULL, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          Sys.sleep(7)
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_rp_ac()[[1]]$donnees[,varRatio]),max_classes$a,style="kmeans",rtimes=10,intervalClosure="left"))
          carac_bornes <- calcul_bornes(analyse_rp_ac()[[1]]$donnees,bornes_analyse,varRatio,max_classes$a,"kmeans")
          updateSelectInput(session,"methode_rp_ac_id",choices = methode_calcul, selected="kmeans")
          bornes <- carac_bornes[[1]]
          pal_classes <- carac_bornes[[2]]
        }else
        {
          pal_classes <- react_bornes_rp_ac()[[2]]
        }
        pal_classes[is.na(pal_classes)] <- "grey"
        palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")

        return(list(palette,bornes,pal_classes))
      })

      fond_territoire_rp_ac <- reactive({
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,crs=4326)
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })

      fond_region_rp_ac <- reactive({
        fond_region <- st_transform(sf_regm(),crs=4326)
        return(fond_region)
      })

      fond_departement_rp_ac <- reactive({
        fond_departement <- st_transform(sf_depm(),crs=4326)
        return(fond_departement)
      })

      fond_select_donnees_elargi_rp_ac <- reactive({
        req(analyse_ronds_sf_rp_ac(),analyse_rp_ac())

        if(elargi_rp_ac())
        {
          fond_donnees_elargi <- analyse_ronds_sf_rp_ac()[[2]][as.data.frame(analyse_ronds_sf_rp_ac()[[2]])[,"CODE"] %in% analyse_rp_ac()[[1]]$donnees_elargi[input$mydonnees_rp_ac_rows_selected,"CODE"],]
          fond_donnees_elargi <- st_transform(fond_donnees_elargi,crs=4326)
          return(fond_donnees_elargi)
        }else
        {
          return(NULL)
        }
      })

      fond_select_donnees_rp_ac <- reactive({
        req(analyse_ronds_sf_rp_ac(),analyse_rp_ac())

        fond_donnees <- analyse_ronds_sf_rp_ac()[[1]][as.data.frame(analyse_ronds_sf_rp_ac()[[1]])[,"CODE"] %in% analyse_rp_ac()[[1]]$donnees[input$mydonnees_rp_ac_rows_selected,"CODE"],]
        if(nrow(fond_donnees)>0)
        {
          fond_donnees <- st_transform(fond_donnees,crs=4326)
          return(fond_donnees)
        }else
        {
          return(NULL)
        }
      })

      fond_select_maille_elargi_rp_ac <- reactive({
        req(fond_elargi_rp_ac())
        if(elargi_rp_ac())
        {
          fond_maille_elargi <- fond_elargi_rp_ac()[[2]][as.data.frame(fond_elargi_rp_ac()[[2]])[,"CODE"] %in% as.data.frame(fondMailleElargi)[input$mymaille_rp_ac_rows_selected,"CODE"],]
          return(fond_maille_elargi)
        }else
        {
          return(NULL)
        }
      })

      fond_select_maille_rp_ac <- reactive({
        req(fond_contour_maille_rp_ac())

        fond_maille <- fond_contour_maille_rp_ac()[[2]][as.data.frame(fond_contour_maille_rp_ac()[[2]])[,"CODE"] %in% as.data.frame(fondMaille)[input$mymaille_rp_ac_rows_selected,"CODE"],]
        return(fond_maille)
      })

      fond_select_contour_rp_ac <- reactive({
        req(fond_contour_maille_rp_ac())

        fond_contour <- fond_contour_maille_rp_ac()[[1]][as.data.frame(fond_contour_maille_rp_ac()[[1]])[,"CODE"] %in% as.data.frame(fondContour)[input$mycontour_rp_ac_rows_selected,"CODE"],]
        return(fond_contour)
      })

      # CONSTRUCTION DE LA MAP EN LEAFLET

      react_fond_rp_ac <- reactive({

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

            fitBounds(lng1 = min(list_bbox_rp_ac()[[1]]),
                      lat1 = min(list_bbox_rp_ac()[[2]]),
                      lng2 = max(list_bbox_rp_ac()[[1]]),
                      lat2 = max(list_bbox_rp_ac()[[2]])
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
            m <- addPolygons(map = m, data = fond_habillage_rp_ac()[[2]][,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1

            )
          }

          # fond de la France metro, DOM ou etranger
          m <- addPolygons(map = m, data = fond_habillage_rp_ac()[[1]][,"LIBGEO"], opacity = 1, # fond_france
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           options = pathOptions(pane = "fond_france", clickable = F),
                           fill = T, fillColor = "white", fillOpacity = 1
          )

          m_save_rp_ac$a <- m

          # AFFICHAGE DU FOND TERRITOIRE

          if(!is.null(fondSuppl))
          {
            m <- addPolygons(map = m, data = fond_territoire_rp_ac(),
                             stroke = TRUE, color = "#BFBFBF", opacity = 1,
                             weight = 0.5,
                             options = pathOptions(pane = "fond_territoire", clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_rp_ac())[,"LIBELLE"], "</font> </b>"),
                             fill = T, fillColor = "white", fillOpacity = 0.001,
                             group = "territoire"
            )
          }

          # AFFICHAGE DES FONDS CONTOUR ET MAILLE

          # fond du contour
          m <- addPolygons(map = m, data = fond_contour_maille_rp_ac()[[1]], opacity = 0.3, #contour_WGS84
                           stroke = TRUE, color = "black", weight = 3,
                           options = pathOptions(pane = "fond_trio3", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[1]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.3,
                           group = "maille_contour"
          )

          # fond de la maille
          m <- addPolygons(map = m, data = fond_contour_maille_rp_ac()[[2]], opacity = 1, #maille_WGS84
                           stroke = TRUE, color = "grey", weight = 1,
                           options = pathOptions(pane = "fond_trio2", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[2]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.001,
                           group = "maille_contour"
          )

          # AFFICHAGE DE L'ANALYSE
          analyse <- k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,FALSE,"centroid",fondChx)
          analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
          analyse_WGS84 <- st_transform(analyse$analyse_points,crs=4326)

          suppressWarnings(test_analyse_maille_classe <- try(analyse$donnees[rev(order(analyse$donnees[,varVolume])),varRatio],silent=T))
          if(class(test_analyse_maille_classe) %in% "try-error")
          {
            return(NULL)
          }else
          {
            analyse_maille_classe <- analyse$donnees[rev(order(analyse$donnees[,varVolume])),varRatio]
          }

          bornes <- react_bornes_init_rp_ac()[[1]]
          suppressWarnings(bornes[length(bornes)] <- min(as.numeric(analyse$donnees[,varRatio])))
          suppressWarnings(bornes[1] <- max(as.numeric(analyse$donnees[,varRatio])))

          pal_classes <- react_bornes_init_rp_ac()[[2]]
          pal_classes[is.na(pal_classes)] <- "grey"
          palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")

          m <- addCircles(map = m,
                          lng = st_coordinates(analyse_WGS84)[,1],
                          lat = st_coordinates(analyse_WGS84)[,2],
                          stroke = TRUE, color = "white",
                          opacity = 1,
                          weight = 1,
                          radius = (calcul_max_rayon_metres_rp_ac()[[1]]/1.25)*sqrt(analyse$donnees[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                          options = pathOptions(pane = "fond_trio1", clickable = T),
                          popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees$TXT1,"<br>",
                                         "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse$donnees$TXT2),
                          fill = T,
                          fillColor = palette(analyse_maille_classe),
                          fillOpacity = 1,
                          group = "taille"
          )

          removeModal()

          showModal(modalDialog(HTML("<font size=+1>Veuillez patientez svp, la carte va s'afficher dans quelques secondes...<br><br><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> ensuite sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7

          return(m)
        }
      })

      # MODIFICATION DES FONDS TERRITOIRE D'ETUDES, REGION ET DEPARTEMENT

      observeEvent(input$ajout_territoire_rp_ac_id,{

        proxy <- leafletProxy("mymap_rp_ac")

        clearGroup(map = proxy, group = "territoire")

        #fond du territoire d'etudes
        if(!is.null(fondSuppl))
        {
          if(input$ajout_territoire_rp_ac_id)
          {
            proxy <- addPolygons(map = proxy, data = fond_territoire_rp_ac(),
                                 stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_territoire", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_rp_ac())[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "territoire"
            )
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$ajout_reg_rp_ac_id,{

        proxy <- leafletProxy("mymap_rp_ac")

        clearGroup(map = proxy, group = "region")

        if(emprise=="FRM")
        {
          if(input$ajout_reg_rp_ac_id)
          {
            #fond des regions
            proxy <- addPolygons(map = proxy, data = fond_region_rp_ac(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 1.5,
                                 options = pathOptions(pane = "fond_reg", clickable = F),
                                 fill = F,
                                 group = "region"
            )
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$ajout_dep_rp_ac_id,{

        proxy <- leafletProxy("mymap_rp_ac")

        clearGroup(map = proxy, group = "departement")

        if(emprise=="FRM")
        {
          if(input$ajout_dep_rp_ac_id)
          {
            #fond des departements
            proxy <- addPolygons(map = proxy, data = fond_departement_rp_ac(),
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

      observeEvent(list(input$monter_fond_rp_ac_id,input$descendre_fond_rp_ac_id),{

        if(as.numeric(input$monter_fond_rp_ac_id)==0 & as.numeric(input$descendre_fond_rp_ac_id)==0) return(NULL)

        proxy <- leafletProxy("mymap_rp_ac")

        clearGroup(map = proxy, group = "maille_contour")
        clearGroup(map = proxy, group = "taille")

        i <- 1 #pour gerer l'ordre des fonds dans le pane
        for(fond in liste_fonds$a)
        {
          if(fond=="analyse")
          {
            suppressWarnings(test_analyse_maille_classe <- try(analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio],silent=T))
            if(class(test_analyse_maille_classe) %in% "try-error")
            {
              return(NULL)
            }else
            {
              analyse_maille_classe <- analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio]
            }

            proxy <- addCircles(map = proxy,
                                lng = st_coordinates(analyse_rp_ac()[[2]])[,1],
                                lat = st_coordinates(analyse_rp_ac()[[2]])[,2],
                                stroke = TRUE, color = "white",
                                opacity = 1,
                                weight = 1,
                                radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                                options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT1,"<br>",
                                               "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT2),
                                fill = T,
                                fillColor = palette_rp_ac()[[1]](analyse_maille_classe),
                                fillOpacity = 1,
                                group = "taille"
            )

            ordre_analyse$a <- i
          }

          if(fond=="maille")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_rp_ac()[[2]], opacity = 1, #maille_WGS84
                                 stroke = TRUE, color = "grey", weight = 1,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[2]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "maille_contour"
            )
          }

          if(fond=="contour")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_rp_ac()[[1]], opacity = 0.3, #contour_WGS84
                                 stroke = TRUE, color = "black", weight = 3,
                                 options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[1]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.3,
                                 group = "maille_contour"
            )
          }

          i <- i + 1
        }

      },ignoreInit = TRUE)

      # MODIFICATION DE LA TAILLE DES RONDS, DU NOMBRE DE CLASSES, DE LA METHODE OU DES BORNES

      observeEvent(list(input$taille_rond_rp_ac_id,input$nb_classes_rp_ac_id,input$methode_rp_ac_id,input$valid_bornes_rp_ac_id),{
        req(input$taille_rond_rp_ac_id,input$nb_classes_rp_ac_id,input$methode_rp_ac_id)
        req(calcul_rond_rp_ac())

        proxy <- leafletProxy("mymap_rp_ac")

        clearGroup(map = proxy, group = "taille")

        suppressWarnings(test_analyse_maille_classe <- try(analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio],silent=T))
        if(class(test_analyse_maille_classe) %in% "try-error")
        {
          return(NULL)
        }else
        {
          analyse_maille_classe <- analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio]
        }

        proxy <- addCircles(map = proxy,
                            lng = st_coordinates(analyse_rp_ac()[[2]])[,1],
                            lat = st_coordinates(analyse_rp_ac()[[2]])[,2],
                            stroke = TRUE, color = "white",
                            opacity = 1,
                            weight = 1,
                            radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                            options = pathOptions(pane = paste0("fond_trio",ordre_analyse$a), clickable = T),
                            popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT1,"<br>",
                                           "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT2),
                            fill = T,
                            fillColor = palette_rp_ac()[[1]](analyse_maille_classe),
                            fillOpacity = 1,
                            group = "taille"
        )

      },ignoreInit = TRUE)

      # MODIFICATION DE LA POSITION DES RONDS (CHX OU CENTROID)

      observeEvent(input$choix_centroid_rp_ac_id,{
        req(input$choix_centroid_rp_ac_id)

        proxy <- leafletProxy("mymap_rp_ac")

        clearGroup(map = proxy, group = "taille")

        suppressWarnings(test_analyse_maille_classe <- try(analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio],silent=T))
        if(class(test_analyse_maille_classe) %in% "try-error")
        {
          return(NULL)
        }else
        {
          analyse_maille_classe <- analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio]
        }

        proxy <- addCircles(map = proxy,
                            lng = st_coordinates(analyse_rp_ac()[[2]])[,1],
                            lat = st_coordinates(analyse_rp_ac()[[2]])[,2],
                            stroke = TRUE, color = "white",
                            opacity = 1,
                            weight = 1,
                            radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                            options = pathOptions(pane = paste0("fond_trio",ordre_analyse$a), clickable = T),
                            popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT1,"<br>",
                                           "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT2),
                            fill = T,
                            fillColor = palette_rp_ac()[[1]](analyse_maille_classe),
                            fillOpacity = 1,
                            group = "taille"
        )

      },ignoreInit = TRUE)

      # MODIFICATION DE LA REPRESENTATION ELARGIE

      observeEvent(list(input$elargi_rp_ac_id,input$opacite_elargi_rp_ac_id,input$taille_rond_rp_ac_id,input$nb_classes_rp_ac_id,input$methode_rp_ac_id,input$valid_bornes_rp_ac_id,input$choix_centroid_rp_ac_id),{
        req(input$elargi_rp_ac_id,input$opacite_elargi_rp_ac_id)

        proxy <- leafletProxy("mymap_rp_ac")

        clearGroup(map = proxy, group = "elargi")

        if(elargi_rp_ac())
        {
          analyse_maille_classe_elargi <- analyse_rp_ac()[[1]]$donnees_elargi[rev(order(analyse_rp_ac()[[1]]$donnees_elargi[,varVolume])),varRatio]

          proxy <- addCircles(map = proxy,
                              lng = st_coordinates(fond_elargi_rp_ac()[[1]])[,1],
                              lat = st_coordinates(fond_elargi_rp_ac()[[1]])[,2],
                              stroke = TRUE, color = "white",
                              opacity = input$opacite_elargi_rp_ac_id/100,
                              weight = 1,
                              radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                              options = pathOptions(pane = "fond_trio3", clickable = T),
                              popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp_ac()[[1]]$donnees_elargi$TXT1,"<br>",
                                             "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse_rp_ac()[[1]]$donnees_elargi$TXT2),
                              fill = T,
                              fillColor = palette_rp_ac()[[1]](analyse_maille_classe_elargi),
                              fillOpacity = input$opacite_elargi_rp_ac_id/100,
                              group = "elargi"
          )

          proxy <- addPolygons(map = proxy, data = fond_elargi_rp_ac()[[2]], opacity = 1, #maille_WGS84
                               stroke = TRUE, color = "grey", weight = 1,
                               options = pathOptions(pane = "fond_trio3", clickable = T),
                               popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_elargi_rp_ac()[[2]])[,"LIBELLE"], "</font> </b>"),
                               fill = T, fillColor = "white", fillOpacity = 0.001,
                               group = "elargi"
          )

        }
      },ignoreInit = TRUE)

      # MODIFICATION DE LA SELECTION DES OBJETS VIA LES TABLEAUX

      observeEvent(list(input$onglets_rp_ac,input$choix_centroid_rp_ac_id),{
        req(input$onglets_rp_ac)

        if(input$onglets_rp_ac == "carte")
        {
          proxy <- leafletProxy("mymap_rp_ac")

          clearGroup(map = proxy, group = "select_donnees")

          if(!is.null(input$mydonnees_rp_ac_rows_selected))
          {
            if(elargi_rp_ac())
            {
              suppressWarnings(proxy <- addCircles(map = proxy,
                                                   lng = st_coordinates(st_centroid(fond_select_donnees_elargi_rp_ac()))[,1],
                                                   lat = st_coordinates(st_centroid(fond_select_donnees_elargi_rp_ac()))[,2],
                                                   stroke = TRUE, color = "#FFFF00",
                                                   opacity = 1,
                                                   weight = 3,
                                                   radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees_elargi[analyse_rp_ac()[[1]]$donnees_elargi[,"CODE"] %in% analyse_rp_ac()[[1]]$donnees_elargi[input$mydonnees_rp_ac_rows_selected,"CODE"],varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                                                   options = pathOptions(pane = "selection", clickable = F),
                                                   fill = F,
                                                   group = "select_donnees")
              )
            }else
            {
              suppressWarnings(proxy <- addCircles(map = proxy,
                                                   lng = st_coordinates(st_centroid(fond_select_donnees_rp_ac()))[,1],
                                                   lat = st_coordinates(st_centroid(fond_select_donnees_rp_ac()))[,2],
                                                   stroke = TRUE, color = "#FFFF00",
                                                   opacity = 1,
                                                   weight = 3,
                                                   radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees[analyse_rp_ac()[[1]]$donnees[,"CODE"] %in% analyse_rp_ac()[[1]]$donnees[input$mydonnees_rp_ac_rows_selected,"CODE"],varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                                                   options = pathOptions(pane = "selection", clickable = F),
                                                   fill = F,
                                                   group = "select_donnees")
              )
            }
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$onglets_rp_ac,{
        req(input$onglets_rp_ac)

        if(input$onglets_rp_ac == "carte")
        {
          proxy <- leafletProxy("mymap_rp_ac")

          clearGroup(map = proxy, group = "select_maille")

          if(!is.null(input$mymaille_rp_ac_rows_selected))
          {
            if(elargi_rp_ac())
            {
              proxy <- addPolygons(map = proxy, data = fond_select_maille_elargi_rp_ac(),
                                   stroke = FALSE,
                                   options = pathOptions(pane = "selection", clickable = F),
                                   fill = T,
                                   fillColor = "#FFFF00",
                                   fillOpacity = 90,
                                   group = "select_maille"
              )
            }else
            {
              proxy <- addPolygons(map = proxy, data = fond_select_maille_rp_ac(),
                                   stroke = FALSE,
                                   options = pathOptions(pane = "selection", clickable = F),
                                   fill = T,
                                   fillColor = "#FFFF00",
                                   fillOpacity = 90,
                                   group = "select_maille"
              )
            }
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$onglets_rp_ac,{
        req(input$onglets_rp_ac)

        if(input$onglets_rp_ac == "carte")
        {
          proxy <- leafletProxy("mymap_rp_ac")

          clearGroup(map = proxy, group = "select_contour")

          if(!is.null(input$mycontour_rp_ac_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_contour_rp_ac(),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "selection", clickable = F),
                                 fill = T,
                                 fillColor = "#FFFF00",
                                 fillOpacity = 90,
                                 group = "select_contour"
            )
          }
        }
      },ignoreInit = TRUE)

      # CONSTRUCTION DE LA LEGENDE

      lon_lat_rp_ac <- reactive({
        click <- input$mymap_rp_ac_click
        lon <- click$lng
        lat <- click$lat
        return(list(lon,lat))
      })

      observeEvent(list(input$mymap_rp_ac_zoom,input$mymap_rp_ac_click,input$type_legende_rp_ac_id,input$titre_ronds_legende_rp_ac_id,input$titre_classes_legende_rp_ac_id,input$taille_rond_rp_ac_id,input$nb_classes_rp_ac_id,input$methode_rp_ac_id,input$valid_bornes_rp_ac_id),{
        req(input$taille_rond_rp_ac_id)

        if(is.null(input$affiche_legende_rp_ac_id)) return(NULL)
        if(input$affiche_legende_rp_ac_id==FALSE) return(NULL)
        if(is.null(lon_lat_rp_ac()[[1]])) return(NULL)

        proxy <- leafletProxy("mymap_rp_ac")

        proxy <- clearGroup(map=proxy, group="leg")
        proxy <- clearMarkers(map=proxy)

        zoom <- as.numeric(input$mymap_rp_ac_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude

        ronds_leg <- construction_ronds_legende(lon_lat_rp_ac()[[1]],lon_lat_rp_ac()[[2]],code_epsg_rp_ac(),input$taille_rond_rp_ac_id)
        lignes <- construction_lignes_legende(ronds_leg,coeff,code_epsg_rp_ac())
        position_leg_classes <- t(data_frame(c(min(st_coordinates(ronds_leg[[1]])[which(st_coordinates(ronds_leg[[1]])[,4]==1),"X"]),min(st_coordinates(ronds_leg[[1]])[which(st_coordinates(ronds_leg[[1]])[,4]==1),"Y"]))))

        if(is.null(input$type_legende_rp_ac_id)) return(NULL)

        if(input$type_legende_rp_ac_id==1) # Litterale
        {
          # On cree les rectangles
          if(is.null(input$nb_classes_rp_ac_id))
            max_classes <- 4
          else
            max_classes <- input$nb_classes_rp_ac_id

          for(i in 1:max_classes)
          {
            # Coordonnees du point haut/gauche des rectangles de la legende
            x_coord_rectangle <- position_leg_classes[1]
            if(i==1) #1er rectangle
            {
              y_coord_rectangle <- position_leg_classes[2]-coeff*1.5 #1,5cm sous les ronds
            }else
            {
              y_coord_rectangle <- y_coord_rectangle-coeff*0.7
            }
            assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
          }

          # On ajoute un cadre blanc autour de la legende
          y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])

          # leaflet du cadre blanc en 1er
          proxy <- addRectangles(map = proxy,
                                 lng1 = position_leg_classes[1]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5,
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

          # leaflet rectangles et valeurs classes
          classes_leg_texte <- analyse_leg_rp_ac()$rupture_classes

          legende$a <- c()
          for(i in 1: max_classes)
          {
            proxy <- addPolygons(map = proxy, data = st_polygon(get(paste0("rectangle_",i))),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = analyse_leg_rp_ac()$pal_classes[i],
                                 fillOpacity = 1,
                                 group="leg"
            )

            if(i==1)
            {
              label_rectangle <- paste0(format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," et plus")

              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                           label = label_rectangle,
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              legende$a <- c(legende$a,label_rectangle)

            }else if (i>1 && i<max_classes)
            {
              label_rectangle <- paste0("De ", format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))

              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                           label = label_rectangle,
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              legende$a <- c(legende$a,label_rectangle)

            }else #i==length(max_classes)
            {
              label_rectangle <- paste0("Moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))

              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                           label = label_rectangle,
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )
              legende$a <- c(legende$a,label_rectangle)

            }
          }
          # leaflet titre 2
          x_titre_2 <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
          y_titre_2 <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.2

          proxy <- addLabelOnlyMarkers(map = proxy,
                                       lng = x_titre_2, lat = y_titre_2,
                                       label = input$titre_classes_legende_rp_ac_id,
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "14px"
                                                                   )),
                                       group="leg"
          )
        }

        if(input$type_legende_rp_ac_id==2) # Numerique
        {
          # On cree les rectangles
          if(is.null(input$nb_classes_rp_ac_id)) return(NULL)

          max_classes <- input$nb_classes_rp_ac_id

          for(i in 1:max_classes)
          {
            # Coordonnees du point haut/gauche des rectangles de la legende
            x_coord_rectangle <- position_leg_classes[1]
            if(i==1) #1er rectangle
            {
              y_coord_rectangle <- position_leg_classes[2]-coeff
            }else
            {
              y_coord_rectangle <- y_coord_rectangle-coeff*0.5
            }
            assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
          }

          # On ajoute un cadre blanc autour de la legende
          y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])

          # leaflet du cadre blanc en 1er
          proxy <- addRectangles(map = proxy,
                                 lng1 = position_leg_classes[1]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5,
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

          # leaflet rectangles et valeurs classes
          classes_leg_num <- analyse_leg_rp_ac()$rupture_classes

          for(i in 1: max_classes)
          {
            proxy <- addPolygons(map = proxy, data = st_polygon(get(paste0("rectangle_",i))),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = analyse_leg_rp_ac()$pal_classes[i],
                                 fillOpacity = 1,
                                 group="leg"
            )

            if(i<max_classes)
            {
              x1 <- max(get(paste0("rectangle_",i))[[1]][,1])
              y1 <- min(get(paste0("rectangle_",i))[[1]][,2])
              x2 <- max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.2
              y2 <- min(get(paste0("rectangle_",i))[[1]][,2])
              ligne <- st_linestring(rbind(c(x1,y1),c(x2,y2)))

              proxy <- addPolygons(map = proxy, data = ligne,
                                   color = "black",
                                   weight = 1,
                                   options = pathOptions(pane = "fond_legende", clickable = F),
                                   fill = F,
                                   fillOpacity = 1,
                                   group="leg"
              )

              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = x2, lat = y2,
                                           label = as.character(format(round(classes_leg_num[i+1],3),big.mark=" ",decimal.mark=",",nsmall=0)),
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group="leg"
              )

            }
          }

          # On ajoute la legende de classes a l'analyse

          # leaflet titre 2
          x_titre_2 <- min(get("rectangle_1")[[1]][,1])
          y_titre_2 <- max(get("rectangle_1")[[1]][,2])+coeff*0.2

          proxy <- addLabelOnlyMarkers(map = proxy,
                                       lng = x_titre_2, lat = y_titre_2,
                                       label = input$titre_classes_legende_rp_ac_id,
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "14px"
                                                                   )),
                                       group="leg"
          )
        }

        suppressWarnings(proxy <- addCircles(map = proxy,
                                             lng = st_coordinates(st_centroid(ronds_leg[[1]]))[,1],
                                             lat = st_coordinates(st_centroid(ronds_leg[[1]]))[,2],
                                             stroke = TRUE,
                                             opacity = 1,
                                             color = "#2B3E50",
                                             weight = 2,
                                             radius = c(calcul_rond_rp_ac(),calcul_rond_rp_ac()/sqrt(3)),
                                             options = pathOptions(pane = "fond_legende", clickable = F),
                                             fill = T,
                                             fillColor = "white",
                                             fillOpacity = 1,
                                             group="leg"
        ))

        # leaflet lignes
        proxy <- addPolygons(map = proxy, data = lignes[[1]],
                             stroke = TRUE,
                             opacity = 1,
                             color = "#2B3E50",
                             weight = 2,
                             options = pathOptions(pane = "fond_legende", clickable = F),
                             fill = F,
                             fillOpacity = 1,
                             group="leg"
        )

        # leaflet valeur ronds
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = lignes[[2]][2,1], lat = lignes[[2]][2,2], #ligne_grand
                                     label = as.character(format(calcul_max_rayon_metres_rp_ac()[[2]],big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )

        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = lignes[[3]][2,1], lat = lignes[[3]][2,2], #ligne_petit
                                     label = as.character(format(round(calcul_max_rayon_metres_rp_ac()[[2]]/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group="leg"
        )

        #leaflet titre 1
        proxy <- addLabelOnlyMarkers(map = proxy,
                                     lng = lignes[[4]], lat = lignes[[5]], #x_titre_1 et y_titre_1
                                     label = input$titre_ronds_legende_rp_ac_id,
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "14px"
                                                                 )),
                                     group="leg"
        )
      })

      construction_legende_rp_ac <- reactive({
        zoom <- as.numeric(input$mymap_rp_ac_zoom)
        coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude

        ronds_leg <- construction_ronds_legende(lon_lat_rp_ac()[[1]],lon_lat_rp_ac()[[2]],code_epsg_rp_ac(),input$taille_rond_rp_ac_id)
        lignes <- construction_lignes_legende(ronds_leg,coeff,code_epsg_rp_ac())
        ronds_leg[[2]] <- cbind(ronds_leg[[2]],VALEUR=c(max(data[,varVolume]),max(data[,varVolume])/3))

        return(list(ronds_leg,lignes,coeff))
      })

      # AJOUT DES ONGLETS SAUVEGARDE

      observeEvent(input$save_carte_rp_ac_id,{

        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Sauvegarde de la carte en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        insert_save$a <- insert_save$a + 1
        nb_save_carte <- insert_save$a-remove_carte$a

        m_save <- m_save_rp_ac$a

        if(nb_save_carte>6)
        {
          insert_save$a <- insert_save$a - 1
          showModal(modalDialog(HTML("<font size=+1>Vous ne pouvez pas sauvegarger plus de 6 cartes. Veuillez en supprimer avant de continuer.</font> "), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }

        output[[paste0("mymap_save_",insert_save$a,"_rp_ac")]] <- renderLeaflet({

          if(!is.null(fondSuppl))
          {
            if(input$ajout_territoire_rp_ac_id)
            {
              m_save <- addPolygons(map = m_save, data = fond_territoire_rp_ac(),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_rp_ac())[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }

          if(input$ajout_reg_rp_ac_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_region_rp_ac(),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }

          if(input$ajout_dep_rp_ac_id)
          {
            m_save <- addPolygons(map = m_save, data = fond_departement_rp_ac(),
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
              suppressWarnings(test_analyse_maille_classe <- try(analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio],silent=T))
              if(class(test_analyse_maille_classe) %in% "try-error")
              {
                return(NULL)
              }else
              {
                analyse_maille_classe <- analyse_rp_ac()[[1]]$donnees[rev(order(analyse_rp_ac()[[1]]$donnees[,varVolume])),varRatio]
              }

              m_save <- addCircles(map = m_save,
                                   lng = st_coordinates(analyse_rp_ac()[[2]])[,1],
                                   lat = st_coordinates(analyse_rp_ac()[[2]])[,2],
                                   stroke = TRUE, color = "white",
                                   opacity = 1,
                                   weight = 1,
                                   radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                                   options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                   popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT1,"<br>",
                                                  "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse_rp_ac()[[1]]$donnees$TXT2),
                                   fill = T,
                                   fillColor = palette_rp_ac()[[1]](analyse_maille_classe),
                                   fillOpacity = 1
              )
            }

            if(fond=="maille")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_rp_ac()[[2]], opacity = 1, #maille_WGS84
                                    stroke = TRUE, color = "grey", weight = 1,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[2]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }

            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = fond_contour_maille_rp_ac()[[1]], opacity = 0.3, #contour_WGS84
                                    stroke = TRUE, color = "black", weight = 3,
                                    options = pathOptions(pane = paste0("fond_trio",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[1]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.3
              )
            }

            i <- i + 1
          }

          if(elargi_rp_ac())
          {
            analyse_maille_classe_elargi <- analyse_rp_ac()[[1]]$donnees_elargi[rev(order(analyse_rp_ac()[[1]]$donnees_elargi[,varVolume])),varRatio]

            m_save <- addCircles(map = m_save,
                                 lng = st_coordinates(fond_elargi_rp_ac()[[1]])[,1],
                                 lat = st_coordinates(fond_elargi_rp_ac()[[1]])[,2],
                                 stroke = TRUE, color = "white",
                                 opacity = input$opacite_elargi_rp_ac_id/100,
                                 weight = 1,
                                 radius = calcul_rond_rp_ac()*sqrt(analyse_rp_ac()[[1]]$donnees_elargi[,varVolume]/calcul_max_rayon_metres_rp_ac()[[2]]),
                                 options = pathOptions(pane = "fond_trio3", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_rp_ac()[[1]]$donnees_elargi$TXT1,"<br>",
                                                "<b> <font color=#2B3E50>",varRatio," : </font></b>",analyse_rp_ac()[[1]]$donnees_elargi$TXT2),
                                 fill = T,
                                 fillColor = palette_rp_ac()[[1]](analyse_maille_classe_elargi),
                                 fillOpacity = input$opacite_elargi_rp_ac_id/100
            )

            m_save <- addPolygons(map = m_save, data = fond_contour_maille_rp_ac()[[2]], opacity = 1, #maille_WGS84
                                  stroke = TRUE, color = "grey", weight = 1,
                                  options = pathOptions(pane = "fond_trio3", clickable = T),
                                  popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_rp_ac()[[2]])[,"LIBELLE"], "</font> </b>"),
                                  fill = T, fillColor = "white", fillOpacity = 0.001
            )
          }

          zoom <- as.numeric(input$mymap_rp_ac_zoom)
          coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude

          ronds_leg <- construction_ronds_legende(lon_lat_rp_ac()[[1]],lon_lat_rp_ac()[[2]],code_epsg_rp_ac(),input$taille_rond_rp_ac_id)
          lignes <- construction_lignes_legende(ronds_leg,coeff,code_epsg_rp_ac())
          position_leg_classes <- t(data_frame(c(min(st_coordinates(ronds_leg[[1]])[which(st_coordinates(ronds_leg[[1]])[,4]==1),"X"]),min(st_coordinates(ronds_leg[[1]])[which(st_coordinates(ronds_leg[[1]])[,4]==1),"Y"]))))

          if(input$type_legende_rp_ac_id==1) # Litterale
          {
            # On cree les rectangles
            max_classes <- input$nb_classes_rp_ac_id

            for(i in 1:max_classes)
            {
              # Coordonnees du point haut/gauche des rectangles de la legende
              x_coord_rectangle <- position_leg_classes[1]
              if(i==1) #1er rectangle
              {
                y_coord_rectangle <- position_leg_classes[2]-coeff*1.5 #1,5cm sous les ronds
              }else
              {
                y_coord_rectangle <- y_coord_rectangle-coeff*0.7
              }
              assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
            }

            # On ajoute un cadre blanc autour de la legende
            y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])

            # leaflet du cadre blanc en 1er
            m_save <- addRectangles(map = m_save,
                                    lng1 = position_leg_classes[1]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5,
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

            # leaflet rectangles et valeurs classes
            classes_leg_texte <- analyse_leg_rp_ac()$rupture_classes

            for(i in 1: max_classes)
            {

              m_save <- addPolygons(map = m_save, data = st_polygon(get(paste0("rectangle_",i))),
                                    stroke = FALSE,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = analyse_leg_rp_ac()$pal_classes[i],
                                    fillOpacity = 1
              )

              if(i==1)
              {
                label_rectangle <- paste0(format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," et plus")

                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                              label = label_rectangle,
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
              }else if (i>1 && i<max_classes)
              {
                label_rectangle <- paste0("De ", format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))

                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                              label = label_rectangle,
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
              }else #i==length(max_classes)
              {
                label_rectangle <- paste0("Moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))

                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1, lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                              label = label_rectangle,
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )
              }
            }

            # leaflet titre 2
            x_titre_2 <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
            y_titre_2 <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.2

            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = x_titre_2, lat = y_titre_2,
                                          label = input$titre_classes_legende_rp_ac_id,
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "14px"
                                                                      ))
            )
          }

          if(input$type_legende_rp_ac_id==2) # Numerique
          {
            # On cree les rectangles
            max_classes <- input$nb_classes_rp_ac_id

            for(i in 1:max_classes)
            {
              # Coordonnees du point haut/gauche des rectangles de la legende
              x_coord_rectangle <- position_leg_classes[1]
              if(i==1) #1er rectangle
              {
                y_coord_rectangle <- position_leg_classes[2]-coeff
              }else
              {
                y_coord_rectangle <- y_coord_rectangle-coeff*0.5
              }
              assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
            }

            # On ajoute un cadre blanc autour de la legende
            y_coord_rectangle <- min(get(paste0("rectangle_",max_classes))[[1]][,2])

            # leaflet du cadre blanc en 1er
            m_save <- addRectangles(map = m_save,
                                    lng1 = position_leg_classes[1]-coeff*0.5, lat1 = lignes[[5]]+coeff*0.5,
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

            # leaflet rectangles et valeurs classes
            classes_leg_num <- analyse_leg_rp_ac()$rupture_classes

            for(i in 1: max_classes)
            {
              m_save <- addPolygons(map = m_save, data = st_polygon(get(paste0("rectangle_",i))),
                                    stroke = FALSE,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = analyse_leg_rp_ac()$pal_classes[i],
                                    fillOpacity = 1
              )

              if(i<max_classes)
              {
                x1 <- max(get(paste0("rectangle_",i))[[1]][,1])
                y1 <- min(get(paste0("rectangle_",i))[[1]][,2])
                x2 <- max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.2
                y2 <- min(get(paste0("rectangle_",i))[[1]][,2])
                ligne <- st_linestring(rbind(c(x1,y1),c(x2,y2)))

                m_save <- addPolygons(map = m_save, data = ligne,
                                      color = "black",
                                      weight = 1,
                                      options = pathOptions(pane = "fond_legende", clickable = F),
                                      fill = F,
                                      fillOpacity = 1
                )

                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = x2, lat = y2,
                                              label = as.character(format(round(classes_leg_num[i+1],3),big.mark=" ",decimal.mark=",",nsmall=0)),
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          ))
                )

              }
            }

            # On ajoute la legende de classes a l'analyse

            # leaflet titre 2
            x_titre_2 <- min(get("rectangle_1")[[1]][,1])
            y_titre_2 <- max(get("rectangle_1")[[1]][,2])+coeff*0.2

            m_save <- addLabelOnlyMarkers(map = m_save,
                                          lng = x_titre_2, lat = y_titre_2,
                                          label = input$titre_classes_legende_rp_ac_id,
                                          labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                      style = list(
                                                                        "color" = "black",
                                                                        "font-size" = "14px"
                                                                      ))
            )
          }

          suppressWarnings(m_save <- addCircles(map = m_save,
                                                lng = st_coordinates(st_centroid(ronds_leg[[1]]))[,1],
                                                lat = st_coordinates(st_centroid(ronds_leg[[1]]))[,2],
                                                stroke = TRUE,
                                                opacity = 1,
                                                color = "#2B3E50",
                                                weight = 2,
                                                radius = c(calcul_rond_rp_ac(),calcul_rond_rp_ac()/sqrt(3)),
                                                options = pathOptions(pane = "fond_legende", clickable = F),
                                                fill = T,
                                                fillColor = "white",
                                                fillOpacity = 1
          ))

          # leaflet lignes
          m_save <- addPolygons(map = m_save, data = lignes[[1]],
                                stroke = TRUE,
                                opacity = 1,
                                color = "#2B3E50",
                                weight = 2,
                                options = pathOptions(pane = "fond_legende", clickable = F),
                                fill = F,
                                fillOpacity = 1
          )

          # leaflet valeur ronds
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = lignes[[2]][2,1], lat = lignes[[2]][2,2], #ligne_grand
                                        label = as.character(format(calcul_max_rayon_metres_rp_ac()[[2]],big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )

          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = lignes[[3]][2,1], lat = lignes[[3]][2,2], #ligne_petit
                                        label = as.character(format(round(calcul_max_rayon_metres_rp_ac()[[2]]/3,0),big.mark=" ",decimal.mark=",",nsmall=0)),
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "12px"
                                                                    ))
          )

          #leaflet titre 1
          m_save <- addLabelOnlyMarkers(map = m_save,
                                        lng = lignes[[4]], lat = lignes[[5]], #x_titre_1 et y_titre_1
                                        label = input$titre_ronds_legende_rp_ac_id,
                                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                    style = list(
                                                                      "color" = "black",
                                                                      "font-size" = "14px"
                                                                    ))
          )

          removeModal()

          m_save
        })

        output[[paste0("remove_carte_",nb_save_carte,"_rp_ac")]] <- renderUI({
          actionButton(paste0("remove_carte_",nb_save_carte,"_rp_ac_id"),label="X Supprimer la carte", style="color:#FFFFFF; border-color:#FFFFFF; background-color:#2B3E50")
        })

        appendTab(inputId = "onglets_rp_ac",
                  tabPanel(title=HTML(paste0("<font color=#AFC9E0> Carte ",insert_save$a,"</font>")),value=paste0("carte",nb_save_carte),uiOutput(paste0("remove_carte_",nb_save_carte,"_rp_ac")),leafletOutput(paste0("mymap_save_",insert_save$a,"_rp_ac"),width="100%",height = 800)),
                  select = TRUE,
                  session = session
        )

      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_1_rp_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp_ac",
                  target = "carte1",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_2_rp_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp_ac",
                  target = "carte2",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_3_rp_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp_ac",
                  target = "carte3",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_4_rp_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp_ac",
                  target = "carte4",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_5_rp_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp_ac",
                  target = "carte5",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_6_rp_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_rp_ac",
                  target = "carte6",
                  session = session
        )
      }, ignoreInit = TRUE)

      # TABLEAUX DE DONNEES, MAILLE ET CONTOUR

      output$mydonnees_rp_ac <- DT::renderDataTable(DT::datatable({
        if(elargi_rp_ac())
          data <- analyse_rp_ac()[[1]]$donnees_elargi
        else
          data <- analyse_rp_ac()[[1]]$donnees
        tableau_donnees <- data[,c("CODE","LIBELLE",varVolume,varRatio)]
      },  style = 'bootstrap'
      ))

      output$mymaille_rp_ac <- DT::renderDataTable(DT::datatable({
        if(elargi_rp_ac())
          data <- as.data.frame(fondMailleElargi)
        else
          data <- as.data.frame(fondMaille)
        tableau_maille <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))

      output$mycontour_rp_ac <- DT::renderDataTable(DT::datatable({
        data <- as.data.frame(fondContour)
        tableau_contour <- data[,c(1:2)]
      },  style = 'bootstrap'
      ))

      # ENVOI DU LEAFLET A L'UI

      output$mymap_rp_ac <- renderLeaflet({
        react_fond_rp_ac()
      })

    }

    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
    }
