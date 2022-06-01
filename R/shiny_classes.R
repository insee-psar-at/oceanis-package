shiny_classes <-
function(data,fondMaille,fondMailleElargi=NULL,fondContour,fondSuppl=NULL,idData,varRatio,emprise="FRM",fondEtranger=NULL)
  {
    options("stringsAsFactors"=FALSE)
  
    # Verification des parametres
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18 <- NULL

    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondMailleElargi)) if(any(!any(class(fondMailleElargi) %in% "sf"),!any(class(fondMailleElargi) %in% "data.frame"))) msg_error3 <- "Le fond de maille elargie doit etre un objet sf / "
    if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error4 <- "Le fond de contour doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error5 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varRatio)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(emprise)!="character")) msg_error8 <- "La valeur doit etre de type caractere ('FRM', '971', '972', '973', '974', '976' ou '999') / "

    if(length(names(data))<2) msg_error9 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error10 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondMailleElargi)) if(length(names(fondMailleElargi))<3) msg_error11 <- "Le fond de maille elargie n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(length(names(fondContour))<3) msg_error12 <- "Le fond de contour n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error13 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

    if(!any(names(data) %in% idData))  msg_error14 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varRatio))  msg_error15 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!emprise %in% c("FRM","971","972","973","974","976","999")) msg_error16 <- "La variable emprise doit etre 'FRM', '971', '972', '973', '974', '976' ou '999' / "
    if(!is.null(fondEtranger)) if(any(!any(class(fondEtranger) %in% "sf"),!any(class(fondEtranger) %in% "data.frame"))) msg_error17 <- "Le fond etranger doit etre un objet sf / "
    if(!is.null(fondEtranger)) if(length(names(fondEtranger))<3) msg_error18 <- "Le fond etranger n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),!is.null(msg_error13),
           !is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),!is.null(msg_error17),
           !is.null(msg_error18)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,msg_error17,msg_error18)))
    }

    nb_up <- reactiveValues(a=0)
    nb_down <- reactiveValues(a=0)
    ordre_analyse <- reactiveValues(a=1,b=2)
    insert_save <- reactiveValues(a=0)
    remove_carte <- reactiveValues(a=0)
    liste_fonds_ac <- reactiveValues(a=c("analyse/maille","contour"))
    m_save_ac <- reactiveValues(a=0)

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
      if(any(Encoding(fondMailleElargi$LIBELLE) %in% "latin1")){
        fondMailleElargi$LIBELLE<-iconv(fondMailleElargi$LIBELLE,"latin1","UTF-8")
      }
    }
    epsg_etranger <- NULL
    if(!is.null(fondEtranger))
    {
      names(fondEtranger)[1] <- "CODE"
      names(fondEtranger)[2] <- "LIBGEO"
      if(any(Encoding(fondEtranger$LIBGEO) %in% "latin1")){
        fondEtranger$LIBGEO<-iconv(fondEtranger$LIBGEO,"latin1","UTF-8")
      }

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
      if(any(Encoding(fondSuppl$LIBELLE) %in% "latin1")){
        fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","UTF-8")
      }
    }

    if(any(Encoding(fondMaille$LIBELLE) %in% "latin1")){
      fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","UTF-8")
    }
    if(any(Encoding(fondContour$LIBELLE) %in% "latin1")){
      fondContour$LIBELLE<-iconv(fondContour$LIBELLE,"latin1","UTF-8")
    }

    ui <- navbarPage("OCEANIS", id="menu",

                     theme = shinytheme("superhero"),

                     tabPanel("Carte",value="carte",
                              sidebarLayout(

                                sidebarPanel(width = 3,
                                             style = "overflow-y:scroll; min-height: 1000px; max-height: 1000px",
                                             h4(HTML("<b><font color=#95BAE2>VARIABLES</font></b>")),
                                             uiOutput("variable_classe_ac"),
                                             tags$hr(style="border: 5px solid #5182B6"), #337ab7
                                             h4(HTML("<b><font color=#95BAE2>FONDS</font></b>")),
                                             fluidRow(
                                               column(width=9, offset=0.5,
                                                      uiOutput("ordre_fonds_ac")
                                               ),
                                               column(width=1,
                                                      br(),
                                                      br(),
                                                      htmlOutput("monter_fond_ac", inline=FALSE),
                                                      htmlOutput("descendre_fond_ac", inline=FALSE)
                                               )
                                             ),
                                             uiOutput("elargi_ac"),
                                             conditionalPanel(condition = 'input.elargi_ac_id',
                                                              uiOutput("opacite_elargi_ac")
                                             ),
                                             uiOutput("ajout_territoire_ac"),
                                             uiOutput("ajout_reg_ac"),
                                             uiOutput("ajout_dep_ac"),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>CLASSES</font></b>")),
                                             uiOutput("liste_classes_ac"),
                                             uiOutput("methode_ac"),
                                             uiOutput("palette_insee_ac"),
                                             uiOutput("distribution_variable_ac"),
                                             conditionalPanel(condition = 'input.distribution_variable_ac_id',
                                                              verticalLayout(
                                                                wellPanel(
                                                                  style="background: #EDF2F9; width:340px",
                                                                  plotOutput("distribution_ac"),
                                                                  br(),
                                                                  uiOutput("slider_bornes_ac"),
                                                                  uiOutput("valid_slider_bornes_ac")
                                                                )
                                                              )
                                             ),
                                             conditionalPanel(condition = 'input.methode_ac_id=="manuel"',
                                                              br(),
                                                              uiOutput("zone_bornes_max_ac"),
                                                              uiOutput("zone_bornes_ac"),
                                                              uiOutput("zone_bornes_min_ac"),
                                                              br(),
                                                              uiOutput("valid_bornes_ac")
                                             ),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>L\u00c9GENDE</font></b>")),
                                             uiOutput("titre_classes_legende_ac"),
                                             br(),
                                             uiOutput("affiche_legende_ac"),
                                             uiOutput("type_legende_ac"),
                                             br(),
                                             tags$hr(style="border: 5px solid #5182B6"),
                                             h4(HTML("<b><font color=#95BAE2>SAUVEGARDE</font></b>")),
                                             uiOutput("save_carte_ac"),
                                             br(),
                                             conditionalPanel(condition = 'input.type_legende_ac_id==1 && input.mymap_ac_click',
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
                                                                                 uiOutput("sortie_qgis_ac"),
                                                                                 br(),
                                                                                 uiOutput("titre1_qgis_ac"),
                                                                                 uiOutput("titre2_qgis_ac"),
                                                                                 uiOutput("source_qgis_ac"),
                                                                                 tags$head(tags$style(HTML('#export_qgis_ac_id{background-color:#337ab7}'))),
                                                                                 uiOutput("export_qgis_ac")
                                                                               )
                                                                       )
                                                                       )

                                             ),
                                             br(),
                                             uiOutput("aide_image_ac"),
                                             br()
                              ),
                              mainPanel(
                                tags$head(
                                  tags$style(HTML(".leaflet-container { background: #AFC9E0; }"))
                                ),
                                tabsetPanel(id="onglets_ac",
                                            tabPanel(title=HTML("<b>Carte</b>"),value="carte",
                                                     leafletOutput("mymap_ac",width="112%",height = 950)),
                                            tabPanel(title=HTML(paste0("<b>Donn","\u00e9","es/Maille</b>")),value="maille",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mymaille_ac",width="112%",height = 950)),
                                            tabPanel(title=HTML("<b>Contour</b>"),value="contour",
                                                     h5("S\u00e9lectionnez une ou plusieurs lignes pour ensuite les visualiser sur la carte."),
                                                     DT::dataTableOutput("mycontour_ac",width="112%",height = 950))
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

        output$variable_classe_ac <- renderUI({
          selectInput("variable_classe_ac_id", label=h5("Variable des classes (en ratio)"), choices = varRatio, selected = varRatio)
        })

        # FONDS

        output$ordre_fonds_ac <- renderUI({
          selectInput("ordre_fonds_ac_id", label=h5("Modifier l'ordre des fonds"), choices = liste_fonds_ac$a, multiple=TRUE, selectize=FALSE, selected = "analyse")
        })
        output$monter_fond_ac <- renderUI({
          actionButton("monter_fond_ac_id", label="", icon=icon("arrow-up"))
        })
        output$descendre_fond_ac <- renderUI({
          actionButton("descendre_fond_ac_id", label="", icon=icon("arrow-down"))
        })

        if(!is.null(fondMailleElargi))
        {
          output$elargi_ac <- renderUI({
            checkboxInput("elargi_ac_id", label = HTML("Afficher une repr\u00e9sentation \u00e9largie de l'analyse<br>(parfois long)"),
                          value = if(is.null(fondMailleElargi)) FALSE else TRUE)
          })
          output$opacite_elargi_ac <- renderUI({
            sliderInput("opacite_elargi_ac_id", label = h5("Opacit\u00e9 de l'analyse \u00e9largie"), value=60, min=0, max=100, step=5, ticks=FALSE)
          })
        }

        output$ajout_territoire_ac <- renderUI({
          checkboxInput("ajout_territoire_ac_id", label = "Afficher le fond des territoires",
                        value = if(is.null(fondSuppl)) FALSE else TRUE)
        })
        output$ajout_reg_ac <- renderUI({
          checkboxInput("ajout_reg_ac_id", label = "Afficher le fond des r\u00e9gions",
                        value = FALSE)
        })
        output$ajout_dep_ac <- renderUI({
          checkboxInput("ajout_dep_ac_id", label = "Afficher le fond des d\u00e9partements",
                        value = FALSE)
        })

        # CLASSES

        output$liste_classes_ac <- renderUI({
          selectInput("nb_classes_ac_id", label = h5("Nombre de classes"),
                      choices = nb_classes_ac()[[1]], selected = nb_classes_ac()[[1]][1])
        })
        output$methode_ac <- renderUI({
          selectInput("methode_ac_id", label = h5("M\u00e9thode de calcul des classes"),
                      choices = methode_calcul, selected="kmeans")
        })
        
        output$palette_insee_ac <- renderUI({
          selectInput("palette_insee_ac_id", label = h5("Palette de couleurs"), 
                      choices = nb_classes_ac()[[2]], selected=nb_classes_ac()[[2]][1])
        })

        output$distribution_variable_ac <- renderUI({
          
          bsButton(inputId="distribution_variable_ac_id",label="Distribution de la variable", style="btn btn-info", icon = icon("chart-bar"),
                   type = "toggle", block = FALSE, disabled = FALSE,
                   value = FALSE)
        })

        observeEvent(input$distribution_variable_ac_id,{
          
          output$distribution_ac <- renderPlot({
            dt_donnees <- data.frame(VAR=as.numeric(analyse_ac()$donnees[,varRatio]))
            ggplot(dt_donnees, aes(x=.data$VAR)) +
                   stat_bin(breaks=unique(sort(c(min(dt_donnees$VAR),new_bornes_ac(),max(dt_donnees$VAR, na.rm = TRUE)))), closed = "left", fill="#5182B6", col="white") +
                   scale_x_continuous(breaks=unique(sort(c(min(dt_donnees$VAR),new_bornes_ac(),max(dt_donnees$VAR, na.rm = TRUE)))), labels = round(unique(sort(c(min(dt_donnees$VAR),new_bornes_ac(),max(dt_donnees$VAR, na.rm = TRUE)))),2)) +
                   ggtitle(label=paste0("Distribution de la variable  : ",varRatio)) +
                   xlab(label = varRatio)
          })
          
          output$slider_bornes_ac <- renderUI({
            lapply(1:(as.numeric(input$nb_classes_ac_id)-1)+1, function(i) {
              sliderInput(inputId = paste0("slider_bornes_", i,"_ac_id"), label = NULL,
                          value = rev(react_bornes_ac()[[1]])[i], min = round(min(react_bornes_ac()[[1]]),3), max = round(max(react_bornes_ac()[[1]]),3), step = 0.001) #min = rev(react_bornes_ac()[[1]])[i-1], max = rev(react_bornes_ac()[[1]])[i+1]
            })
          })
          
          output$valid_slider_bornes_ac <- renderUI({
            actionButton("valid_slider_bornes_ac_id",label=label_bouton_ac(), icon=icon("sync"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          })
        },ignoreInit = TRUE)

        label_bouton_ac <- eventReactive(input$methode_ac_id,{
          if(input$methode_ac_id=="manuel")
          {
            label_bouton <- "Valider les bornes manuelles"
          }else
          {
            label_bouton <- "Basculer en mode manuel"
          }
          return(label_bouton)
        })

        new_bornes_ac <- reactive({
          bornes <- vector()
          for (i in 2:(as.numeric(input$nb_classes_ac_id))) {
            bornes<-c(bornes,input[[paste0("slider_bornes_", i,"_ac_id")]])
          }
          return(bornes)
        })

        output$zone_bornes_max_ac <- renderUI({
          
          HTML(paste0("Borne max : ", round(max(as.numeric(analyse_ac()$donnees[,varRatio])),3)))
          
        })
        
        output$zone_bornes_ac <- renderUI({
          
          if(!is.null(input$methode_ac_id))
          {
            if(input$methode_ac_id=="manuel")
              suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),as.numeric(input$nb_classes_ac_id),style="kmeans",rtimes=10,intervalClosure="left"))
            else
              suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),as.numeric(input$nb_classes_ac_id),style=input$methode_ac_id,rtimes=10,intervalClosure="left"))
            
            carac_bornes <- calcul_bornes(analyse_ac()$donnees,bornes_analyse,varRatio,input$nb_classes_ac_id,input$methode_ac_id,input$palette_insee_ac_id)
            
            if(!is.null(input$nb_classes_ac_id))
            {
              if(input$methode_ac_id=="manuel")
              {
                lapply(1:(as.numeric(input$nb_classes_ac_id)-1), function(i) {
                  numericInput(inputId = paste0("bornes_", i,"_ac_id"), label = paste("Choix de la borne ", i),
                               value = round(rev(carac_bornes[[1]])[i+1],3))
                })
              }
            }
          }
        })
        
        output$zone_bornes_min_ac <- renderUI({
          
          HTML(paste0("Borne min : ", round(min(as.numeric(analyse_ac()$donnees[,varRatio])),3)))
          
        })

        output$valid_bornes_ac <- renderUI({
          actionButton("valid_bornes_ac_id",label="Rafraichir la carte", icon=icon("refresh"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        })

        # LEGENDE

        output$titre_classes_legende_ac <- renderUI({
          textInput("titre_classes_legende_ac_id", label = h5("Titre de la l\u00e9gende des classes"), value = "")
        })

        output$affiche_legende_ac <- renderUI({
          checkboxInput("affiche_legende_ac_id", label = "Activer le d\u00e9placement de la l\u00e9gende au clic",
                        value = TRUE)
        })

        output$type_legende_ac <- renderUI({
          radioButtons("type_legende_ac_id", label = h5("Type de l\u00e9gende"),
                       choices = list("Litterale" = 1, "En echelle" = 2),
                       selected = 1, inline = TRUE)
        })

        # SAUVEGARDE

        output$save_carte_ac <- renderUI({
          actionButton("save_carte_ac_id", label=HTML("<font size=3>Sauvegarder la carte dans un onglet</font>"), style="color:#FFFFFF; background-color:#DF691A")
        })

        output$sortie_qgis_ac <- renderUI({
          tags$div(class="input-group",
                   HTML('<input type="text" id="sortie_qgis_ac_id" class="form-control" placeholder="Nom du projet" aria-describedby="sortie_qgis_ac_id">
                        <span class="input-group-addon" id="sortie_qgis_ac_id">.qgs</span>'))
        })

        output$titre1_qgis_ac <- renderUI({
          textInput("titre1_qgis_ac_id", label = h5("Titre informatif"), value = "", placeholder= "Facultatif")
        })

        output$titre2_qgis_ac <- renderUI({
          textInput("titre2_qgis_ac_id", label = h5("Titre descriptif"), value = "", placeholder= "Facultatif")
        })

        output$source_qgis_ac <- renderUI({
          textInput("source_qgis_ac_id", label = h5("Source de la carte"), value = sourc)
        })

        output$aide_image_ac <- renderUI({
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
      observeEvent(list(input$monter_fond_ac_id,input$descendre_fond_ac_id),{

        ordre <- c()
        if(as.numeric(input$monter_fond_ac_id)>nb_up$a)
        {
          ordre <- 2
          nb_up$a <- nb_up$a+1
        }
        if(as.numeric(input$descendre_fond_ac_id)>nb_down$a)
        {
          ordre <- 1
          nb_down$a <- nb_down$a+1
        }

        if(is.null(input$ordre_fonds_ac_id)) pos_select <- 0 else pos_select <- which(liste_fonds_ac$a==input$ordre_fonds_ac_id)

        if(pos_select>0)
        {
          if(pos_select==ordre) liste_fonds_ac$a <- liste_fonds_ac$a[c(2,1)]

          updateSelectInput(session, "ordre_fonds_ac_id",
                            choices = liste_fonds_ac$a,
                            selected = input$ordre_fonds_ac_id
          )
        }
      },ignoreInit = TRUE)

      # Pour modifier l'ordre des fonds, ici monter un fond
      observeEvent(input$monter_fond_ac_id,{
        if(is.null(input$ordre_fonds_ac_id)) pos_select <- 1 else pos_select <- which(liste_fonds_ac$a==input$ordre_fonds_ac_id)

        if(length(pos_select)==1)
        {
          if(pos_select==2) liste_fonds_ac$a <- liste_fonds_ac$a[c(2,1)]
        }
        updateSelectInput(session, "ordre_fonds_ac_id",
                          choices = liste_fonds_ac$a,
                          selected = input$ordre_fonds_ac_id
        )
      }, ignoreInit = TRUE)

      # Pour modifier l'ordre des fonds, ici descendre un fond
      observeEvent(input$descendre_fond_ac_id,{
        if(is.null(input$ordre_fonds_ac_id)) pos_select <- 3 else pos_select <- which(liste_fonds_ac$a==input$ordre_fonds_ac_id)

        if(length(pos_select)==1)
        {
          if(pos_select==1) liste_fonds_ac$a <- liste_fonds_ac$a[c(2,1)]
        }
        updateSelectInput(session, "ordre_fonds_ac_id",
                          choices = liste_fonds_ac$a,
                          selected = input$ordre_fonds_ac_id
        )
      }, ignoreInit = TRUE)

      # Pour calculer les bornes des classes
      react_bornes_ac <- reactive({
        if(is.null(input$nb_classes_ac_id) | is.null(input$methode_ac_id))
        {
          max_classes$a <- 3
          methode <- "kmeans"
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
        }else if(input$nb_classes_ac_id=="" | input$methode_ac_id=="")
        {
          max_classes$a <- 3
          methode <- "kmeans"
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
        }else
        {
          max_classes$a <- as.numeric(input$nb_classes_ac_id)
          if(is.na(max_classes$a)) return(NULL)
          methode <- as.character(input$methode_ac_id)
          if(!methode %in% c("manuel"))
          {
            suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),max_classes$a,style=methode,rtimes=10,intervalClosure="left"))
          }
        }
        if(methode!="manuel")
        {
          carac_bornes <- calcul_bornes(analyse_ac()$donnees,bornes_analyse,varRatio,max_classes$a,methode,input$palette_insee_ac_id)
        }else if(methode=="manuel")
        {
          carac_bornes <- react_bornes_manuel_1_ac()
        }
        
        return(carac_bornes) #list(bornes=carac_bornes[[1]],pal_classes=carac_bornes[[2]])
      })
      
      # Pour calculer les bornes des classes
      react_bornes_init_ac <- reactive({
        
        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),3,style="kmeans",rtimes=10,intervalClosure="left"))
        if(min(bornes_analyse$brks)<0 & max(bornes_analyse$brks)>=0)
        {
          palette_init <- "Insee_Rouge"
        }else
        {
          palette_init <- "Insee_Bleu"
        }
        carac_bornes <- calcul_bornes(analyse_ac()$donnees,bornes_analyse,varRatio,3,"kmeans",palette_init)
        return(carac_bornes) #list(bornes=carac_bornes[[1]],pal_classes=carac_bornes[[2]])
      })
      
      # Pour mettre à jour les bornes en mode manuel
      react_bornes_manuel_1_ac <- eventReactive(input$valid_bornes_ac_id,{
        
        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),max_classes$a,style="kmeans",rtimes=10,intervalClosure="left"))
        bornes <- vector()
        for (i in 1:(as.numeric(input$nb_classes_ac_id)-1)) {
          bornes<-c(bornes,input[[paste0("bornes_", i,"_ac_id")]])
        }
        
        bornes_analyse$brks <- c(min(bornes_analyse$brks), bornes, max(bornes_analyse$brks))
        
        carac_bornes <- calcul_bornes(analyse_ac()$donnees,bornes_analyse,varRatio,input$nb_classes_ac_id,input$methode_ac_id,input$palette_insee_ac_id)
        
        bornes <- c(carac_bornes[[1]][1],bornes,carac_bornes[[1]][length(carac_bornes[[1]])])
        bornes <- sort(unique(bornes),decreasing = T)
        
        carac_bornes[[1]] <- bornes
        
        return(carac_bornes)
      },ignoreNULL = TRUE)
      
      # Pour mettre à jour les bornes dans la distribution
      observeEvent(input$valid_slider_bornes_ac_id,{
        updateSelectInput(session, inputId = "methode_ac_id", selected = "manuel")
        for (i in 0:(as.numeric(input$nb_classes_ac_id))+1) {
          updateNumericInput(session, inputId = paste0("bornes_", i,"_ac_id"), value = input[[paste0("slider_bornes_", i,"_ac_id")]])
        }
      },ignoreInit = TRUE)

      # Pour renvoyer la fourchette de classes possible
      nb_classes_ac <- reactive({
        if(is.null(varRatio)) return(NULL)
        
        donnees <- analyse_ac()$donnees[,varRatio]
        
        suppressWarnings(
          if(min(donnees)<0 & max(donnees)>0) # Si + et -
          {
            if(length(donnees)>3 & length(donnees)<9)
            {
              max_classes <- c(3:(length(donnees)-1))
            }else
            {
              max_classes <- c(3:9)
            }
            max_palettes <- c("Insee_Rouge","Insee_Jaune")
          }else if(min(donnees)>0) # Si tout +
          {
            if(length(donnees)>3 & length(donnees)<6)
            {
              max_classes <- c(3:(length(donnees)-1))
            }else
            {
              max_classes <- c(3:6)
            }
            max_palettes <- c("Insee_Bleu","Insee_Jaune","Insee_Rouge","Insee_Violet","Insee_Turquoise","Insee_Vert","Insee_Gris")
          }else if(max(donnees)<0) # Si tout -
          {
            if(length(donnees)>3 & length(donnees)<6)
            {
              max_classes <- c(3:(length(donnees)-1))
            }else
            {
              max_classes <- c(3:6)
            }
            max_palettes <- c("Insee_Bleu","Violet_Neg","Turquoise_Neg","Vert_Neg","Gris_Neg")
          }
        )
        return(list(max_classes,max_palettes))
      })
      observe({nb_classes_ac()})

      # Pour exporter la carte en projet Qgis

      output$export_qgis_ac <- renderUI({
        downloadButton("downloadProjetQgis_ac", label="Exporter")
      })

      output$downloadProjetQgis_ac <- downloadHandler(contentType = "zip",
                                                      filename = function(){
                                                        paste0(input$sortie_qgis_ac_id,".zip")
                                                      },
                                                      content = function(file){
                                                        owd <- setwd(tempdir())
                                                        on.exit(setwd(owd))
                                                        
                                                        rep_sortie <- dirname(file)
                                                        
                                                        dir.create("layers",showWarnings = F)
                                                        
                                                        files <- EXPORT_PROJET_QGIS_AC(file)
                                                        
                                                        zip::zip(zipfile = paste0("./",basename(file)),
                                                                 files = files,
                                                                 mode = "cherry-pick")
                                                      }
      )

      EXPORT_PROJET_QGIS_AC <- function(file)
      {
        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i> <font size=+1>Export du projet Qgis en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        sortie <- input$sortie_qgis_ac_id
        
        files <- c("layers", paste0(sortie,".qgs"))
        
        rep_sortie <- dirname(file)

        if(is.null(input$nb_classes_ac_id))
        {
          max_classes <- 4
        }else
        {
          max_classes <- input$nb_classes_ac_id
        }
        
        if(!is.null(lon_lat_ac()[[1]]))
        {
          suppressWarnings(test_affiche_leg <- try(table_classe <- data.frame(classe=c(max_classes:1),label=legende$a,couleurs=analyse_leg_ac()$pal_classes, stringsAsFactors = F),silent=TRUE))
          if(class(test_affiche_leg) %in% "try-error")
          {
            showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
            return(NULL)
          }else
          {
            table_classe <- data.frame(classe=c(max_classes:1),label=legende$a,couleurs=analyse_leg_ac()$pal_classes, stringsAsFactors = F)
          }
        }else
        {
          showModal(modalDialog(HTML("<font size=+1><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> d'abord sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }
        
        if(elargi_ac())
        {
          analyse_donnees_elargi <- analyse_ac()[[3]] #donnees elargi
          analyse_maille_elargi <- analyse_ac()[[4]] #fond maille elargi

          names_donnees_elargi <- names(analyse_donnees_elargi)
          analyse_donnees_elargi <- data.frame(analyse_donnees_elargi,val=analyse_donnees_elargi[,varRatio],classe=palette_ac()[[1]](analyse_donnees_elargi[,varRatio]))
          names(analyse_donnees_elargi) <- c(names_donnees_elargi,"val","classe")

          analyse_classes_elargi <- merge(table_classe,analyse_donnees_elargi,by.x="couleurs",by.y="classe")

          analyse_classes_elargi <- analyse_classes_elargi[,c("CODE","LIBELLE",varRatio,"val","classe")]

          analyse_classes_elargi <- analyse_classes_elargi[order(analyse_classes_elargi[,varRatio],decreasing = T),]

          analyse_maille_elargi <- merge(analyse_maille_elargi,analyse_classes_elargi[,c("CODE",varRatio,"val","classe")],by="CODE")
          analyse_maille_elargi <- analyse_maille_elargi[,c("CODE","LIBELLE",varRatio,"val","classe","geometry")]
          analyse_maille_elargi <- st_sf(analyse_maille_elargi,stringsAsFactors = FALSE)

          fond_classes_elargi <- analyse_maille_elargi

          fond_classes_elargi <- st_transform(fond_classes_elargi, crs= as.numeric(code_epsg_ac()))

          fond_maille_elargi <- st_transform(fond_elargi_ac(), crs= as.numeric(code_epsg_ac()))

          suppressWarnings(st_write(fond_classes_elargi, paste0(rep_sortie,"/layers/fond_maille_elargi_carte.shp"), delete_dsn = TRUE, quiet = TRUE))
          suppressWarnings(st_write(fond_maille_elargi,paste0(rep_sortie,"/layers/fond_maille_elargi.shp"), delete_dsn = TRUE, quiet = TRUE))
        }
        
        analyse_donnees <- analyse_ac()[[1]] #donnees
        analyse_maille <- analyse_ac()[[2]] #fond maille

        names_donnees <- names(analyse_donnees)
        analyse_donnees <- data.frame(analyse_donnees,val=analyse_donnees[,varRatio],classe=palette_ac()[[1]](analyse_donnees[,varRatio]))
        names(analyse_donnees) <- c(names_donnees,"val","classe")
        analyse_classes <- merge(table_classe,analyse_donnees,by.x="couleurs",by.y="classe")
        analyse_classes <- analyse_classes[,c("CODE","LIBELLE",varRatio,"val","classe")]
        analyse_classes <- analyse_classes[order(analyse_classes[,varRatio],decreasing = T),]
        analyse_maille <- merge(analyse_maille,analyse_classes[,c("CODE",varRatio,"val","classe")],by="CODE")
        analyse_maille <- analyse_maille[,c("CODE","LIBELLE",varRatio,"val","classe","geometry")]
        analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
        fond_classes <- analyse_maille

        fond_contour <- st_transform(fondContour, crs= as.numeric(code_epsg_ac()))
        if(!is.null(fondSuppl) && input$ajout_territoire_ac_id) fond_territoire <- st_transform(fond_territoire_ac(), crs= as.numeric(code_epsg_ac()))
        if(input$ajout_dep_ac_id) fond_departement <- st_transform(fond_departement_ac(), crs= as.numeric(code_epsg_ac()))
        if(input$ajout_reg_ac_id) fond_region <- st_transform(fond_region_ac(), crs= as.numeric(code_epsg_ac()))
        fond_france <- st_transform(fond_habillage_ac()[[1]], crs= as.numeric(code_epsg_ac()))
        fond_pays <- st_transform(fond_habillage_ac()[[2]], crs= as.numeric(code_epsg_ac()))
        
        suppressWarnings(st_write(fond_classes, paste0(rep_sortie,"/layers/fond_maille_carte.shp"), delete_dsn = TRUE, quiet = TRUE))
        suppressWarnings(st_write(fond_contour,paste0(rep_sortie,"/layers/fond_contour.shp"), delete_dsn = TRUE, quiet = TRUE))
        if(exists("fond_territoire")) if(!is.null(fond_territoire)) suppressWarnings(st_write(fond_territoire, paste0(rep_sortie,"/layers/fond_territoire.shp"), delete_dsn = TRUE, quiet = TRUE))
        if(exists("fond_departement")) if(!is.null(fond_departement)) suppressWarnings(st_write(fond_departement, paste0(rep_sortie,"/layers/fond_departement.shp"), delete_dsn = TRUE, quiet = TRUE))
        if(exists("fond_region")) if(!is.null(fond_region)) suppressWarnings(st_write(fond_region,paste0(rep_sortie,"/layers/fond_region.shp"), delete_dsn = TRUE, quiet = TRUE))
        suppressWarnings(st_write(fond_france,paste0(rep_sortie,"/layers/fond_france.shp"), delete_dsn = TRUE, quiet = TRUE))
        if(exists("fond_pays")) if(!is.null(fond_pays)) suppressWarnings(st_write(fond_pays,paste0(rep_sortie,"/layers/fond_pays.shp"), delete_dsn = TRUE, quiet = TRUE))
        
        titre1 <- paste0(input$titre1_qgis_ac_id,"\n")
        titre2 <- input$titre2_qgis_ac_id
        source <- input$source_qgis_ac_id
        annee <- format(Sys.time(), format = "%Y")
        variable_a_representer <- varRatio
        titre_leg_ronds <- ""
        titre_leg_classes <- input$titre_classes_legende_ac_id

        l <- c()
        if(exists("fond_territoire")) l <- "fond_territoire"
        if(exists("fond_departement")) l <- c(l,"fond_departement")
        if(exists("fond_region")) l <- c(l,"fond_region")

        if(elargi_ac())
        {
          l=c("fond_france",
              "fond_contour",
              l,
              "fond_maille_carte",
              "fond_maille_elargi_carte"
          )
        }else
        {
          l=c("fond_france",
              "fond_contour",
              l,
              "fond_maille_carte"
          )
        }

        l <- c(l,"fond_pays")
        
        export_projet_qgis_classes(l,rep_sortie,sortie,titre1,titre2,source,titre_leg_classes,table_classe,variable_a_representer,annee)

        removeModal()
        
        showModal(modalDialog(HTML(paste0("<font size=+1>Le projet Qgis a \u00e9t\u00e9 cr","\u00e9","ee.</font>")), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        return(files)
      }

      elargi_ac <- reactive({
        if(is.null(input$elargi_ac_id))
        {
          elargi <- FALSE
        }else
        {
          elargi <- input$elargi_ac_id
        }
        return(elargi)
      })

      code_epsg_ac <- reactive({
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

      analyse_ac <- reactive({

        suppressWarnings(test_k_classes <- try(k_classes(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varRatio,elargi_ac()),silent=T))
        if(class(test_k_classes) %in% "try-error")
        {
          return(NULL)
        }else
        {
          analyse <- k_classes(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varRatio,elargi_ac())
        }

        if(elargi_ac())
        {
          analyse <- list(donnees=analyse[[1]],fond_maille=fondMaille,donnees_elargi=analyse[[2]],fond_maille_elargi=fondMailleElargi)
        }else
        {
          analyse <- list(donnees=analyse[[1]],fond_maille=fondMaille)
        }

        if(is.null(analyse$donnees))
        {
          showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          erreur_maille$a <- TRUE
          return(NULL)
        }

        analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(round(analyse$donnees[,varRatio],3), big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        if(elargi_ac())
        {
          analyse$donnees_elargi[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(round(analyse$donnees_elargi[,varRatio],3), big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
        }

        return(analyse)
      })

      analyse_leg_ac <- reactive({
        analyse <- analyse_ac()
        analyse$rupture_classes <- palette_ac()[[2]] #bornes
        analyse$pal_classes <- rev(palette_ac()[[3]]) # pal_classes
        return(analyse)
      })

      fond_habillage_ac <- reactive({

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

      fond_contour_maille_ac <- reactive({

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

      fond_elargi_ac <- reactive({
        if(elargi_ac())
        {
          maille_WGS84_elargi <- st_transform(fondMailleElargi,crs=4326)
        }
        return(maille_WGS84_elargi)
      })

      list_bbox_ac <- reactive({
        req(fond_contour_maille_ac())

        list_bbox <- list(c(st_bbox(fond_contour_maille_ac()[[1]])[1],st_bbox(fond_contour_maille_ac()[[1]])[3]),c(st_bbox(fond_contour_maille_ac()[[1]])[2],st_bbox(fond_contour_maille_ac()[[1]])[4]))
        return(list_bbox)
      })

      palette_ac <- reactive({
        bornes <- react_bornes_ac()[[1]]

        if(is.null(bornes)) return(NULL)

        if(elargi_ac()) # On redefini le min et le max de la serie pour eviter les valeurs en NA
        {
          bornes[length(bornes)] <- min(as.numeric(analyse_ac()$donnees_elargi[,varRatio]))
          bornes[1] <- max(as.numeric(analyse_ac()$donnees_elargi[,varRatio]), na.rm = TRUE)
        }else
        {
          bornes[length(bornes)] <- min(as.numeric(analyse_ac()$donnees[,varRatio]))
          bornes[1] <- max(as.numeric(analyse_ac()$donnees[,varRatio]), na.rm = TRUE)
        }

        if(length(unique(bornes)) != length(bornes))
        {
          removeModal()
          showModal(modalDialog(HTML(paste0("<font size=+1>Les bornes calcul\u00e9es avec la methode '",input$methode_ac_id,"' ne sont pas uniques. La methode kmeans a donc \u00e9t\u00e9 retenue.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          Sys.sleep(7)
          suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse_ac()$donnees[,varRatio]),max_classes$a,style="kmeans",rtimes=10,intervalClosure="left"))
          carac_bornes <- calcul_bornes(analyse_ac()$donnees,bornes_analyse,varRatio,max_classes$a,"kmeans",input$palette_insee_ac_id)
          updateSelectInput(session,"methode_ac_id",choices = methode_calcul, selected="kmeans")
          bornes <- carac_bornes[[1]]
          pal_classes <- carac_bornes[[2]]
        }else
        {
          pal_classes <- react_bornes_ac()[[2]]
        }
        pal_classes[is.na(pal_classes)] <- "grey"
        palette<-colorBin(palette=pal_classes, domain=0:100, bins=bornes, na.color="grey")

        return(list(palette,bornes,pal_classes))
      })

      palette_init_ac <- reactive({

        bornes <- react_bornes_init_ac()[[1]]

        if(is.null(bornes)) return(NULL)

        test_bornes <- try(bornes[length(bornes)] <- min(as.numeric(analyse_ac()$donnees[,varRatio])), silent=TRUE)

        if(class(test_bornes) %in% "try-error")
        {
          return(NULL)
        }else
        {
          bornes[1] <- max(as.numeric(analyse_ac()$donnees[,varRatio]), na.rm = TRUE)
        }

        pal_classes <- react_bornes_init_ac()[[2]]

        pal_classes[is.na(pal_classes)] <- "grey"
        palette<-colorBin(palette=pal_classes, domain=0:100, bins=bornes, na.color="grey")

        return(list(palette,bornes,pal_classes))
      })

      fond_territoire_ac <- reactive({
        if(!is.null(fondSuppl))
        {
          fond_territoire <- st_transform(fondSuppl,crs=4326)
          return(fond_territoire)
        }else
        {
          return(NULL)
        }
      })

      fond_region_ac <- reactive({
        fond_region <- st_transform(sf_regm(),crs=4326)
        return(fond_region)
      })

      fond_departement_ac <- reactive({
        fond_departement <- st_transform(sf_depm(),crs=4326)
        return(fond_departement)
      })

      fond_select_maille_elargi_ac <- reactive({
        req(fond_elargi_ac())

        fond_maille_elargi <- fond_elargi_ac()[as.data.frame(fond_elargi_ac())[,"CODE"] %in% as.data.frame(fondMailleElargi)[input$mymaille_ac_rows_selected,"CODE"],]
        return(fond_maille_elargi)
      })

      fond_select_maille_ac <- reactive({
        req(fond_contour_maille_ac())

        fond_maille <- fond_contour_maille_ac()[[2]][as.data.frame(fond_contour_maille_ac()[[2]])[,"CODE"] %in% as.data.frame(fondMaille)[input$mymaille_ac_rows_selected,"CODE"],]
        return(fond_maille)
      })

      fond_select_contour_ac <- reactive({
        req(fond_contour_maille_ac())

        fond_contour <- fond_contour_maille_ac()[[1]][as.data.frame(fond_contour_maille_ac()[[1]])[,"CODE"] %in% as.data.frame(fondContour)[input$mycontour_ac_rows_selected,"CODE"],]
        return(fond_contour)
      })

      # CONSTRUCTION DE LA MAP EN LEAFLET

      react_fond_ac <- reactive({

        if(input$menu=="carte")
        {
          showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>\u00c9laboration de la carte...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

          if(is.null(fondEtranger))
          {
            proj4 <- st_crs(fondMaille)$proj4string
          }else{
            proj4 <- st_crs(fondEtranger)$proj4string
          }
          
          # Construction de la map par defaut

          m <- leaflet(padding = 0,
                       options = leafletOptions(
                         preferCanvas = TRUE,
                         transition = 2,
                         crs = leafletCRS(crsClass = "L.Proj.CRS",
                                          code = paste0("EPSG:", code_epsg_ac()),
                                          proj4def = proj4,
                                          resolutions = 2^(16:1)
                         )
                       )) %>%

            setMapWidgetStyle(list(background = "#AFC9E0")) %>%

            addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">OCEANIS - \u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%

            fitBounds(lng1 = min(list_bbox_ac()[[1]]),
                      lat1 = min(list_bbox_ac()[[2]]),
                      lng2 = max(list_bbox_ac()[[1]]),
                      lat2 = max(list_bbox_ac()[[2]])
            ) %>%

            # On ajoute une barre d'echelle
            addScaleBar(position = 'bottomright',
                        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
            ) %>%

            # Pour gerer l'ordre des calques
            addMapPane(name = "fond_pays", zIndex = 401) %>%
            addMapPane(name = "fond_france", zIndex = 402) %>%
            addMapPane(name = "fond_dep", zIndex = 403) %>%
            addMapPane(name = "fond_reg", zIndex = 404) %>%
            addMapPane(name = "fond_territoire", zIndex = 405) %>%
            addMapPane(name = "fond_duo2", zIndex = 406) %>%
            addMapPane(name = "fond_duo1", zIndex = 407) %>%
            addMapPane(name = "selection", zIndex = 408) %>%

            addMapPane(name = "fond_legende", zIndex = 409)

          # AFFICHAGE DES FONDS D'HABILLAGE

          if(emprise %in% c("FRM","973"))
          {
            # fond des pays
            m <- addPolygons(map = m, data = fond_habillage_ac()[[2]][,"LIBGEO"], opacity = 1, # sauf la France
                             stroke = TRUE, color = "white",
                             weight = 1,
                             options = pathOptions(pane = "fond_pays", clickable = F),
                             fill = T, fillColor = "#CCCCCC", fillOpacity = 1
            )
          }

          # fond de la France metro, DOM ou etranger
          m <- addPolygons(map = m, data = fond_habillage_ac()[[1]][,"LIBGEO"], opacity = 1,
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           options = pathOptions(pane = "fond_france", clickable = F),
                           fill = T, fillColor = "white", fillOpacity = 1
          )

          m_save_ac$a <- m

          # AFFICHAGE DU FOND TERRITOIRE

          if(!is.null(fondSuppl))
          {
            m <- addPolygons(map = m, data = fond_territoire_ac(),
                             stroke = TRUE, color = "#BFBFBF", opacity = 1,
                             weight = 0.5,
                             options = pathOptions(pane = "fond_territoire", clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ac())[,"LIBELLE"], "</font> </b>"),
                             fill = T, fillColor = "white", fillOpacity = 0.001,
                             group = "territoire"
            )
          }
          # AFFICHAGE DU FOND CONTOUR
          m <- addPolygons(map = m, data = fond_contour_maille_ac()[[1]], opacity = 0.3,
                           stroke = TRUE, color = "black", weight = 3,
                           options = pathOptions(pane = "fond_duo2", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ac()[[1]])[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.3,
                           group = "duo"
          )

          # AFFICHAGE DE LA MAILLE ET DE L'ANALYSE
          suppressWarnings(test_analyse_maille_classe <- try(analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio],silent=T))
          if(class(test_analyse_maille_classe) %in% "try-error")
          {
            return(NULL)
          }else
          {
            analyse_maille_classe <- analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio]
          }

          analyse_maille <- merge(fond_contour_maille_ac()[[2]][,c("CODE","geometry")],analyse_ac()$donnees[,c("CODE","LIBELLE",varRatio,"TXT1")],by="CODE")
          names(analyse_maille)[3] <- varRatio
          analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varRatio])),]
          analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)

          m <- addPolygons(map = m, data = analyse_maille, opacity = 1,
                           stroke = TRUE, color = "white", weight = 1,
                           options = pathOptions(pane = "fond_duo1", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                           fill = T,
                           fillColor = palette_init_ac()[[1]](analyse_maille_classe),
                           fillOpacity = 1,
                           group = "duo"
          )
          
          removeModal()

          showModal(modalDialog(HTML("<font size=+1>Veuillez patientez svp, la carte va s'afficher dans quelques secondes...<br><br><i class=\"fa fa-hand-pointer-o fa-fw\"></i><b>Double-cliquez</b> ensuite sur la carte pour afficher la l\u00e9gende.</font> "), size="m", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7

          return(m)
        }
      })

      # MODIFICATION DES FONDS TERRITOIRE D'ETUDES, REGION ET DEPARTEMENT

      observeEvent(input$ajout_territoire_ac_id,{

        proxy <- leafletProxy("mymap_ac")

        clearGroup(map = proxy, group = "territoire")

        #fond du territoire d'etudes
        if(!is.null(fondSuppl))
        {
          if(input$ajout_territoire_ac_id)
          {
            proxy <- addPolygons(map = proxy, data = fond_territoire_ac(),
                                 stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                 weight = 0.5,
                                 options = pathOptions(pane = "fond_territoire", clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire_ac())[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.001,
                                 group = "territoire"
            )
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$ajout_reg_ac_id,{

        proxy <- leafletProxy("mymap_ac")

        clearGroup(map = proxy, group = "region")

        if(emprise=="FRM")
        {
          if(input$ajout_reg_ac_id)
          {
            #fond des regions
            proxy <- addPolygons(map = proxy, data = fond_region_ac(),
                                 stroke = TRUE, color = "grey", opacity = 1,
                                 weight = 1.5,
                                 options = pathOptions(pane = "fond_reg", clickable = F),
                                 fill = F,
                                 group = "region"
            )
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$ajout_dep_ac_id,{

        proxy <- leafletProxy("mymap_ac")

        clearGroup(map = proxy, group = "departement")

        if(emprise=="FRM")
        {
          if(input$ajout_dep_ac_id)
          {
            #fond des departements
            proxy <- addPolygons(map = proxy, data = fond_departement_ac(),
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

      observeEvent(list(input$monter_fond_ac_id,input$descendre_fond_ac_id),{

        if(as.numeric(input$monter_fond_ac_id)==0 & as.numeric(input$descendre_fond_ac_id)==0) return(NULL)

        proxy <- leafletProxy("mymap_ac")

        clearGroup(map = proxy, group = "contour")
        clearGroup(map = proxy, group = "classe")

        i <- 1 #pour gerer l'ordre des fonds dans le pane

        for(fond in liste_fonds_ac$a)
        {
          if(fond=="analyse/maille")
          {
            suppressWarnings(test_analyse_maille_classe <- try(analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio],silent=T))
            if(class(test_analyse_maille_classe) %in% "try-error")
            {
              return(NULL)
            }else
            {
              analyse_maille_classe <- analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio]
            }

            analyse_maille <- merge(fond_contour_maille_ac()[[2]][,c("CODE","geometry")],analyse_ac()$donnees[,c("CODE","LIBELLE",varRatio,"TXT1")],by="CODE")
            names(analyse_maille)[3] <- varRatio
            analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varRatio])),]
            analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)

            proxy <- addPolygons(map = proxy, data = analyse_maille, opacity = 1,
                                 stroke = TRUE, color = "white", weight = 1,
                                 options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                                 fill = T,
                                 fillColor = palette_ac()[[1]](analyse_maille_classe),
                                 fillOpacity = 1,
                                 group = "classe"
            )

            ordre_analyse$a <- i
          }

          if(fond=="contour")
          {
            proxy <- addPolygons(map = proxy, data = fond_contour_maille_ac()[[1]], opacity = 0.3,
                                 stroke = TRUE, color = "black", weight = 3,
                                 options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                 popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_contour_maille_ac()[[1]])[,"LIBELLE"], "</font> </b>"),
                                 fill = T, fillColor = "white", fillOpacity = 0.3,
                                 group = "contour"
            )
          }

          i <- i + 1
        }
      },ignoreInit = TRUE)

      # MODIFICATION DU NOMBRE DE CLASSES, DE LA METHODE, DES BORNES OU DE LA PALETTE

      observeEvent(list(input$nb_classes_ac_id,input$methode_ac_id,input$valid_bornes_ac_id,input$palette_insee_ac_id),{
        req(input$nb_classes_ac_id,input$methode_ac_id,input$palette_insee_ac_id)

        proxy <- leafletProxy("mymap_ac")

        clearGroup(map = proxy, group = "classe")

        suppressWarnings(test_analyse_maille_classe <- try(analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio],silent=T))
        if(class(test_analyse_maille_classe) %in% "try-error")
        {
          return(NULL)
        }else
        {
          analyse_maille_classe <- analyse_ac()$donnees[rev(order(analyse_ac()[[1]][,varRatio])),varRatio]
        }

        analyse_maille <- merge(fond_contour_maille_ac()[[2]][,c("CODE","geometry")],analyse_ac()$donnees[,c("CODE","LIBELLE",varRatio,"TXT1")],by="CODE")
        names(analyse_maille)[3] <- varRatio
        analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varRatio])),]
        analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)

        proxy <- addPolygons(map = proxy, data = analyse_maille, opacity = 1,
                             stroke = TRUE, color = "white", weight = 1,
                             options = pathOptions(pane = paste0("fond_duo",ordre_analyse$a), clickable = T),
                             popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                             fill = T,
                             fillColor = palette_ac()[[1]](analyse_maille_classe),
                             fillOpacity = 1,
                             group = "classe"
        )

      },ignoreInit = TRUE)

      # MODIFICATION DE LA REPRESENTATION ELARGIE

      observeEvent(list(input$elargi_ac_id,input$opacite_elargi_ac_id,input$nb_classes_ac_id,input$methode_ac_id,input$valid_bornes_ac_id,input$palette_insee_ac_id),{
        req(input$opacite_elargi_ac_id,input$nb_classes_ac_id,input$methode_ac_id,input$palette_insee_ac_id)

        proxy <- leafletProxy("mymap_ac")

        clearGroup(map = proxy, group = "elargi")

        if(elargi_ac())
        {
          analyse_maille_classe_elargi <- analyse_ac()$donnees_elargi[rev(order(analyse_ac()$donnees_elargi[,varRatio])),varRatio]

          analyse_maille_elargi <- merge(fond_elargi_ac()[,c("CODE","geometry")],analyse_ac()$donnees_elargi[,c("CODE","LIBELLE",varRatio,"TXT1")],by="CODE")
          names(analyse_maille_elargi)[3] <- varRatio
          analyse_maille_elargi <- analyse_maille_elargi[rev(order(as.data.frame(analyse_maille_elargi)[,varRatio])),]
          analyse_maille_elargi <- st_sf(analyse_maille_elargi,stringsAsFactors = FALSE)

          proxy <- addPolygons(map = proxy, data = analyse_maille_elargi, opacity = input$opacite_elargi_ac_id/100,
                               stroke = TRUE, color = "white", weight = 1,
                               options = pathOptions(pane = "fond_duo2", clickable = T),
                               popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT1),
                               fill = T,
                               fillColor = palette_ac()[[1]](analyse_maille_classe_elargi),
                               fillOpacity = input$opacite_elargi_ac_id/100,
                               group = "elargi"
          )
        }
      },ignoreInit = TRUE)

      # MODIFICATION DE LA SELECTION DES OBJETS VIA LES TABLEAUX

      observeEvent(input$onglets_ac,{

        if(input$onglets_ac == "carte")
        {
          proxy <- leafletProxy("mymap_ac")

          clearGroup(map = proxy, group = "select_donnees")

          if(!is.null(input$mymaille_ac_rows_selected))
          {
            if(elargi_ac())
            {
              proxy <- addPolygons(map = proxy, data = fond_select_maille_elargi_ac(),
                                   stroke = TRUE, weight = 3,
                                   color="#FFFF00",
                                   options = pathOptions(pane = "selection", clickable = F),
                                   fill = F,
                                   group = "select_donnees"
              )
            }else
            {
              proxy <- addPolygons(map = proxy, data = fond_select_maille_ac(),
                                   stroke = TRUE, weight = 3,
                                   color="#FFFF00",
                                   options = pathOptions(pane = "selection", clickable = F),
                                   fill = F,
                                   group = "select_donnees"
              )
            }
          }
        }
      },ignoreInit = TRUE)

      observeEvent(input$onglets_ac,{

        if(input$onglets_ac == "carte")
        {
          proxy <- leafletProxy("mymap_ac")

          clearGroup(map = proxy, group = "select_contour")

          if(!is.null(input$mycontour_ac_rows_selected))
          {
            proxy <- addPolygons(map = proxy, data = fond_select_contour_ac(),
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

      lon_lat_ac <- reactive({
        click <- input$mymap_ac_click
        lon <- click$lng
        lat <- click$lat
        return(list(lon,lat))
      })

      observeEvent(list(input$mymap_ac_click,input$type_legende_ac_id,input$titre_classes_legende_ac_id,input$nb_classes_ac_id,input$methode_ac_id,input$palette_insee_ac_id,input$valid_bornes_ac_id),{
        if(is.null(input$affiche_legende_ac_id)) return(NULL)

        if(input$affiche_legende_ac_id==FALSE) return(NULL)

        if(is.null(lon_lat_ac()[[1]])) return(NULL)

        proxy <- leafletProxy("mymap_ac")

        proxy <- clearGroup(map=proxy, group="leg")
        proxy <- clearMarkers(map=proxy)

        pt <- st_sfc(st_geometry(st_point(c(lon_lat_ac()[[1]],lon_lat_ac()[[2]]))), crs = 4326)
        pt <- st_transform(pt, crs = as.numeric(code_epsg_ac()))
        coord_pt <- st_coordinates(pt)[1:2]
        
        position_leg <- t(data.frame(c(coord_pt[1],coord_pt[2])))

        if(is.null(input$type_legende_ac_id)) return(NULL)
        
        if(input$type_legende_ac_id==1) # Litterale
        {
          # On cree les rectangles
          if(is.null(input$nb_classes_ac_id)) return(NULL)
          
          max_classes <- as.numeric(input$nb_classes_ac_id)

          large <- as.numeric((st_bbox(fondMaille)[4] - st_bbox(fondMaille)[2]) / 20)
          
          for(i in 1:max_classes)
          {
            # Coordonnees du point haut/gauche des rectangles de la legende
            x_coord_rectangle <- position_leg[1]
            if(i==1) #1er rectangle
            {
              y_coord_rectangle <- position_leg[2]
            }else
            {
              y_coord_rectangle <- y_coord_rectangle - large - large / 4
            }
            assign(paste0("rectangle_",i),st_sfc(st_polygon(list(matrix(c(x_coord_rectangle,               y_coord_rectangle,
                                                                          x_coord_rectangle + large * 1.5, y_coord_rectangle,
                                                                          x_coord_rectangle + large * 1.5, y_coord_rectangle - large,
                                                                          x_coord_rectangle,               y_coord_rectangle - large,
                                                                          x_coord_rectangle,               y_coord_rectangle),
                                                                        ncol=2, byrow=TRUE))),
                                                 crs = as.numeric(code_epsg_ac())))
          }

          # On ajoute un cadre blanc autour de la legende
          
          # leaflet rectangles et valeurs classes
          classes_leg_texte <- analyse_leg_ac()$rupture_classes
          
          label_rectangle <- c()
          legende$a <- c()
          for(i in 1:max_classes)
          {
            if(i==1)
            {
              lbl <- paste0(format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," et plus")
              label_rectangle <- c(label_rectangle, lbl)
            }else if (i>1 && i<max_classes)
            {
              lbl <- paste0("De ", format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
              label_rectangle <- c(label_rectangle, lbl)
            }else #i==length(max_classes)
            {
              lbl <- paste0("Moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
              label_rectangle <- c(label_rectangle, lbl)
            }
            legende$a <- c(legende$a,lbl)
          }
          
          ltext <- max(nchar(label_rectangle)) / 2.5
          
          vec <- matrix(c(position_leg[1] - large / 2,                     position_leg[2] + large * 2,
                          position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] + large * 2,
                          position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] - large * (max_classes + (max_classes-1)/4 + 1),
                          position_leg[1] - large / 2,                     position_leg[2] - large * (max_classes + (max_classes-1)/4 + 1),
                          position_leg[1] - large / 2,                     position_leg[2] + large * 2),
                        5,2,byrow=T)
          
          rectangle <- st_sfc(st_polygon(list(vec)), crs = as.numeric(code_epsg_ac()))
          
          rectangle <- st_transform(rectangle, crs = 4326)
          
          # leaflet du cadre blanc en 1er
          proxy <- addPolygons(map = proxy,
                               data = rectangle,
                               stroke = FALSE,
                               options = pathOptions(pane = "fond_legende", clickable = F),
                               fill = T,
                               fillColor = "white",
                               fillOpacity = 0.8,
                               group = "leg"
          )
          
          for(i in 1:max_classes)
          {
            proxy <- addPolygons(map = proxy,
                                 data = st_transform(get(paste0("rectangle_",i)), crs = 4326),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = analyse_leg_ac()$pal_classes[i],
                                 fillOpacity = 1,
                                 group = "leg"
            )
            
            pt_label <- st_sfc(st_geometry(st_point(c(max(st_coordinates(get(paste0("rectangle_",i))[[1]])[,1]) + large / 10,
                                                      mean(st_coordinates(get(paste0("rectangle_",i))[[1]])[,2])))),
                               crs = as.numeric(code_epsg_ac()))
            pt_label <- st_transform(pt_label, crs = 4326)
            
            proxy <- addLabelOnlyMarkers(map = proxy,
                                         lng = st_coordinates(pt_label)[1],
                                         lat = st_coordinates(pt_label)[2],
                                         label = label_rectangle[i],
                                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                     style = list(
                                                                       "color" = "black",
                                                                       "font-size" = "12px"
                                                                     )),
                                         group = "leg"
            )
          }
          
          # On ajoute la legende de classes a l'analyse

          # leaflet titre
          
          pt_titre <- st_sfc(st_geometry(st_point(c(min(st_coordinates(pt)[,"X"]),
                                                    max(st_coordinates(pt)[,"Y"]) + large))),
                             crs = as.numeric(code_epsg_ac()))
          pt_titre <- st_transform(pt_titre, crs = 4326)
          
          proxy <- addLabelOnlyMarkers(map = proxy,
                                       lng = st_coordinates(pt_titre)[1],
                                       lat = st_coordinates(pt_titre)[2],
                                       label = input$titre_classes_legende_ac_id,
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "14px"
                                                                   )),
                                       group = "leg"
          )
        }

        if(input$type_legende_ac_id==2) # Numerique
        {
          # On cree les rectangles
          if(is.null(input$nb_classes_ac_id)) return(NULL)
          
          max_classes <- as.numeric(input$nb_classes_ac_id)
          
          large <- as.numeric((st_bbox(fondMaille)[4] - st_bbox(fondMaille)[2]) / 20)
          
          for(i in 1:max_classes)
          {
            # Coordonnees du point haut/gauche des rectangles de la legende
            x_coord_rectangle <- position_leg[1]
            if(i==1) #1er rectangle
            {
              y_coord_rectangle <- position_leg[2]
            }else
            {
              y_coord_rectangle <- y_coord_rectangle - large
            }
            assign(paste0("rectangle_",i),st_sfc(st_polygon(list(matrix(c(x_coord_rectangle,               y_coord_rectangle,
                                                                          x_coord_rectangle + large * 1.5, y_coord_rectangle,
                                                                          x_coord_rectangle + large * 1.5, y_coord_rectangle - large,
                                                                          x_coord_rectangle,               y_coord_rectangle - large,
                                                                          x_coord_rectangle,               y_coord_rectangle),
                                                                        ncol=2, byrow=TRUE))),
                                                 crs = as.numeric(code_epsg_ac())))
          }
          
          # On ajoute un cadre blanc autour de la legende
          
          # leaflet rectangles et valeurs classes
          classes_leg_num <- analyse_leg_ac()$rupture_classes
          
          ltext <- max(nchar(classes_leg_num)) / 2.5
          
          vec <- matrix(c(position_leg[1] - large / 2,                     position_leg[2] + large * 2,
                          position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] + large * 2,
                          position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] - large * (max_classes + 1),
                          position_leg[1] - large / 2,                     position_leg[2] - large * (max_classes + 1),
                          position_leg[1] - large / 2,                     position_leg[2] + large * 2),
                        5,2,byrow=T)
          
          rectangle <- st_sfc(st_polygon(list(vec)), crs = as.numeric(code_epsg_ac()))
          
          rectangle <- st_transform(rectangle, crs = 4326)
          
          # leaflet du cadre blanc en 1er
          proxy <- addPolygons(map = proxy,
                               data = rectangle,
                               stroke = FALSE,
                               options = pathOptions(pane = "fond_legende", clickable = F),
                               fill = T,
                               fillColor = "white",
                               fillOpacity = 0.8,
                               group = "leg"
          )
          
          for(i in 1:max_classes)
          {
            proxy <- addPolygons(map = proxy,
                                 data = st_transform(get(paste0("rectangle_",i)), crs = 4326),
                                 stroke = FALSE,
                                 options = pathOptions(pane = "fond_legende", clickable = F),
                                 fill = T,
                                 fillColor = analyse_leg_ac()$pal_classes[i],
                                 fillOpacity = 1,
                                 group = "leg"
            )
            
            if(i<max_classes)
            {
              x1 <- max(st_coordinates(get(paste0("rectangle_",i))[[1]])[,1])
              y1 <- min(st_coordinates(get(paste0("rectangle_",i))[[1]])[,2])
              x2 <- max(st_coordinates(get(paste0("rectangle_",i))[[1]])[,1]) + large*0.2
              y2 <- min(st_coordinates(get(paste0("rectangle_",i))[[1]])[,2])
              ligne <- st_sfc(st_linestring(rbind(c(x1,y1),c(x2,y2))), crs = as.numeric(code_epsg_ac()))
              
              proxy <- addPolygons(map = proxy,
                                   data = st_transform(ligne, crs = 4326),
                                   color = "black",
                                   weight = 1,
                                   options = pathOptions(pane = "fond_legende", clickable = F),
                                   fill = F,
                                   fillOpacity = 1,
                                   group = "leg"
              )
              
              pt_label <- st_sfc(st_geometry(st_point(c(x2,y2))),
                                 crs = as.numeric(code_epsg_ac()))
              pt_label <- st_transform(pt_label, crs = 4326)
              
              proxy <- addLabelOnlyMarkers(map = proxy,
                                           lng = st_coordinates(pt_label)[1],
                                           lat = st_coordinates(pt_label)[2],
                                           label = as.character(format(round(classes_leg_num[i+1],3),big.mark=" ",decimal.mark=",",nsmall=0)),
                                           labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                       style = list(
                                                                         "color" = "black",
                                                                         "font-size" = "12px"
                                                                       )),
                                           group = "leg"
              )
              
            }
          }
          
          # leaflet titre
          
          pt_titre <- st_sfc(st_geometry(st_point(c(min(st_coordinates(pt)[,"X"]),
                                                    max(st_coordinates(pt)[,"Y"]) + large))),
                             crs = as.numeric(code_epsg_ac()))
          pt_titre <- st_transform(pt_titre, crs = 4326)
          
          proxy <- addLabelOnlyMarkers(map = proxy,
                                       lng = st_coordinates(pt_titre)[1],
                                       lat = st_coordinates(pt_titre)[2],
                                       label = input$titre_classes_legende_ac_id,
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "14px"
                                                                   )),
                                       group = "leg"
          )
        }
      })

      # AJOUT DES ONGLETS SAUVEGARDE

      observeEvent(input$save_carte_ac_id,{

        showModal(modalDialog(HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Sauvegarde de la carte en cours...</font> "), size="m", footer=NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
        insert_save$a <- insert_save$a + 1
        nb_save_carte <- insert_save$a-remove_carte$a

        m_save <- m_save_ac$a

        if(nb_save_carte>6)
        {
          insert_save$a <- insert_save$a - 1
          showModal(modalDialog(HTML("<font size=+1>Vous ne pouvez pas sauvegarger plus de 6 cartes. Veuillez en supprimer avant de continuer.</font> "), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(NULL)
        }

        output[[paste0("mymap_save_",insert_save$a,"_ac")]] <- renderLeaflet({

          if(!is.null(fondSuppl))
          {
            if(isolate(input$ajout_territoire_ac_id))
            {
              m_save <- addPolygons(map = m_save,
                                    data = isolate(fond_territoire_ac()),
                                    stroke = TRUE, color = "#BFBFBF", opacity = 1,
                                    weight = 0.5,
                                    options = pathOptions(pane = "fond_territoire", clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(isolate(fond_territoire_ac()))[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.001
              )
            }
          }

          if(isolate(input$ajout_reg_ac_id))
          {
            m_save <- addPolygons(map = m_save,
                                  data = isolate(fond_region_ac()),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 1.5,
                                  options = pathOptions(pane = "fond_reg", clickable = F),
                                  fill = F
            )
          }

          if(isolate(input$ajout_dep_ac_id))
          {
            m_save <- addPolygons(map = m_save, data = isolate(fond_departement_ac()),
                                  stroke = TRUE, color = "grey", opacity = 1,
                                  weight = 0.5,
                                  options = pathOptions(pane = "fond_dep", clickable = F),
                                  fill = F
            )
          }

          i <- 1 #pour gerer l'ordre des fonds dans le pane

          for(fond in isolate(liste_fonds_ac$a))
          {
            if(fond=="analyse/maille")
            {
              suppressWarnings(test_analyse_maille_classe <- try(isolate(analyse_ac())$donnees[rev(order(isolate(analyse_ac())[[1]][,varRatio])),varRatio],silent=T))
              if(class(test_analyse_maille_classe) %in% "try-error")
              {
                return(NULL)
              }else
              {
                analyse_maille_classe <- isolate(analyse_ac())$donnees[rev(order(isolate(analyse_ac())[[1]][,varRatio])),varRatio]
              }

              analyse_maille <- merge(isolate(fond_contour_maille_ac())[[2]][,c("CODE","geometry")],isolate(analyse_ac())$donnees[,c("CODE","LIBELLE",varRatio,"TXT1")],by="CODE")
              names(analyse_maille)[3] <- varRatio
              analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varRatio])),]
              analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)

              m_save <- addPolygons(map = m_save, data = analyse_maille, opacity = 1,
                                    stroke = TRUE, color = "white", weight = 1,
                                    options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                                    fill = T,
                                    fillColor = isolate(palette_ac())[[1]](analyse_maille_classe),
                                    fillOpacity = 1
              )
            }

            if(fond=="contour")
            {
              m_save <- addPolygons(map = m_save, data = isolate(fond_contour_maille_ac())[[1]], opacity = 0.3, #contour_WGS84
                                    stroke = TRUE, color = "black", weight = 3,
                                    options = pathOptions(pane = paste0("fond_duo",i), clickable = T),
                                    popup = paste0("<b> <font color=#2B3E50>",as.data.frame(isolate(fond_contour_maille_ac())[[1]])[,"LIBELLE"], "</font> </b>"),
                                    fill = T, fillColor = "white", fillOpacity = 0.3
              )
            }

            i <- i + 1
          }

          if(isolate(elargi_ac()))
          {
            analyse_maille_classe_elargi <- isolate(analyse_ac())$donnees_elargi[rev(order(isolate(analyse_ac())$donnees_elargi[,varRatio])),varRatio]

            analyse_maille_elargi <- merge(isolate(fond_elargi_ac())[,c("CODE","geometry")],isolate(analyse_ac())$donnees_elargi[,c("CODE","LIBELLE",varRatio,"TXT1")],by="CODE")
            names(analyse_maille_elargi)[3] <- varRatio
            analyse_maille_elargi <- analyse_maille_elargi[rev(order(as.data.frame(analyse_maille_elargi)[,varRatio])),]
            analyse_maille_elargi <- st_sf(analyse_maille_elargi,stringsAsFactors = FALSE)

            m_save <- addPolygons(map = m_save, data = analyse_maille_elargi, opacity = isolate(input$opacite_elargi_ac_id)/100,
                                  stroke = TRUE, color = "white", weight = 1,
                                  options = pathOptions(pane = "fond_duo2", clickable = T),
                                  popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT1),
                                  fill = T,
                                  fillColor = isolate(palette_ac())[[1]](analyse_maille_classe_elargi),
                                  fillOpacity = isolate(input$opacite_elargi_ac_id)/100
            )
          }
          
          if(!is.null(isolate(lon_lat_ac())[[1]]))
          {
            pt <- st_sfc(st_geometry(st_point(c(isolate(lon_lat_ac())[[1]],isolate(lon_lat_ac())[[2]]))), crs = 4326)
            pt <- st_transform(pt, crs = as.numeric(isolate(code_epsg_ac())))
            coord_pt <- st_coordinates(pt)[1:2]
            
            position_leg <- t(data.frame(c(coord_pt[1],coord_pt[2])))
            
            if(is.null(isolate(input$type_legende_ac_id))) return(NULL)
            
            if(isolate(input$type_legende_ac_id==1)) # Litterale
            {
              # On cree les rectangles
              if(is.null(isolate(input$nb_classes_ac_id))) return(NULL)
              
              max_classes <- as.numeric(isolate(input$nb_classes_ac_id))
              
              large <- as.numeric((st_bbox(fondMaille)[4] - st_bbox(fondMaille)[2]) / 20)
              
              for(i in 1:max_classes)
              {
                # Coordonnees du point haut/gauche des rectangles de la legende
                x_coord_rectangle <- position_leg[1]
                if(i==1) #1er rectangle
                {
                  y_coord_rectangle <- position_leg[2]
                }else
                {
                  y_coord_rectangle <- y_coord_rectangle - large - large / 4
                }
                assign(paste0("rectangle_",i),st_sfc(st_polygon(list(matrix(c(x_coord_rectangle,               y_coord_rectangle,
                                                                              x_coord_rectangle + large * 1.5, y_coord_rectangle,
                                                                              x_coord_rectangle + large * 1.5, y_coord_rectangle - large,
                                                                              x_coord_rectangle,               y_coord_rectangle - large,
                                                                              x_coord_rectangle,               y_coord_rectangle),
                                                                            ncol=2, byrow=TRUE))),
                                                     crs = as.numeric(isolate(code_epsg_ac()))))
              }
              
              # On ajoute un cadre blanc autour de la legende
              
              # leaflet rectangles et valeurs classes
              classes_leg_texte <- isolate(analyse_leg_ac())$rupture_classes
              
              label_rectangle <- c()
              for(i in 1:max_classes)
              {
                if(i==1)
                {
                  lbl <- paste0(format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," et plus")
                  label_rectangle <- c(label_rectangle, lbl)
                }else if (i>1 && i<max_classes)
                {
                  lbl <- paste0("De ", format(round(classes_leg_texte[i+1],3), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
                  label_rectangle <- c(label_rectangle, lbl)
                }else #i==length(max_classes)
                {
                  lbl <- paste0("Moins de ", format(round(classes_leg_texte[i],3), big.mark=" ",decimal.mark=",",nsmall=0))
                  label_rectangle <- c(label_rectangle, lbl)
                }
              }
              
              ltext <- max(nchar(label_rectangle)) / 2.5
              
              vec <- matrix(c(position_leg[1] - large / 2,                     position_leg[2] + large * 2,
                              position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] + large * 2,
                              position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] - large * (max_classes + (max_classes-1)/4 + 1),
                              position_leg[1] - large / 2,                     position_leg[2] - large * (max_classes + (max_classes-1)/4 + 1),
                              position_leg[1] - large / 2,                     position_leg[2] + large * 2),
                            5,2,byrow=T)
              
              rectangle <- st_sfc(st_polygon(list(vec)), crs = as.numeric(isolate(code_epsg_ac())))
              
              rectangle <- st_transform(rectangle, crs = 4326)
              
              # leaflet du cadre blanc en 1er
              m_save <- addPolygons(map = m_save,
                                    data = rectangle,
                                    stroke = FALSE,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = "white",
                                    fillOpacity = 0.8,
                                    group = "leg"
              )
              
              for(i in 1: max_classes)
              {
                m_save <- addPolygons(map = m_save,
                                      data = st_transform(get(paste0("rectangle_",i)), crs = 4326),
                                      stroke = FALSE,
                                      options = pathOptions(pane = "fond_legende", clickable = F),
                                      fill = T,
                                      fillColor = isolate(analyse_leg_ac())$pal_classes[i],
                                      fillOpacity = 1,
                                      group = "leg"
                )
                
                pt_label <- st_sfc(st_geometry(st_point(c(max(st_coordinates(get(paste0("rectangle_",i))[[1]])[,1]) + large / 10,
                                                          mean(st_coordinates(get(paste0("rectangle_",i))[[1]])[,2])))),
                                   crs = as.numeric(isolate(code_epsg_ac())))
                pt_label <- st_transform(pt_label, crs = 4326)
                
                m_save <- addLabelOnlyMarkers(map = m_save,
                                              lng = st_coordinates(pt_label)[1],
                                              lat = st_coordinates(pt_label)[2],
                                              label = label_rectangle[i],
                                              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                          style = list(
                                                                            "color" = "black",
                                                                            "font-size" = "12px"
                                                                          )),
                                              group = "leg"
                )
              }
              
              # On ajoute la legende de classes a l'analyse
              
              # leaflet titre
              
              pt_titre <- st_sfc(st_geometry(st_point(c(min(st_coordinates(pt)[,"X"]),
                                                        max(st_coordinates(pt)[,"Y"]) + large))),
                                 crs = as.numeric(isolate(code_epsg_ac())))
              pt_titre <- st_transform(pt_titre, crs = 4326)
              
              m_save <- addLabelOnlyMarkers(map = m_save,
                                            lng = st_coordinates(pt_titre)[1],
                                            lat = st_coordinates(pt_titre)[2],
                                            label = isolate(input$titre_classes_legende_ac_id),
                                            labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                        style = list(
                                                                          "color" = "black",
                                                                          "font-size" = "14px"
                                                                        )),
                                            group = "leg"
              )
            }
            
            if(isolate(input$type_legende_ac_id==2)) # Numerique
            {
              # On cree les rectangles
              if(is.null(isolate(input$nb_classes_ac_id))) return(NULL)
              
              max_classes <- as.numeric(isolate(input$nb_classes_ac_id))
              
              large <- as.numeric((st_bbox(fondMaille)[4] - st_bbox(fondMaille)[2]) / 20)
              
              for(i in 1:max_classes)
              {
                # Coordonnees du point haut/gauche des rectangles de la legende
                x_coord_rectangle <- position_leg[1]
                if(i==1) #1er rectangle
                {
                  y_coord_rectangle <- position_leg[2]
                }else
                {
                  y_coord_rectangle <- y_coord_rectangle - large
                }
                assign(paste0("rectangle_",i),st_sfc(st_polygon(list(matrix(c(x_coord_rectangle,               y_coord_rectangle,
                                                                              x_coord_rectangle + large * 1.5, y_coord_rectangle,
                                                                              x_coord_rectangle + large * 1.5, y_coord_rectangle - large,
                                                                              x_coord_rectangle,               y_coord_rectangle - large,
                                                                              x_coord_rectangle,               y_coord_rectangle),
                                                                            ncol=2, byrow=TRUE))),
                                                     crs = as.numeric(isolate(code_epsg_ac()))))
              }
              
              # On ajoute un cadre blanc autour de la legende
              
              # leaflet rectangles et valeurs classes
              classes_leg_num <- isolate(analyse_leg_ac())$rupture_classes
              
              ltext <- max(nchar(classes_leg_num)) / 2.5
              
              vec <- matrix(c(position_leg[1] - large / 2,                     position_leg[2] + large * 2,
                              position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] + large * 2,
                              position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] - large * (max_classes + 1),
                              position_leg[1] - large / 2,                     position_leg[2] - large * (max_classes + 1),
                              position_leg[1] - large / 2,                     position_leg[2] + large * 2),
                            5,2,byrow=T)
              
              rectangle <- st_sfc(st_polygon(list(vec)), crs = as.numeric(isolate(code_epsg_ac())))
              
              rectangle <- st_transform(rectangle, crs = 4326)
              
              # leaflet du cadre blanc en 1er
              m_save <- addPolygons(map = m_save,
                                    data = rectangle,
                                    stroke = FALSE,
                                    options = pathOptions(pane = "fond_legende", clickable = F),
                                    fill = T,
                                    fillColor = "white",
                                    fillOpacity = 0.8,
                                    group = "leg"
              )
              
              for(i in 1: max_classes)
              {
                
                m_save <- addPolygons(map = m_save,
                                      data = st_transform(get(paste0("rectangle_",i)), crs = 4326),
                                      stroke = FALSE,
                                      options = pathOptions(pane = "fond_legende", clickable = F),
                                      fill = T,
                                      fillColor = isolate(analyse_leg_ac())$pal_classes[i],
                                      fillOpacity = 1,
                                      group = "leg"
                )
                
                if(i<max_classes)
                {
                  x1 <- max(st_coordinates(get(paste0("rectangle_",i))[[1]])[,1])
                  y1 <- min(st_coordinates(get(paste0("rectangle_",i))[[1]])[,2])
                  x2 <- max(st_coordinates(get(paste0("rectangle_",i))[[1]])[,1]) + large*0.2
                  y2 <- min(st_coordinates(get(paste0("rectangle_",i))[[1]])[,2])
                  ligne <- st_sfc(st_linestring(rbind(c(x1,y1),c(x2,y2))), crs = as.numeric(isolate(code_epsg_ac())))
                  
                  m_save <- addPolygons(map = m_save,
                                        data = st_transform(ligne, crs = 4326),
                                        color = "black",
                                        weight = 1,
                                        options = pathOptions(pane = "fond_legende", clickable = F),
                                        fill = F,
                                        fillOpacity = 1,
                                        group = "leg"
                  )
                  
                  pt_label <- st_sfc(st_geometry(st_point(c(x2,y2))),
                                     crs = as.numeric(isolate(code_epsg_ac())))
                  pt_label <- st_transform(pt_label, crs = 4326)
                  
                  m_save <- addLabelOnlyMarkers(map = m_save,
                                                lng = st_coordinates(pt_label)[1],
                                                lat = st_coordinates(pt_label)[2],
                                                label = as.character(format(round(classes_leg_num[i+1],3),big.mark=" ",decimal.mark=",",nsmall=0)),
                                                labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                            style = list(
                                                                              "color" = "black",
                                                                              "font-size" = "12px"
                                                                            )),
                                                group = "leg"
                  )
                  
                }
              }
              
              # On ajoute la legende de classes a l'analyse
              
              # leaflet titre
              
              pt_titre <- st_sfc(st_geometry(st_point(c(min(st_coordinates(pt)[,"X"]),
                                                        max(st_coordinates(pt)[,"Y"]) + large))),
                                 crs = as.numeric(isolate(code_epsg_ac())))
              pt_titre <- st_transform(pt_titre, crs = 4326)
              
              m_save <- addLabelOnlyMarkers(map = m_save,
                                            lng = st_coordinates(pt_titre)[1],
                                            lat = st_coordinates(pt_titre)[2],
                                            label = isolate(input$titre_classes_legende_ac_id),
                                            labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                        style = list(
                                                                          "color" = "black",
                                                                          "font-size" = "14px"
                                                                        )),
                                            group = "leg"
              )
            }
          }
          
          removeModal()
          
          m_save
        })

        output[[paste0("remove_carte_",nb_save_carte,"_ac")]] <- renderUI({
          actionButton(paste0("remove_carte_",nb_save_carte,"_ac_id"),label="X Supprimer la carte", style="color:#FFFFFF; border-color:#FFFFFF; background-color:#2B3E50")
        })

        appendTab(inputId = "onglets_ac",
                  tabPanel(title=HTML(paste0("<font color=#AFC9E0> Carte ",insert_save$a,"</font>")),value=paste0("carte",nb_save_carte),uiOutput(paste0("remove_carte_",nb_save_carte,"_ac")),leafletOutput(paste0("mymap_save_",insert_save$a,"_ac"),width="100%",height = 800)),
                  select = TRUE,
                  session = session
        )

      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_1_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac",
                  target = "carte1",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_2_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac",
                  target = "carte2",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_3_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac",
                  target = "carte3",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_4_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac",
                  target = "carte4",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_5_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac",
                  target = "carte5",
                  session = session
        )
      }, ignoreInit = TRUE)

      observeEvent(input$remove_carte_6_ac_id,{
        remove_carte$a <- remove_carte$a + 1
        removeTab(inputId = "onglets_ac",
                  target = "carte6",
                  session = session
        )
      }, ignoreInit = TRUE)

      # TABLEAUX DE DONNEES, MAILLE ET CONTOUR

      output$mymaille_ac <- DT::renderDataTable(DT::datatable({
        if(elargi_ac())
          data <- analyse_ac()$donnees_elargi
        else
          data <- analyse_ac()$donnees

        tableau_maille <- data[order(data$CODE),c("CODE","LIBELLE",varRatio)]
      },  style = 'bootstrap'
      ))

      output$mycontour_ac <- DT::renderDataTable(DT::datatable({
        data <- as.data.frame(fondContour)
        tableau_contour <- data[order(data$CODE),c(1:2)]
      },  style = 'bootstrap'
      ))

      # ENVOI DU LEAFLET A L'UI

      output$mymap_ac <- renderLeaflet({
        react_fond_ac()
      })

    }

    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
    }
