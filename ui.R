library("shiny")
library("leaflet")
library("RColorBrewer")

colors <- brewer.pal(9, "YlOrRd")
pal <- colorRampPalette(colors)
col <- pal(28)

shinyUI(fluidPage(
  theme = "yeti.css",
  tags$head(tags$style("
    .tab-pane .leaflet-map-output {box-shadow:0 0 2px 1px grey;padding:2px;position:absolute;background-color:grey;}
    #mapmetro {margin-left:20%;}
    #mapReunion {margin-top:750px;margin-left:20%;}
    #mapMayotte {margin-top:750px;margin-left:32%;}
    #mapGuadeloupe {margin-top:750px;margin-left:44%;}
    #mapMartinique {margin-top:750px;margin-left:56%;}
    #mapGuyane {margin-top:750px;margin-left:68%;}
    .leaflet-bar {box-shadow: 0px 0px 0px transparent !important;}
    .leaflet-bar a, .leaflet-bar a:hover {display:none !important;}
    #mapmetro .leaflet-control-container div.leaflet-bottom.leaflet-right div.leaflet-control-attribution.leaflet-control {background: none repeat scroll 0% 0% transparent !important;}
    #mapmetro .leaflet-control-container div.leaflet-bottom.leaflet-right div.leaflet-control-attribution.leaflet-control a:nth-child(1) {color: transparent !important;display:none !important;}
    .floater {background-color: #FFF;padding: 8px;opacity: 0.7;border-radius: 6px;box-shadow: 0px 0px 15px rgba(0, 0, 0, 0.2);line-height:20px}
  ")),
  
  h3("Analyse des choix post-ECN"),
  br(),
  fluidRow(
    column(2,h4("Options globales :")),
    
    column(3,  
      selectInput(inputId = "Spe",
        label = "Quelle spécialité ?",
          choices = list(
          "Toutes les spécialités"=000,
          "Anatomie et cytologie pathologique"=012,
          "Anesthésie réanimation"=004,
          "Biologie médicale"=003, 
          "Cardiologie et maladies vasculaires"=013,
          "Chirurgie générale"=028, 
          "Chirurgie Orale"=032, 
          "Dermatologie et vénérologie"=014,                            
          "Endocrinologie, diabète, maladies métaboliques"=015,         
          "Gastro-entérologie et hépatologie"=016,                      
          "Génétique médicale"=017,
          "Gynécologie médicale"=005,  
          "Gynécologie obstétrique"=009, 
          "Hématologie"=018,
          "Médecine du Travail"=006,
          "Médecine générale"=011,   
          "Médecine interne"=019,                                       
          "Médecine nucléaire"=020,                                     
          "Médecine physique et de réadaptation"=021,                   
          "Néphrologie"=022,
          "Neurochirurgie"=029,  
          "Neurologie"=023,                                             
          "Oncologie"=024,
          "Ophtalmologie"=030, 
          "ORL"=031,
          "Pédiatrie"=002, 
          "Pneumologie"=025,
          "Psychiatrie"=010, 
          "Radiodiagnostic et imagerie médicale"=026,                   
          "Rhumatologie"=027,
          "Santé publique"=007
        )
      )
    )
  ),

  br(),
  
  fluidRow(
    column(2,h4("Base de données utilisée : ")),
    
    column(9,
      radioButtons(inputId = "ChoixBDD", 
        label = "",
        choices = list("2010"="affectations2010","2011"="affectations2011","2012"="affectations2012","2013"="affectations2013","01/09/2014 (simulations)"="simulations2014","27/09/2014 (affectations)"="affectations2014"),
        selected = "affectations2014", inline = TRUE
      )
    )
  ),
  
  hr(),
  
  tabsetPanel(
    tabPanel(h4("Carto"), 
      fluidRow(
        column(2,h4("Options : ")),
        
        column(3,
           conditionalPanel(
             condition = "input.ChoixBDD == 'simulations2014' | input.ChoixBDD == 'affectations2014'",
             selectInput(inputId = "meth.order",
               label = "Couleurs des subdivisions basées sur",
               choices = list(
                 "Rang médian"="median",
                 "Rang moyen"="mean",
                 "Rang max"="max",
                 "Rang min"="min",
                 "Rang - 3e quart"="TQuart",
                 "Rang - 1e quart"="PQuart" ,
                 "Rang limite" ="rgL",
                 "Postes pourvus" = "PPP",
                 "Attractivité" ="Attr"
               )
             )
           ),
           
           conditionalPanel(
             condition = "input.ChoixBDD != 'simulations2014' & input.ChoixBDD != 'affectations2014'",
             selectInput(inputId = "meth.order",
               label = "Couleurs des subdivisions basées sur",
               choices = list(
                 "Rang médian"="median",
                 "Rang moyen"="mean",
                 "Rang max"="max",
                 "Rang min"="min",
                 "Rang - 3e quart"="TQuart",
                 "Rang - 1e quart"="PQuart" ,
                 "Rang limite" ="rgL",
                 "Postes pourvus" = "PPP",
                 "% de femmes" = "Sexe",
                 "Age" = "Age",
                 "Attractivité" ="Attr"
               )
             )
           )
        ),

        column(6,
           radioButtons(inputId="Choix.indic", 
              label="Indicateur postes pourvus :",
              choices= list(
                "Pourcentage"="pourcent",
                "Nombre"="nbr",
                "Offre"="Offre"
                ),
              selected = "pourcent", inline = TRUE
           )
        )
      ),
      
      fluidRow(
        column(2,
               radioButtons(inputId="Restcand", 
                            label="Restreindre les candidats (postes pourvus, age, sexe):",
                            choices= list("oui"="oui","non"="non"),
                            selected = "non", inline = TRUE
               )
        ),
        
        column(9,
          column(4,
            conditionalPanel(
              condition = "input.Restcand == 'oui'",
              sliderInput(
                inputId="Rang.min",
                label="Borne 1", 
                min=1,
                max=8304,
                value=1,
                step = 50,
                ticks = FALSE
              )
            )
          ),
               
          column(4, offset=1,
            conditionalPanel(
              condition = "input.Restcand == 'oui'",
              sliderInput(
                inputId="Rang.max",
                label="Borne 2", 
                min=1,
                max=8304,
                value=8304,
                step = 50,
                ticks = FALSE
              )
            )
          )
        )
      ),
      
      fluidRow(
        column(5,
           radioButtons(inputId="choixAge", 
              label="Indicateur age :",
              choices= list(
                "Médian"="med",
                "Moyen"="moy",
                "1e quart"="PQuart",
                "3e quart"="TQuart",
                "Minimum"="min",
                "Maximum"="max"
              ),
              selected = "med", inline = TRUE
           )
        ),
        column(5,
           radioButtons(inputId="Choix.calc", 
              label="Choix de la méthode de calcul (attractivité - valable que si une spé a été selectionnée):",
              choices= list("Rang global"="glob","Rang dans la spé/subdivision"="sel"),
              selected = "glob", inline = TRUE
           )
        )
      ),
      
      hr(),
      
      leafletMap(
        "mapmetro", "60%", "800px",
        #initialTileLayer = NULL,
        initialTileLayer = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        initialTileLayerAttribution = NULL,
        options=list(
          center = c(46, 2),
          zoom = 6,
          maxZoom = 7,
          minZoom =5,
          maxBounds = list(list(30, -180), list(60, 180)),
          dragging = TRUE,
          doubleClickZoom = FALSE
        )
      ),
      
      leafletMap(
        "mapReunion", "12%", "200px",
        #initialTileLayer = NULL,
        initialTileLayer = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        initialTileLayerAttribution = HTML('&copy;
        <a href="http://openstreetmap.org">OpenStreetMap</a> contributors,
        <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>'
        ),
        options=list(
          center = c(-21.15, 55.5),
          zoom = 8,
          maxZoom = 9,
          minZoom =7,
          maxBounds = list(list(-30, -180), list(-10, 180)),
          dragging = TRUE,
          doubleClickZoom = FALSE
        )
      ),
      
      leafletMap(
        "mapMayotte", "12%", "200px",
        #initialTileLayer = NULL,
        initialTileLayer = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        initialTileLayerAttribution = HTML('&copy;
        <a href="http://openstreetmap.org">OpenStreetMap</a> contributors,
        <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>'
        ),
        options=list(
          center = c(-12.8, 45.15),
          zoom = 9,
          maxZoom = 10,
          minZoom = 8,
          maxBounds = list(list(-20, -180), list(-0, 180)),
          dragging = TRUE,
          doubleClickZoom = FALSE
        )
      ),
      
      leafletMap(
        "mapGuadeloupe", "12%", "200px",
        #initialTileLayer = NULL,
        initialTileLayer = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        initialTileLayerAttribution = HTML('&copy;
                                           <a href="http://openstreetmap.org">OpenStreetMap</a> contributors,
                                           <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>'
        ),
        options=list(
          center = c(16.15, -61.4),
          zoom = 8,
          maxZoom = 9,
          minZoom =7,
          maxBounds = list(list(0, -180), list(30, 180)),
          dragging = TRUE,
          doubleClickZoom = FALSE
        )
      ),
      
      leafletMap(
        "mapGuyane", "12%", "200px",
        #initialTileLayer = NULL,
        initialTileLayer = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        initialTileLayerAttribution = HTML('&copy;
                                           <a href="http://openstreetmap.org">OpenStreetMap</a> contributors,
                                           <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>'
        ),
        options=list(
          center = c(3.9, -53.1),
          zoom = 6,
          maxZoom = 7,
          minZoom =5,
          maxBounds = list(list(-16, -180), list(24, 180)),
          dragging = TRUE,
          doubleClickZoom = FALSE
        )
      ),
      
      leafletMap(
        "mapMartinique", "12%", "200px",
        #initialTileLayer = NULL,
        initialTileLayer = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        initialTileLayerAttribution = HTML('&copy;
                                           <a href="http://openstreetmap.org">OpenStreetMap</a> contributors,
                                           <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>'
        ),
        options=list(
          center = c(14.65, -61),
          zoom = 8,
          maxZoom = 9,
          minZoom =7,
          maxBounds = list(list(0, -180), list(30, 180)),
          dragging = TRUE,
          doubleClickZoom = FALSE
        )
      )
    ),
    
    tabPanel(h4("Notice"),
             h3("Notice",style="text-decoration:underline;"),
             p("Les étudiants CESP comme ceux des armées ne sont pas inclus aux analyses pour 2014 (y compris pour l'avancement des choix)."),
             p("Pour les autres années, ils sont incorporés aux résultats sans distinction possible. De ce fait une correction a du être faite sur les données d'offre de poste : les offres de postes classiques et celles pour les CESP ont été fusionnées."),
             p("De ce fait le calcul des rangs limites pour les années 2010-2013 part de l'hypothèse que les étudiants CESP sont classés derniers de leur spécialité/subdivision (probable pour les spécialités médicales et chirurgicales, improbable pour la MG)."),
             p("L\'ensemble des données sont issues de : ", a("https://www.cngsante.fr/chiron2014/celine/listing.html", href="https://www.cngsante.fr/chiron2014/celine/listing.html"), "ainsi que sur les arrêtés de classement et d'affectations.")
    )
  ),
  
  absolutePanel(
    left = "82%", top = "65%", width="auto", style = "", fixed=TRUE, class = "floater",
    uiOutput("Legende")
  ),
  
  absolutePanel(
    left = "2%", top = "60%", width = "17%", fixed=TRUE, class = "floater",
    uiOutput("Info")
  )
))