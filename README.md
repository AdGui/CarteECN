# CarteECN
Carte dynamique des données ECN 2010-2014

Consultable à l'adresse : http://shiny.adgui.eu ou http://shiny.adgui.eu/CartoECN-leaflet/.

Lancement en local (nécessite l'installation du package shiny + ceux requis dans server.R) :
```r
library('shiny')
runGitHub('CarteECN','AdGui')
```
Le package leaflet ne correspond pas à celui developpé par l'équipe de rstudio mais par J. Cheng seul. Il est possible de l'installer depuis GitHub.
```r
library('devtools')
install_github('leaflet-shiny','jcheng5')
```

Le code est fonctionnel, mais assez moche, si j'ai le temps je ferai un peu de nettoyage/optimisation.

Le temps de chargement initial est variable (dépendant de la puissance du pc, en pratique jamais au delà de 30s).

La création de la carte de zonage ECN est dans le script "Script additionnels/CarteLeafletLocale.R"
