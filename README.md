# CarteECN
Carte dynamique des données ECN 2010-2014

Consultable à l'adresse : http://shinyapp.adgui.eu (possibilité de blocage en institution).

Lancement en local (nécessite l'installation du package shiny + ceux requis dans server.R) :
```r
library(shiny)
runGitHub('CarteECN','AdGui')
```
Le code est fonctionnel, mais assez moche, si j'ai le temps je ferai un peu de nettoyage/optimisation.

Le temps de chargement initial est variable (dépendant de la puissance du pc, en pratique jamais au delà d'une minute).

La création de la carte de zonage ECN est dans le script Script additionnels/CarteLeafletLocale


