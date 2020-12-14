#############Fonction_1#############

#S->I->R

#' euler
#'
#' @param Sbis #Personnes saines
#' @param Ibis #Personnes infectées
#' @param Rbis #Personnes retirées
#' @param a #Taux de transmission
#' @param b #Taux de guérison
#' @param deltaT #Pas #Si DeltaT = 0, choix du pas optimal automatique
#' @param taille #Nombre de jours choisi
#'
#' @return un dataframe
#' @export
#'
#' @examples
#' euler(500 ,10 ,0 ,0.001 ,0.03,0.1,100)
euler<-function(Sbis,Ibis,Rbis,a,b,deltaT,taille)  # fonction calculant à  chaque tour l'évolution des variables S, I et R
{
  J<-0  #Non modulable       #Jour 0

  if (deltaT==0) {
    deltaT <- 30/(Sbis + Ibis +Rbis)  #Si deltaT = 0, choix du pas optimal automatique en fonction de l’effectif de départ choisi
  }

  resul<-data.frame(Sbis, Ibis, Rbis, J) #création d’un data frame pour nos données
  colnames(resul) <- c("S","I","R","J")  #nommage des colonnes
  repeat{
    J <- J+deltaT #Compteur de jours

    #création de variables memoire
    Suc <- Sbis
    Reco <- Rbis
    Infect <- Ibis
    #approximation d’Euler
    Sbis <- Suc + (-a * Suc * Infect)*deltaT
    Rbis <- Reco + (b * Infect)*deltaT
    Ibis <- (Suc+Infect+Reco) - Sbis - Rbis

    SIR <- data.frame(Sbis,Ibis,Rbis,J)
    colnames(SIR) <- c("S","I","R","J")
    resul<-rbind(resul,SIR)  #concaténation des nouvelles lignes avec la première ligne du tableau

    if (J>=taille) break; #arrêt de la boucle si nombre de jour atteint
  }
  return(resul) #retourne le tableau final

}
