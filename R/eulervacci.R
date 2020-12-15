#############Fonction_3#############

#S->I->R
#S---->R


#' eulervacci
#'
#' @param Sbis #Personnes saines
#' @param Ibis #Personnes infectées
#' @param Rbis #Personnes retirées
#' @param a #Taux de transmission
#' @param b #Taux de guérison
#' @param deltaT #Pas #Si DeltaT = 0, choix du pas optimal automatique
#' @param taille #Nombre de jours choisi
#' @param tv # Taux de vaccination par semaines
#'
#' @return un dataframe
#' @export
#'
#' @examples
#' eulervacci(500 ,10 ,0 ,0.001 ,0.03,0.1,100,0.04)
eulervacci<-function(Sbis,Ibis,Rbis,a,b,deltaT,taille,tv) # fonction calculant à  chaque tour l’évolution des variables S, I et R
{
  J<-0  #Non modulable       #Jour 0

  if (deltaT==0) {
    deltaT <- 30/(Sbis + Ibis +Rbis) #Si deltaT = 0, choix du pas optimal automatique
  }
  resul<-data.frame(Sbis, Ibis, Rbis, J)
  colnames(resul) <- c("S","I","R","J")

  repeat{
    J <- J+deltaT #Compteur de jours
    #création de variables memoire
    Suc <- Sbis
    Reco <- Rbis
    Infect <- Ibis

    pV<-(rbinom(1,round(Suc),tv))    #loi binomial generant le taux à retirer de S

    Sbis <- Suc + (-a * Suc * Infect)*deltaT - (pV/(7/deltaT)) #taux retiré de S à chaque semaines
    Rbis <- Reco + (b * Infect)*deltaT + (pV/(7/deltaT)) #taux affecté à R à chaque semaines
    Ibis <- (Suc+Infect+Reco) - Sbis - Rbis
    SIR <- data.frame(Sbis,Ibis,Rbis,J)
    colnames(SIR) <- c("S","I","R","J");
    resul<-rbind(resul,SIR) #concaténation des nouvelles lignes avec la première ligne du tableau

    if (J>=taille) break #arrêt de la boucle si nombre de jour atteint
  }
  return(resul) #retourne le tableau final

}
