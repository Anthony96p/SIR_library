#############Fonction_5#############


#' eulermorta
#'
#' @param Sbis #Personnes saines
#' @param Ibis #Personnes infectées
#' @param Rbis #Personnes retirées
#' @param a #Taux de transmission
#' @param b #Taux de guérison
#' @param deltaT #Pas #Si DeltaT = 0, choix du pas optimal automatique
#' @param taille #Nombre de jours choisi
#' @param mu # proba de mortalité par mois
#' @param tv # Taux de vaccination par semaines
#'
#' @return un dataframe
#' @export
#'
#' @examples
#' eulermorta(500 ,10 ,0 ,0.001 ,0.03,0.1,100,0.2,0.04)
eulermortavacci<-function(Sbis,Ibis,Rbis,a,b,deltaT,taille,mu,tv) # fonction calculant à chaque tour l’évolution des variables S, I et R
{
  J<-0    #Non modulable       #Jour 0
  total <- (Sbis + Ibis + Rbis)
  LimH <- (1/3*(Sbis+Ibis+Rbis)) #limite des lits d'hopitaux (fixée à 1/3 de la population total)


  if (deltaT==0) {
    deltaT <- 30/(Sbis + Ibis +Rbis) #Si deltaT = 0, choix du pas optimal automatique
  }

  total <- (Sbis + Ibis + Rbis)
  listeSIR<-data.frame(Sbis, Ibis, Rbis,total, J)
  colnames(listeSIR) <- c("S","I","R","total","J")
  resul<-listeSIR

  repeat{
    total <- (Sbis + Ibis + Rbis)
    J <- J+deltaT #Compteur de jours


    Suc <- Sbis
    Reco <- Rbis
    Infect <- Ibis

    if (Infect<=LimH){ # Si le nombre d'infectés est inferieur nombre de lit d'hopital, alors la mortalité de la maladie est divisée par 5
      mubis <- mu/5
    }else{# A l'inverse, si le nombre d'infectés est superieur nombre de lit d'hopital, alors la mortalité de la maladie est à sont niveau normal
      mubis <- mu
    }

    Tm<-(rbinom(1,round(Infect),mubis)) #loi binomiale générant un taux à retirer de I

    pV<-(rbinom(1,round(Suc),tv))    #loi binomial generant le taux à retirer de S pour R

    Sbis <- Suc + (-a * Suc * Infect)*deltaT - (pV/(7/deltaT)) #taux retiré de S à chaque semaines
    Rbis <- Reco + (b * Infect)*deltaT + (pV/(7/deltaT)) #taux affecté à R à chaque semaines
    Ibis <- (Suc+Infect+Reco) - Sbis - Rbis-((Tm/(30/deltaT))) #On retire de I un taux par mois

    SIR <- data.frame(Sbis,Ibis,Rbis,total,J)
    colnames(SIR) <- c("S","I","R","total","J");
    resul<-rbind(resul,SIR) #concaténation des nouvelles lignes avec la première ligne du tableau

    if (J>=taille) break #arrêt de la boucle si nombre de jour atteint
  }
  return(resul)

}
