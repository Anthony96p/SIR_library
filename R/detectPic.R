#############Fonction_2#############

detectPic<-function(resul,a,b) #Fonction qui détermine les coordonnées du pic
{

  Ipeaks<-resul[resul$I == max(resul$I),][1,]$I   #Nombre d’ infectées au jour du pic
  Tpeaks<-resul[resul$I == max(resul$I),][1,]$J   #Jour du pic d'infectées
  Coord<-data.frame(Ipeaks,Tpeaks) #dataframe jour du pic et nbr d’infectées correspondant

  #Si pas de pic, retourner un message à l’utilisateur
  if(is.na(Coord$Tpeaks)==TRUE){
    print("DetectPic : Le pic n'a pas pu être détecté ")
  }
  return(Coord) #retourne tableau de pic ou message
}
