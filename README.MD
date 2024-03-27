# Compilateur 


### Instruction pour la compilation 
Pour compiler le programme faire : 
       
    dune build main.exe

dans le terminal 

Pour executer le programme sur les tests faire :

    ./main.exe ./tests/<dossier_au_choix>/<nom_du_fichier>.test 

Pour convertir le fichier compiler en assembleur : 

    ./main.exe ./tests/<dossier_au_choix>/<nom_du_fichier>.test > <nom_voulu>.s 

Ensuite ouvir spim : 

    spim 
    load "nom_voulu.s" 
    run

### Description du langage 

Le langage présenté est un pseudo langage basé sur le c qui reprend les definitions de variable , la gestion d'entier , les booléens , appel de fonction , et des conditions et boucle.
Je vous invite à regarder les test pour prendre en main ce compilateur 

### Point d'amélioration 

Pour le compléter il faudrait que j'ajoute la gestion des chaîne de caractères , les chevrons pour verifié qui est plus grand cela pourrait servir pour les conditions car actuelement il ne gere que si c'est égaux ou non égaux.
Faire en sorte que le fichier asm généré puisse être exécuter correctement car j'ai un soucis de mémoire


#### Create by Jonny MATHANARUBAN