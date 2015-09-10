### Cours 1 ###


## Introduction ##

--------------------- Propriete des langages algebriques ----------------------
x represente un symbole terminal

Tout langage algebrique (hors-contexte) propre est engendree par une grammaire 
lexicalisee
                     regle: V -> V*xV*

Tout langage algebrique propre est engendree par une grammaire sous forme
normal de Greibach
                     regle: V -> xV*
forme de Greibach double:
                     regle: V -> x
                            V -> xA*y
-------------------------------------------------------------------------------

---------------- Automate a pile et forme normale de Greibach -----------------
Ine grammaire sous forme normale de Greibach permet de definir facilement un 
automate a pile sans etat (non deterministe en general)
-------------------------------------------------------------------------------

la normalisation de Greibach conserve l'ambiguitee de la grammaire et peut
introduire de l'ambiguitee dans une grammaire non ambigue


