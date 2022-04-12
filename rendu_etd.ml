type dimension = int;; (*restreint aux entiers strictement positifs*)(*C'EST LA TAILLE DU TABLEAU*)

type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)

type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)


type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
               | Libre 
               | Code of string (*une chaine restreinte a 3 caracteres*);;


type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
          
type coup = Du of case * case | Sm of case list;;

let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

let est_case ((i,j,k):case):bool=
 (i+j+k=0);;

(*A REMPLIR*)
let est_dans_etoile ((i, j, k) : case) (dim:dimension) : bool = 
  (i>= -dim && j>= -dim && k>= -dim) || (i<=dim && j<=dim && k<=dim)

;;

let est_dans_losange ((i, j, k) : case)  (dim:dimension): bool =
  -dim <= j && k <= dim && -dim <= k && k <= dim 
;;           
let rec associe a l defaut=
  match l with
  | [] -> defaut
  | (a2, b) :: suite -> if a = a2 then b else associe a suite defaut;;

(*AFFICHAGE (fonctionne si les fonctions au dessus sont remplies)*)
(*transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k)*)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
    if m = (4 * dim) + 1 then " " (*fin de ligne*)
    else
      let c = transfo m n in
      if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
        "   "^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
    let rec affiche_aux n =
      if n = - 2 * dim - 1 then ()
      else
      begin
      print_endline (affiche_ligne n (-4*dim-1) config);
      print_endline "\n";
      affiche_aux (n - 1)
      end
    in
    affiche_aux (2*dim+1);;

let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;

(* La configuation initiale avec deux joueurs et un plateau de dimension 2*)
let conf_init : configuration =
  ([((3, -1, -2), Jaune); ((3, -2, -1), Jaune); ((4, -2, -2), Jaune);
    ((5, -3, -2), Jaune); ((-3, 1, 2), Vert); ((-3, 2, 1), Vert);
    ((-4, 2, 2), Vert); ((-5, 3, 2), Vert)],
   [Vert; Jaune], 2);;

affiche conf_init;;

(*A essayer apres avoir fait remplir_init
affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
*)

let tourner_case (m:int) (c:case) : case =
  let rec tourner_case_1 (m:int) (c:case) : case =
    if m = 0 then
      c
    else
      let (i,j,k)=tourner_case_1 (m-1) c in
        let (i,j,k)=(-k,-i,-j) in
          (i,j,k)
  in
  tourner_case_1 m c
;;

let translate (c:case) (v:vecteur) : case =
  let (c1,c2,c3)=c in
  let (v1,v2,v3)=v in
  let (d1,d2,d3)=(c1+v1,c2+v2,c3+v3) in
  (d1,d2,d3)
;;

let diff_case (l:case) (r:case) : vecteur =
  let (l1,l2,l3)=l in
  let (r1,r2,r3)=r in
  let (v1,v2,v3)=(l1-r1,l2-r2,l3-r3) in
  (v1,v2,v3)
;;

let sont_cases_voisines (l:case) (r:case) : bool =
  let (l1,l2,l3)=l in 
  let (r1,r2,r3)=r in 
  let (k1,k2,k3)=(l1-r1,l2-r2,l3-r3) in
  (k1= 1 && k2= -1 && k3=0) ||
  (k1=1 && k2=0 && k3= -1) ||
  (k1=0 && k2=1 && k3= -1) ||
  (k1= -1 && k2=1 && k3=0) ||
  (k1= -1 && k2=0 && k3=1) ||
  (k1=0 && k2= -1 && k3=1)
;;

let abs (x:int) : int =
  if x < 0 then -x else x
;;

let calcul_pivot ((x1,y1,z1):case) ((x2,y2,z2):case) : case option =
  let (x,y,z)=(x2-x1,y2-y1,z2-z1) in
  if x=0 then if y= -z && (abs(y) mod 2)=0 then Some ((x2+x1)/2,(y2+y1)/2,(z2+z1)/2) else None
  else if y=0 then if x= -z && (abs(x) mod 2)=0 then Some ((x2+x1)/2,(y2+y1)/2,(z2+z1)/2) else None
  else if z=0 then if y= -x && (abs(y) mod 2)=0 then Some ((x2+x1)/2,(y2+y1)/2,(z2+z1)/2) else None
 else None
;;

let vec_et_dist ((x1,y1,z1):case) ((x2,y2,z2):case) : vecteur*int =
  let (x,y,z)=(x2-x1,y2-y1,z2-z1) in
  if x=0 then
    let d=abs y in
    (x/d,y/d,z/d),d
  else
    if y=0 then
    let d=abs z in
    (x/d,y/d,z/d),d
    else
      let d=abs x in
      (x/d,y/d,z/d),d
;;

let tourner_liste (l : 'a list) : 'a list =
  if l=[]
    then []
  else let (pr::fin)=l in
    fin@[pr]
;;

let rec der_liste (l:'a list) : 'a =
  match l with
  |pr::[] -> pr
  |pr::fin -> der_liste fin
;;

let rec remplir_segment (m:int)((i,j,k):case): case list =
  match m with
  | 0 -> []
  | 1 -> [(i,j,k)]
  | x -> [(i,j,k)]@(remplir_segment (x-1) (i,j+1,k-1))
;;


let rec remplir_triangle_bas (m:int)((i,j,k):case): case list =
  match m with
  | 0 -> []
  | n -> (remplir_segment n (i,j, k))@(remplir_triangle_bas (n-1)) (i-1,j+1,k)
;;

let rec remplir_triangle_haut (m:int)((i,j,k):case): case list =
  match m with
  | 0 -> []
  | n -> (remplir_segment n (i,j, k))@(remplir_triangle_haut (n-1) (i+1,j,k-1))
;;

let rec colorie (co:couleur) (list_ca:case list) : case_coloree list =
  match list_ca with
  | [] -> []
  | pr::fin -> (pr, co)::(colorie co fin)
;;

let nombre_joueurs (liste_couleur:couleur list) : int =
  List.length liste_couleur
;;

let rec tourner_liste_case_coloree (m:int) (liste_case_coloree:case_coloree list) : case_coloree list =
  match liste_case_coloree with
  | [] -> []
  | (case,couleur)::fin -> (tourner_case m case, couleur)::tourner_liste_case_coloree m fin
;;

let tourner_config (config:configuration) : configuration =
  let (liste_case_coloree,liste_couleur,dim)=config in
  let nb_joueurs=nombre_joueurs liste_couleur in
  (tourner_liste_case_coloree (6/nb_joueurs) liste_case_coloree,liste_couleur,dim)
;;

let rec remplir_liste_case_coloree (nb_joueurs:int) (joueurs:couleur list) (dim:dimension) : case_coloree list =
  match joueurs with
  | [] -> []
  | pr::fin -> (colorie pr (remplir_triangle_bas dim (-dim-1,1,dim)))@(tourner_liste_case_coloree (6/nb_joueurs) (remplir_liste_case_coloree nb_joueurs fin dim))
;;

let remplir_init (joueurs:couleur list) (dim:dimension) : configuration =
  let liste_cases_coloree=
  remplir_liste_case_coloree (nombre_joueurs joueurs) joueurs dim
  in (liste_cases_coloree,joueurs,dim)
;;

let quelle_couleur (ca:case) (co:configuration) : couleur =
  let (liste_case_coloree,liste_couleur,dim)=co in
  associe ca liste_case_coloree Libre
;;

let rec supprime_dans_config (conf:configuration) (c:case) : configuration =
  let (liste_case_coloree,liste_couleur,dim) = conf in
  ((List.filter (fun (ca,co) -> ca<>c) liste_case_coloree),liste_couleur,dim)
;;

let est_coup_valide (conf:configuration) (Du(c1,c2):coup) : bool =
  let (liste_case_coloree,liste_couleur,dim)=conf in
  let joueur_courant::fin= liste_couleur in
  (sont_cases_voisines c1 c2) && 
  (associe c1 liste_case_coloree Libre)=joueur_courant &&
  (associe c2 liste_case_coloree Libre)=Libre &&
  (est_dans_losange c2 dim)
;;

let appliquer_coup (conf:configuration) (Du(c1,c2)) : configuration =
  let (liste_case_coloree,liste_couleur,dim)=conf in
  let joueur_courant::fin= liste_couleur in
  let nouvelle_conf=supprime_dans_config conf c1 in
  let (liste_case_coloree,liste_couleur,dim)=nouvelle_conf in
  (liste_case_coloree@[(c2,joueur_courant)],liste_couleur,dim)
;;

let mettre_a_jour_configuration (conf:configuration) (c:coup) : configuration =
  if est_coup_valide conf c then  appliquer_coup conf c else failwith "Ce coup n'est pas valide, le joueur doit rejouer"
;;

let addition_vecteur ((x1,x2,x3):case)((y1,y2,y3):case):case =
  (x1+y1,x2+y2,x3+y3);;

let rec est_libre_seg (c1:case)(c2:case)(c:configuration):bool=
let (vec,dist)=vec_et_dist c1 c2 in match dist with
| 0 -> true
| x -> (quelle_couleur (addition_vecteur c1 vec) c)=Libre && (est_libre_seg (addition_vecteur c1 vec) c2 c)
;;
