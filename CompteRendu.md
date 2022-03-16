# Compte Rendu

## Question 1

- i < -dim correspond au **camp du Sud**
- i > dim correspond au **camp du Nord**
- j < -dim correspond au **camp de coté numéro deux comme indiqué ci dessous** (insérer image)
- correspond au point le plus haut du **camp nord**
- correspond au le point le plus haut à gauche du **camp Sud**
- correspond à **l'hexagone plus le camp nord, le camp de coté trois et le camp de coté cinq**.

![alt text](./image/CompteRendu/1646328781217.png)

## Question 2

La formule booléenne qui est vraie si et seulement si une case est dans le losange Nord-Sud :

```
-dim <= j && k <= dim && -dim <= k && k <= dim\
```

Implémentation :

```
    let est_dans_losange ((i,j,k):case) (dim:dimension) : bool =
        -dim <= j && k <= dim && -dim <= k && k <= dim 
    ;;
```

## Question 3

La formule booléenne qui est vraie si et seulement si une case est dans l’étoile est :

```
(i>= -dim && j>= -dim && k>= -dim) || (i<=dim && j<=dim && k<=dim)
```


Implémentation :

```
    let est_dans_étoile ((i,j,k):case) (dim:dimension) : bool =
        (i>= -dim && j>= -dim && k>= -dim) || (i<=dim && j<=dim && k<=dim)

    ;;
```

## Question 4

```
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
```

## Question 5

```
let translate (c:case) (v:vecteur) : case =
  let (c1,c2,c3)=c in
  let (v1,v2,v3)=v in
  let (d1,d2,d3)=(c1+v1,c2+v2,c3+v3) in
  (d1,d2,d3)
;;
```

## Question 6

```
let diff_case (l:case) (r:case) : vecteur =
  let (l1,l2,l3)=l in
  let (r1,r2,r3)=r in
  let (v1,v2,v3)=(l1-r1,l2-r2,l3-r3) in
  (v1,v2,v3)
;;
```

## Question 7

```
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
```

## Question 8

```
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
```

## Question 9

```
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
      (x/d,y/d,z/d),d (*Demander au prof d'ocaml*)
;;
```
