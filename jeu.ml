open OcsfmlSystem

type u =
    {
      point: float * float ;
      iteration_maximale: int ;
      mutable iteration_actuelle: int ;


      mutable temps_ecoule: int ;
      timer: clock ;
    }
type t = u option ref 


let nouveau () =
  ref None


let lancer jeu =
  
  let x0 = Random.float 2. in
  let y0 = Random.float 1. in
  let iteration_maximale = 1 + Random.int 9 in
  
  let (x1, y1) =
    if x0 < 1. && x0 +. y0 < 1.
    then (1. -. x0, 1. -. y0)
    else if x0 > 1. && x0 -. y0 > 1.
    then (3. -. x0, 1. -. y0)
    else (x0, y0)
  in

  
  let point = (x1 /. 2., y1) in
  jeu := Some
    {
      point ;
      iteration_maximale ;
      iteration_actuelle = 1 ;
      temps_ecoule = 0 ;
      timer = new clock 
    }

let fini j =
  match !j with
  | Some jeu ->
     jeu.iteration_actuelle = jeu.iteration_maximale
     && jeu.temps_ecoule > 500
  | None -> false
	      
let a_gagne j =
  match !j with
  | Some jeu ->
     let rec impl it (x, y) =
       if it > jeu.iteration_maximale
       then true
       else begin
	   if y <= 1. /. 2.
	   then impl (succ it) (2. *. (x -. 1. /. 4.), 2. *. y)
	   else if y >= 2. *. x
	   then impl (succ it) (2. *. x, 2. *. (y -. 1. /. 2.))
	   else if y +. 2. *. x >= 2.
	   then impl (succ it) (2. *. (x -. 1. /. 2.), 2. *. (y -. 1. /. 2.))
	   else false
	 end
     in impl 1 jeu.point
  | None -> false
	      
let mettre_a_jour j =
  match !j with
  | Some jeu ->
     begin
       let t = jeu.timer#restart in
       jeu.temps_ecoule <- jeu.temps_ecoule + Time.as_milliseconds t ;
       if not (fini j) && jeu.temps_ecoule > 500
       then
	 begin
	   jeu.iteration_actuelle <- succ jeu.iteration_actuelle ;
	   jeu.temps_ecoule <- jeu.temps_ecoule mod 500
	 end
     end
  | None -> ()
	    
let en_cours f j =
  match !j with
  | Some jeu -> f jeu
  | None -> ()
