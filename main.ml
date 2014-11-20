open OcsfmlWindow
open OcsfmlGraphics


let afficher_point (fenetre:#render_window) (x, y) base =
  let hauteur = sqrt 3. /. 2. *. base in
  let position = (x *. base, y *. hauteur) in
  fenetre#draw (new circle_shape ~position ~radius:3. ~fill_color:Color.green ())

let afficher_texte (fenetre:#render_window) font t =
  let (w, h) = fenetre#get_view#get_size in
  let textgr =
    new text
	~string:t
	~position:(w /. 2., h -. 50.)
	~font
	~character_size:75 ()
  in
  let rect = textgr#get_local_bounds in
  textgr#set_origin (rect.FloatRect.width /. 2.) (rect.FloatRect.height) ;
  fenetre#draw textgr
	       
let _ =
  let font = new font (`File "sansation.ttf") in
  
  let vm = VideoMode.create ~w:800 ~h:1300 () in
  let context = ContextSettings.create ~antialising_level:50 () in
  let window = new render_window vm "Kaloterie" ~context in

  let jeu = Jeu.nouveau () in
  let cache = Sierpinski.nouveau () in

  

  let rec poll_event () =
    let open Event in
    match window#poll_event with
    | None -> ()
    | Some Closed
    | Some KeyPressed { code = KeyCode.Q ; control = true ; _ } ->
       (window#close ; poll_event ())
    | Some KeyPressed { code = KeyCode.Space ; control = true ; _ } ->
       Jeu.lancer jeu
    | Some Resized { width ; height } ->
       let view =
	 new view
	     (`Rect
	       (FloatRect.create
		  ~position:(0.,0.)
		  ~size:(float width, float height) ()))
       in
       window#set_view view
    | _ -> poll_event ()
		      
  in
  
  let rec main_loop () =
    if window#is_open
    then
      begin
	poll_event () ;
	Jeu.mettre_a_jour jeu ;
	window#clear ();
	Jeu.en_cours
	  (fun j ->
	   let w = fst (window#get_view#get_size) in
	   Sierpinski.afficher window cache w j.Jeu.iteration_actuelle ;
	   afficher_point window j.Jeu.point w ;
	   if Jeu.fini jeu
	   then
	     begin
	        afficher_texte window font @@
			       if Jeu.a_gagne jeu
			       then "Gagné"
			       else "Perdu"
	     end
	  ) jeu ;
	window#display ;
	main_loop ()
      end
  in
  main_loop () ;
  font#destroy
