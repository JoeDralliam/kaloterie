open OcsfmlGraphics

let sierpinsky n base =
  let rec impl n (sx, sy) base acc =
    let hauteur = sqrt 3. /. 2. *. base in
    if n = 1
    then begin
	Vertex.create ~color:Color.red ~position:(sx, sy) ()
	:: Vertex.create ~color:Color.red ~position:(sx +. base /. 2., sy -. hauteur) ()
	:: Vertex.create ~color:Color.red ~position:(sx +. base, sy) ()
	:: acc
      end
    else
      let n' = pred n in
      let base' = base /. 2. in
      impl n' (sx, sy) base' acc
      |> impl n' (sx +. base' /. 2., sy -. hauteur /. 2.) base'
      |> impl n' (sx +. base', sy) base'
  in
  assert (n > 0) ;
  let hauteur = sqrt 3. /. 2. *. base in
  let vertices = impl (n + 1) (0., hauteur) base [] in
  new vertex_array ~primitive_type:PrimitiveType.Triangles vertices
 

type u =
       {
	 iteration: int;
	 valeur: vertex_array
       }
type cache = u option ref

let nouveau () = ref None
	 
let afficher (fenetre:#render_window) cache base iteration =
  let cachee =
    match !cache with
    | Some c when c.iteration = iteration ->
       c
    | _ ->
       let c =
	 {
	   iteration ;
	   valeur = sierpinsky iteration base
	 }
       in
       (cache := Some c ; c)
  in fenetre#draw cachee.valeur
     
