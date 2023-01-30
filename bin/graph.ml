open AST;;

exception Empty

module Graph =
struct 
    type label = expression
    type node = Node of int
    type vert = node * node * label list
    type graph = node list * vert list

    (** Affiche les pos_exp *)
    let print_pos_exp exp =
        match exp with
        | Forward n -> print_string "Forward "; print_int n
        | Backward n -> print_string "Backward "; print_int n

    (** Affiche les labels d'une arête *)
    let rec print_labs labs =
        match labs with
        | [] -> print_string " "
        | l :: lt ->
            begin
                match l with
                | Const exp -> 
                    print_string ("Const \""^exp^"\"");
                    if lt <> [] 
                    then print_string "; ";
                    print_labs lt
                | Extract (p1, p2) ->
                    print_string "Extract (";
                    print_pos_exp p1;
                    print_string ", ";
                    print_pos_exp p2;
                    print_string (")"^"; ");
                    print_labs lt
            end
    
    (** Affiche les arêtes d'un graphe *)
    let rec print_verts verts =
        match verts with 
        | [] -> print_string "[END GRAPH]\n\n"
        | (Node n1, Node n2, labs) :: t ->
            print_int n1; print_string " -> "; print_int n2;
            print_string " : "; print_labs labs; print_string "\n";
            print_verts t

    (** Affiche la liste de noeuds *)
    let print_node n = 
        let rec aux n =
            match n with 
            | [] -> print_string "[END NODE]\n"
            | h :: t ->
                begin match h with
                | Node h -> 
                    print_string "Node "; print_int h;
                    print_char ' '; aux t
                end
        in
        print_string "[BEGIN NODE] ";
        aux n

    (** Affiche un graphe *)
    let print (graph : graph) =
        print_string "[BEGIN GRAPH]\n";
        match graph with
        | n, verts ->
            print_node n; print_verts verts

    (** Retourne la liste ns1 sans les éléments de ns2 *)
    let rec minus ns1 ns2 =
        let rec min ns n =
            match ns with
            | [] -> []
            | h :: ns -> 
                if h = n 
                then min ns n 
                else 
                h :: (min ns n)
        in
        match ns2 with
        | [] -> ns1
        | n :: ns2 ->
            minus (min ns1 n) ns2

    (** Renvoie la liste des noeuds atteignables depuis start_node *)
    let reachable_nodes vs start_node =
        let rec reachable_nodes_from vs start_node n =
            match vs with
            | [] -> 
                start_node
            | (n1, n2, labs) :: vs ->
                if n1=n && labs<>[] && (not (List.mem n2 start_node))
                then reachable_nodes_from vs (n2 :: start_node) n
                else reachable_nodes_from vs start_node n
        in
        let rec aux vs visited non_visited acc =
            match non_visited with
            | [] -> acc
            | n :: non_visited ->
                let new_start =
                    reachable_nodes_from vs acc n
                in                 
                aux vs 
                (n :: visited) 
                (non_visited @ (minus (minus new_start non_visited) visited)) 
                (new_start)
        in
        aux vs [] start_node start_node

    (** Renvoie le noeud dont la distance est la plus petite *)
    let smallest part ds =
        (* Récupère la liste de distances correspondant à la 
        partition part *)
        let rec ds_part part ds =
            match part with 
            | [] -> []
            | Node n :: part -> 
                List.nth ds n :: (ds_part part ds)
        in
        (* Renvoie le noeud de plus petite distance de la partition 
        part *)
        let rec aux min_node min ds part =
            match ds, part with
            | [], [] -> min_node
            | h :: ds, p :: part when h<min-> 
                aux p h ds part
            | _ :: ds, _ :: part ->
                aux min_node min ds part
            | _, _ -> raise (Invalid_argument "smallest")
        in
        aux (List.hd part) (List.hd (ds_part part ds)) (ds_part part ds) part

    (** Donnne le poids d'une arête *)
    let get_weight_of l len =
        let get_weight_of_a_single_label _l len=
            match _l with 
                |Const(_) -> len + 1
                |Extract(_, _) -> len
        in 
        match l with
        |([]) -> raise (Invalid_argument ("label n'a pas la bonne taille"))
        |(h::[]) -> get_weight_of_a_single_label h len
        |(_::_) -> raise (Invalid_argument ("label n'a pas la bonne taille"))


    (** Retourne la valeur dans la liste list correspondant au 
    noeud n *)
    let nth_node n list =
        match n with
        | Node n -> List.nth list n 

    (** Retourne une liste avec la valeur v à l'indice Node n *)
    let set_node n v list =
        let rec set_int n v list =
            match list with
            | [] -> raise (Invalid_argument "index out of bounds")
            | _ :: t when n=0 -> 
                v :: t
            | h :: t -> 
                h :: set_int (n - 1) v t
        in
        match n with
        | Node n -> set_int n v list

    let vert_bet_nodes n1 n2 =
        match n1, n2 with
        | Node n1, Node n2 -> n2 - n1

    (** Met à jour les distances *)
    let rec update small ns vs dt_pd =
        (* Met à jour le poids du noeud n si necéssaire en partant de
        small *)
        let rec update_from_to small n vs dt_pd =
            match vs, dt_pd with
            | (n1, n2, labs) :: _, (dt, pd) when n1=small && n2=n ->
                let len = vert_bet_nodes n1 n2 in
                if get_weight_of labs len + nth_node n1 dt < nth_node n2 dt 
                then 
                    set_node n2 (get_weight_of labs len + nth_node n1 dt) dt,
                    set_node n2 n1 pd
                else (dt, pd)
            | _ :: vs, (_, _) ->
                update_from_to small n vs dt_pd 
            | [], (_, _) -> 
                dt_pd
        in
        (* on veut matcher small avec tous les autres noeuds existants *)
        match ns with
        | n :: ns when n<>small ->
            update small ns vs (update_from_to small n vs dt_pd)
        | _ :: ns ->
            update small ns vs dt_pd 
        | [] -> dt_pd

    (** Applique l'algorithme de Dijkstra et renvoie la liste des
    predécesseurs *)
    let dijkstra g =
        (* Initialise la distance *)
        let rec init_dist ns i =
            match ns with
            | [] -> []
            | Node n :: t -> 
                if n = i 
                then 0 :: (init_dist t i)
                else 100 :: (init_dist t i)
        in
        (* Initialise la distance et les predécesseurs *)
        let init ns i =
            (init_dist ns i, ns)
        in
        (* Tant que la partition n'est pas vide, on met à jour
        les distances depuis le noeud le plus petit *)
        let rec tant_que part ns vs dt_pd =
            let small : node= 
                match part, dt_pd with
                | [], _ -> Node 0
                | _, (dt, _) -> smallest part dt
            in
            match part with
            | [] -> dt_pd
            | _ ->
                tant_que (minus part [small]) ns vs (update small ns vs dt_pd)
        in
        match g with
        | ns, vs ->
            begin
            match tant_que ns ns vs (init ns 0) with
            | _, pd -> pd
            end

    (** Génère une liste d'expression correspondant au programme concat *)
    let concat g =
        let rec concat_prog pred vs vs_init nth =
            match vs, (List.nth pred nth) with
            | _, Node n3  when nth=n3 ->
                []
            | (Node n1, Node n2, [lab]) :: _, Node n3 when n2=nth && n1=n3 ->
                lab :: concat_prog pred vs_init vs_init n3
            | _ :: vs , _ -> 
                concat_prog pred vs vs_init nth
            | _, _ ->
                raise (Invalid_argument "concat")
        in
        let concat_dij pred vs =
            List.rev (concat_prog pred vs vs (List.length pred - 1))
        in
        match g with
        | _, vs -> concat_dij (dijkstra g) vs 
            
    (** Applique extrac st ed sur input *)
    let apply_extract (input: string) st ed =
        (* Donne l'index correspondant à l'expression i *)
        let b_to_f i =
            match i with 
            | Backward b -> 
                (String.length input) - b
            | Forward f -> f
        in
        String.sub input (b_to_f st) ((b_to_f ed) - (b_to_f st))

    (** Applique un programme concat à un input *)
    let rec apply_concat input prog =
        match prog with
        | [] -> ""
        | expr :: prog ->
            begin
            match expr with
            | Const c ->
                c^(apply_concat input prog)
            | Extract (st, ed) ->
                (apply_extract input st ed)^(apply_concat input prog)
            end 


    (** Intersection de deux set de la forme (i, j) où i et j sont
    les indexes de début et de fin de la fonction extract *)
    let inter_set v1 v2: pos_expression list * pos_expression list * label list =
        (* Renvoie l'arrete Some v si elle est dans vs
        sinon None *)
        let rec vert_in_set v vs =
            match vs with
            | [] -> None
            | h :: t -> 
                if v = h 
                then Some v
                else vert_in_set v t
        in 
        (* Intersection de deux ensembles v1 v2 *)
        let rec aux v1 v2 acc =
            match v1 with
            | [] -> acc
            | h :: t -> 
                begin
                match vert_in_set h v2 with
                | None -> aux t v2 acc
                | Some l ->
                    aux t v2 (l :: acc)
                end
        in
        match v1, v2 with
        | (p1, p2, c1), (p3, p4, c2) ->
            (aux p1 p3 [], aux p2 p4 [], aux c1 c2 [])
    
    (** Récupère deux ensembles de tous les i et tous les j 
    possibles et renvoie les labels Extract (i, j)  *)
    let get_verts v : label list =
        let rec pair_i_j is js acc : label list =
            match is, js with
            | _, [] -> 
                acc
            | [], _ -> 
                acc
            | i :: is, j :: js -> 
                pair_i_j is js (Extract (i, j) :: acc)
        in
        match v with
        | (is, js, cs) -> 
            pair_i_j is js cs

    (** Récupère les labels Extract (i, j) et renvoie deux ensembles
    de tous les i et tous les j possibles *)
    let get_set v =
        let rec aux v set_i set_j const=
            match v with
            | [] -> (set_i, set_j, const)
            | Extract (i, j) :: t ->
                begin 
                match List.exists (fun x -> i = x) set_i, 
                List.exists (fun x -> j = x) set_j with
                | true, true -> aux t set_i set_j const
                | true, false -> aux t set_i (j :: set_j) const 
                | false, true -> aux t (i :: set_i) set_j const
                | false, false -> aux t (i :: set_i) (j :: set_j) const
                end
            | h  :: t -> aux t set_i set_j (h :: const)
        in
        aux v [] [] []

    (** Intersection de deux ensembles d'arêtes *)
    let inter_vert v1s v2s nodes len : vert list =
        let rec inter_v_vs v1 v2s nodes len acc : vert list =
            match v1, v2s with
            | _, [] -> 
                acc
            | (Node i1, Node i2, lab1), (Node i3, Node i4, lab2) :: v2s ->
                inter_v_vs v1 v2s nodes len
                ((List.nth  nodes (i1 * len + i3),
                List.nth nodes (i2 * len + i4),
                get_verts (inter_set (get_set lab1) (get_set lab2))) :: acc)
        in 
        let rec inter_vs_vs v1s v2s nodes len acc =
            match v1s with
            | [] -> acc
            | v1 :: v1s ->
                inter_vs_vs v1s v2s nodes len ((inter_v_vs v1 v2s nodes len []) @ acc)
        in
        inter_vs_vs v1s v2s nodes len []

    (** Intersection de deux ensembles de noeuds *)
    let inter_node n1s n2s =
        let rec inter_n_ns i1 n2s len acc =
            match n2s with
            | [] -> acc
            | Node i2 :: n2s ->
                inter_n_ns i1 n2s len (Node (i1 * len + i2) :: acc)
        in
        let rec inter_ns_ns n1s n2s acc =
            match n1s with
            | [] -> acc
            | Node i1 :: n1s ->
                inter_ns_ns n1s n2s (inter_n_ns i1 n2s (List.length n2s) [] @ acc)
        in
        List.rev (inter_ns_ns n1s n2s [])

    (** Crée une liste avec le nombre de noeuds passé en paramètre + 1 *)
    let rec create_node nb acc =
        match nb with
        | 0 -> Node 0 :: acc
        | _ -> create_node (nb - 1) (Node nb :: acc)

    let rename g =
        let rec rename_vs nodes corres vs =
            match vs with
            | [] -> [] 
            | (n1, n2, lab) :: vs -> 
                (List.nth nodes (List.assoc n1 corres), 
                List.nth nodes (List.assoc n2 corres),
                lab) :: (rename_vs nodes corres vs)
        in
        let rec rename_node ns nb =
            match ns with
            | [] -> []
            | n :: ns ->
                (n, nb) :: rename_node ns (nb + 1)          
        in
        match g with
        | ns, vs -> 
            let nodes = create_node (List.length ns - 1) []
            in 
            nodes, rename_vs nodes (rename_node ns 0) vs

    (** Ne laisse qu'une pos_exp par arête *)
    let clean_graph g =
        let rec sort ns acc =
            let rec insert e ns =
                match ns, e with
                | Node n :: _, Node en when en <n -> 
                    e :: ns
                | n :: t, _ -> 
                    n :: (insert e t)
                | [], _ -> [e]
            in
            match ns with
            | [] -> acc
            | h :: t ->
                sort t (insert h acc)
        in
        let reach ns vs =
            match ns with
            | n :: _ -> sort (reachable_nodes vs [n]) []
            | [] -> raise Empty 
        in
        let rec clean_verts vs ns =
            match vs with
            | (n1, n2, l :: _) :: vs ->
                if (List.exists (fun x -> x=n1) ns)
                then (n1, n2, [l]) :: (clean_verts vs ns)
                else clean_verts vs ns
            | (_, _, []) :: vs -> 
                clean_verts vs ns
            | [] -> []
        in
        match g with 
        | ns, vs ->
            let nodes =
                reach ns vs
            in
            rename (nodes, clean_verts vs nodes)

    (** Intersection de deux graphes *)
    let inter (g1:graph) (g2:graph) : graph = 
        match g1 with
        | n1, v1 ->
            begin 
            match g2 with
            | n2, v2 ->
                let nodes =
                    inter_node n1 n2
                in
                (*print_string "\n\nINTERSECTION DU GRAPHE :\n";
                print g1;
                print_string "\n\nET DU GRAPHE :\n";
                print g2;
                print_string "\n\nINTERSECTION :\n";
                print (clean_graph (nodes, inter_vert v1 v2 nodes (List.length n2)));*)
                clean_graph (nodes, inter_vert v1 v2 nodes (List.length n2))
            end   
end