open Graph;;
open AST;;
open FileTreatment;;
open FileReader;;


(** Convertit une string en tableau de caractères *)
let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []

(** Donne tous les substrings possibles d'une chaîne de caractères et les
    retourne sous la forme d'un index de départ, un index de fin et un 
    tableau de caractères *)
let sub exp =
    let rec sub_of len exp acc =
        match exp with
        | [] -> List.rev acc
        | c :: word -> 
            if len <= (List.length exp) && len > 0
            then sub_of (len - 1) word (c :: acc)
            else List.rev acc
    in
    let rec dim_exp exp acc len strt =
        match exp with
        | [] -> acc
        | _ :: word -> 
            if len <= (List.length exp) && len > 0
            then dim_exp word ((strt, (strt + len), 
                (sub_of len exp [])) :: acc) len (strt + 1)
            else dim_exp word acc len (strt + 1)
    in
    let rec all_sub exp len acc =
        if len > List.length exp 
        then acc
        else all_sub exp (len + 1) ((dim_exp exp [] len 0) @ acc) 
    in
    all_sub exp 1 []

(** Retourne une liste de paires d'index pour forward *)
let indexes_for inp exp =
    let rec lab_strt_ed inp exp strt ed =
        match inp, exp with
        | _, [] -> Some (strt, ed)
        | h_inp :: t_inp, h_exp :: t_exp ->
            if h_inp = h_exp 
            then  lab_strt_ed t_inp t_exp strt (ed + 1)
            else None
        | _ -> None
    in
    let rec lab_strt inp exp strt acc =
        match inp with
        | [] -> acc
        | _ :: t -> 
            let res = lab_strt_ed inp exp strt strt
            in
            begin
                match res with
                | None -> lab_strt t exp (strt + 1) acc
                | Some (s, e) -> lab_strt t exp (strt + 1) ((s, e) :: acc)
            end
    in
    lab_strt inp exp 0 []

(** Forward, Backward label *)
let rec lab_from indexes len =
    match indexes with
    | [] -> []
    | h :: t ->
        begin
            match h with
            | (s, e) ->
                Extract (Forward s, Forward e) ::
                Extract (Forward s, Backward (len - e)) ::
                Extract (Backward (len - s), Forward e) ::
                Extract (Backward (len - s), Backward (len - e)) ::
                lab_from t len
        end

(** Crée les labels const depuis les substrings *)
let rec lab_from_sub sub nodes inp =
    let rec string_from_tab tab =
        match tab with
        | [] -> ""
        | c :: word -> String.make 1 c ^ (string_from_tab word)
    in
    match sub with
    | [] -> []
    | (n1, n2, tab) :: t -> 
        if List.length nodes > n1 && List.length nodes > n2
       then
        (List.nth nodes n1, 
        List.nth nodes n2, 
        (Const (string_from_tab tab)) :: 
        (lab_from (indexes_for inp tab) (List.length inp))) ::
        (lab_from_sub t nodes inp)
        else raise Empty

(** Crée le graphe correspondant à une paire d'entrée/sortie *)
let create_graph_from_line line : Graph.graph =
    match line with
    | (inp, exp) ->
        let nodes = Graph.create_node (String.length exp) []
        in
        (nodes, lab_from_sub (sub (explode exp)) nodes (explode inp))

(** Crée un graphe résultant de l'intersection des graphes
    des différentes lignes du fichier *)
let rec create_graph_from_list file_list =
    match file_list with
    | [] -> ([], [])
    | [x] -> 
        create_graph_from_line x
    | h :: t ->
        Graph.inter (create_graph_from_line h) (create_graph_from_list t)

let create_graph_from file =
    create_graph_from_list (file_to_list file)

let create_prog_from file =
    let prog = Graph.concat (create_graph_from file)
    in 
    prog

let apply_file file_path prog =
    let rec apply file prog =
        match file with
        | [] -> ""
        | [""] -> ""
        | input :: file ->
            input^"\t"^(Graph.apply_concat input prog ^"\n"^ (apply file prog))
    in
    apply (String.split_on_char '\n' (read_lines file_path)) prog

    

    