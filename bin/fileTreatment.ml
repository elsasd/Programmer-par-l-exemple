open FileReader;;

exception Wrong_input

(** Convertit un fichier en tableau de paire de string : chaque 
    ligne du fichier est une ligne du tableau elle-même divisée
    en paire (entrée, resultat_attendu)*)
let file_to_list file : (string * string) list =
    let rec split_in_out lines =
        match lines with
        | [] -> []
        | h :: t ->
            let h_exp =  String.split_on_char '\t' h
            in 
            match List.length h_exp with
            | 2 -> (List.nth h_exp 0, List.nth h_exp 1) :: (split_in_out t)
            | 1 -> split_in_out t
            | _ -> raise Wrong_input
    in
    split_in_out (String.split_on_char '\n' (read_lines file))


(** Convertit un fichier en tableau de paire de string 
    et affiche chaque ligne tu tableau en faisant un retour 
    à la ligne : utilisé pour les tests initiaux *)
let print_file file = 
    let tab = file_to_list file
    in
    let rec print_line t =
        match t with
        | [] -> print_string "\n[END FILE]\n\n";
        | h :: t -> 
            begin 
            match h with
            | (f, s) -> 
                print_string f;
                print_string " : ";
                print_string s;
                if t <> [] 
                then print_char '\n';
                print_line t;
            end
    in
    print_string "[BEGIN FILE]\n";
    print_line tab

