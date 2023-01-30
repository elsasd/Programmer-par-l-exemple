(** Lit un fichier et retourne la chaîne de caractères
    qui correspond au fichier *)
let read_lines file =
    let in_ch = open_in file in
    let rec read_line str =
        let line = 
            try Some (input_line in_ch)
            with End_of_file -> 
                None
        in
        match line with
        | None -> str
        | Some s_line -> read_line (str^s_line^"\n");
    in 
    read_line "";;
(*ICI process = fonction qu'on voudra appliquer au lignes*)
