open GraphFile;;
open Graph;;
open FileTreatment;;

let () =
    match Array.length Sys.argv with 
    | n when n=3 || n=4  -> 
        if (Sys.argv.(1)="genconcat") then
            let prog = create_prog_from Sys.argv.(2) in
            print_file Sys.argv.(2);
            print_string "[DEBUT DU PROG]\n";
            Graph.print_labs (prog);
            print_string "\n[FIN DU PROG]\n";
            (if (n=4)
            then
                (let file = apply_file (Sys.argv.(3)) prog
                in
                print_string "\n[DEBUT DE L'INPUT -> OUTPUT]\n";
                print_string (file);
                print_string "[FIN DE L'INPUT -> OUTPUT]\n";))
        else 
            raise (Invalid_argument "Seule la commande 'genconcat' est acceptée")
    | _ -> raise (Invalid_argument "\n\nVotre commande doit être de la forme : \n\n'./dune build projet -- genconcat <fichier>'\n\nOu de la forme :\n\n./'dune build projet --genconcat <fichier1> <fichier2>'\n\n")

