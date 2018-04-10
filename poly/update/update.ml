(* File update.ml 

#load "unix.cma"

to compile: ocamlopt -o update unix.cmxa update.ml
*)

module VCS = struct

    let operations = [
        (".bzr", ["bzr pull"; "bzr update"]);
        (".hg",  ["hg pull"; "hg update"]);
        (".svn", ["svn update"]);
        (".git", ["git pull"]);
    ]

    let get_dirs path = 
        let files = Array.map (Filename.concat path) (Sys.readdir path) in
            List.filter Sys.is_directory (Array.to_list files)

    let update_project path =
        let find_sentinel names =
            let flip f x y = f y x in
            let keys = List.map fst operations in
                List.filter (flip List.mem keys) names in
        let folder_paths = get_dirs path in
        let folder_names = List.map Filename.basename folder_paths in
        let sentinels = find_sentinel folder_names in
        let execute s = List.map Sys.command (List.assoc s operations) in
        begin
            print_newline();
            print_endline(path);
            Unix.chdir path;
            List.flatten (List.map execute sentinels)
        end

    let update_root path =
        let projects = get_dirs path in
            List.map update_project projects
   
    (* main module entry point *)
    let update path =
        let sum xs = List.fold_left (fun x y -> x + y) 0 xs in
        let result = sum (List.flatten (update_root path)) in
            if result > 0 then print_endline "errors found"
            else print_endline "done."
end

let srcdir = "/home/sa/src"

let main () = 
    let tail lst =
       match lst with
               [] -> []
        | x :: xs -> xs
    in
    let args = Array.to_list Sys.argv in
        begin
            if List.length args < 2
            then VCS.update srcdir
            else List.iter VCS.update (tail args);
            print_newline();
            exit 0
        end;;
main ()


