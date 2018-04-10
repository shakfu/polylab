
#light

open System
open System.IO
open System.Diagnostics


let ops = [
    (".bzr", ["bzr pull"; "bzr update"])
    (".hg",  ["hg pull"; "hg update"])
    (".svn", ["svn update"])
    (".git", ["git pull"])] |> Map.ofList

let srcdir = "/home/sa/src"

let run (cmd : string) =
    let args = cmd.Split()
    let x, y = args.[0], args.[1]
    Process.Start(x, y).WaitForExit()


let update path = 
    let get_dirs path = Array.toList(Directory.GetDirectories(path))
    let projects = get_dirs path
    for proj in projects do
        let dirs = get_dirs proj
        for dir in dirs do
            let dirname = Path.GetFileName(dir)
            if ops.ContainsKey(dirname) then
                printfn "\nproj: %A" proj
                Directory.SetCurrentDirectory(proj)
                for cmd in ops.Item(dirname) do
                    run cmd |> ignore

[<EntryPoint>]
let main args = 
    //printfn "args: %A" args
    if args.Length > 0 then Array.iter update args
    else update srcdir
    0

