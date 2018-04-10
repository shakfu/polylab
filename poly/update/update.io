#!/usr/bin/env io

ops := Map with(
    ".bzr", list("bzr pull", "bzr update"),
    ".hg" , list("hg pull", "hg update"), 
    ".svn", list("svn update"),
    ".git", list("git pull"))

VCS := Object clone

VCS update := method(path,
    root := Directory with(path)
    writeln("root: ", path)
    root directories foreach(project,
        project directories foreach(dir,
            if(ops hasKey(dir name),
                writeln("\nproject: ", project name, " @ ", project path)
                dir setCurrentWorkingDirectory(project path)
                ops at(dir name) foreach(cmd, System system(cmd))))))

VCS main := method( 
    if(System args size < 2, update("/home/sa/src"),
        System args rest foreach(path, update(path))))

# entry point

VCS main

