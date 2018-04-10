require "luarocks.loader"
require "lfs"

ops = {
    ['.bzr'] = {'bzr pull', 'bzr update'},
    ['.hg' ] = {'hg pull', 'hg update'},
    ['.svn'] = {'svn update'},
    ['.git'] = {'git pull'}
}

function get_dirs(path)
    res = {}
    if string.sub(path, -1) == "/" then
        path = string.sub(path, 1, -2)
    end
    for entry in lfs.dir(path) do
        if entry ~= "." and entry ~= ".." then 
            fullpath = path .. "/" .. entry
	        local attr = lfs.attributes(fullpath)
	        if attr.mode == "directory" then
                res[fullpath] = entry
            end
        end
    end
    return res
end

function update_root(path)
    print("root:", path)
    local projects = get_dirs(path)
    for project, i in pairs(projects) do
        print("proj:", project)
        subdirs = get_dirs(project)
        for subdir, j in pairs(subdirs) do
            print(j, subdir)
            if ops[j] then
                for _, cmd in ipairs(ops[j]) do
                    lfs.chdir(project)
                    os.execute(cmd)
                end
            end
        end    
    end
end
 
src = "/home/sa/src"

update_root(src)


