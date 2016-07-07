import subprocess

join = lambda lst: " ".join(lst)

def debug(d):
    keys = sorted(d.keys())
    for k in keys:
        print k,':', d[k]
    print

def get_packages():
    process = subprocess.Popen(['dpkg', '-l'], shell=False, 
                                                stdout=subprocess.PIPE)
    stdout, stderr =  process.communicate()
    lines = stdout.splitlines()
    packages = []
    for line in lines[5:]:
        i = line.split()
        code, name, version, desc = i[0], i[1], i[2], join(i[3:])
        d = dict(
            code = i[0], 
            name = i[1], 
            version = i[2], 
            desc =  join(i[3:]),
        )
        packages.append(d)
        #debug(d)
    return packages

if __name__ == '__main__':
    get_packages()