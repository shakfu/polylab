namespace org.update

import System
import System.IO
import System.Diagnostics

class UpdateManager():

	ops = {
		'.bzr': ['bzr pull', 'bzr update'],
		'.hg' : ['hg pull', 'hg update'],
		'.svn': ['svn update'],
		'.git': ['git pull'],
	}

	def run_process(cmd as string):
		name, args = cmd.Split()
		proc = Process()
		proc.EnableRaisingEvents = false
		proc.StartInfo.FileName = name
		proc.StartInfo.Arguments = args
		proc.Start()
		proc.WaitForExit()

	def update(path as string):
		for proj in Directory.GetDirectories(path):
			print "\nproj: ${proj}"
			for dir in Directory.GetDirectories(proj):
				dirname = Path.GetFileName(dir)
				if dirname in ops:
					Directory.SetCurrentDirectory(proj)
					cmds = ops[dirname]
					for cmd in cmds:
						self.run_process(cmd)

def Main(argv as (string)):
	app = UpdateManager()
	if argv.Length > 0:
		for arg in argv:
			app.update(arg)
	else:
		app.update("/home/sa/src")

