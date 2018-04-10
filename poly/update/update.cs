using System;
using System.IO;
using System.Diagnostics;

class UpdateManager {
	public static void runProcess(String name, String args) {
		Process proc = new Process();
		 proc.EnableRaisingEvents = false;
		 proc.StartInfo.FileName = name;
		 proc.StartInfo.Arguments = args;
		 proc.Start();
		 proc.WaitForExit();
	} 
    
    public static void scanDirectory(string path) {
		Console.WriteLine("scanning: {0}\n", path);

		foreach(String project in Directory.GetDirectories(path)) {
			Console.WriteLine("\nupdating: {0}", project);

			foreach(String dir in Directory.GetDirectories(project)) {
				String directory = Path.GetFileName(dir);

				Directory.SetCurrentDirectory(project);

				switch (directory) {
				case ".bzr":
					runProcess("bzr", "pull");
					runProcess("bzr", "update");
					break;

				case ".svn":
					runProcess("svn", "update");
					break;

				case ".git":
					runProcess("git", "pull");
					break;

				case ".hg":
					runProcess("hg", "pull");
					runProcess("hg", "update");
					break;

				default:
					break;
				}

			}

		}

	}

	public static void Main(string[]args) {
		String PATH = "/home/sa/src";

		if (args.Length >= 1) {
			foreach(string arg in args) {
				scanDirectory(arg);
			}
		} else {
			scanDirectory(PATH);
		}
	}
}

