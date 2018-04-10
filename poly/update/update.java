import java.io.*;

public class update {
	public static File currentDirectory;
	
	public static void runProcess(String ... args) {
		//System.out.printf("\t\t\tprocess:%s args:%s\n", args[0], args[1]);
		
		// invoke process builder
		ProcessBuilder pb = new ProcessBuilder(args);
		pb.directory(currentDirectory);
		try {
			Process p = pb.start();
			InputStreamReader in_reader = new InputStreamReader(
					new BufferedInputStream(p.getInputStream()));
			BufferedReader reader = new BufferedReader(in_reader);
			while (true) {
				String line = reader.readLine();
				if (line == null)
					break;
				System.out.println(line);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void updateSourceDir(File path){
		System.out.printf("root:%s\n", path.getAbsoluteFile());
		
		FileFilter filter = new FileFilter() {
			public boolean accept(File file) {
				return file.isDirectory();
			}
		};
		
		for (File project : path.listFiles(filter)) {
			System.out.printf("\nproject:%s\n", project.getAbsoluteFile());
			currentDirectory = project;
			
			for (File dir : project.listFiles(filter)) {
				//System.out.printf("\t\tdir:%s\n", dir.getAbsoluteFile());
				
				if (dir.getName().equals(".bzr")) {
					runProcess("bzr", "pull");
					runProcess("bzr", "update");
				}
				else if (dir.getName().equals(".hg")) {
					runProcess("hg", "pull");
					runProcess("hg", "update");					
				}
				else if (dir.getName().equals(".svn")) {
					runProcess("svn", "update");
				}
				else if (dir.getName().equals(".git")) {
					runProcess("git", "pull");
				}
				else
					continue;
			}
		}
	}
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		if (args.length < 2) {
			updateSourceDir(new File("/home/sa/src"));
		}
		else {
			for (String arg : args) {
				updateSourceDir(new File(arg));
			}
		}
	}
}
