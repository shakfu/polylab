import java.io._


object update {
  
  def runProcess(in_dir: File, cmds: List[String]): Unit = {
	  for (cmd <- cmds) {
	 	var args = cmd.split(" ")
		val pb = new ProcessBuilder(args: _*)
	    pb.directory(in_dir)
	    try {
	      val process = pb.start()
	      val in_reader = new InputStreamReader(
	        new BufferedInputStream(process.getInputStream()))
	      val reader = new BufferedReader(in_reader)
	      var doNext = true
	      while (doNext) {
	        var line = reader.readLine()
	        if (line == null)
	          doNext = false
	        else 
	          println(line)
	       }
	    } catch {
	      	case e: IOException => e.printStackTrace()
	    }
    }
  }

  def update(path: File) =  {
	val ops : Map[String, List[String]] = Map(
	    ".bzr" -> List("bzr pull", "bzr update"),
	    ".svn" -> List("svn update"),
	    ".hg"  -> List("hg pull", "hg update"),
	    ".git" -> List("git pull"))

    val filter = new FileFilter() {
        def accept(file: File) = file.isDirectory()
    }
    
    for(project : File <- path.listFiles(filter)) {
    	println("\nproject: " + project.getAbsoluteFile())
    	for(dir : File <- project.listFiles(filter)) {
    		//println("\t\tdir: " + dir)
    		if (ops.contains(dir.getName()))
    			runProcess(project, ops.apply(dir.getName()))
    	}
    }
  }

  def main(args: Array[String]) = {
    if (args.length > 1)
      args.foreach( (arg: String) => update(new File(arg)) )
    else update(new File("/home/sa/src"))
  }
}

