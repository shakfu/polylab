csc -deploy db.scm
chicken-install -deploy -p $PWD/db postgresql
chicken-install -deploy -p $PWD/db sql-null