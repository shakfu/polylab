db := SQLite3 clone
db setPath("myDatabase.sqlite")
db open
db exec("CREATE TABLE Dbm (key, value)")
db exec("CREATE INDEX DbmIndex ON Dbm (key)")
db exec("INSERT INTO Dbm ('key', 'value') VALUES ('a', '123')")
db exec("INSERT INTO Dbm ('key', 'value') VALUES ('a', 'efg')")
rows := db exec("SELECT key, value FROM Dbm WHERE key='a'")
db exec("DELETE FROM Dbm WHERE key='a'")
rows := db exec("SELECT key, value FROM Dbm WHERE key='a'")
db close
