module Main where
import Database.SQLite
main = 
    do  c <- openConnection "db.sqlite"
        execStatement_ c "create table t1 (id INTEGER, age INTEGER);"
        execStatement_ c "insert into t1 (id, age) values (1,2);"
        -- insertRow c "t1" "(id,3)"
        -- result <- execStatement c "select * from t1;"
        closeConnection c
        print $ "Hey it works " ++ friend
    where
        friend = "Buddy"
