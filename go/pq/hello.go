package main

import (
	"code/mylib"
	"database/sql"
	"fmt"
	"log"
	_ "github.com/lib/pq"
)

func main() {
	fmt.Printf("hello, world!\n")
	fmt.Printf("Sqrt(2)=%v\n", mylib.Sqrt(2))
	db, err := sql.Open("postgres", "user=sa dbname=sa password=sa")
	if err != nil {
		log.Fatal(err)
	}
	rows, err := db.Query("select * from users")
	defer rows.Close()
	for rows.Next() {
		var name string
		var passwd string
		if err := rows.Scan(&name, &passwd); err != nil {
			log.Fatal(err)
		}
		fmt.Printf("login is %s has passwd %s \n", name, passwd)
	}
	if err := rows.Err(); err != nil {
		log.Fatal(err)
	}
}
