package model

import (
	"../util"
	"../util/log"
	"database/sql"
	"fmt"
	"github.com/jinzhu/gorm"
	_ "github.com/lib/pq"
)

type Database struct {
	Type       string
	Ip         string
	Port       int
	Dbname     string
	User       string
	Password   string
	Connection *sql.DB
}

func Postgres(dbname string, user string, password string) *sql.DB {
	connection, err := sql.Open(
		"postgres",
		fmt.Sprintf("user=%s dbname=%s password=%s",
			user, dbname, password))
	util.Check(err)
	return connection
}

func DefaultDB() *sql.DB {
	return Postgres("sa", "sa", "sa")
}

func NewDatabase(dbname string, user string, password string) *Database {
	connection := Postgres(dbname, user, password)
	return &Database{"postgres", "127.0.0.1", 5432, dbname, user, password, connection}
}

func (db *Database) Init() {
	db.Type = "postgres"
	db.Ip = "127.0.0.1"
	db.Port = 5432
	db.Dbname = "sa"
	db.User = "sa"
	db.Password = "sa"
	db.Connection = DefaultDB()
}

func (db *Database) Open() {
	if db.Connection != nil {
		db.Connection = Postgres(db.Dbname, db.User, db.Password)
	}
}

func (db *Database) Close() {
	db.Connection.Close()
}

func (db *Database) Execute(query string) {
	_, err := db.Connection.Exec(query)
	util.Check(err)
}

func (db *Database) GetOrm() gorm.DB {
	orm, err := gorm.Open(db.Type, fmt.Sprintf("user=%s password=%s dbname=%s sslmode=disable",
		db.User, db.Password, db.Dbname))
	if err != nil {
		log.Fatal("unable to open orm: %v", err)
	}
	orm.SingularTable(true)
	// orm.DB()
	return orm
}

func (db *Database) GetMissions() []Mission {
	missions := []Mission{}
	orm := db.GetOrm()
	//orm.Find(&missions)
	// orm.Preload("Targets").Find(&missions)
	orm.Preload("Targets.Entries").Find(&missions)
	orm.Close()
	return missions
}
