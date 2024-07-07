package model

import (
	"../util/log"
	"database/sql"
	"encoding/json"
	"fmt"
	"gopkg.in/yaml.v2"
	"io"
	"strings"
)

type Mission struct {
	Id       int
	Name     string
	Keywords string
	Targets  []Target
}

// check for model implementation
var _ Model = &Mission{}

func NewMission(id int, name string, keywords string, targets []Target) *Mission {
	return &Mission{id, name, keywords, targets}
}

func (m *Mission) String() string {
	return fmt.Sprintf("Mission{Id: %v, Name: %v, Keywords: %v, Targets: %v}", m.Id, m.Name, m.Keywords, m.Targets)
}

func (m *Mission) Display() {
	log.Info(m.String())
}

func (m *Mission) Insert(db *sql.DB) {
	_, err := db.Exec("insert into mission (name, keywords, targets) values ($1, $2, $3)", m.Name, m.Keywords, m.Targets)
	if err != nil {
		log.Fatal("cannot create entry: %v", err)
	}
}

func GetMissionByID(db *sql.DB, id int) (*Mission, error) {
	const query = "select * from mission where id = $1"
	var mission Mission
	err := db.QueryRow(query, id).Scan(&mission.Id, &mission.Name, &mission.Keywords, &mission.Targets)
	return &mission, err
}

func (m *Mission) ToYAML() string {
	yml, err := yaml.Marshal(m)
	if err != nil {
		log.Fatal("cannot convert mission %v to yaml: %v", m.Id, err)
	}
	return string(yml)
}

func (m *Mission) FromYAML(yml string) {
	err := yaml.Unmarshal([]byte(yml), m)
	if err != nil {
		log.Fatal("cannot convert mission %v from yaml: %v", m.Id, err)
	}
}

func (m *Mission) ToJSON() string {
	js, err := json.Marshal(m)
	if err != nil {
		log.Fatal("cannot convert mission %v to json: %v", m.Id, err)
	}
	return string(js)
}

func (m *Mission) FromJSON(js string) {
	decoder := json.NewDecoder(strings.NewReader(js))
	for {
		if err := decoder.Decode(m); err == io.EOF {
			break
		} else if err != nil {
			log.Fatal("cannot convert mission %v from json: %v", m.Id, err)
		}
	}
}

func (m *Mission) Run(db *Database) {
	orm := db.GetOrm()
	defer orm.Close()

	for _, target := range m.Targets {
		target.Retrieve()
		if len(target.Entries) > 0 {
			log.Info("SUCCESS: %s is populated", target.Name)
			orm.Save(&target)
			log.Info("Target %s is saved", target.Name)
			// log.Info(target.ToYAML())
		} else {
			log.Warn("FAILURE: %s is NOT populated", target.Name)
		}
	}
}
