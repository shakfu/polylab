package model

import (
	"../util/log"
	"database/sql"
	"encoding/json"
	"fmt"
	"github.com/SlyMarbo/rss"
	"gopkg.in/yaml.v2"
	"io"
	"strings"
)

type Target struct {
	Id          int
	MissionId   int
	Name        string
	Description string
	Url         string
	Keywords    string
	Category    string
	Relevance   float32
	Entries     []Entry
}

// check for model implementation
var _ Model = &Target{}

func NewTarget(id int, missionid int, name string, description string, url string, keywords string, category string, relevance float32, entries []Entry) *Target {
	return &Target{id, missionid, name, description, url, keywords, category, relevance, entries}
}

func (t *Target) String() string {
	return fmt.Sprintf("Target{Id: %v, MissionId: %v, Name: %v, Description: %v, Url: %v, Keywords: %v, Category: %v, Relevance: %v, Entries: %v}", t.Id, t.MissionId, t.Name, t.Description, t.Url, t.Keywords, t.Category, t.Relevance, t.Entries)
}

func (t *Target) Display() {
	log.Info(t.String())
}

func (t *Target) Insert(db *sql.DB) {
	_, err := db.Exec("insert into target (mission_id, name, description, url, keywords, category, relevance, entries) values ($1, $2, $3, $4, $5, $6, $7, $8)", t.MissionId, t.Name, t.Description, t.Url, t.Keywords, t.Category, t.Relevance, t.Entries)
	if err != nil {
		log.Fatal("cannot create entry: %v", err)
	}
}

func GetTargetByID(db *sql.DB, id int) (*Target, error) {
	const query = "select * from target where id = $1"
	var target Target
	err := db.QueryRow(query, id).Scan(&target.Id, &target.MissionId, &target.Name, &target.Description, &target.Url, &target.Keywords, &target.Category, &target.Relevance, &target.Entries)
	return &target, err
}

func (t *Target) ToYAML() string {
	yml, err := yaml.Marshal(t)
	if err != nil {
		log.Fatal("cannot convert target %v to yaml: %v", t.Id, err)
	}
	return string(yml)
}

func (t *Target) FromYAML(yml string) {
	err := yaml.Unmarshal([]byte(yml), t)
	if err != nil {
		log.Fatal("cannot convert target %v from yaml: %v", t.Id, err)
	}
}

func (t *Target) ToJSON() string {
	js, err := json.Marshal(t)
	if err != nil {
		log.Fatal("cannot convert target %v to json: %v", t.Id, err)
	}
	return string(js)
}

func (t *Target) FromJSON(js string) {
	decoder := json.NewDecoder(strings.NewReader(js))
	for {
		if err := decoder.Decode(t); err == io.EOF {
			break
		} else if err != nil {
			log.Fatal("cannot convert target %v from json: %v", t.Id, err)
		}
	}
}

func (t *Target) Retrieve() {
	feed, err := rss.Fetch(t.Url)
	if err != nil {
		log.Error("target '%v' rss from not fetched", t.Name)
		return
	}

	// populate target from feed
	t.Description = feed.Description
	for _, item := range feed.Items {
		t.Entries = append(t.Entries,
			Entry{TargetId: t.Id, Link: item.Title, Url: item.Link})
	}

	// for _, entry := range obj.Entries {
	// 	entry.Display()
	// }

	// log.Info("\nTitle:       %s\nDescription: %s\nLink:        %s\n",
	// 	feed.Title, feed.Description, feed.Link)
	// for _, item := range feed.Items {
	// 	log.Info("%s", item.Title)
	// }
}
