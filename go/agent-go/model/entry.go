package model

import (
	"../util/log"
	"database/sql"
	"encoding/json"
	"fmt"
	_ "github.com/lib/pq"
	"gopkg.in/yaml.v2"
	"io"
	"strings"
)

// ========================================================================================
// Entry Model
// ========================================================================================

type Entry struct {
	Id        int
	TargetId  int
	Link      string
	Url       string
	Category  string
	Relevance float32
}

// check for model implementation
var _ Model = &Entry{}

func NewEntry(id int, targetid int, link string, url string, category string, relevance float32) *Entry {
	return &Entry{id, targetid, link, url, category, relevance}
}

func (e *Entry) String() string {
	return fmt.Sprintf("Entry{Id: %v, TargetId: %v, Link: %v, Url: %v, Category: %v, Relevance: %v}", e.Id, e.TargetId, e.Link, e.Url, e.Category, e.Relevance)
}

func (e *Entry) Display() {
	log.Info(e.String())
}

func (e *Entry) ToYAML() string {
	yml, err := yaml.Marshal(e)
	if err != nil {
		log.Fatal("cannot convert entry %v to yaml: %v", e.Id, err)
	}
	return string(yml)
}

func (e *Entry) FromYAML(yml string) {
	err := yaml.Unmarshal([]byte(yml), e)
	if err != nil {
		log.Fatal("cannot convert entry %v from yaml: %v", e.Id, err)
	}
}

func (e *Entry) ToJSON() string {
	js, err := json.Marshal(e)
	if err != nil {
		log.Fatal("cannot convert entry %v to json: %v", e.Id, err)
	}
	return string(js)
}

func (e *Entry) FromJSON(js string) {
	decoder := json.NewDecoder(strings.NewReader(js))
	for {
		if err := decoder.Decode(e); err == io.EOF {
			break
		} else if err != nil {
			log.Fatal("cannot convert entry %v from json: %v", e.Id, err)
		}
	}
}

// ========================================================================================
// Entry Model Manager
// ========================================================================================

type EntryManager struct {
	Db    *sql.DB
	cache []string
}

func (m *EntryManager) Init() {
	m.Db = DefaultDB()
}

func (m *EntryManager) All() []Entry {
	var entries []Entry
	rows, err := m.Db.Query("select * from entry")
	if err != nil {
		log.Fatal("EntryManager.All: %v", err)
	}
	defer rows.Close()
	for rows.Next() {
		var entry Entry
		if err := rows.Scan(&entry.Id, &entry.TargetId, &entry.Link,
			&entry.Url, &entry.Category, &entry.Relevance); err != nil {
			log.Fatal("EntryManager.All: %v", err)
		}
		entries = append(entries, entry)
	}
	if err := rows.Err(); err != nil {
		log.Fatal("EntryManager.All: %v", err)
	}
	return entries
}

func (m *EntryManager) New(targetid int, link string, url string) *Entry {
	entry := &Entry{TargetId: targetid, Link: link, Url: url}
	if !m.LinkExists(link) {
		err := m.Db.QueryRow(`insert into entry (target_id, link, url, category, relevance) 
			values ($1, $2, $3, $4, $5) returning id`,
			entry.TargetId, entry.Link, entry.Url, "", 0.0).Scan(&entry.Id)
		if err != nil {
			log.Fatal("EntryManager.New: %v", err)
		}
	}
	return entry
}

func (m *EntryManager) Get(id int) (*Entry, error) {
	const query = "select * from entry where id = $1"
	var entry Entry
	err := m.Db.QueryRow(query, id).Scan(&entry.Id, &entry.TargetId, &entry.Link,
		&entry.Url, &entry.Category, &entry.Relevance)
	return &entry, err
}

func (m *EntryManager) Delete(id int) (sql.Result, error) {
	return m.Db.Exec("delete from entry where id=$1", id)
}

func (m *EntryManager) GetByLink(link string) *Entry {
	var e Entry
	var cat sql.NullString
	err := m.Db.QueryRow("select id, target_id, link, url, category from entry where link=$1", link).Scan(
		&e.Id, &e.TargetId, &e.Link, &e.Url, &cat)
	if err != nil {
		log.Fatal("EntryManager cannot retrieve entry from link %v: %v", link, err)
	}
	if cat.Valid {
		e.Category = cat.String
	}
	return &e
}

func (m *EntryManager) LinkExists(link string) bool {
	var entryExists bool
	err := m.Db.QueryRow("select exists(select 1 from entry where link=$1)", link).Scan(&entryExists)
	if err != nil {
		log.Fatal("EntryManager cannot check for existance of link %v: %v", link, err)
	}
	return entryExists
}

func (m *EntryManager) Close() {
	m.Db.Close()
}
