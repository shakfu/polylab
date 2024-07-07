package model

import (
	"github.com/jinzhu/gorm"
	_ "github.com/lib/pq"
	"log"
	"testing"
)

var mission = Mission{
	Name:     "a mission",
	Keywords: "love, sad",
	Targets: []Target{
		{Name: "a link",
			Url:       "http://www.ok.org",
			Keywords:  "love, sad",
			Category:  "blue",
			Relevance: 0.1,
			Entries: []Entry{
				{Link: "a link",
					Url:       "http://www.ok.org",
					Category:  "blue",
					Relevance: 0.1,
				},
				{Link: "a link2",
					Url:       "http://www.me.org",
					Category:  "red",
					Relevance: 0.12,
				},
			},
		},
	},
}

var ymlMission = `
id: 0
name: mission impossible
keywords: love, sad
targets:
- id: 0
  missionid: 0
  name: a link
  url: http://www.ok.org
  keywords: love, sad
  category: blue
  relevance: 0.1
  entries:
  - id: 0
    targetid: 0
    link: a link
    url: http://www.ok.org
    category: blue
    relevance: 0.1
  - id: 0
    targetid: 0
    link: a link2
    url: http://www.me.org
    category: red
    relevance: 0.12
`

const jsonMission = `
{
    "name": "covert ops1",
    "keywords": "love, sad",
    "targets": [
        {
            "name": "a link",
            "url": "http://www.ok.org",
            "keywords": "love, sad",
            "category": "blue",
            "relevance": 0.1,
            "entries": [
                {
                    "link": "a link",
                    "url": "http://www.ok.org",
                    "category": "blue",
                    "relevance": 0.1
                },
                {
                    "link": "a link2",
                    "url": "http://www.me.org",
                    "category": "red",
                    "relevance": 0.12
                }
            ]
        }
    ]
}

`

func TestModelSerialization(t *testing.T) {
	yml := mission.ToYAML()
	log.Println(yml)
	js := mission.ToJSON()
	log.Println(js)
	var m Mission
	m.FromYAML(ymlMission)
	m.FromJSON(jsonMission)
}

func TestModelOrm(t *testing.T) {

	db, err := gorm.Open("postgres", "user=sa password=sa dbname=sa sslmode=disable")
	if err != nil {
		log.Fatal(err)
	}
	db.DB()
	// db.SingularTable(true)

	// setup
	db.CreateTable(&Mission{})
	db.CreateTable(&Target{})
	db.CreateTable(&Entry{})

	mission := Mission{
		Name:     "a mission",
		Keywords: "love, sad",
		Targets: []Target{
			{Name: "a link",
				Url:       "http://www.ok.org",
				Keywords:  "love, sad",
				Category:  "blue",
				Relevance: 0.1,
				Entries: []Entry{
					{Link: "a link",
						Url:       "http://www.ok.org",
						Category:  "blue",
						Relevance: 0.1,
					},
					{Link: "a link2",
						Url:       "http://www.me.org",
						Category:  "red",
						Relevance: 0.12,
					},
				},
			},
		},
	}

	db.Create(&mission)

	entry := Entry{}

	db.First(&entry)
	if entry.Category != "blue" {
		t.Errorf("error: result %s not blue", entry.Category)
	}

	entries := []Entry{}
	db.Find(&entries)

	// for _, obj := range entries {
	//     log.Println("link: ", obj.Link)
	// }

	if entries[0].Category != "blue" {
		t.Errorf("error: result %s not blue", entries[0].Category)
	}

	// tear down
	db.DropTable(&Mission{})
	db.DropTable(&Target{})
	db.DropTable(&Entry{})
}
