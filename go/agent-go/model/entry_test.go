package model

import (
	"github.com/stretchr/testify/assert"
	"testing"
	// _ "github.com/lib/pq"
	// "database/sql"
)

func TestEntry(t *testing.T) {
	assert := assert.New(t)
	// test create entry
	entry := Entry{
		TargetId:  2,
		Link:      "aa",
		Url:       "http://www.python.org",
		Category:  "Mideast",
		Relevance: 0.12,
	}
	repr := entry.String()
	assert.NotNil(repr)
}

func TestEntryManager(t *testing.T) {
	assert := assert.New(t)
	// add tests here
	entryManager := new(EntryManager)
	entryManager.Init()
	defer entryManager.Close()

	// start
	entry := entryManager.New(2, "a link1", "http://www.go.org")
	assert.True(entry.Id > 0)

	entry, _ = entryManager.Get(entry.Id)
	assert.True(entry.Id > 0)

	// test create entry
	entry = entryManager.GetByLink("a link1")
	assert.Equal(entry.Link, "a link1")
	assert.True(entry.Id > 0)

	// test all
	entries := entryManager.All()
	assert.True(len(entries) > 0)

	// test tear down
	_, err := entryManager.Delete(entry.Id)
	assert.Nil(err)
}
