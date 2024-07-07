package parse

import (
	"../../model"
	"container/list"
	"golang.org/x/net/html"
	"log"
	"strings"
)

func ParseHtml(htmltxt string) []model.Entry {
	var entries []model.Entry
	lst := list.New()

	doc, err := html.Parse(strings.NewReader(htmltxt))
	if err != nil {
		log.Fatal(err)
	}
	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "a" {
			for _, a := range n.Attr {
				if a.Key == "href" {
					txt := n.FirstChild
					entry := model.Entry{Link: txt.Data, Url: a.Val}
					lst.PushBack(entry)
					break
				}
			}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(doc)

	for i := lst.Front(); i != nil; i = i.Next() {
		entries = append(entries, i.Value.(model.Entry))
	}
	return entries
}
