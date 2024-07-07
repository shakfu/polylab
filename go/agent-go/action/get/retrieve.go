package get

import (
	"../../util/log"
	"github.com/SlyMarbo/rss"
)

func Retrieve(url string) {
	feed, err := rss.Fetch(url)
	if err != nil {
		log.Error("rss file not fetched")
	}
	log.Info("\nTitle: %s\nDescription: %s\nLink: %s\n",
		feed.Title, feed.Description, feed.Link)

	for _, item := range feed.Items {
		log.Info("%s", item.Title)
	}
}

func main() {
	urls := []string{
		"http://news.google.com/?output=rss",
		"http://news.ycombinator.com/rss",
		"http://www.theguardian.com/world/saudiarabia/rss",
	}

	for _, url := range urls {
		Retrieve(url)
	}

}
