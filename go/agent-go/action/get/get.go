package get

import (
	"../../util/log"
	"io/ioutil"
	"net/http"
	"sync"
)

func Get() {
	var wg sync.WaitGroup
	var urls = []string{
		"http://www.golang.org/",
		"http://www.google.com/",
		"http://www.python.org/",
	}
	for i, url := range urls {
		// Increment the WaitGroup counter.
		wg.Add(1)
		log.Info("added %d", i)
		// Launch a goroutine to fetch the URL.
		go func(url string) {
			// Decrement the counter when the goroutine completes.
			defer wg.Done()
			// Fetch the URL.
			resp, err := http.Get(url)
			if err != nil {
				log.Error("http get error of %s", url)
			}
			defer resp.Body.Close()
			body, err := ioutil.ReadAll(resp.Body)
			log.Info("url: %s, len:%d", url, len(body))
		}(url)
	}
	// Wait for all HTTP fetches to complete.
	wg.Wait()

	log.Info("Done")
}
