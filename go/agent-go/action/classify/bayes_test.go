package classify

import (
	// "fmt"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestClassify(t *testing.T) {
	assert := assert.New(t)

	relevant := []string{"tall", "rich", "handsome"}
	irrelevant := []string{"poor", "smelly", "ugly"}

	classifier := Learn(relevant, irrelevant)

	scores, likely, _ := classifier.LogScores(
		[]string{"tall", "girl"},
	)

	probs, likely, _ := classifier.ProbScores(
		[]string{"tall", "girl"},
	)

	assert.Equal(scores[0], -27.12019549216256)

	assert.Equal(likely, 0)

	assert.Equal(probs[0], 0.99999999997)

	// fmt.Println("scores: ", scores)
	// fmt.Println("likely: ", likely)

	// fmt.Println("probs: ",  probs)
	// fmt.Println("likely: ", likely)

}
