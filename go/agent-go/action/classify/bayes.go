package classify

import (
	"github.com/jbrukh/bayesian"
)

const (
	Relevant   bayesian.Class = "Relevant"
	Irrelevant bayesian.Class = "Irrelevant"
)

func Learn(relevant []string, irrelevant []string) *bayesian.Classifier {
	classifier := bayesian.NewClassifier(Relevant, Irrelevant)
	classifier.Learn(relevant, Relevant)
	classifier.Learn(irrelevant, Irrelevant)
	return classifier
}
