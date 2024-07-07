package model

// Model Interface
type Model interface {
	String() string
	Display()
	ToYAML() string
	FromYAML(string)
	ToJSON() string
	FromJSON(string)
}
