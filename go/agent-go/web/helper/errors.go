package helper

type Errors struct {
	Messages map[string]string
}

func NewErrors() *Errors {
	errors := &Errors{
		Messages: map[string]string{},
	}

	return errors
}

func (errors *Errors) Add(field string, message string) {
	errors.Messages[field] = message
}

func (errors *Errors) Clear() {
	errors.Messages = map[string]string{}
}

func (errors *Errors) HasMessages() bool {
	return len(errors.Messages) > 0
}

// vi:syntax=go
