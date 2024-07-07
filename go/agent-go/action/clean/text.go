package clean

import (
	// "fmt"
	valid "github.com/asaskevich/govalidator"
	"github.com/reiver/go-porterstemmer"
	"strings"
)

type Lexia struct {
	data string
}

func (l *Lexia) Replace(old string, new string) {
	l.data = replace(l.data, old, new)
}

func (l *Lexia) String() string {
	return l.data
}

func (l *Lexia) Stem() string {
	return Stem(l.data)
}

func (l *Lexia) Convert() *Lexia {
	return &Lexia{data: Stem(l.data)}
}

func replace(s string, old string, new string) string {
	return strings.Replace(s, old, new, -1)
}

func splitjoin(s string) string {
	lst := strings.Fields(s)
	return strings.Join(lst, " ")
}

var funcs = []func(string) string{
	func(s string) string { return valid.Trim(s, "") },
	func(s string) string { return valid.WhiteList(s, "a-zA-Z ") },
	strings.ToLower,
	splitjoin,
	func(s string) string { return replace(s, "new", "old") },
}

func Clean(s string) string {
	result := s
	for _, f := range funcs {
		result = f(result)
	}
	return result
}

func Stem(s string) string {
	return porterstemmer.StemString(s)
}

// func main() {
// 	txt := "Happy New Year 1, ok.!? Lovely"

// 	println(Clean(txt))
// 	fmt.Printf("%q\n", strings.Split("a b  c d", " "))
// 	fmt.Printf("%q\n", strings.Fields("a b  c d"))

// 	word := "New Year"
// 	l := Lexia{data: "New Year"}
// 	l.Replace("New", "Old")

// 	fmt.Printf("%s -> %s\n", word, l.String())
// 	fmt.Printf("%s -> %s\n", word, l.Stem())
// 	fmt.Printf("%s -> %s\n", word, l.Convert().String())
// }
