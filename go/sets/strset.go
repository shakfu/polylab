package main

import "fmt"

type StringSet struct {
	set map[string]bool
}

func NewStringSet() *StringSet {
	return &StringSet{make(map[string]bool)}
}

func (set *StringSet) Add(i string) bool {
	_, found := set.set[i]
	set.set[i] = true
	return !found // False if it existed already
}

func (set *StringSet) Contains(i string) bool {
	_, found := set.set[i]
	return found // true if it existed already
}

func (set *StringSet) Remove(i string) {
	delete(set.set, i)
}

func (set *StringSet) Size() int {
	return len(set.set)
}

func main() {
	set := NewStringSet()
	set.Add("a")
	set.Add("b")
	set.Add("c")
	fmt.Println(set.Size())
	fmt.Println(set.Contains("b"))
	set.Remove("c")
	fmt.Println(set.Size())
	fmt.Println(set.Contains("a"))
}
