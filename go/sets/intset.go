package main

import "fmt"

type IntSet struct {
	set map[int]bool
}

func NewIntSet() *IntSet {
	return &IntSet{make(map[int]bool)}
}

func (set *IntSet) Add(i int) bool {
	_, found := set.set[i]
	set.set[i] = true
	return !found	//False if it existed already
}

func (set *IntSet) Contains(i int) bool {
	_, found := set.set[i]
	return found	//true if it existed already
}

func (set *IntSet) Remove(i int) {
	delete(set.set, i)
}

func (set *IntSet) Size() int {
	return len(set.set)
}

func main() {
	set := NewIntSet()
	set.Add(1)
	set.Add(2)
	set.Add(3)
	fmt.Println(set.Size())
	fmt.Println(set.Contains(2))
	set.Remove(2)
	fmt.Println(set.Size())
	fmt.Println(set.Contains(2))
}