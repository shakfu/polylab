package system

import (
	"log"
	"testing"
)

func TestSystem(t *testing.T) {
	Execute("date")
	out := Capture("date")
	log.Println(out)
}

func TestMD5(t *testing.T) {
	hash := GetMD5Hash("Hello World")
	if hash != "b10a8db164e0754105b7a99be72e3fe5" {
		t.Errorf("hash: %s\n", hash)
	}
}
