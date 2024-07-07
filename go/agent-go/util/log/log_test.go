package log

import (
	"fmt"
	"testing"
)

func TestColor(t *testing.T) {
	fmt.Printf("this is %s\n", Cyan("cyan"))
	fmt.Printf("this is %s\n", Green("green"))
	fmt.Printf("this is %s\n", Magenta("magenta"))
	fmt.Printf("this is %s\n", Red("red"))
}

func TestLog(t *testing.T) {
	val := "a value 01"
	Debug("this is debug info: %s", val)
	Info("this is informational")
	Error("this is an error")
	Warn("this is a warning")
}
