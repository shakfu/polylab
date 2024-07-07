package log

import (
	"fmt"
	"github.com/mgutz/ansi"
	"log"
	"os"
)

// color functions
var Green = ansi.ColorFunc("green+b")

var Cyan = ansi.ColorFunc("cyan+b")

var Magenta = ansi.ColorFunc("magenta+b")

var Red = ansi.ColorFunc("red+b")

// colored logging
func genLog(category string, msg string, args ...interface{}) {
	msg = fmt.Sprintf("%s %s\n", category, msg)
	log.Printf(msg, args...)
}

func Debug(msg string, args ...interface{}) {
	genLog(Green("[DEBUG]"), msg, args...)
}

func Info(msg string, args ...interface{}) {
	genLog(Cyan("[INFO] "), msg, args...)
}

func Warn(msg string, args ...interface{}) {
	genLog(Magenta("[WARN] "), msg, args...)
}

func Error(msg string, args ...interface{}) {
	genLog(Red("[ERROR]"), msg, args...)
}

func Fatal(msg string, args ...interface{}) {
	genLog(Red("[ERROR]"), msg, args...)
	os.Exit(1)
}
