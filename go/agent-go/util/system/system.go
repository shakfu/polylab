package system

import (
	"../../util"
	"../log"
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"github.com/howeyc/gopass"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
)

func Execute(cmd string, arg ...string) {
	err := exec.Command(cmd, arg...).Run()
	util.Check(err)
}

func Capture(cmd string, arg ...string) string {
	out, err := exec.Command(cmd, arg...).Output()
	util.Check(err)
	return string(out)
}

func WriteFile(path string, s string) {
	err := ioutil.WriteFile(path, []byte(s), 0644)
	util.Check(err)
}

// Exists returns whether the given file or directory exists or not
func Exists(path string) (bool, error) {
	_, err := os.Stat(path)
	if err == nil {
		return true, nil
	}
	if os.IsNotExist(err) {
		return false, nil
	}
	return true, err
}

// remove files from director
func RemoveFiles(dir string) {
	var path string
	files, err := ioutil.ReadDir(dir)
	util.Check(err)
	for _, f := range files {
		path = filepath.Join(dir, f.Name())
		log.Info("removing: %v", path)
		os.RemoveAll(path)
	}
}

func GetMD5Hash(text string) string {
	hasher := md5.New()
	hasher.Write([]byte(text))
	return hex.EncodeToString(hasher.Sum(nil))
}

// silently get password
func GetPassword(inputs ...string) string {
	var input string
	if len(inputs) > 0 {
		input = inputs[0]
	} else {
		input = "password: "
	}
	fmt.Printf(input)
	password := gopass.GetPasswd()
	return string(password)
}
