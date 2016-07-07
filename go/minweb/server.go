package main

import (
    "fmt"
    "net/http"
)

func helloHandler(w http.ResponseWriter, r *http.Request) {
    fmt.Fprintln(w, "Hello World from Go in minimal Docker container")
}

func main() {
    http.HandleFunc("/", helloHandler)

    fmt.Println("Started, serving at 9001")
    err := http.ListenAndServe(":9001", nil)
    if err != nil {
        panic("ListenAndServe: " + err.Error())
    }
}