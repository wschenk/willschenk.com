package main

import (
        "fmt"
        "io/ioutil"
        "net/http"
        "strings"
)

func write_string_blob(uri string, message string) (string, error) {
        body := strings.NewReader(message)

        client := &http.Client{}
        req, err := http.NewRequest("POST", uri, body)
        req.Header.Add("Content-Type", "application/octet-stream")

        if err != nil {
                return "", err
        }

        resp, err := client.Do(req)

        if err != nil {
                return "", err
        } else {
                body, _ := ioutil.ReadAll(resp.Body)
                resp.Body.Close()
                return string(body), nil
        }
}

func main() {
        md5, err := write_string_blob("http://localhost:8080/put", "This is my string")

        if err != nil {
                panic(err)
        }

        fmt.Print(md5)
}
