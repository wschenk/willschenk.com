package main

import (
        "bytes"
        "fmt"
        "io/ioutil"
        "mime/multipart"
        "net/http"
)

func write_string_as_file(uri string, message string) (string, error) {
        body := new(bytes.Buffer)

        writer := multipart.NewWriter(body)

        part, err := writer.CreateFormFile("file", "filename")

        if err != nil {
                return "", err
        }

        part.Write([]byte(message))

        err = writer.Close()

        if err != nil {
                return "", err
        }

        client := &http.Client{}
        req, err := http.NewRequest("POST", uri, body)
        req.Header.Add("Content-Type", writer.FormDataContentType())

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
        md5, err := write_string_as_file("http://localhost:8080/put", "This is my string")

        if err != nil {
                panic(err)
        }

        fmt.Print(md5)
}
