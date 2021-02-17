package main

import (
        "bytes"
        "fmt"
        "io/ioutil"
        "mime/multipart"
        "net/http"
        "os"
)

func write_file_blob(uri string, path string) (string, error) {
        body := new(bytes.Buffer)

        file, err := os.Open(path)
        if err != nil {
                return "", err
        }
        fileContents, err := ioutil.ReadAll(file)
        if err != nil {
                return "", err
        }
        fi, err := file.Stat()
        if err != nil {
                return "", err
        }
        file.Close()

        writer := multipart.NewWriter(body)

        part, err := writer.CreateFormFile("file", fi.Name())

        if err != nil {
                return "", err
        }

        part.Write(fileContents)

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
        md5, err := write_file_blob("http://localhost:8080/put", "/home/wschenk/mobiledownloads/talk.pdf")

        if err != nil {
                panic(err)
        }

        fmt.Print(md5)
}
