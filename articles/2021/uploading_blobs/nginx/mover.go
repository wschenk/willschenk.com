package main

import (
        "crypto/md5"
        "fmt"
        "io/ioutil"
        "log"
        "net/http"
        "net/http/httputil"
        "os"
        "strings"
)

func formHandler(w http.ResponseWriter, r *http.Request) {
        contentType := r.Header.Get("Content-Type")
        log.Printf( "Content-Type %s\n", contentType );
        if strings.HasPrefix(contentType,"multipart/form-data") {
                moveFile(w, r)
        } else {
                saveFile(w, r)
        }
}

func moveFile(w http.ResponseWriter, r *http.Request) {
        r.ParseMultipartForm(10 << 20)
        // Save a copy of this request for debugging.
        requestDump, err := httputil.DumpRequest(r, true)
        if err != nil {
                fmt.Println(err)
        }
        fmt.Println(string(requestDump))

        if err := r.ParseForm(); err != nil {
                fmt.Fprintf(w, "ParseForm() err: %v", err)
                return
        }
        log.Print("POST request successful")
        log.Printf("Filename     : %s\n", r.FormValue("file_name"))
        log.Printf("Content Type : %s\n", r.FormValue("file_content_type"))
        log.Printf("MD5          : %s\n", r.FormValue("file_md5"))
        log.Printf("Size         : %s\n", r.FormValue("file_size"))
        log.Printf("Path         : %s\n", r.FormValue("file_path"))

        md5 := r.FormValue("file_md5")
        err = os.Rename(r.FormValue("file_path"), fmt.Sprintf("/blobs/%s", md5))
        if err != nil {
                fmt.Print(err)
        } else {
                fmt.Fprintf(w, "%s", md5)
        }
}

func saveFile(w http.ResponseWriter, r *http.Request) {
        log.Print("Storing raw post")
        body, _ := ioutil.ReadAll(r.Body)
        defer r.Body.Close()

        md5string := fmt.Sprintf("%x", md5.Sum(body))

        ioutil.WriteFile(fmt.Sprintf("blobs/%s", md5string), body, 0666)

        // Return the key
        fmt.Fprintf(w, "%s", md5string)
}

func main() {
        http.HandleFunc("/", formHandler)
        http.HandleFunc("/mover", formHandler)

        log.Print("Starting server at port 8080")
        if err := http.ListenAndServe(":8080", nil); err != nil {
                log.Fatal(err)
        }
}
