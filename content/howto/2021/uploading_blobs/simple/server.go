package main

import (
        "crypto/md5"
        "fmt"
        "io/ioutil"
        "log"
        "net/http"
        "os"
        "strings"
)

func putHandler(w http.ResponseWriter, r *http.Request) {
        log.Print("Proccessing file upload")

        contentType := r.Header.Get( "Content-Type" );

        if( strings.HasPrefix( "multipart/form-data", contentType ) ) {
                uploadFile( w, r );
        } else {
                handlePost( w, r );
        }
}

func uploadFile(w http.ResponseWriter, r *http.Request) {
        log.Print( "Processing file upload" )
        // Set upload limit
        r.ParseMultipartForm(10 << 20)

        file, handler, err := r.FormFile("file")
        if err != nil {
                fmt.Println("Error Retrieving the File")
                fmt.Println(err)
                return
        }
        defer file.Close()
        log.Printf("Uploaded File: %+v\n", handler.Filename)
        log.Printf("Content type : %+v\n", handler.Header.Get("Content-Type"))
        log.Printf("File Size    : %+v\n", handler.Size)
        log.Printf("MIME Header  : %+v\n", handler.Header)

        // read all of the contents of our uploaded file into a
        // byte array
        fileBytes, err := ioutil.ReadAll(file)
        if err != nil {
                log.Print(err)
                fmt.Println(err)
                return
        }

        md5string := fmt.Sprintf("%x", md5.Sum(fileBytes))

        ioutil.WriteFile(fmt.Sprintf("blobs/%s", md5string), fileBytes, 0666)

        // Return the key
        fmt.Fprintf(w, "%s", md5string)

}

func handlePost(w http.ResponseWriter, r *http.Request) {
        log.Print( "Storing raw post" )
        body, _ := ioutil.ReadAll( r.Body )
        defer r.Body.Close()

        md5string := fmt.Sprintf("%x", md5.Sum(body))

        ioutil.WriteFile(fmt.Sprintf("blobs/%s", md5string), body, 0666)

        // Return the key
        fmt.Fprintf(w, "%s", md5string)
}

func mkdir_p(dir string) {
        _, err := os.Stat(dir)

        if os.IsNotExist(err) {
                log.Print("Creating ", dir)
                errDir := os.MkdirAll(dir, 0755)
                if errDir != nil {
                        log.Fatal(err)
                }

        }
}

func main() {
        mkdir_p("blobs")

        fs := http.FileServer(http.Dir("./blobs"))
        http.Handle("/get/", http.StripPrefix("/get/", fs))
        http.HandleFunc("/put", putHandler)

        log.Print("Starting server on port 8080")
        if err := http.ListenAndServe(":8080", nil); err != nil {
                log.Fatal(err)
        }
}
