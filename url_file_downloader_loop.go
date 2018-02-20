package main

import (
    "fmt"
    "io"
    "log"
    "net/http"
    "os"
)

func main() {
    url_template := "http://questionablecontent.net/comics/%d.png"

    for comic_number := 1; comic_number < 3647; comic_number++ {
        to_fetch := fmt.Sprintf(url_template, comic_number)
        download_file(to_fetch, fmt.Sprintf("%d", comic_number))
    }
}

func download_file(url string, destination_name string) {
    response, http_fetch_error := http.Get(url)
    file, file_creation_error := os.Create(destination_name)
    _, file_copy_error := io.Copy(file, response.Body) // io.Copy dumps the response body to file, supporting large files

    defer response.Body.Close()

    if ((http_fetch_error != nil) || (file_creation_error != nil) || (file_copy_error != nil)) {
        log.Fatal("whoops")
    }

    file.Close()
}
