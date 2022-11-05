package main

import (
    "log"
    "time"

    "github.com/xuri/excelize/v2"
)

func main() {
    
    mybool := true
    f := excelize.NewFile()

    f.SetCellValue("Sheet1", "B2", 100)
    f.SetCellValue("Sheet1", "A1", 50)

    now := time.Now()

    f.SetCellValue("Sheet1", "A4", now.Format(time.ANSIC))
    f.SetCellBool("Sheet1", "A5", mybool)

    if err := f.SaveAs("simple.xlsx"); err != nil {
        log.Fatal(err)
    }
}

