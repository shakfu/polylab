// select.swift
import AppKit

func selectFile() -> URL? {
    let dialog = NSOpenPanel()
    dialog.allowedFileTypes = ["jpg", "png"]
    guard dialog.runModal() == .OK else { return nil }
    return dialog.url
}

print(selectFile()?.absoluteString ?? "")

