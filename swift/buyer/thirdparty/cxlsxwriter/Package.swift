// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "cxlsxwriter",
    products: [
        .library(
            name: "cxlsxwriter",
            targets: ["cxlsxwriter"]),
    ],
    dependencies: [
        .package(url: "https://github.com/jmcnamara/libxlsxwriter", from:
                 "1.1.1")
    ],
    targets: [
        .systemLibrary(name: "cxlsxwriter")
    ]
)

