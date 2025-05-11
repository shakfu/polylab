// swift-tools-version:5.1

import PackageDescription


let package = Package(
    name: "swiftbuyer",
    platforms: [
        .macOS(.v10_15)
    ],
    products: [
        .library(name: "buyerlib", targets: ["buyerlib"]),
        .executable(name: "buyer", targets: ["buyer"]),
    ],
    dependencies: [
        .package(path: "thirdparty/cxlsxwriter"),
        .package(url: "https://github.com/michaelnisi/skull", from: "11.0.4"),
        .package(url: "https://github.com/apple/swift-argument-parser", from: "0.3.0"),
        .package(url: "https://github.com/stephencelis/SQLite.swift.git", from: "0.12.0"),
        .package(url: "https://github.com/stencilproject/Stencil.git", from: "0.14.0"),
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
        // .package(url: "https://github.com/AudioKit/AudioKit", .branch("v5.2.0")),
    ],
    targets: [
        .target(
            name: "buyerlib",
            dependencies: ["cfactorial", "cxlsxwriter", "Skull", "Stencil", "SQLite"]),
            // dependencies: ["cfactorial", "cxlsxwriter","Skull", "Stencil", "SQLite", "AudioKit"]),
        .testTarget(
            name: "buyerlibTests",
            dependencies: ["buyerlib"]),

        .target(
            name: "buyer",
            dependencies: ["buyerlib", 
                .product(name: "ArgumentParser", package: "swift-argument-parser"),
                .product(name: "Logging", package: "swift-log"),
            ]),
        .testTarget(
            name: "buyerTests",
            dependencies: ["buyer"]), 

        .target(
            name: "cfactorial",
            path: "./src/factorial"),
    ]
)
