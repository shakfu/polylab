import ArgumentParser
import Logging
import buyerlib

let logger = Logger(label: "org.me.swiftbuyer.main")

struct Buyer: ParsableCommand {
    static var configuration = CommandConfiguration(
        abstract: "A utility to help buying worthwhile things.",

        version: "0.0.1",

        // subcommands: [Status.self, Search.self, Report.self, Sound.self, Demo.self],
        subcommands: [Status.self, Search.self, Report.self, Demo.self],

        defaultSubcommand: Status.self
    )

    @Flag(name: .shortAndLong, help: "Set debug mode on")
    var debug: Bool = false

    //--------------------------------------------------------
    // SUBCOMMANDS

    struct Status: ParsableCommand {
        static var configuration = CommandConfiguration(
            abstract: "Print the status of the buyer."
        )

        mutating func run() {
            print(today())
            logger.info("Hello World!")
            demo_stencil()
        }
    }

    struct Search: ParsableCommand {
        static var configuration = CommandConfiguration(
            abstract: "Search the buyer database."
        )

        mutating func run() {
            logger.info("searching now")
            demo_sqlite()
        }
    }

    struct Report: ParsableCommand {
        static var configuration = CommandConfiguration(
            abstract: "Generate a buyer report."
        )

        mutating func run() {
            logger.info("reporting now")
            demo_cxlsxwriter()

        }
    }

    // struct Sound: ParsableCommand {
    //     static var configuration = CommandConfiguration(
    //         abstract: "Generate a sound."
    //     )

    //     mutating func run() {
    //         logger.info("generating sound now")
    //         demo_audiokit()

    //     }
    // }

    struct Demo: ParsableCommand {
        static var configuration = CommandConfiguration(
            abstract: "Demo a feature."
        )

        mutating func run() {
            logger.info("demonstrating feature now")
            demo_feature()
        }
    }
}

Buyer.main()
