import Foundation
import SQLite

public func demo_sqlite() {
    do {
        let db = try Connection("./db.sqlite3")

        try db.execute(
            """
                BEGIN TRANSACTION;
                CREATE TABLE person (
                    id INTEGER PRIMARY KEY NOT NULL,
                    email TEXT UNIQUE NOT NULL,
                    name TEXT
                );
                CREATE TABLE note (
                    id INTEGER PRIMARY KEY NOT NULL,
                    title TEXT NOT NULL,
                    body TEXT NOT NULL,
                    published_at DATETIME
                );
                END TRANSACTION;
            """)
    } catch let error {
        print("error: \(error)")
    }
}
