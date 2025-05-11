import XCTest
@testable import buyerlib

final class buyerlibTests: XCTestCase {
    func testExample() {
        // This is an example of a functional test case.
        // Use XCTAssert and related functions to verify your tests produce the correct
        // results.
        XCTAssertEqual(buyerlib().text, "Hello, World!")
    }

    func testTransaction() {
        let p1 = Product(name: "Morphagene", price: 530.12)
        let p2 = Product(name: "Arbhar", price: 599.99)
        let v = Vendor(name: "PC", products: [p1, p2])
        let b = Buyer(name: "Sam")
        let t = Transaction(buyer: b, vendor: v, products: [p1, p2])
        XCTAssertEqual(t.total(), 1130.1100000000001)
    }

    func testBuy() {
        let p1 = Product(name: "apple", price: 10.00)
        let p2 = Product(name: "orange", price: 15.00)
        let v = Vendor(name: "grocers", products: [p1, p2])
        let b = Buyer(name: "Sunny")
        let t = buy(buyer: b, vendor: v, products: [p1, p2])
        XCTAssertEqual(t.total(), 25.00)
    }

    func testDB() {
        demo()
        XCTAssertEqual(1, 1)
    }

    static var allTests = [
        ("testExample", testExample),
        ("testTransaction", testTransaction),
        ("testBuy", testBuy),
        ("testDB", testDB),        
    ]
}
