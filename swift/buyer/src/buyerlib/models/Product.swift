class Product: CustomStringConvertible {
    var name: String
    var price: Double
    var description: String {
        "(\(name), \(price))"
    }

    init(name: String, price: Double) {
        self.name = name
        self.price = price
    }
}
