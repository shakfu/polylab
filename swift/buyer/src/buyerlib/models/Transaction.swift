class Transaction {
    var buyer: User
    var vendor: Vendor
    var products: [Product]

    init(buyer: User, vendor: Vendor, products: [Product]) {
        self.buyer = buyer
        self.vendor = vendor
        self.products = products
    }

    func total() -> Double {
        products.reduce(0.0) { x, p in x + p.price }
    }
}
