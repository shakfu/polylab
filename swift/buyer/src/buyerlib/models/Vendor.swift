class Vendor {
    var name: String
    var currency: Currency = Currency.USD
    var discount: Float = 0.0
    var brands: [Brand] = []
    var products: [Product]

    init(name: String, products: [Product]) {
        self.name = name
        self.products = products
    }

    func add_product(
        _ brand_name: String,
        _ product_name: String,
        _ price: Float,
        _ discount: Float
    ) {
        if discount > 0.0 {
            print(
                "\(brand_name)/\(product_name) at \(price) discounted by \(discount * 100)% added")

        } else {
            print("\(brand_name)/\(product_name) added")
        }
    }
}
