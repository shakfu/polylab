class Quote {
    // var id: Int
    // var date_created: Date
    var product: Product
    var vendor: Vendor
    var currency: Currency = .USD
    var value: Float = 0.0
    var discount: Float = 0.0

    init(product: Product, vendor: Vendor, value: Float) {
        self.product = product
        self.vendor = vendor
        self.value = value
    }
}
