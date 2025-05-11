class CatalogEntry {
    var category: String
    var product: Product
    var quote: Quote

    init(category: String, product: Product, quote: Quote) {
        self.category = category
        self.product = product
        self.quote = quote
    }
}
