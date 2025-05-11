class Catalog {
    var name: String
    var categories: [String] = []
    var entries: [CatalogEntry] = []

    init(name: String) {
        self.name = name
    }
}
