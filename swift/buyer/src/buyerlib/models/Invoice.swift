class Invoice {
    var invoiceFrom: String
    var invoiceTo: String
    var lineitems: [InvoiceItem] = []

    init(invoiceFrom: String, invoiceTo: String) {
        self.invoiceFrom = invoiceFrom
        self.invoiceTo = invoiceTo
    }
}
