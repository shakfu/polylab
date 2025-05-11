/// A purchase order request or purchase requisition is a request
/// sent internally within a company to obtain purchased goods and services,
/// including stock.
///
/// requires:
///
/// 	- clearly specified requirements
/// 	- reference to budget
///
/// 	- inventory status

class PurchaseRequest {
    var from: String
    var to: String
    var description: String
    var budget: Double = 0.0

    init(from: String, to: String, description: String) {
        self.from = from
        self.to = to
        self.description = description
    }
}
