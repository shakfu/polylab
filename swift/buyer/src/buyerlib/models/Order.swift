enum OrderError: Error {
    case NotComplete
    case NotValid
    case NotSent
    case NotReceived
}

class Order {
    var from: String
    var to: String
    var lineitems: [OrderItem] = []

    init(from: String, to: String) {
        self.from = from
        self.to = to
    }
}
