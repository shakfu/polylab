import Foundation

class Requirement {
    var customer: Customer
    var date: Date
    var amount: Float
    var currency: Currency = Currency.USD
    var description: String

    init(customer: Customer, amount: Float, description: String) {
        self.customer = customer
        self.amount = amount
        self.description = description
        self.date = Date()
    }
}
