import Foundation

protocol Subject {
    // a person or an automated agent (also called Party)
    var name: String { get set }
}

protocol Contactable {
    var address: String { get set }
    var country: String { get set }
    var telephone: String { get set }
    var email: String { get set }
}

protocol Approvable {}

protocol Request {
    var title: String { get set }
    var description: String { get set }
    var requestType: String { get set }
    var isApproved: Bool { get set }
    var isBudgeted: Bool { get set }
    var createdOn: Date { get }
    var createBy: User { get }
}
