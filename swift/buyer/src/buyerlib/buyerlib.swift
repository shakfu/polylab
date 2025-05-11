import AppKit
import Foundation

struct buyerlib {
    var text = "Hello, World!"

    // auth
    var users = [String: User]()
    var roles = [String: Role]()
    var permissions = Set<Permission>()
    var groups = [String: Group]()

    // core
    var brands = [String: Brand]()
    var vendors = [String: Vendor]()
    var products = [String: Product]()

    // this way is fine as well
    var transactions: [String: Transaction] = [:]
    var namespace = [String: Any]()
    var list = [Any]()
}
