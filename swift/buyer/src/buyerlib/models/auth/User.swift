import cfactorial

// user - permission
class User: Subject {
    var name: String
    var roles: [Role]

    init(name: String, roles: [Role]) {
        self.name = name
        self.roles = roles
    }

    func factor(_ n: Int) -> Int {
        // c-func accept Int32 then wrap return with Int
        return Int(factorial(Int32(n)))

    }
}
