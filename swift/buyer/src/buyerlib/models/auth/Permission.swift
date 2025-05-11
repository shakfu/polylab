class Permission: Hashable {
    var code: String

    init(code: String) {
        self.code = code
    }

    static func == (lhs: Permission, rhs: Permission) -> Bool {
        return lhs.code == rhs.code
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(code)
    }
}
