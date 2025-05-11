class Group {
    var name: String
    var permissions: Set<Permission>

    init(name: String, permissions: [Permission]) {
        self.name = name
        self.permissions = Set(permissions)
    }

    init(name: String, permissions: Set<Permission>) {
        self.name = name
        self.permissions = permissions
    }
}
