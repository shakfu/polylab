class Agent: Subject {
    var name: String
    var roles: [Role]

    init(name: String, roles: [Role]) {
        self.name = name
        self.roles = roles
    }
}
