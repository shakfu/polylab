enum RoleType {
    case Buyer
    case Approver
}

class Role {
    // Job function or title which defines an authority level
    var name: String
    var reports_to: Role
    var roles: [Role]
    var groups: [Group]

    init(name: String, reports_to: Role, roles: [Role], groups: [Group]) {
        self.name = name
        self.reports_to = reports_to
        self.roles = roles
        self.groups = groups
    }
}
