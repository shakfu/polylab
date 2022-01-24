// DO NOT EDIT. This file is machine-generated and constantly overwritten.
// Make changes to Person.swift instead.

import Foundation
import CoreData

public enum PersonAttributes: String {
    case age = "age"
    case name = "name"
}

open class _Person: NSManagedObject {

    // MARK: - Class methods

    open class func entityName () -> String {
        return "Person"
    }

    open class func entity(managedObjectContext: NSManagedObjectContext) -> NSEntityDescription? {
        return NSEntityDescription.entity(forEntityName: self.entityName(), in: managedObjectContext)
    }

    @nonobjc
    open class func fetchRequest() -> NSFetchRequest<Person> {
        return NSFetchRequest(entityName: self.entityName())
    }

    // MARK: - Life cycle methods

    public override init(entity: NSEntityDescription, insertInto context: NSManagedObjectContext?) {
        super.init(entity: entity, insertInto: context)
    }

    public convenience init?(managedObjectContext: NSManagedObjectContext) {
        guard let entity = _Person.entity(managedObjectContext: managedObjectContext) else { return nil }
        self.init(entity: entity, insertInto: managedObjectContext)
    }

    // MARK: - Properties

    @NSManaged open
    var age: Int16 // Optional scalars not supported

    @NSManaged open
    var name: String?

    // MARK: - Relationships

}

