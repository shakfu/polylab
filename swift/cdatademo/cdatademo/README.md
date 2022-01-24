#  Code store


```swift

//
//  main.swift
//  cdatademo
//
//  Created by sa on 2/8/21.
//  Copyright © 2021 me. All rights reserved.
//

import Foundation
import CoreData

print("Hello, World!")

@objc(PersonMO)
public class PersonMO: NSManagedObject {

    @nonobjc public class func fetchRequest() -> NSFetchRequest<PersonMO> {
        return NSFetchRequest<PersonMO>(entityName: "Person")
    }

    @NSManaged public var name: String
    @NSManaged public var age: Int16

}



//final class PersonMO: NSManagedObject {
//
//    public class func fetchRequest() -> NSFetchRequest<PersonMO> {
//        return NSFetchRequest<PersonMO>(entityName: "Person")
//    }
//
//    @NSManaged public var name: String
//    @NSManaged public var age: Int16
//}

//let personEntity = NSEntityDescription()
//personEntity.name = "Person"
//personEntity.managedObjectClassName = NSStringFromClass(PersonMO.self)
//
//let authorName = NSAttributeDescription()
//authorName.name = "name"
//authorName.attributeType = .stringAttributeType
//
//let authorage = NSAttributeDescription()
//authorage.name = "age"
//authorage.attributeType = .integer16AttributeType
//
//let model = NSManagedObjectModel()
//model.entities = [ personEntity ]

let persistentContainer = NSPersistentContainer(name: "Model")

let description = NSPersistentStoreDescription()

description.type = NSInMemoryStoreType // set desired type
 
if description.type == NSSQLiteStoreType || description.type == NSBinaryStoreType {
    // for persistence on local storage we need to set url
    description.url = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)
        .first?.appendingPathComponent("database")
}
persistentContainer.persistentStoreDescriptions = [description]


persistentContainer.loadPersistentStores { description, error in
    if let error = error {
        print("could not load store \(error.localizedDescription)")
        return
    }

    print("store loaded")
}

var context: NSManagedObjectContext {
    return persistentContainer.viewContext
}

//let context = persistentContainer.viewContext


let person = PersonMO(context: context)
person.name = "sa"
person.age = 21
context.insert(person)
try context.save()







```

