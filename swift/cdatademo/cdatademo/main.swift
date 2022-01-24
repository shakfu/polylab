//
//  main.swift
//  cdatademo
//
//  Created by sa on 2/8/21.
//  Copyright © 2021 me. All rights reserved.
//

import Foundation
import CoreData

var container: NSPersistentContainer!

container = NSPersistentContainer(name: "Model")

container.loadPersistentStores { storeDescription, error in
    if let error = error {
        print("Unresolved error \(error)")
    }
}

func saveContext() {
    if container.viewContext.hasChanges {
        do {
            try container.viewContext.save()
        } catch {
            print("An error occurred while saving: \(error)")
        }
    }
}


let context = container.viewContext


let person = Person(context: context)
person.name = "sa"
person.age = 21
//context.insert(person)
//try context.save()
// this also works
saveContext()
