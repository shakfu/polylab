//
//  AppDelegate.swift
//  xibdemo
//
//  Created by sa on 2/27/21.
//

import Cocoa

@main
class AppDelegate: NSObject, NSApplicationDelegate {

    @IBOutlet var window: NSWindow!

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        // Insert code here to initialize your application
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }

    @IBAction func hello(_ sender: Any) {
        print("HELLO WORLD")
    }

}

