#!/usr/bin/env swift

import Foundation

print("Testing Starlink connection...")

let task = Process()
task.executableURL = URL(fileURLWithPath: "/opt/homebrew/bin/grpcurl")
task.arguments = [
    "-plaintext",
    "-max-time", "3",
    "-d", "{\"get_status\":{}}",
    "192.168.100.1:9200",
    "SpaceX.API.Device.Device/Handle"
]

let pipe = Pipe()
let errorPipe = Pipe()
task.standardOutput = pipe
task.standardError = errorPipe

do {
    try task.run()
    task.waitUntilExit()
    
    let exitCode = task.terminationStatus
    print("Exit code: \(exitCode)")
    
    let data = pipe.fileHandleForReading.readDataToEndOfFile()
    let errorData = errorPipe.fileHandleForReading.readDataToEndOfFile()
    
    if let errorOutput = String(data: errorData, encoding: .utf8), !errorOutput.isEmpty {
        print("Error: \(errorOutput)")
        
        if errorOutput.contains("connection refused") {
            print("✅ Detected: Connection refused - Starlink offline")
        }
    }
    
    if exitCode != 0 {
        print("✅ Command failed - should show offline status")
    } else {
        print("✅ Command succeeded - should show online status")
    }
    
} catch {
    print("✅ Exception: \(error) - should show offline status")
}