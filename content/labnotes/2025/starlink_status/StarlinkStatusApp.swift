#!/usr/bin/env swift

//
// Starlink Status Monitor
// A menubar app that monitors Starlink performance with real-time statistics
//
// Usage:
//   ./StarlinkStatusApp.swift                 # Run with minimal output
//   ./StarlinkStatusApp.swift --verbose       # Run with detailed logging
//   ./StarlinkStatusApp.swift -v              # Run with detailed logging (short form)
//

import Cocoa
import Foundation

struct StarlinkStatus: Codable {
    let apiVersion: String
    let dishGetStatus: DishGetStatus
}

struct DishGetStatus: Codable {
    let deviceInfo: DeviceInfo
    let deviceState: DeviceState
    let obstructionStats: ObstructionStats?
    let alerts: Alerts?
    let downlinkThroughputBps: Double?
    let uplinkThroughputBps: Double?
    let popPingLatencyMs: Double?
    let popPingDropRate: Double?
    let outage: Outage?
    let gpsStats: GPSStats?
    let alignmentStats: AlignmentStats?
    let readyStates: ReadyStates?
}

struct DeviceInfo: Codable {
    let softwareVersion: String
    let countryCode: String
    let bootcount: Int
}

struct DeviceState: Codable {
    let uptimeS: String
}

struct ObstructionStats: Codable {
    let fractionObstructed: Double?
}

struct Alerts: Codable {
    let roaming: Bool?
}

struct Outage: Codable {
    let cause: String
}

struct GPSStats: Codable {
    let gpsValid: Bool?
    let gpsSats: Int?
}

struct AlignmentStats: Codable {
    let attitudeEstimationState: String?
}

struct ReadyStates: Codable {
    let scp: Bool?
    let l1l2: Bool?
    let xphy: Bool?
    let aap: Bool?
    let rf: Bool?
}

enum StarlinkState {
    case online
    case booting
    case offline
    case error
    case starting
    
    var icon: String {
        switch self {
        case .online:
            return "🛰️"
        case .booting:
            return "🔄"
        case .offline:
            return "📡"
        case .error:
            return "❌"
        case .starting:
            return "⏳"
        }
    }
    
    var description: String {
        switch self {
        case .online:
            return "Online"
        case .booting:
            return "Booting"
        case .offline:
            return "Offline"
        case .error:
            return "Error"
        case .starting:
            return "Starting"
        }
    }
}

struct NetworkStats {
    let timestamp: Date
    let latency: Double?
    let dropRate: Double?
    let downSpeed: Double?
    let upSpeed: Double?
    let obstruction: Double?
}

class StarlinkStatusApp: NSObject, NSApplicationDelegate, NSMenuDelegate {
    var statusItem: NSStatusItem!
    var menu: NSMenu!
    var timer: Timer?
    var currentStatus: StarlinkStatus?
    var networkHistory: [NetworkStats] = []
    let maxHistoryCount = 60 // Keep 5 minutes of history (60 * 5 seconds)
    var verboseLogging = false
    var previousState: StarlinkState = .starting
    
    func log(_ message: String) {
        if verboseLogging {
            print(message)
        }
    }
    
    func applicationDidFinishLaunching(_ notification: Notification) {
        // Parse command line arguments
        let args = CommandLine.arguments
        verboseLogging = args.contains("--verbose") || args.contains("-v")
        
        if verboseLogging {
            print("🚀 StarlinkStatusApp starting up with verbose logging...")
        } else {
            print("🛰️ Starlink Status Monitor started")
        }
        
        setupStatusItem()
        setupMenu()
        startStatusUpdates()
    }
    
    func setupStatusItem() {
        log("🔧 Setting up status item...")
        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        statusItem.button?.title = "⏳" // Show starting state initially
        log("🔧 Status item created, setting up menu...")
    }
    
    func setupMenu() {
        log("🔧 Setting up initial menu...")
        menu = NSMenu()
        
        // Show initial starting state
        updateMenu(state: .starting, status: nil)
        
        // IMPORTANT: Assign the menu to the status item
        menu.delegate = self
        statusItem.menu = menu
        log("🔧 Menu assigned to status item with delegate")
    }
    
    @objc func quit() {
        NSApplication.shared.terminate(nil)
    }
    
    // NSMenuDelegate method - called when menu is about to open
    func menuWillOpen(_ menu: NSMenu) {
        log("🔧 Menu will open - refreshing content...")
        if let status = currentStatus {
            let state = determineState(from: status)
            updateMenu(state: state, status: status)
        } else {
            log("⚠️ No current status available for menu - showing offline state")
            updateMenu(state: .offline, status: nil)
        }
    }
    
    func startStatusUpdates() {
        timer = Timer.scheduledTimer(withTimeInterval: 5.0, repeats: true) { _ in
            self.updateStatus()
        }
        updateStatus() // Initial update
    }
    
    func updateStatus() {
        log("📡 Starting status update...")
        
        // Run the network call in background to prevent UI blocking
        DispatchQueue.global(qos: .background).async {
            self.performNetworkCall()
        }
    }
    
    func performNetworkCall() {
        let task = Process()
        task.executableURL = URL(fileURLWithPath: "/opt/homebrew/bin/grpcurl")
        task.arguments = [
            "-plaintext",
            "-max-time", "5", // 5 second timeout for faster failure detection
            "-d", "{\"get_status\":{}}",
            "192.168.100.1:9200",
            "SpaceX.API.Device.Device/Handle"
        ]
        
        log("🔧 Running command with timeout: \(task.executableURL?.path ?? "unknown") \(task.arguments?.joined(separator: " ") ?? "")")
        
        let pipe = Pipe()
        let errorPipe = Pipe()
        task.standardOutput = pipe
        task.standardError = errorPipe
        
        // Set up a timeout mechanism
        var hasTimedOut = false
        let timeoutTimer = Timer.scheduledTimer(withTimeInterval: 7.0, repeats: false) { _ in
            hasTimedOut = true
            if task.isRunning {
                task.terminate()
                self.log("⏰ Command timed out after 7 seconds")
            }
        }
        
        do {
            try task.run()
            task.waitUntilExit()
            timeoutTimer.invalidate()
            
            if hasTimedOut {
                log("⏰ Network request timed out")
                // Clear cached status when connection times out
                self.currentStatus = nil
                DispatchQueue.main.async {
                    self.updateUI(state: .offline, status: nil)
                }
                return
            }
            
            let exitCode = task.terminationStatus
            log("📊 Command exit code: \(exitCode)")
            
            let data = pipe.fileHandleForReading.readDataToEndOfFile()
            let errorData = errorPipe.fileHandleForReading.readDataToEndOfFile()
            
            if let errorOutput = String(data: errorData, encoding: .utf8), !errorOutput.isEmpty {
                log("❌ Command stderr: \(errorOutput)")
                
                // Check for common network errors
                if errorOutput.contains("connection refused") || 
                   errorOutput.contains("network unreachable") ||
                   errorOutput.contains("timeout") ||
                   errorOutput.contains("no route to host") {
                    log("🌐 Network connectivity issue detected")
                    // Clear cached status when connection fails
                    self.currentStatus = nil
                    DispatchQueue.main.async {
                        self.updateUI(state: .offline, status: nil)
                    }
                    return
                }
            }
            
            if exitCode != 0 {
                log("❌ Command failed with exit code: \(exitCode)")
                // Clear cached status when connection fails
                self.currentStatus = nil
                DispatchQueue.main.async {
                    self.updateUI(state: .offline, status: nil)
                }
                return
            }
            
            if let output = String(data: data, encoding: .utf8), !output.isEmpty {
                log("📥 Command output length: \(output.count) characters")
                log("📥 Command output preview: \(String(output.prefix(200)))...")
                parseStatusResponse(output)
            } else {
                log("❌ No output received from command")
                DispatchQueue.main.async {
                    self.updateUI(state: .offline, status: nil)
                }
            }
        } catch {
            timeoutTimer.invalidate()
            log("❌ Failed to execute command: \(error)")
            // Clear cached status when command execution fails
            self.currentStatus = nil
            DispatchQueue.main.async {
                self.updateUI(state: .offline, status: nil)
            }
        }
    }
    
    func parseStatusResponse(_ response: String) {
        log("🔍 Parsing JSON response...")
        
        guard let data = response.data(using: .utf8) else {
            log("❌ Failed to convert response to UTF8 data")
            DispatchQueue.main.async {
                self.updateUI(state: .error, status: nil)
            }
            return
        }
        
        do {
            let status = try JSONDecoder().decode(StarlinkStatus.self, from: data)
            log("✅ Successfully decoded JSON response")
            currentStatus = status
            
            let state = determineState(from: status)
            log("📊 Determined state: \(state)")
            
            // Add to network history
            let networkStats = NetworkStats(
                timestamp: Date(),
                latency: status.dishGetStatus.popPingLatencyMs,
                dropRate: status.dishGetStatus.popPingDropRate,
                downSpeed: status.dishGetStatus.downlinkThroughputBps,
                upSpeed: status.dishGetStatus.uplinkThroughputBps,
                obstruction: status.dishGetStatus.obstructionStats?.fractionObstructed
            )
            addNetworkStats(networkStats)
            
            DispatchQueue.main.async {
                self.updateUI(state: state, status: status)
            }
        } catch {
            log("❌ Failed to decode Starlink response: \(error)")
            if verboseLogging {
                log("📄 Raw response that failed to parse:")
                log(response)
            }
            DispatchQueue.main.async {
                self.updateUI(state: .error, status: nil)
            }
        }
    }
    
    func determineState(from status: StarlinkStatus) -> StarlinkState {
        log("🔍 Determining state from status...")
        
        if let outage = status.dishGetStatus.outage {
            log("⚠️ Outage detected: \(outage.cause)")
            if outage.cause == "BOOTING" {
                return .booting
            }
            return .offline
        }
        
        log("✅ No outage detected")
        
        if let readyStates = status.dishGetStatus.readyStates {
            log("🔧 Ready states - scp: \(readyStates.scp ?? false), l1l2: \(readyStates.l1l2 ?? false), xphy: \(readyStates.xphy ?? false), aap: \(readyStates.aap ?? false), rf: \(readyStates.rf ?? false)")
            
            if let scp = readyStates.scp,
               let l1l2 = readyStates.l1l2,
               let xphy = readyStates.xphy,
               let aap = readyStates.aap,
               let rf = readyStates.rf,
               scp && l1l2 && xphy && aap && rf {
                log("✅ All ready states are true - device is online")
                return .online
            } else {
                log("⚠️ Not all ready states are true - device is offline")
            }
        } else {
            log("❌ No ready states found in response")
        }
        
        return .offline
    }
    
    func addNetworkStats(_ stats: NetworkStats) {
        networkHistory.append(stats)
        if networkHistory.count > maxHistoryCount {
            networkHistory.removeFirst()
        }
        log("📊 Network history now has \(networkHistory.count) entries")
        log("📊 Latest dropRate: \(stats.dropRate?.description ?? "nil"), latency: \(stats.latency?.description ?? "nil")")
    }
    
    func calculatePingSuccessRate() -> Double {
        guard !networkHistory.isEmpty else { return 0.0 }
        
        let totalSamples = networkHistory.count
        var successfulSamples = 0
        
        for stat in networkHistory {
            // If dropRate is nil, it means connection is working (100% success)
            // If dropRate is a number, success rate = (1 - dropRate)
            if let dropRate = stat.dropRate {
                if dropRate == 0.0 {
                    successfulSamples += 1
                } else if dropRate > 0.0 && dropRate <= 1.0 {
                    // Partial success based on drop rate (though this is usually 0 or 1)
                    successfulSamples += Int((1.0 - dropRate) * 100) > 50 ? 1 : 0
                }
            } else {
                // nil dropRate means connection is working normally
                successfulSamples += 1
            }
        }
        
        return Double(successfulSamples) / Double(totalSamples) * 100.0
    }
    
    func getAverageLatency() -> Double? {
        let validLatencies = networkHistory.compactMap { $0.latency }.filter { $0 > 0 }
        guard !validLatencies.isEmpty else { return nil }
        return validLatencies.reduce(0.0, +) / Double(validLatencies.count)
    }
    
    func getAverageSpeed() -> (down: Double?, up: Double?) {
        let validDownSpeeds = networkHistory.compactMap { $0.downSpeed }.filter { $0 > 0 }
        let validUpSpeeds = networkHistory.compactMap { $0.upSpeed }.filter { $0 > 0 }
        
        let avgDown = validDownSpeeds.isEmpty ? nil : validDownSpeeds.reduce(0.0, +) / Double(validDownSpeeds.count)
        let avgUp = validUpSpeeds.isEmpty ? nil : validUpSpeeds.reduce(0.0, +) / Double(validUpSpeeds.count)
        
        return (avgDown, avgUp)
    }
    
    func updateUI(state: StarlinkState, status: StarlinkStatus?) {
        log("🎨 Updating UI with state: \(state) (\(state.icon))")
        
        // Check for state transitions and show notifications
        checkForStateTransitions(newState: state)
        
        DispatchQueue.main.async {
            self.statusItem.button?.title = state.icon
            self.updateMenu(state: state, status: status)
            self.log("✅ UI updated successfully")
        }
        
        previousState = state
    }
    
    func checkForStateTransitions(newState: StarlinkState) {
        // Notify when Starlink comes online from booting
        if previousState == .booting && newState == .online {
            showNotification(title: "🛰️ Starlink Connected", message: "Your Starlink is now online and ready to use!")
            log("🔔 Showed notification: Starlink connected")
        }
        // Also notify when coming online from offline (reconnection)
        else if (previousState == .offline || previousState == .starting) && newState == .online {
            showNotification(title: "🛰️ Starlink Online", message: "Connection established successfully!")
            log("🔔 Showed notification: Starlink online")
        }
        // Notify when going offline from online
        else if previousState == .online && (newState == .offline || newState == .error) {
            showNotification(title: "📡 Starlink Disconnected", message: "Connection lost. Monitoring for reconnection...")
            log("🔔 Showed notification: Starlink disconnected")
        }
    }
    
    func showNotification(title: String, message: String) {
        // Use AppleScript to show notifications - works better for command line apps
        let script = """
        display notification "\(message)" with title "\(title)" sound name "default"
        """
        
        let appleScript = NSAppleScript(source: script)
        var error: NSDictionary?
        appleScript?.executeAndReturnError(&error)
        
        if let error = error {
            log("❌ Failed to show notification: \(error)")
        }
    }
    
    func updateMenu(state: StarlinkState, status: StarlinkStatus?) {
        log("🔧 Updating menu with \(networkHistory.count) history entries...")
        menu.removeAllItems()
        
        // Header - make it bold and enabled
        menu.addItem(NSMenuItem(title: "🛰️ STARLINK STATUS", action: nil, keyEquivalent: ""))
        menu.addItem(NSMenuItem.separator())
        
        // Current Status with emoji indicators
        let statusEmoji = state == .online ? "✅" : state == .booting ? "🔄" : state == .offline ? "⚠️" : state == .error ? "❌" : "⏳"
        menu.addItem(NSMenuItem(title: "\(statusEmoji) \(state.description.uppercased())", action: nil, keyEquivalent: ""))
        
        // Show helpful message when offline/error/starting
        if state == .offline || state == .error || state == .starting {
            menu.addItem(NSMenuItem.separator())
            menu.addItem(NSMenuItem(title: "🔍 TROUBLESHOOTING", action: nil, keyEquivalent: ""))
            
            if state == .starting {
                menu.addItem(NSMenuItem(title: "• Attempting first connection...", action: nil, keyEquivalent: ""))
                menu.addItem(NSMenuItem(title: "• This may take a few seconds", action: nil, keyEquivalent: ""))
            } else if state == .offline {
                menu.addItem(NSMenuItem(title: "• Check WiFi connection", action: nil, keyEquivalent: ""))
                menu.addItem(NSMenuItem(title: "• Verify Starlink is powered on", action: nil, keyEquivalent: ""))
                menu.addItem(NSMenuItem(title: "• Ensure 192.168.100.1 is reachable", action: nil, keyEquivalent: ""))
            } else {
                menu.addItem(NSMenuItem(title: "• Connection timed out", action: nil, keyEquivalent: ""))
                menu.addItem(NSMenuItem(title: "• Check network connectivity", action: nil, keyEquivalent: ""))
            }
            
            // Show last successful connection if available
            if !networkHistory.isEmpty {
                let lastUpdate = networkHistory.last!.timestamp
                let timeAgo = Int(Date().timeIntervalSince(lastUpdate))
                let timeStr = timeAgo < 60 ? "\(timeAgo)s ago" : timeAgo < 3600 ? "\(timeAgo/60)m ago" : "\(timeAgo/3600)h ago"
                menu.addItem(NSMenuItem(title: "📊 Last data: \(timeStr)", action: nil, keyEquivalent: ""))
            }
        }
        
        if let status = status {
            let dishStatus = status.dishGetStatus
            
            // Uptime with emoji
            if let uptimeSeconds = Int(dishStatus.deviceState.uptimeS) {
                let uptime = formatUptime(uptimeSeconds)
                menu.addItem(NSMenuItem(title: "⏰ Uptime: \(uptime)", action: nil, keyEquivalent: ""))
            }
            
            menu.addItem(NSMenuItem.separator())
            
            // Network Performance Section - bold header
            menu.addItem(NSMenuItem(title: "📊 NETWORK PERFORMANCE", action: nil, keyEquivalent: ""))
            
            // Ping Success Rate with visual indicator
            let successRate = calculatePingSuccessRate()
            let pingEmoji = successRate >= 95 ? "🟢" : successRate >= 80 ? "🟡" : "🔴"
            menu.addItem(NSMenuItem(title: "\(pingEmoji) Success Rate: \(String(format: "%.1f", successRate))%", action: nil, keyEquivalent: ""))
            
            // Current Performance with better formatting
            if let downlink = dishStatus.downlinkThroughputBps {
                let mbps = String(format: "%.1f", downlink / 1_000_000)
                let speedEmoji = downlink > 50_000_000 ? "🟢" : downlink > 10_000_000 ? "🟡" : "🔴"
                menu.addItem(NSMenuItem(title: "\(speedEmoji) ↓ \(mbps) Mbps", action: nil, keyEquivalent: ""))
            }
            
            if let uplink = dishStatus.uplinkThroughputBps {
                let mbps = String(format: "%.1f", uplink / 1_000_000)
                let speedEmoji = uplink > 10_000_000 ? "🟢" : uplink > 1_000_000 ? "🟡" : "🔴"
                menu.addItem(NSMenuItem(title: "\(speedEmoji) ↑ \(mbps) Mbps", action: nil, keyEquivalent: ""))
            }
            
            // Average Performance (if we have history)
            if networkHistory.count > 3 {
                let avgSpeeds = getAverageSpeed()
                if let avgDown = avgSpeeds.down {
                    let mbps = String(format: "%.1f", avgDown / 1_000_000)
                    menu.addItem(NSMenuItem(title: "📈 ↓ \(mbps) Mbps (5min avg)", action: nil, keyEquivalent: ""))
                }
                
                if let avgUp = avgSpeeds.up {
                    let mbps = String(format: "%.1f", avgUp / 1_000_000)
                    menu.addItem(NSMenuItem(title: "📈 ↑ \(mbps) Mbps (5min avg)", action: nil, keyEquivalent: ""))
                }
            }
            
            // Latency with visual indicators
            if let latency = dishStatus.popPingLatencyMs, latency > 0 {
                let ms = String(format: "%.0f", latency)
                let latencyEmoji = latency < 30 ? "🟢" : latency < 100 ? "🟡" : "🔴"
                menu.addItem(NSMenuItem(title: "\(latencyEmoji) Ping: \(ms)ms", action: nil, keyEquivalent: ""))
            }
            
            if let avgLatency = getAverageLatency(), networkHistory.count > 3 {
                let ms = String(format: "%.0f", avgLatency)
                menu.addItem(NSMenuItem(title: "📊 Ping: \(ms)ms (avg)", action: nil, keyEquivalent: ""))
            }
            
            // Drop Rate with clear indicators
            if let dropRate = dishStatus.popPingDropRate {
                let percentage = String(format: "%.1f", dropRate * 100)
                let dropEmoji = dropRate == 0 ? "🟢" : dropRate < 0.1 ? "🟡" : "🔴"
                menu.addItem(NSMenuItem(title: "\(dropEmoji) Packet Loss: \(percentage)%", action: nil, keyEquivalent: ""))
            } else {
                menu.addItem(NSMenuItem(title: "🟢 Packet Loss: 0.0%", action: nil, keyEquivalent: ""))
            }
            
            // Obstruction with visual indicator
            if let obstructionStats = dishStatus.obstructionStats,
               let fractionObstructed = obstructionStats.fractionObstructed {
                let percentage = String(format: "%.2f", fractionObstructed * 100)
                let obstructionEmoji = fractionObstructed < 0.01 ? "🟢" : fractionObstructed < 0.05 ? "🟡" : "🔴"
                menu.addItem(NSMenuItem(title: "\(obstructionEmoji) Obstruction: \(percentage)%", action: nil, keyEquivalent: ""))
            }
            
            // History Info with more detail
            if !networkHistory.isEmpty {
                let timeSpan = networkHistory.count * 5
                let minutes = timeSpan / 60
                let seconds = timeSpan % 60
                let timeStr = minutes > 0 ? "\(minutes)m \(seconds)s" : "\(seconds)s"
                menu.addItem(NSMenuItem(title: "📊 History: \(timeStr) (\(networkHistory.count) samples)", action: nil, keyEquivalent: ""))
            }
            
            menu.addItem(NSMenuItem.separator())
            
            // Device Info Section with better formatting
            menu.addItem(NSMenuItem(title: "🔧 DEVICE INFO", action: nil, keyEquivalent: ""))
            menu.addItem(NSMenuItem(title: "📱 \(dishStatus.deviceInfo.softwareVersion)", action: nil, keyEquivalent: ""))
            menu.addItem(NSMenuItem(title: "🔄 Boot #\(dishStatus.deviceInfo.bootcount)", action: nil, keyEquivalent: ""))
            
            // GPS Info with status indicator
            if let gpsStats = dishStatus.gpsStats,
               let gpsValid = gpsStats.gpsValid,
               let gpsSats = gpsStats.gpsSats {
                let gpsEmoji = gpsValid ? "🟢" : "🔴"
                let gpsStatus = gpsValid ? "LOCKED" : "NO LOCK"
                menu.addItem(NSMenuItem(title: "\(gpsEmoji) GPS: \(gpsStatus) (\(gpsSats) sats)", action: nil, keyEquivalent: ""))
            }
            
            // Roaming with clear indicator
            if let alerts = dishStatus.alerts,
               let roaming = alerts.roaming,
               roaming {
                menu.addItem(NSMenuItem(title: "🌍 ROAMING ACTIVE", action: nil, keyEquivalent: ""))
            }
        }
        
        menu.addItem(NSMenuItem.separator())
        let updateTime = formatTime(Date())
        menu.addItem(NSMenuItem(title: "🕒 Updated: \(updateTime)", action: nil, keyEquivalent: ""))
        menu.addItem(NSMenuItem.separator())
        menu.addItem(NSMenuItem(title: "❌ Quit", action: #selector(quit), keyEquivalent: "q"))
        
        log("🔧 Menu updated with \(menu.items.count) items")
    }
    
    func formatUptime(_ seconds: Int) -> String {
        let hours = seconds / 3600
        let minutes = (seconds % 3600) / 60
        let secs = seconds % 60
        
        if hours > 0 {
            return String(format: "%dh %dm %ds", hours, minutes, secs)
        } else if minutes > 0 {
            return String(format: "%dm %ds", minutes, secs)
        } else {
            return String(format: "%ds", secs)
        }
    }
    
    func formatTime(_ date: Date) -> String {
        let formatter = DateFormatter()
        formatter.timeStyle = .medium
        return formatter.string(from: date)
    }
}

// Main app setup
let app = NSApplication.shared
let delegate = StarlinkStatusApp()
app.delegate = delegate
app.setActivationPolicy(.accessory) // Run in background
app.run()