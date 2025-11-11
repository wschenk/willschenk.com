import Cocoa
import WebKit

class App: NSObject, NSApplicationDelegate, WKScriptMessageHandler, WKNavigationDelegate {
    var win:NSWindow!
    var web:WKWebView!

    func applicationDidFinishLaunching(_ n: Notification) {
        // 1) Setup menu bar
        setupMenuBar()
        
        // 2) Configure a JS bridge named "app"
        let ucc = WKUserContentController()
        ucc.add(self, name: "app")
        let js = """
        window.sendToSwift = (data) =>
          window.webkit.messageHandlers.app.postMessage(data);
        console.log('Injected bridge ready');
        """
        ucc.addUserScript(.init(source: js, injectionTime: .atDocumentStart, forMainFrameOnly: true))

        let cfg = WKWebViewConfiguration(); cfg.userContentController = ucc

        // 3) Window + WebView
        win = NSWindow(contentRect: NSMakeRect(0,0,900,700), styleMask: [.titled,.closable,.resizable,.fullSizeContentView], backing: .buffered, defer: false)
        win.collectionBehavior = [.fullScreenPrimary]
        web = WKWebView(frame: .zero, configuration: cfg)
        web.navigationDelegate = self
        win.contentView = web
        win.center(); win.title = "Web Browser"; win.makeKeyAndOrderFront(nil)

        // 4) Parse command line arguments
        var url = "http://localhost:3000"
        var startFullScreen = false
        
        for (index, arg) in CommandLine.arguments.enumerated() {
            if arg == "--fullscreen" || arg == "-f" {
                startFullScreen = true
            } else if index == 1 && !arg.hasPrefix("-") {
                url = arg
            }
        }
        
        // 5) Load URL and handle fullscreen
        web.load(URLRequest(url: URL(string: url)!))
        
        if startFullScreen {
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
                self.win.toggleFullScreen(nil)
            }
        }
    }

    // JS -> Swift
    func userContentController(_ u: WKUserContentController, didReceive m: WKScriptMessage) {
        print("JS sent:", m.body) // do whatever you want with the data
        // Example: reply back to JS
        web.evaluateJavaScript("console.log('Swift got your message: \(String(describing: m.body))')")
    }

    // Page finished loading: Swift -> JS
    func webView(_ webView: WKWebView, didFinish nav: WKNavigation!) {
        // Call into JS on the page
        web.evaluateJavaScript("""
          document.title = 'Loaded via Swift';
          if (window.sendToSwift) sendToSwift({type:'ready', time: Date.now()});
          'ok'
        """) { result, error in
            if let error = error { print("JS error:", error) }
            else { print("JS result:", result ?? "nil") }
        }
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ app: NSApplication) -> Bool {
        return true
    }
    
    func setupMenuBar() {
        let mainMenu = NSMenu()
        
        // App menu
        let appMenuItem = NSMenuItem()
        mainMenu.addItem(appMenuItem)
        
        let appMenu = NSMenu()
        appMenuItem.submenu = appMenu
        
        let quitMenuItem = NSMenuItem(title: "Quit Web Browser", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
        appMenu.addItem(quitMenuItem)
        
        NSApplication.shared.mainMenu = mainMenu
    }
}

let app = NSApplication.shared
let delegate = App()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.activate(ignoringOtherApps: true)
app.run()
