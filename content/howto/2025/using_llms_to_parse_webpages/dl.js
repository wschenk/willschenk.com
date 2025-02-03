const puppeteer = require("rebrowser-puppeteer");
const fs = require("fs").promises;
const path = require("path");

// Get URL from command line arguments
const url = process.argv[2];

if (!url) {
  console.error("Please provide a URL as a command line argument");
  console.error("Usage: node script.js <url>");
  process.exit(1);
}

async function downloadPage(url) {
  try {
    // Launch browser
    const browser = await puppeteer.launch({
      headless: "new", // Using new headless mode,
      args: ["--disable-blink-features=AutomationControlled"],
      defaultViewport: null,
    });

    // Create new page
    const page = await browser.newPage({ defaultViewport: null });
    const customUA =
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36";

    // Set custom user agent
    await page.setUserAgent(customUA);

    // Navigate to URL
    console.log(`Navigating to ${url}...`);
    await page.goto(url, {
      waitUntil: "networkidle0", // Wait until network is idle
    });

    // Get page title for filename
    const title = await page.title();
    const sanitizedTitle = title.replace(/[^a-z0-9]/gi, "_").toLowerCase();

    // Create downloads directory if it doesn't exist
    const downloadDir = path.join(process.cwd(), "downloads");
    await fs.mkdir(downloadDir, { recursive: true });

    // Save HTML content
    const htmlContent = await page.content();
    const htmlPath = path.join(downloadDir, `${sanitizedTitle}.html`);
    await fs.writeFile(htmlPath, htmlContent);
    console.log(`HTML saved to: ${htmlPath}`);

    // Save screenshot
    const screenshotPath = path.join(downloadDir, `${sanitizedTitle}.png`);
    await page.screenshot({
      path: screenshotPath,
      fullPage: true,
    });
    console.log(`Screenshot saved to: ${screenshotPath}`);

    // Close browser
    await browser.close();
    console.log("Download completed successfully!");
  } catch (error) {
    console.error("An error occurred:", error);
    process.exit(1);
  }
}

// Run the download function
downloadPage(url);
