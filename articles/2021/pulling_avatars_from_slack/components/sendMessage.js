const { App } = require("@slack/bolt");

export default async function handler(req, res) {
  const app = new App({
    signingSecret: process.env.SLACK_SIGNING_SECRET,
    token: process.env.SLACK_BOT_TOKEN,
  });

  const { id, message } = req.query;

  console.log(`Sending ${message} to ${id}`);

  if (id && message) {
    try {
      // Call the chat.postMessage method using the WebClient
      const result = await app.client.chat.postMessage({
        channel: id,
        text: message,
      });

      console.log(result);

      res.status(200).json({ id, message, ok: result.ok });
    } catch (error) {
      console.error(error);
      res.status(500).json({ id, message, result: error.toString() });
    }

    res.status(200).json({ id, message, action: "Sent" });
  } else {
    res.status(404).json({ action: "Not Sent", reason: "Missing params" });
  }
}
