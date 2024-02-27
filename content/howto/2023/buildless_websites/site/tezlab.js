const WEB_API_URL = "http://localhost:3000";
const CLIENT_SECRET = "fFnIlj3nSWZrdvRoaaxXu7R87JBczq4zVohGBgLcnOg";
const CLIENT_ID = "EgaE_fxzvo26TXROOh368bzuoISA332_U7B7aVz0Sew";

export async function login(user, password) {
    const data = {
        username: user,
        password: password,
        client_id: CLIENT_ID,
        client_secret: CLIENT_SECRET,
        scope: "mobile",
        grant_type: "password",
    };

    const response = await fetch(`${WEB_API_URL}/oauth/token`, {
        method: "POST",
        body: JSON.stringify(data),
        headers: { "Content-Type": "application/json" },
    });

    if (!response.ok) {
        flash_and_redirect(response.statusText, "/");
    }

    const json = await response.json();
    console.log("/oauth/token response", json);
    setToken(json.access_token);

    return getToken();
}

export function logout() {
    flash_and_redirect("You've been logged out", "/");
}
