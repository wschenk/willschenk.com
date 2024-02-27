export function setToken(token) {
    window.localStorage.setItem("token", token);
}

export function getToken() {
    return window.localStorage.getItem("token");
}

export function clearSession() {
    window.locationStorage.clear()
}

async function authedGet(url, data) {
    const final_url = `${WEB_API_URL}${url}`;
    console.log("authGet", final_url, getToken());
    const response = await fetch(final_url, {
        headers: {
            "Content-Type": "application/json",
            Authorization: `Bearer ${getToken()}`,
        },
    });

    if (!response.ok) {
        window.localStorage.clear();
        flash_and_redirect(response.statusText, "/");

        console.log("error", response.statusText);
        return { error: true, errMessage: response.statusText };
    }

    const reply = await response.json();

    console.log("Got reply", reply);

    return reply;
  }

async function authedPost(url, data) {
    const final_url = `${WEB_API_URL}${url}`;
    console.log("authedPost", final_url, getToken());
    const response = await fetch(final_url, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
            Authorization: `Bearer ${getToken()}`,
        },
    });

    if (!response.ok) {
        window.localStorage.clear();
        flash_and_redirect(response.statusText, "/");

        console.log("error", response.statusText);
        return { error: true, errMessage: response.statusText };
    }

    const reply = await response.json();

    console.log("Got reply", reply);

    return reply;
  }
