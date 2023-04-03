import "https://deno.land/x/dotenv/load.ts";

function usage_and_quit() {
    console.log("Set SITE_ID and PLAUSIBLE_API in the environment or .env file");
    Deno.exit(1);
}

const SITE_ID = Deno.env.get('SITE_ID')
const PLAUSIBLE_API = Deno.env.get('PLAUSIBLE_API')

if (SITE_ID == undefined || PLAUSIBLE_API == undefined)
    usage_and_quit();

async function greatest_hits(period = "6mo", limit = 10) {
    const request = fetch(
        `https://plausible.io/api/v1/stats/breakdown?site_id=${SITE_ID}&period=${period}&property=event:page&limit=${limit}`,
        {
            headers: new Headers({
                'Authorization': `Bearer ${PLAUSIBLE_API}`
            })
        }
    )

    return request
        .then(response => response.json())
}

async function referrers(page: string, period = "6mo") {
    const limit = 10
    const request = fetch(
        `https://plausible.io/api/v1/stats/breakdown?site_id=${SITE_ID}&period=${period}&property=event:page&limit=${limit}`,
        {
            headers: new Headers({
                'Authorization': `Bearer ${PLAUSIBLE_API}`
            })
        }
    )

    return request
        .then(response => response.json())
}

console.log(await greatest_hits("12mo", 20))
console.log(await referrers("/articles/2020/tailwind_and_rails/", "12mo"))
