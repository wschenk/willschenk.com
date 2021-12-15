const { App } = require('@slack/bolt');
require('dotenv').config()
const { PrismaClient } = require('@prisma/client')
const prisma = new PrismaClient()

const fs = require('fs')

const app = new App({
  signingSecret: process.env.SLACK_SIGNING_SECRET,
  token: process.env.SLACK_BOT_TOKEN,
});

(async () => {
    console.log( "Getting user list from slack" )
    const slackdata = await app.client.users.list()


    if( slackdata["ok"] ) {
        const data = []
        slackdata["members"].map( (e) => {            
            data.push({
                id: e["id"],
                name: e["name"],
                email: e["profile"]["email"],
                deleted: e["deleted"],
                admin: e["is_admin"],
                restricted: e["is_restricted"],
                bot: e["is_bot"],
                tz: e["tz"],
                title: e["profile"]["title"],
                skype: e["profile"]["skype"],
                real_name: e["profile"]["real_name"],
                display_name: e["profile"]["display_name"],
                status_text: e["profile"]["status_text"],
                status_email: e["profile"]["status_email"],
                custom_image: e["profile"]["is_custom_image"],
                original_image: e["profile"]["image_original"],
            })
        })

        console.log( `Found ${data.length} accounts, updating database` );

        while( data.length > 0 ) {
            const e = data.pop();

            // console.log( `Updating/created ${e['name']}` )

            const user = await prisma.slackUser.upsert({
                where: { id: e['id'] },
                update: e,
                create: e
            })

        }

        console.log( "Done" );
    }
})();
