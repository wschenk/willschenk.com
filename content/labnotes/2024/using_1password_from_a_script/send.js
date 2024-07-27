// send.js
import { Resend } from 'resend';

if( process.env.RESEND_API === undefined ) {
    console.log( "Please set RESEND_API" );
    process.exit(1);

}

const resend = new Resend(process.env.RESEND_API);

resend.emails.send({
  from: 'onboarding@resend.dev',
  to: 'wschenk@gmail.com',
  subject: 'Hello World',
  html: '<p>Congrats on sending your <strong>first email</strong>!</p>'
});

console.log( "email sent!" );
