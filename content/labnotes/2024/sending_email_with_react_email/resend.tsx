import { Resend } from 'resend';
import Email from './emails/template';

if( process.env.RESEND_API === undefined ) {
    console.log( "Please set RESEND_API" );
    process.exit(1);
}

const resend = new Resend(process.env.RESEND_API);

(async function() {
    const results = await resend.emails.send({
        from: 'onboarding@resend.dev',
        to:   'wschenk@gmail.com',
        subject: 'Test email',
        react: <Email />,
    });

    console.log( "Email sent" )
    console.log( results );
})();
