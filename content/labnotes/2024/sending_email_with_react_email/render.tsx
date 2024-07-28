import Email from './emails/template';
import { render } from '@react-email/components';

console.log( render( <Email />, {
    pretty: true,
} ) );
