// pages/_app.js
import 'bootstrap/dist/css/bootstrap.css'

export default function MyApp({ Component, pageProps }) {
    return <div className="container">
        <Component {...pageProps} />
        </div>
}
