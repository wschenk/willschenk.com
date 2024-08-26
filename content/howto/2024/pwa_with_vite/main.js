import '@unocss/reset/tailwind.css';
import './main.css';
import { pwaInfo } from 'virtual:pwa-info'
import { registerSW } from 'virtual:pwa-register'

console.log(pwaInfo)
registerSW({ immediate: true })
