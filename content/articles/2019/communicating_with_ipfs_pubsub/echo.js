const ipfs = require('ipfs-http-client')()

const topic = 'fruit-of-the-day'
const receiveMsg = (msg) => console.log(msg, msg.data.toString())

ipfs.pubsub.subscribe(topic, receiveMsg, (err) => {
  if (err) {
    return console.error(`failed to subscribe to ${topic}`, err)
  }
  console.log(`subscribed to ${topic}`)

  const msg = Buffer.from('banana')

  ipfs.pubsub.publish(topic, msg, (err) => {
    if (err) {
      return console.error(`failed to publish to ${topic}`, err)
    }
    // msg was broadcasted
    console.log(`published to ${topic}`)
  })
})

console.log( "waiting" )


