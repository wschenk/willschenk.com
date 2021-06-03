require 'redis'

class Handler
  def run(req)
    @redis = Redis.new( host: "redis-master.default", password: File.read( '/var/openfaas/secrets/password' ) )

    return @redis.incr( 'mykey' ) end
end
