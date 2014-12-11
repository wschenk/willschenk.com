---
title: How to track your coworkers
subtitle: Simple passive network surveillance
date: 2014-10-31 00:00 UTC
tags: toys, ruby, redis, howto
disqus_id: silvrback-wschenk-9332
---

How much information do you bleed?

Ever wonder who is else is using your network?  Or,who has actually showed up at the office?

## Networking primer

The simplest thing we can do to make this work is to check to see which devices have registered themselves on the network.  As devices come and go, they connect automatically, so we will have a pretty good idea if people are there or not.  Phones seem to attach and detach quite frequency, probably to conserve battery, so if we are want to answer the question "is so-and-so in the office" we'll need to add additional logic to determine how far spaced the "sighting" events are to mean that the person has left the office, rather than the phone has gone to sleep.

There are a couple of ways to do this. One is to log on you your router and look at the `DHCP` routing tables to see which devices have leased an IP address.  Have you looked at those router webpages though?  Pretty gross.  You'd need to do some scraping, something different for each of the router types, all together pretty gross.

Another thing to consider is `DNS multicast`, also called `Bonjour` or zero-config networking.  This is pretty much the standard now for services announcing themselves on the human-used networks.  (Server rooms have fancier service discovery mechanisms.)  If you want to find a printer, or someone else's iTunes library it works great, but it doesn't do much for phones.

We are going to do this using ping.   This is simple and works everywhere.

## The plan

The code below using `ruby` and `redis` to track which devices have been seen.  You'll need to have `redis` installed and running for this code to work, but it should be easily portable to any Unix system, not just OSX.  So if you have a `RaspberryPI`laying around, it would be run to run it on there.

This code does the following things:

1. Find the local broadcast address.  This is normally `192.168.1.255` on home routers but could be anything.
2. Send 4 pings to the broadcast address and listen for replies.  It may take some time to return back,which is why we have multiple pings.  Collect those IP addresses as a response.
3. Do a reverse DNS lookup on those IP addresses, and mark them as seen in `redis`.
4. (Commented out) Look at the arp table to see if any other devices have announced themselves.  This will discover mode devices, but the ARP cache lasts an unknown amount of time, so this will not correctly track leaving events.
5. The `def seen` method will set a key in `redis` and expire it in `30` seconds.  The scan runs every `10` seconds.  So if we having seen a device in 2 or 3 scans then we assume it has left the network.  We look at each of the keys in the redis set `hosts` to see if it still exists, and if not, assume that the device has left.

## The Code

The only `OSX` ism of this script is that its using `osascript` to push a notification to the desktop that a device name has come on or left.  See below for further things to play with:

```
#!/usr/bin/env ruby -wKU

require 'resolv'
require 'redis'

def notify( text )
  system( "/usr/bin/osascript -e \"display notification \\\"#{text}\\\"\"" )
end

def scan
  # Look for the network broadcast address
  broadcast = `ifconfig -a | grep broadcast`.split[-1]

  # puts "Broadcast Address #{broadcast}"

  unless broadcast =~ /\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/
    puts "#{broadcast} doesn't look correct"
    exit 1
  end

  # Ping the broadcast address 4 times and wait for responses
  ips = `ping -t 4 #{broadcast}`.split(/\n/).collect do |x|
    if x =~ /(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}):/
      $1
    else
      nil
    end
  end.select { |x| x && x != broadcast}.sort.uniq

  dns = Resolv.new

  # Do a host name lookup on all of the addresses and put in redis
  ips.each do |ip|
    name = dns.getname ip
    seen( name )
  end

  # If you want to catch the other devices that don't respond to pings
  # but have an arp, you can scan below.  However, arp addresses get
  # cached for a while, 
  # Catch the other devices that don't respond to pings but have arp addresses
  # (pinging the broadcast address will help populate the local arp table)
  # `arp -a`.split( /\n/ ).collect do |x|
  #   if x =~ /^([^\s]*).*\((\d+\.\d+\.\d+\.\d+)\)/
  #     seen( $1 )
  #   end
  # end

  # Look through all of the members in redis, and for any ones that have
  # expired say that they've left the system
  redis = Redis.new

  redis.smembers( "hosts" ).each do |host|
    unless redis.get host
      redis.srem "hosts", host

      puts "#{Time.new} #{host} left the network"
      notify "#{host} left the network"
    end
  end
end

# Mark the name as being seen.  Create a key with the name and expire it in
# 30 seconds.  Also add it to the hosts set.  If we don't see the name again
# in 3 sweeps, the key will expire.
def seen( name )
  name = name.downcase
  redis = Redis.new
  unless redis.get name
    puts "#{Time.new} #{name} joined the network"
    notify "#{name} joined the network"
  end
  redis.set name, "1"
  redis.expire name, "30"
  redis.sadd "hosts", name
end

# Run forever
while true
  scan
  sleep 10
end
```

## Next steps

Other things to play around with:

1. `nmap -O hostname` will do a scan of the device to see what operation system it is.  However, this is a little more intrusive, needs to run as root, and takes a while to run so it should be queued.
2. Instead of using osascript, post the device joined, left, or presence events to an internal website to make a better UI.
3. This would allow you to map multiple device names to a person.  For example, I know that `willschiphone5s.home` is my phone and `combray.home` is my computer, but how do you keep a mapping of those names to people?
4. `MAC addresses` should be stable for devices once they may contact to the site.  We can get this from the `arp` tables, and those can't be changed like the computer names can.

Remember, information can bleed and there's a whole lot you can do to see about the people around you.