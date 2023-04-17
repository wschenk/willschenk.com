---
title: "Kafka Everywhere: New Plans and Pricing for Apache Kafka on Heroku"
date: 2017-09-14
origin: 
alternate: http://feedproxy.google.com/~r/heroku/~3/L7hbqbV_ArE/kafka-on-heroku-new-plans
type: link
author: Rand Fitzpatrick
---

Event-driven architectures are on the rise, in response to fast-moving data and constellations of inter-connected systems. In order to support this trend, last year we released [Apache Kafka on Heroku](https://www.heroku.com/kafka) - a gracefully integrated, fully managed, and carefully optimized element of Heroku's platform that is the culmination of years of experience of running many hundreds of Kafka clusters in production and contributing code to the Kafka ecosystem.

Today, we are excited to announce additional plans and pricing in our Kafka offering in order to make Apache Kafka more accessible, and to better support development, testing, and low volume production needs.

## [Apache Kafka on Heroku: Now With More Flexibility and Speed](https://blog.heroku.com/kafka-on-heroku-new-plans#apache-kafka-on-heroku-now-with-more-flexibility-and-speed)

[Apache Kafka](https://kafka.apache.org/) is a powerful, distributed streaming platform, and the dominant open source solution in managing high scale event streams. Kafka enables you to easily design and implement architectures for many important use cases, such as elastic queuing, data pipelines & analytics, and microservices coordination. Apache Kafka on Heroku removes the complexity and cost of running Kafka, making its valuable resources available to a broad range of developers and applications.

The new addition to our managed Kafka plans, **Basic,** is based on a robust multi-tenant Kafka architecture. Multi-tenancy provides for much faster access to Kafka, with new resources being available in a matter of seconds, rather than minutes, and a much more accessible price point. This allows us to better serve application creators needing Kafka configurations that are more suitable for development or staging, or as production environments that don't require the full capacity of a dedicated cluster (as provided in our existing **Standard** and **Extended** plans).

| | **Basic** | **Standard** | **Extended** |
| --- | --- | --- | --- |
| **Cluster** | Shared | Dedicated | Dedicated |
| **Event Stream Volume** | Low | High | Massive |
| **Ideal Use Case** | Develop + Test,   
Low-Scale Production | Production | Production |
| **Price ($/month)** | $100+ | $1,000+ | $4,000+ |

These new multi-tenant plans allow for rapid creation of Kafka add-ons, easy integration into Heroku applications, and the world-class monitoring and tooling you have come to expect from Heroku.

<iframe allowfullscreen src="https://player.vimeo.com/video/233739649" width="640" height="230"></iframe>
## [Get Started Today](https://blog.heroku.com/kafka-on-heroku-new-plans#get-started-today)

Using one of the new plans is as simple as provisioning an instance of the Kafka add-on and attaching it to a Heroku app:

    heroku addons:create heroku-kafka:basic-0 -a sushi-app

We are excited to see what you build with Kafka! Full details of the new Kafka plans can be found in [Heroku Elements](https://elements.heroku.com/addons/heroku-kafka) and in [Heroku Dev Center](https://devcenter.heroku.com/articles/multi-tenant-kafka-on-heroku). If you have any questions or feedback, please let us know at [kafka@heroku.com](mailto:kafka@heroku.com).

 ![](http://feeds.feedburner.com/~r/heroku/~4/L7hbqbV_ArE)