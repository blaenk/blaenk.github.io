---
title: RabbitMQ
published: July 27, 2014
excerpt: Open source AMQP-compliant message broker
comments: off
toc: left
---

For every TCP connection there could be multiple channels through which AMQP commands may be sent, each channel with its own unique ID. There are three parts to successful routing of an AMQP message: exchanges, queues, and bindings. Exchanges are where produces publish messages, bindings are how messages get routed from an exchange to a given queue, and queues are where messages end up.

*[AMQP]: Advanced Message Queuing Protocol

Consumers can receive messages from a queue by subscribing with `basic.consume`, which automatically takes another message off of the queue after consuming or rejecting the previous one. Alternatively, it's possible to consume a single message off of the queue using `basic.get`, which essentially subscribes to the queue and unsubscribes after retrieving a single message.

If a message arrives at a queue and there are no subscribed consumers, it waits there until one subscribes and consumes it off of the queue. Conversely, if there are multiple consumers, messages are served to them in a round-robin fashion. Messages that are published to an exchange that has no queue to be routed to are discarded by RabbitMQ.

Every consumed message must be acknowledged either by sending an explicit acknowledge with `basic.ack` or setting the `auto_ack` parameter to true when subscribing to the queue, which automatically considers a message to be acknowledge as soon as the consumer receives it.

If a consumer receives a message and disconnects before acknowledging, RabbitMQ redelivers it to the next subscribed consumer. If instead a consumer simply forgets to acknowledge a message, it won't be sent anymore messages since it assumes that the consumer isn't finished processing the unacknowledged message.

It's possible to explicitly reject a message instead of acknowledging it, either by disconnecting from RabbitMQ or on RabbitMQ > 2.0 using the `basic.reject` command. Using `basic.reject`, if the `requeue` parameter is set to true, RabbitMQ will redeliver the message to the next subscribed consumer. In future versions of RabbitMQ, setting the `requeue` parameter to false can send the message to a dead letter queue which can be used for inspection. Alternatively, a message can be discarded by simply acknowledging it.

Queues can be created by consumers or producers using the `queue.declare` command, but consumers can't declare a queue while subscribed to another on the same channel. Consumers must first unsubscribe, placing the queue in transmit mode. If a queue name isn't provided, a random one is created.

Queues can be set as exclusive or auto-delete. Exclusive makes the queue private so that it can only be consumed by one consumer. Auto-delete automatically deletes the queue when the last consumer unsubscribes.

If an existing queue is declared, and all parameters match the existing queue, RabbitMQ considers the declaration successful and fail otherwise. A queue can be checked for existence by setting the `passive` parameter to true.

