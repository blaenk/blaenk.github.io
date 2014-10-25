---
title: Redis
published: April 19, 2014
excerpt: An in-memory data-structure server
comments: off
toc: left
---

[Redis] is a data structure server. It can be used to speed up operations that might be too simple for a heavy handed traditional database. It supports strings, lists, ordered and unordered sets, and hashes. This is meant to provide an overview of the data structures and commands available to Redis. The full list of commands is available in the [documentation].

[Redis]: http://redis.io
[documentation]: http://redis.io/commands

There's also a [guide on MDN] with suggestions on how to use Redis.

[guide on MDN]: https://developer.mozilla.org/en-US/docs/Mozilla/Redis_Tips

* toc

# Strings

Strings can store values of type byte string, integer (signed platform size), or floating-point (64-bit doubles). Strings support simple key-value store operations such as `GET`, `SET`, and `DEL`. The `DEL` command returns the number of items that were deleted.

Values can be incremented/decremented by 1 with `INCR`/`DECR` or by an arbitrary amount with `INCRBY`/`DECRBY`, or `INCRYBYFLOAT` for floats. If these are used on a key that doesn't exist or is an empty string, it's assumed to have a value of zero.

Strings also support string operations such as `APPEND` for concatenation, substring reading and writing with `GETRANGE` and `SETRANGE`.

Strings can also be manipulated as bitstring values with `GETBIT` and `SETBIT`, the population count can be retrieved with `BITCOUNT`, and bitwise operations can be performed with `BITOP`.

# Lists

Linked-lists support left/right push and pop with `LPUSH`/`RPUSH` and `LPOP`/`RPOP`. Specific elements can be retrieved with `LINDEX`, where -1 is the last index, and a range with `LRANGE`. Lists can be trimmed so that they only include the items in the range provided to `LTRIM`.

There are also potentially-blocking list manipulation commands that are often used for the development of messaging and task queues.

The operations `BLPOP` and `BRPOP` which pop the left/right-most item from the first non-empty list provided, blocking until one of the source lists is empty, or until the provided timeout in seconds [^select].

[^select]: This of course reminds me of the [`select`] system call used for synchronous I/O multiplexing.

[`select`]: http://man7.org/linux/man-pages/man2/select.2.html

The operation `RPOPLPUSH` pops the right-most item from the source and pushes it onto the left end of the destination, also returning the item to the user. Similarly, `BRPOPLPUSH` does the same but blocks until the source is not empty, or until the provided timeout in seconds.

# Sets

Sets can be added to and removed from with `SADD` and `SREM`. The `SPOP` command removes and returns a random item from the set. The `SMOVE` command can be used to move items between sets. Membership tests are possible with `SISMEMBER`. The number of items in a set (its cardinality) can be retrieved with `SCARD`.

All of the members from the set can be fetched with `SMEMBERS`. An arbitrary number of distinct random items can be retrieved with `SRANDMEMBER`, though if the arbitrary number is negative the items returned may not necessarily be distinct.

Set operations such as intersection, union, and difference are possible with `SINTER`, `SUNION`, and `SDIFF`. There are also `STORE`-suffixed variants to these set operations that store their results in a specified key instead of returning them.

# Ordered Sets

Ordered sets hold unique keys called members and associated values called scores that are limited to floating-point numbers (doubles) and are used to store the items in ascending order.

Members can be added and removed with `ZADD` and `ZREM`. The set's cardinality can be retrieved with `ZCARD`. More specifically, `ZCOUNT` can be used to count the number of items whose scores fall within the provided range.

The `ZINCRBY` command can be used to increment the score of a member by a specific amount. The `ZSCORE` command can be used to get an item's score. The `ZRANGEBYSCORE` command can be used to get the items whose scores fall within the provided range. It's also possible to remove items from the set based on whether or not they fall within a provided range of scores or ranks with `ZREMRANGEBYSCORE` and `ZREMRANGEBYRANK`.

The `ZRANK` command can be used to get the position of an item in the ascending order of the set. The `ZRANGE` command can be used to retrieve the items whose ranks fall within the provided range, with or without scores.

Given that the ordering of ordered sets is ascending, many commands have analogues that operate on the set as if it were in _descending_ order, such as `ZREVRANK` and `ZREVRANGE`.

There are set operations `ZINTERSTORE` and `ZUNIONSTORE` whose semantics differ from regular set operations due to the added complexity of score-handling. These commands can take as option an operation to perform to aggregate the scores of items with the same keys, the default of which is to add them but can also be to take the min or max. Another option that can be passed are weights that are used to multiply the scores of the corresponding set before passing them to the aggregation operation. If these ordered-set operations are used on a regular set, the scores of the items in that set are interpreted as being 1.

# Hashes

Hashes can be thought of as a separate namespace within Redis. They support setting, getting, and deleting values with `HSET`, `HGET`, and `HDEL`. There are `HMSET` and `HMGET` variants that allow for _multiple_ fields to be retrieved or key-value pairs to be set. The `HLEN` command yields the number of key-value pairs in the hash.

The `HGETALL` command can get all of the key-value pairs, though it's possible to get only one or the other with `HKEYS` and `HVALUES`. The existence of a key in a hash can be checked with `HEXISTS`.

`HINCRBY`/`HINCRBYFLOAT` can be used to increment a value by a specific amount.

# Publish/Subscribe {#publish-subscribe}

Redis supports publish/subscribe operations. There are straightforward commands `SUBSCRIBE`, `UNSUBSCRIBE`, and `PUBLISH`. There are also (un)subscribe commands that apply to channels that match a given pattern: `PSUBSCRIBE` and `PUNSUBSCRIBE`.

If a client gets disconnected and a message is sent before it can reconnect, it will never see the message. For this reason, it may be desirable to use a message broker such as [RabbitMQ]. The `client-output-buffer-limit` option is used to configure the maximum client buffer size.

[RabbitMQ]: http://www.rabbitmq.com/

# Sorting

Sorting can be performed on lists, sets, and ordered sets---even according to data in other structures.

``` python
conn.rpush('sort-input', 23, 15, 110, 7)

conn.hset('d-7', 'field', 5)
conn.hset('d-15', 'field', 1)
conn.hset('d-23', 'field', 9)
conn.hset('d-110', 'field', 3)

conn.sort('sort-input', by='d-*->field')
# ['15', '110', '7', '23']

conn.sort('sort-input', by='d-*->field', get='d-*->field')
# ['1', '3', '5', '9']
```

# Transactions

Simple transactions are possible in Redis by using the `MULTI` command to designate that the following commands are to be part of a transaction, followed by the commands themselves and `EXEC`, which prompts Redis to execute all of the commands sequentially. Transactions also help to reduce round trips between Redis and the client.

# Key Expiration

Keys can be deleted automatically by Redis after a certain time or at a specific time by using key expiration. The `EXPIRE` command can be used to make a key expire in a given number of seconds, while `EXPIREAT` allows a specific time to be specified. The `TTL` command gets the amount of time remaining before expiration. The `PERSIST` command removes the expiration from a key.

There are `P`-prefixed variants such as `PTTL`, `PEXPIRE`, and `PEXPIREAT` which operate at the millisecond level.

