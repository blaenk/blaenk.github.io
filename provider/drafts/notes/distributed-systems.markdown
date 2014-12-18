---
title: Distributed Systems
published: September 15, 2014
excerpt: Solving problems with multiple computers
comments: off
toc: left
---

I'm using [Distributed Systems for Fun and Profit] for an introduction to the field of distributed systems.

[Distributed Systems for Fun and Profit]: http://book.mixu.net/distsys/

_Scalability_ is the ability of a system, network, or process to handle a increasing work in a capable manner. _Availability_ is the proportion of a system is in functioning condition. _Fault tolerance_ is the ability to behave in a well-defined manner when faults occur.

Distributed systems are constrained by the number of nodes and the distance between them. Varying these quantities has an effect on the distributed system. Increasing the number of nodes increases the probability of failure in the system (reducing availability) and may increase the need for communication between them (reducing performance). Increasing the distance between the nodes increases the minimum latency.

Abstractions and models are often used to facilitate the conceptualization and design of distributed systems by removing certain real-world aspects that aren't relevant to solving the problem. The types of models may include system models, failure models, and consistency models.

The distribution of the necessary data can affect perfromance and fault tolerance. _Partitioning_ refers to splitting the data over multiple nodes to increase parallel processing. _Replication_ refers to copying or caching data on multiple nodes to reduce the distance between the client and the server, which also increasing fault tolerance.
