# Mob

Mob aim to be a distributed, decentralized, fault-tolerant and self-organized network that enable the deployment and scaling of a set of services.

# What is the state of the project

At the time the project is intended as a proof of concept, developed during an internship. It is **not** production-ready and it is still in [development](https://github.com/mobos/mob/issues).

The rest of this README will introduce the main architecture and components of Mob.

# Architecture

## Mob Network

![mob network](/docs/mob-network.png?raw=true)

It consists of nodes that connected to each other build a **peer-to-peer network**. The network has the task to **decentralize** information, to make system **fault-tolerant** and to mantain all the services deployed in it.

## Mob Node Overview

![mob node overview](/docs/mob-node-overview.png?raw=true)

Each node consists of three main components:

### Interface

The interface of a node allows us to communicate with the entire network in order to perform queries or commands, such as the retrieval of all services and its statuses or deployment of a new or updated service. The interface component is purely optional and it is intended to be used for control purposes.

### Peer

Here is the place where portion of the network's information is stored and maintained in a distributed and fault-torelant fashion.

### Supervisor

The main task of this component is to handle all the deployed services by maintaining the life cycle of each one and satisfying constraints like restart policies or dependency with other services.

# The code

*How to run tests*

```
$ rebar3 eunit
```

*How to run tests for a single module*

```
$ rebar3 eunit --module [module_name]
```

Example:

```
$ rebar3 eunit --module bash
```
