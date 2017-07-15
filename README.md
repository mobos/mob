# Mob [![Build Status](https://travis-ci.org/mobos/mob.svg?branch=master)](https://travis-ci.org/mobos/mob)

Mob aim to be a distributed, decentralized, fault-tolerant and self-organized network that enable the deployment of heterogenous services.

# State of the project

Currently the project is intended as a proof of concept, developed during an internship. It is **not** production-ready and it is still in [development](https://github.com/mobos/mob/issues).

The README will introduce Mob architecture and main components.

# Mob Architecture

## Network

![mob network](/docs/mob-network.png?raw=true)

It consists of nodes which connected to each other build a **peer-to-peer network**. The network has the task to **decentralize** information, to make system **fault-tolerant** and to mantain all information about services deployed in it.

## Node Overview

From the perspective of peer-to-peer paradigm, a node is the main and unique component with which the network is built.

![mob node overview](/docs/mob-node-overview.png?raw=true)

Each node consists of four main responsabilities:

### Interface

The interface component is *purely optional* and it is intended to be used for control purposes. It allows us to communicate with the entire network in order to perform queries or commands, such as
the retrieval of all services and their status, the deployment of a new or updated service, and so on.

### Peer

It is responsible to mantain portion of the network's information in a distributed and fault-torelant fashion.

### Supervisor

The main task of this component is to handle all the deployed services by maintaining the life cycle of each one, satisfying constraints like restart policies or dependencies with other services.

### Providers

A provider defines a task type *(e.g. Bash, Docker, rkt, Xen, KVM, GlusterFS, lambda, and more)* that can be deployed in the network. Your network capabilities can be easily extended by enabling or implementing a new provider.

Each node in the network can be enabled to support different providers, by making it able to handle a task of such type.

# Getting started

## Requirements

* [Erlang/OTP 18](http://erlang.org/)
* [Rebar3](http://www.rebar3.org/)

## Run tests

```
$ rebar3 as prod compile
$ rebar3 eunit --dir test/
$ rebar3 eunit --dir test/it
```

*How to run tests for a single module*

```
$ rebar3 eunit --module [module_name]
```

Example:

```
$ rebar3 eunit --module bash
```

## How to create a Mob network

Create a release

```
$ rebar3 release
```

Start a node

```
$ mob -p [PORT_NUMBER] -P [LIST_OF_PROVIDER_NAME]
```

Example:

```
$ ./mob -p 6666 -P docker,bash
> ...
```
*It will start an Erlang shell with a running mob node*

## Add a new node in the network

```
$ ./mob -p 7777 -P bash
> ...
```

**Join the new node in the network**

```
$ ./mobcli -p 6666 -j 127.0.0.1:7777
```

## Deploy a service

```
./mobcli -p 6666 -d '"
                  {
                      \'name\': \'docker-echoes\',
                      \'provider\': \'docker\',
                      \'params\': {
                           \'image\': \'alpine:edge\',
                           \'args\': \'/bin/sh -c "while [ true ]; do echo ciao; sleep 5; done"\'
                      }
                  }"'

```
