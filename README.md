# Surrogate

Surrogate is a forward and reverse proxy server.

Developed using ErlIDE 0.8.1 with Eclipse Helios (3.6.0) using standard Erlang R13B
package included in Ubuntu 10.04.

Surrogate is being upgraded to an erlang/elixir project built using mix with Elixir 1.11.1
and Erlang 23. Further upgrades will be done once use of the now deprecated gen_fsm is replaced.

Surrogate is released under the Apache license.  See NOTICE for details.

Forward proxy modes:
http proxy with CONNECT support for SSL proxy.
transparent HTTP proxy mode (no configuration changes required)
SOCKS proxy supporting version 4 and version 5.

Reverse proxy modes:
HTTP load balancer
HTTPS load balancer

read conf/proxy.conf for configuration examples.  Please contact skruger at fastinfra.com with questions.
