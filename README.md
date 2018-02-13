# Erlang node discovery #

Allows to organize Erlang/Elixir node discovery using the information
about nodes provided in config. Useful in cases when your Erlang/Elixir nodes can be started/re-started
on different hosts, as it happens in Mesos.


## Basic configuration ##

```
[
    {erlang_node_discovery, [
        {db_callback, erlang_node_discovery_db},
        % List of hosts where apps can be started
        {hosts, ["host1.local", "host2.local", "host3.local"]},

        % List of nodenames and their ports
        {node_ports, [
            {app1, 17012},
            {app2, 17013},
            {app3, 17113}
        ]}
    ]}
].
```


## Using the application with EPMDLESS ##

It might be useful for cases when you want to organize a service discovery and don't want to relay on
standard distribution protocol. See more details about EPMDLESS here: https://github.com/oltarasenko/epmdless



```
{ erlang_node_discovery, [
    {db_callback, epmdless_dist},
    {hosts, ["host1.local", "host2.local"]},
    {node_ports, [
        {'app1', 17012},
        {'app2', 17013},
        {'app3', 17015}
    ]},
    {cookie, app_cookie}
]}
  ```