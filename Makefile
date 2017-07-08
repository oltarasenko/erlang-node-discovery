PROJECT = erlang_node_discovery
PROJECT_DESCRIPTION = erlang node discovery application
PROJECT_VERSION = 0.1.0

SHELL_OPTS = -boot start_sasl -s erlang_node_discovery_app start -config config/devel.config

devel: all shell

include erlang.mk
