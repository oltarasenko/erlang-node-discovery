PROJECT = erlang-node-discovery
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

SHELL_OPTS = -config config/devel.config

devel: all shell

include erlang.mk
