PROJECT = erlang_node_discovery
PROJECT_DESCRIPTION = erlang node discovery application
PROJECT_VERSION = 0.1.3

ERLC_OPTS = +'{parse_transform, rewrite_logging}'
SHELL_OPTS = -boot start_sasl -s erlang_node_discovery_app start -config config/devel.config

BUILD_DEPS = rewrite_logging
dep_rewrite_logging = git https://github.com/dmzmk/rewrite_logging 0.1.0


compile: all


devel: all shell


include erlang.mk
