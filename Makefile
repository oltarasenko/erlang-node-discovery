PROJECT = erlang_node_discovery
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = lager recon egraylog
dep_lager = git git://github.com/basho/lager.git 3.0.1
dep_recon = git git://github.com/ferd/recon.git 2.2.0
dep_egraylog = git git://github.com/scrapinghub/egraylog.git 0.2.3

SHELL_OPTS = +K true -boot start_sasl -s erlang_node_discovery_app start -config config/devel.config

devel: all shell

include erlang.mk
