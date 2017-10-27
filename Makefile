PROJECT = minirest
PROJECT_DESCRIPTION = A Mini RESTful API Framework
PROJECT_VERSION = 0.1

DEPS = jsx mochiweb

dep_jsx      = git https://github.com/talentdeficit/jsx
dep_mochiweb = git https://github.com/emqtt/mochiweb master

ERLC_OPTS += +debug_info

EUNIT_OPTS = verbose

TEST_ERLC_OPTS += +debug_info

CT_SUITES = minirest

COVER = true

include erlang.mk
