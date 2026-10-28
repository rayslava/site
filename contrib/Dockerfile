# syntax=docker/dockerfile:1
#
# Multi-stage build producing a standalone SBCL executable.
#   deps    - quicklisp dependencies, cached until site.asd changes
#   jscl    - jscl.js bootstrap, cached until JSCL_REF changes
#   test    - runs the test suite (docker build --target test .)
#   build   - compiles the site and saves the executable
#   repo    - the site's git repository, for the admin pull/refresh actions
#   runtime - slim image with the executable and static files
#
# Build from the repository root: docker build -f contrib/Dockerfile .
# Pass the git directory as the "gitdir" context to include the repository:
#   docker build -f contrib/Dockerfile --build-context gitdir=.git .

ARG SBCL_IMAGE=fukamachi/sbcl:2.6.7-ubuntu
ARG RUNTIME_IMAGE=ubuntu:24.04
ARG QL_DIST=2026-01-01
ARG JSCL_REF=80c0cd6c27a951896efc45819766873a6dc4483e
ARG DYNA_REF=151c453e071d4fa5667b6bdb5085e01d8d2b5f27

FROM ${SBCL_IMAGE} AS base
RUN apt-get update \
    && apt-get install -y --no-install-recommends git ca-certificates libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Empty unless overridden with the "gitdir" build context
FROM scratch AS gitdir

FROM base AS repo
RUN --mount=from=gitdir,target=/gitdir \
    if [ -f /gitdir/HEAD ]; then \
        cp -R /gitdir /opt/site.git \
        && git config --file /opt/site.git/config --unset-all core.worktree; \
        git config --file /opt/site.git/config core.bare true \
        && git --git-dir=/opt/site.git branch -f image HEAD; \
    else \
        mkdir /opt/site.git; \
    fi

FROM base AS jscl
ARG JSCL_REF
ADD --keep-git-dir=true https://github.com/jscl-project/jscl.git#${JSCL_REF} /jscl
RUN cd /jscl && sbcl --non-interactive --load jscl.lisp --eval '(jscl:bootstrap)'

FROM base AS deps
ARG QL_DIST
ARG DYNA_REF
ADD https://github.com/rayslava/dyna.git#${DYNA_REF} /root/.roswell/lisp/quicklisp/local-projects/dyna
RUN sbcl --non-interactive \
    --eval "(ql-dist:install-dist \"http://beta.quicklisp.org/dist/quicklisp/${QL_DIST}/distinfo.txt\" :prompt nil :replace t)"
COPY site.asd /build/site.asd
WORKDIR /build
RUN sbcl --non-interactive \
    --eval '(asdf:load-asd #p"/build/site.asd")' \
    --eval '(ql:quickload (append (asdf:system-depends-on (asdf:find-system :site)) (list :fiveam)))'

FROM deps AS source
COPY . /build

FROM source AS test
# file(1) detects MIME types of uploads
RUN apt-get update && apt-get install -y --no-install-recommends file && rm -rf /var/lib/apt/lists/*
RUN unset AWS_ACCESS_KEY AWS_SECRET_KEY; sbcl --non-interactive \
    --load site.asd \
    --eval '(ql:quickload :site/tests)' \
    --eval '(unless (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :all-tests :site.tests)) (uiop:quit 1))'

FROM source AS build
RUN sbcl --non-interactive \
    --load site.asd \
    --eval '(ql:quickload :site)' \
    --eval '(sb-ext:save-lisp-and-die "/build/site-server" :executable t :toplevel (function site.main:main) :save-runtime-options nil)'

FROM ${RUNTIME_IMAGE} AS runtime
RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates libssl3t64 libzstd1 zlib1g curl file git \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --system --uid 101 --user-group --home-dir /site/var site \
    && mkdir -p /site/var && chown site:site /site/var \
    && git config --system --add safe.directory /opt/site.git
WORKDIR /site
COPY --from=repo /opt/site.git /opt/site.git
COPY --from=build /build/site-server /usr/local/bin/site-server
COPY contrib/entrypoint.sh /usr/local/bin/site-entrypoint
COPY static /site/static
COPY --from=jscl /jscl/dist/jscl.js /site/static/jscl.js
# uid 101 is nginx's worker user, so the admin refresh action can delete
# nginx's cache files when the cache volume is mounted (NGINX_CACHE)
USER site
# /site/var is meant to be a volume: the git checkout for the admin
# pull/refresh actions and its compiled fasls
ENV SITE_PORT=8080
EXPOSE 8080
HEALTHCHECK --interval=15s --timeout=5s --start-period=60s --start-interval=1s --retries=3 \
    CMD curl -fsS -o /dev/null http://localhost:8080/health || exit 1
ENTRYPOINT ["/usr/local/bin/site-entrypoint"]
