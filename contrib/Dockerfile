FROM sbcl
LABEL maintainer "Slava Barinov <rayslava@gmail.com>"
EXPOSE 4005 8080

RUN apt-get install --yes git curl
ADD ../site.asd /tmp/site.asd
RUN sbcl --eval "(ql:update-client)" --eval "(ql:update-all-dists)" --eval "(ql:quickload :quicklisp-slime-helper)"
RUN sbcl --load "/tmp/site.asd" --eval "(ql:quickload :site)" || true
ADD start.lisp /root/start.lisp
ENTRYPOINT ["/usr/bin/sbcl", "--load", "/root/start.lisp"]
