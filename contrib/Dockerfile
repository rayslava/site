FROM sbcl
LABEL maintainer "Slava Barinov <rayslava@gmail.com>"
EXPOSE 4005 8080

RUN apt-get install --yes git curl
ADD start.lisp /root/start.lisp
ENTRYPOINT ["/usr/bin/sbcl", "--load", "/root/start.lisp"]
