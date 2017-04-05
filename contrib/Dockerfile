FROM sbcl
LABEL maintainer "Slava Barinov <rayslava@gmail.com>"
EXPOSE 4005 8080

RUN apt-get install --yes git curl
RUN echo "\n\n\n\n\n" | sbcl --eval "(ql:update-client)" --eval "(ql:update-all-dists)"
RUN sbcl --eval "(ql:quickload :quicklisp-slime-helper)" --eval "(mapcar #'ql:quickload '(:hunchentoot :cl-who :ht-simple-ajax :cl-css :local-time :dyna :zs3 :jonathan :trivial-mimes :ironclad))"
ADD start.lisp /root/start.lisp
ENTRYPOINT ["/usr/bin/sbcl", "--load", "/root/start.lisp"]
