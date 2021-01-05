FROM sbcl
LABEL maintainer "Slava Barinov <rayslava@gmail.com>"
EXPOSE 4005 8080

RUN apt-get update && apt-get upgrade --yes
RUN apt-get install --yes git curl file
RUN git config --global user.email 'server@amazon.com' && git config --global user.name 'server'
RUN cd ~/quicklisp/local-projects && git clone https://github.com/rayslava/dyna.git
RUN sbcl --eval "(ql:update-client :prompt nil)" --eval "(ql:update-all-dists :prompt nil)"
RUN sbcl --eval "(ql:quickload :quicklisp-slime-helper :prompt nil)" --eval "(mapcar (lambda (pkg) (ql:quickload pkg :prompt nil)) '(:hunchentoot :cl-who :ht-simple-ajax :cl-css :local-time :dyna :zs3 :jonathan :trivial-mimes :ironclad))"
ADD start.lisp /root/start.lisp
ENTRYPOINT ["/usr/bin/sbcl", "--load", "/root/start.lisp"]
