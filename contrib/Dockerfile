FROM fukamachi/sbcl:2.5.1-ubuntu
LABEL maintainer "Slava Barinov <rayslava@gmail.com>"
EXPOSE 4005 8080

RUN apt update && apt upgrade --yes
RUN apt install -y git curl file
RUN apt clean
RUN git config --global user.email 'server@amazon.com' && git config --global user.name 'server'
RUN sbcl --eval "(ql:update-client :prompt nil)" --eval "(ql:update-all-dists :prompt nil)"
RUN mkdir -p ~/.roswell/lisp/quicklisp/local-projects && cd ~/.roswell/lisp/quicklisp/local-projects && git clone https://github.com/rayslava/dyna.git
RUN sbcl --eval "(ql:quickload :quicklisp-slime-helper :prompt nil)" --eval "(mapcar (lambda (pkg) (ql:quickload pkg :prompt nil)) '(:hunchentoot :cl-who :ht-simple-ajax :cl-css :cl-json :local-time :dyna :zs3 :jonathan :trivial-mimes :ironclad :asn1 :trivia :cl-base64 :uuid :quri :cl-ppcre :cl-json-helper))"
ADD start.lisp /root/start.lisp
ENTRYPOINT ["/usr/local/bin/sbcl", "--load", "/root/start.lisp"]
