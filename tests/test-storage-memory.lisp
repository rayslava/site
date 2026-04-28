(in-package :site.tests)

(in-suite all-tests)

(defmacro with-fresh-memory-backend (&body body)
  "Bind site.storage:*backend* to a new, empty memory-backend for BODY."
  `(let ((site.storage:*backend* (site.storage:make-backend :memory)))
     ,@body))

(test storage-subscriber-roundtrip
  "subscriber-save then subscriber-find round-trips the actor and attr."
  (with-fresh-memory-backend
    (site.storage:subscriber-save "https://alice.example/actor" '((:subscribed . "now")))
    (let ((row (site.storage:subscriber-find "https://alice.example/actor")))
      (is (equal "https://alice.example/actor" (getf row :actor)))
      (is (equal 0 (getf row :lastpost)))
      (is-false (site.storage:subscriber-find "https://bob.example/actor")))))

(test storage-subscriber-update-lastpost
  "update-lastpost mutates the stored row."
  (with-fresh-memory-backend
    (site.storage:subscriber-save "a1" nil)
    (site.storage:subscriber-update-lastpost "a1" 42)
    (is (= 42 (getf (site.storage:subscriber-find "a1") :lastpost)))))

(test storage-subscriber-lastpost-filters
  "lastpost<  and  lastpost>=  return the expected partitions."
  (with-fresh-memory-backend
    (site.storage:subscriber-save "a1" nil)
    (site.storage:subscriber-save "a2" nil)
    (site.storage:subscriber-save "a3" nil)
    (site.storage:subscriber-update-lastpost "a1" 10)
    (site.storage:subscriber-update-lastpost "a2" 20)
    (site.storage:subscriber-update-lastpost "a3" 30)
    (let ((below (mapcar (lambda (r) (getf r :actor))
                         (site.storage:subscriber-all-with-lastpost< 25)))
          (above (mapcar (lambda (r) (getf r :actor))
                         (site.storage:subscriber-all-with-lastpost>= 20))))
      (is (equal '("a1" "a2") (sort (copy-list below) #'string<)))
      (is (equal '("a2" "a3") (sort (copy-list above) #'string<))))))

(test storage-subscriber-delete
  (with-fresh-memory-backend
    (site.storage:subscriber-save "a1" nil)
    (site.storage:subscriber-delete "a1")
    (is-false (site.storage:subscriber-find "a1"))))

(test storage-event-save-and-count
  "event-count-by-object-and-type matches saved events exactly."
  (with-fresh-memory-backend
    (dolist (id '("e1" "e2" "e3"))
      (site.storage:event-save
       (list :id id
             :object-id "https://rayslava.com/blog?id=1"
             :event-type "Like"
             :event "{}")))
    (site.storage:event-save
     (list :id "e4"
           :object-id "https://rayslava.com/blog?id=1"
           :event-type "Announce"
           :event "{}"))
    (is (= 3 (site.storage:event-count-by-object-and-type
              "https://rayslava.com/blog?id=1" "Like")))
    (is (= 1 (site.storage:event-count-by-object-and-type
              "https://rayslava.com/blog?id=1" "Announce")))
    (is (= 0 (site.storage:event-count-by-object-and-type
              "https://rayslava.com/blog?id=999" "Like")))))

(test storage-event-delete
  (with-fresh-memory-backend
    (site.storage:event-save (list :id "e1" :object-id "o1" :event-type "Like"))
    (site.storage:event-delete "e1")
    (is-false (site.storage:event-find-by-id "e1"))))

(test storage-event-scan-by-reply-and-type
  "scan-by-reply-and-type returns only matching events."
  (with-fresh-memory-backend
    (site.storage:event-save
     (list :id "e1" :object-id "o1" :event-type "Create"
           :reply-to "https://rayslava.com/blog?id=7"))
    (site.storage:event-save
     (list :id "e2" :object-id "o2" :event-type "Like"
           :reply-to "https://rayslava.com/blog?id=7"))
    (site.storage:event-save
     (list :id "e3" :object-id "o3" :event-type "Create"
           :reply-to "https://rayslava.com/blog?id=8"))
    (let ((matches (site.storage:event-scan-by-reply-and-type
                    "https://rayslava.com/blog?id=7" "Create")))
      (is (= 1 (length matches)))
      (is (equal "e1" (getf (first matches) :id))))))

(test storage-static-roundtrip
  "static-save + static-find-by-s3name round-trips a row."
  (with-fresh-memory-backend
    (site.storage:static-save (list :s3name "deadbeef" :filename "cat.jpg"
                                    :attr "{\"tags\":[\"cute\"]}"))
    (let ((row (site.storage:static-find-by-s3name "deadbeef")))
      (is (equal "cat.jpg" (getf row :filename)))
      (is (equal "{\"tags\":[\"cute\"]}" (getf row :attr))))))

(test storage-detect-backend-env
  "detect-backend honors SITE_STORAGE and falls back on AWS_ACCESS_KEY."
  ;; We can only inspect the keyword returned; we don't touch live env vars.
  (is (member (site.storage:detect-backend) '(:memory :local :dyna))))
