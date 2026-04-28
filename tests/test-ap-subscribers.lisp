(in-package :site.tests)

(in-suite all-tests)

(test activitypub-unsubscribe-removes-subscriber
  "Calling unsubscribe with a subscriber instance deletes its row."
  (with-memory-storage
    (site.storage:subscriber-save "https://alice.example/actor"
                                  '((:subscribed . "when")))
    (is-true (site.storage:subscriber-find "https://alice.example/actor"))
    (let ((sub (make-instance 'site.activitypub::activitypub-subscriber
                              :actor "https://alice.example/actor")))
      (site.activitypub::unsubscribe sub))
    (is-false (site.storage:subscriber-find "https://alice.example/actor"))))

(test activitypub-update-lastpost-mutates-row
  "update-lastpost rewrites the stored lastpost column."
  (with-memory-storage
    (site.storage:subscriber-save "https://bob.example/actor" nil)
    (let ((sub (make-instance 'site.activitypub::activitypub-subscriber
                              :actor "https://bob.example/actor")))
      (site.activitypub::update-lastpost sub 1234567890))
    (is (= 1234567890
           (getf (site.storage:subscriber-find "https://bob.example/actor")
                 :lastpost)))))

(test activitypub-maybe-deliver-new-posts-empty-subscribers
  "With no subscribers, delivery is a no-op (zero HTTP calls) and returns."
  (with-memory-storage
    ;; No subscribers, any number of fedi-tagged posts.
    (let ((posts (list (make-instance 'site.blog-post:blog-post
                                      :id 3700000000
                                      :subject "fedi post"
                                      :tags '("en" "fedi")
                                      :post (lambda () "<p>body</p>")))))
      ;; Must not raise and must not call dex:post (no subscribers).
      (finishes (site.activitypub:maybe-deliver-new-posts posts)))))

(test activitypub-maybe-deliver-new-posts-ignores-non-fedi
  "Posts without the fedi tag are filtered out before delivery attempts."
  (with-memory-storage
    ;; Even with a subscriber present, a post without :fedi tag must not
    ;; produce a delivery attempt. If the filter broke, send-signed would
    ;; try dex:post and the test would fail with a network error.
    (site.storage:subscriber-save "https://x.example/actor" nil)
    (let ((posts (list (make-instance 'site.blog-post:blog-post
                                      :id 3700000001
                                      :subject "plain post"
                                      :tags '("en")
                                      :post (lambda () "<p>x</p>")))))
      (finishes (site.activitypub:maybe-deliver-new-posts posts)))))

(test activitypub-maybe-update-post-no-subscribers
  "maybe-update-post with no subscribers returns 0."
  (with-memory-storage
    (let ((post (make-instance 'site.blog-post:blog-post
                               :id 3700000002
                               :subject "up"
                               :tags '("en" "fedi")
                               :post (lambda () "<p>x</p>"))))
      (is (= 0 (site.activitypub::maybe-update-post post))))))

(test activitypub-subscriber-save-is-upsert
  "subscriber-save on an existing actor replaces the attr but preserves
lastpost — this is what the Follow-rebinds-attr flow relies on."
  (with-memory-storage
    (site.storage:subscriber-save "a" '((:subscribed . "first")))
    (site.storage:subscriber-update-lastpost "a" 42)
    (site.storage:subscriber-save "a" '((:subscribed . "second")))
    (let ((row (site.storage:subscriber-find "a")))
      (is (equal '((:subscribed . "second")) (getf row :attr)))
      (is (= 42 (getf row :lastpost))))))
