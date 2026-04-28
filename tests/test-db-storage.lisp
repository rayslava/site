(in-package :site.tests)

(in-suite all-tests)

(test db-storage-compute-s3-name-is-md5
  "compute-s3-name returns the 32-char hex MD5 of the input filename."
  (let ((hash (site.db-storage:compute-s3-name "cat.jpg")))
    (is (stringp hash))
    (is (= 32 (length hash)))
    ;; Deterministic for the same input.
    (is (string= hash (site.db-storage:compute-s3-name "cat.jpg")))
    (is (string/= hash (site.db-storage:compute-s3-name "dog.jpg")))))

(test db-storage-list-empty-under-memory-backend
  "A fresh memory backend exposes no registered static files."
  (with-memory-storage
    (is (null (site.db-storage:list-available-statics)))))

(test db-storage-upload-then-list-then-delete
  "upload-file -> list-available-statics -> delete-static round-trips
through the memory backend for metadata and blob storage."
  (with-memory-storage
    (uiop:with-temporary-file (:pathname tmp-path :stream s :keep t)
      (format s "hello, static world")
      (finish-output s)
      (site.db-storage:upload-file
       tmp-path
       (list :filename "greeting.txt" :tags '("demo")))
      ;; Metadata row is present with the right filename.
      (let ((rows (site.db-storage:list-available-statics)))
        (is (= 1 (length rows)))
        (is (equal "greeting.txt" (getf (first rows) :filename))))
      ;; Lookup by s3name works.
      (let ((row (site.storage:static-find-by-s3name
                  (site.db-storage:compute-s3-name "greeting.txt"))))
        (is-true row)
        (is (equal "greeting.txt" (getf row :filename))))
      ;; Deleting removes both metadata and the blob entry.
      (site.db-storage:delete-static
       (site.db-storage:compute-s3-name "greeting.txt"))
      (is (null (site.db-storage:list-available-statics))))))

(test db-storage-ensure-memory-noop
  "ensure-static-storage! is safe to call on a memory backend."
  (with-memory-storage
    (finishes (site.db-storage:ensure-static-storage!))
    (finishes (site.db-storage:ensure-static-storage!))))

(test db-storage-upload-local-backend-writes-file
  "The local backend copies the uploaded file onto disk under its static-dir."
  (let* ((tmpdir (merge-pathnames "site-local-upload/"
                                  (uiop:ensure-directory-pathname
                                   (uiop:temporary-directory))))
         (site.storage:*backend*
          (site.storage:make-backend :local :static-dir tmpdir)))
    (unwind-protect
         (uiop:with-temporary-file (:pathname tmp-path :stream s :keep t)
           (format s "local payload")
           (finish-output s)
           (site.db-storage:upload-file
            tmp-path
            (list :filename "payload.bin")))
      (let ((dest (merge-pathnames
                   (site.db-storage:compute-s3-name "payload.bin")
                   tmpdir)))
        (is-true (probe-file dest))
        (is (equal "local payload" (alexandria:read-file-into-string dest))))
      (uiop:delete-directory-tree tmpdir :validate t :if-does-not-exist :ignore))))

(test db-storage-public-url-memory-vs-local
  "Memory backend yields memory:// URL; local backend yields /uploads/... ."
  (with-memory-storage
    (is (search "memory://" (site.storage:static-public-url "abc"))))
  (let ((site.storage:*backend* (site.storage:make-backend :local)))
    (is (search "/uploads/" (site.storage:static-public-url "abc")))))
