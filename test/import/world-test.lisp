;;;; world-test.lisp --- the manifest table is data, and the probe is a stat.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The table lives in the core (NLK:*AGENT-WORLDS*, import/worlds.lisp) so
;;;; the first frame can ask what is on the box before this folder loads.
;;;; These gates hold it to what makes that safe: rows that carry nothing
;;;; but data, and a probe that opens no file.

(in-package #:nodecode.test)

(nlk:access (world nlk::agent-world))

(deftest import-world-manifest-carries-data-and-nothing-else ()
  ;; The moment a row carries a function this table has turned back into a
  ;; reader per world, which is the design this folder exists to avoid.
  (is (plusp (length nlk:*agent-worlds*)) "there are worlds to read")
  (dolist (world nlk:*agent-worlds*)
    (let ((name world.name))
      (is (and (stringp name) (plusp (length name))) "every row is named")
      (is (and (stringp world.label) (plusp (length world.label)))
          (format nil "~a carries a label" name))
      (is (nlk:agent-world-roots world) (format nil "~a says where it lives" name))
      (is (nlk:agent-world-evidence world) (format nil "~a says what proves it" name))
      (dolist (value (append world.roots
                             world.evidence
                             world.skip
                             world.enter
                             world.retire
                             (list world.last-used world.takeover-unit)))
        (is (or (null value) (stringp value))
            (format nil "~a carries only strings, never code" name)))))
  (is (= (length nlk:*agent-worlds*)
         (length (remove-duplicates (nlk:agent-world-names) :test #'string=))))
  (is (equal "hermes" (nlk:agent-world-name (nlk:find-agent-world "HERMES"))))
  (is (null (nlk:find-agent-world "not-a-world"))))

(deftest import-world-detection-is-a-stat-over-a-box (with-import-runtime (home box))
  ;; One PROBE-FILE per evidence path and one FILE-WRITE-DATE per world
  ;; found: no file is opened, which is what makes it safe on a first frame.
  (is (null (nlk:detect-agent-worlds)) "an empty box carries no past")
  (make-commandcode-fixture box)
  (make-openclaw-fixture box)
  (let ((found (nlk:detect-agent-worlds)))
    (is (equal '("openclaw" "commandcode")
               (sort (mapcar (lambda (row) (nlk:agent-world-name (nlk:detected-world-world row)))
                             found)
                     #'string>)))
    (dolist (row found)
      (is (probe-file (nlk:detected-world-root row)) "each names a root that exists")
      (is (integerp (nlk:detected-world-last-used row))))))

(deftest import-world-detection-orders-by-last-used
    (with-import-runtime (home box :commandcode :openclaw))
  ;; Touch Command Code's dated file so it is the newest world.
  (sleep 1.1)
  (write-temp-file (merge-pathnames ".commandcode/auth.json" box))
  (is (equal "commandcode"
             (nlk:agent-world-name
              (nlk:detected-world-world (first (nlk:detect-agent-worlds)))))))
