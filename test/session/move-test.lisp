;;;; move-test.lisp --- a session follows its folder when that moves.

(in-package #:nodecode.test)

(defun moved-session-folder (id) "ID's folder as FIND-SESSION reads it, its closing slash dropped."
  (string-right-trim "/" (nlk:durable-session-cwd (nlk:find-session id))))

(defun moved-folder (root &rest names) "The folder NAMES under ROOT, as a session records it: no closing slash."
  (string-right-trim "/" (namestring (merge-pathnames (format nil "~{~a/~}" names) root))))

(deftest move-sessions-moves-a-folder-and-every-folder-inside-it (with-temp-store ())
  (with-temp-workspace (root "old/keep" "old/sub/keep" "new/keep" "new/sub/keep" "other/keep")
    (nlk:create-session :id "mv-room" :cwd (concatenate 'string (moved-folder root "old") "/"))
    (nlk:create-session :id "mv-deep" :cwd (moved-folder root "old" "sub"))
    (nlk:create-session :id "mv-else" :cwd (moved-folder root "other"))
    (nlk:create-session :id "mv-sibling" :cwd (moved-folder root "old2"))
    (let ((moves (nlk:move-sessions (moved-folder root "old") (moved-folder root "new"))))
      (is (equal '("mv-room" "mv-deep") (mapcar #'first moves)) "the folder and the one inside it, in the order they were created")
      (is (equal (moved-folder root "new") (moved-session-folder "mv-room")) "the room reads its new folder")
      (is (equal (moved-folder root "new" "sub") (moved-session-folder "mv-deep")) "a folder inside keeps its place")
      (is (equal (moved-folder root "other") (moved-session-folder "mv-else")) "another folder stays")
      (is (equal (moved-folder root "old2") (moved-session-folder "mv-sibling")) "a folder whose name only starts the same stays")
      (let ((facts (nlk:page-events :session-id "mv-room")))
        (is (equal (concatenate 'string (moved-folder root "old") "/")
                   (nlk:session-created-cwd (first facts))) "the created fact is history, never rewritten")
        (is-present (moved (find "session.moved" facts :key #'nlk:event-kind :test #'string=)) "the move is a fact of its own"
          (is (equal (moved-folder root "new") (nlk:session-moved-cwd moved)))
          (is (equal (concatenate 'string (moved-folder root "old") "/") (nlk:session-moved-from moved)) "it names the folder it left")))
      (nlk:standby-session :id "mv-lane" :parent "mv-room")
      (is (equal (moved-folder root "new") (string-right-trim "/" (nlk:standby-record-cwd (nlk:standby-session-record "mv-lane")))) "a lane opened after the move works where its room does now")
      (flet ((listed (id)
               (getf (find id (nlk:list-sessions) :key (lambda (row) (getf row :id)) :test #'equal)
                     :working-directory)))
        (is (equal (moved-folder root "new") (listed "mv-room")) "the session directory follows the move")
        (let ((kept (nlk:list-sessions)))
          (sqlite:execute-non-query (nlk::require-open-store) "delete from session_directory")
          (nlk::apply-schema (nlk::require-open-store))
          (is (equal kept (nlk:list-sessions)) "the fill derives the moved folder from the log")))
      (is (null (nlk:move-sessions (moved-folder root "old") (moved-folder root "new"))) "nothing is left at the old folder to move"))))

(deftest move-sessions-writes-nothing-when-a-destination-is-missing (with-temp-store ())
  (with-temp-workspace (root "old/keep" "old/sub/keep" "new/keep")
    (nlk:create-session :id "mv-top" :cwd (moved-folder root "old"))
    (nlk:create-session :id "mv-inner" :cwd (moved-folder root "old" "sub"))
    (is (signals-error nlk:missing-session-folder
          (nlk:move-sessions (moved-folder root "old") (moved-folder root "absent"))) "no folder at the destination")
    (is (signals-error nlk:missing-session-folder
          (nlk:move-sessions (moved-folder root "old") (moved-folder root "new"))) "new/sub is not there for the inner session")
    (is (equal (list (moved-folder root "old") (moved-folder root "old" "sub"))
               (list (moved-session-folder "mv-top") (moved-session-folder "mv-inner"))) "a refused move moves neither")
    (is (equal '("mv-top")
               (mapcar #'first (nlk:move-sessions (moved-folder root "old") (moved-folder root "new")
                                                  :sessions '("mv-top")))) "SESSIONS moves only the ones it names")
    (is (equal (list (moved-folder root "new") (moved-folder root "old" "sub"))
               (list (moved-session-folder "mv-top") (moved-session-folder "mv-inner"))))))

(deftest sh-in-a-session-whose-folder-is-gone-names-the-move (with-temp-store ())
  (with-temp-workspace (root "here/keep")
    (nlk:create-session :id "mv-gone" :cwd (moved-folder root "gone"))
    (let ((nle::*current-durable-turn*
            (nlk::%make-durable-turn :session-id "mv-gone" :turn-id "mv-turn"
                                     :command-id "c" :definition-revision "test")))
      (let ((message (handler-case (progn (nle::sh "true") "no refusal")
                       (error (condition) (princ-to-string condition)))))
        (is (search (moved-folder root "gone") message) "the refusal names the folder")
        (is (search "nlk:move-sessions" message) "and the move that fixes it"))
      (is (search (moved-folder root "here") (nle::sh "pwd" :directory (moved-folder root "here"))) ":directory still runs a command elsewhere")
      (nlk:move-sessions (moved-folder root "gone") (moved-folder root "here"))
      (is (search (moved-folder root "here") (nle::sh "pwd")) "after the move the session runs in its folder"))))
