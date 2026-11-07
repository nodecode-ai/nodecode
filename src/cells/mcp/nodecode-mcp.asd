;;;; nodecode-mcp.asd --- MCP servers as Lisp functions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional cell, NOT part of the organism core: nothing in src/src names
;;;; this system. The folder loader (kernel cells.lisp) loads it at boot when
;;;; its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which connects every server named under
;;;; mcp.servers in ~/.nodecode/config.jsonc — the same contract the retired
;;;; Zig client read — and registers the /mcp slash command.
;;;;
;;;; Mono-tool: no provider tool is registered. Every remote tool becomes a
;;;; Lisp function in the MCP package, (mcp:files/read-file :path "x"),
;;;; generated from the server's tools/list with a real &key lambda list and
;;;; the description as its docstring, over one chokepoint (mcp:call ...).
;;;; A <harness key="mcp"> block carrying the live catalog teaches the model
;;;; the vocabulary in every session; it is injected live, never stored, so
;;;; removing the folder leaves nothing behind.
;;;;
;;;; Two transports, as the Zig client had: stdio (a child process, newline
;;;; delimited JSON-RPC) and streamable HTTP (one POST per message, JSON or
;;;; SSE reply). No reader thread: every read happens on the calling thread
;;;; under that call's own deadline, which is also the whole cancellation
;;;; story — an eval thread is never interrupted by Esc, only by its deadline
;;;; or (eval-interrupt).
;;;;
;;;; Every dependency named below is already in the serving image (a
;;;; transitive dependency of nodecode): LOAD-CELLS freezes the loaded
;;;; systems immutable and then loads this one at boot.
;;;;
;;;; The .asd sits INSIDE its folder; presence is enabled. The
;;;; `just mcp-*` recipes register this directory with ASDF the same way.

(defsystem "nodecode-mcp"
  :description "Tools from MCP servers you list in the config"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "shasht" "dexador" "flexi-streams" "bordeaux-threads")
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "rpc")
               (:file "stdio")
               (:file "http")
               (:file "registry")
               (:file "surface")
               (:file "wrappers")
               (:file "primer")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mcp/test"))))

(defsystem "nodecode-mcp/test"
  :description "MCP cell tests. Registered into the shared nodecode.test registry; RUN-MCP-TESTS filters by the MCP-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-mcp" "nodecode/test" "clack" "clack-handler-hunchentoot")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "config-test")
               (:file "rpc-test")
               (:file "stdio-test")
               (:file "http-test")
               (:file "surface-test")
               (:file "wrappers-test")
               (:file "primer-test")
               (:file "cell-test")
               (:file "route-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mcp-tests)))
