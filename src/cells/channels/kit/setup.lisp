;;;; setup.lisp --- the setup primer: the conversation is the wizard.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every cell teaches the model its runtime vocabulary through the manual;
;;;; this teaches setup the same way, and only while there is setup to do. An adapter folder is present but its lane is not running -- no
;;;; section, or a refused one -- so the model, asked "set up discord", knows
;;;; the section's keys, the probe that turns names into ids, the write
;;;; (CONFIG-SET) and the restart (RESTART-CELLS), and the portal walk the
;;;; operator has to do themselves. hermes-agent and openclaw write the walk
;;;; into their setup wizards; here the adapter writes it once, as its
;;;; section declaration (NLK:DEFINE-SECTION), and the setup wizard's panel
;;;; and this primer read the same declaration.
;;;;
;;;; Delivery is (help :channels), set at start while *UNCONFIGURED* names an
;;;; adapter: every request's help section carries one line naming them, and
;;;; the text is read when asked. Both are pure functions of which adapters
;;;; need setup, so between restarts the prompt prefix is byte-identical and
;;;; the provider's cache holds. The rules at the end are
;;;; rules, not gates: full authority is the design (no approval chain), so
;;;; what openclaw refuses structurally this says plainly, and the durable
;;;; turn.tool_result row is the audit.

(in-package #:nodecode-channel-kit)

(defparameter +setup-summary+
  "channel adapters installed but not running (~{~a~^, ~}): how to set one up from this conversation"
  "The help section's line for (help :channels), a format string over the
adapters that need setup.")

(defparameter +where-hook-key+ "nodecode-channel-kit-where"
  "The live-section hook key the lane's `where' sentence rides under.")

;;; Data: an operator's layer can reword it.
(defparameter +channel-setup-primer+
  "Channel setup, from this conversation. A channel adapter folder is installed but its lane is not running; (cells) says which and why (unconfigured: no channels.<id> section yet; refused: the section is there but refuses to start, with the reason). To bring one up:
1. Ask the operator which environment variable holds the bot token (or which file). Never ask for the token itself and never write a token into the config: the section names the variable - bot_token_env for Discord, token_env for Telegram - and the lane reads it when it starts.
2. Write the members you mean: (config-set '(\"channels\" \"discord\") '(:bot_token_env \"DISCORD_BOT_TOKEN\" :require_mention t)) - an object merges member by member, so it sets those two and touches nothing else; (config-get '(\"channels\" \"discord\")) reads the section back to check. A present section is enabled; :enabled :false vetoes it. Repeat what config-set answered - the diff - to the operator.
3. (nck:probe \"discord\") checks the token against the platform and lists what the bot can see - its own identity, the servers and channels or the chats that have written to it - so ids are picked by name, never typed from memory; the answer never carries the token. Put the ids the operator names into allowed_channels (Discord) or allowed_chats (Telegram), allowed_users, and owner with config-set, as strings; at least one allowlist must be non-empty or the lane refuses to start, because channel messages run with full host authority.
4. (restart-cells) stops and starts every peripheral from the config as it is now; (cells) and /channels then show the lane's state. A wrong token stops at the probe, before any lane starts.
Rules: write references to secrets, never values; write the members you mean, never a section you read back - a secret reads as \"[set]\" and writing that back over the secret is refused; never echo a token, even one the operator pasted; say the diff."
  "The generic half of (help :channels).")

;;; SETF, never LET: the harness advice reads it on the turn worker.
(defvar *unconfigured* '()
  "The channel ids whose adapter folder is present but whose lane is not
running -- no section, or a refused one -- after the last START-CELL
(cell.lisp): the subjects of the setup primer.")

(defun setup-primer-text (&optional (ids *unconfigured*))
  "The whole block for IDS, or NIL when nothing needs setting up."
  (when ids
    (format nil "~a~{~%~%~a~}"
            +channel-setup-primer+
            (loop for id in ids
                  for declared = (nlk:find-section (list "channels" id))
                  ;; The adapter's declared section as the primer spells it.
                  collect (if declared
                              (nlk:section-text declared)
                              (format nil "channels.~a: the adapter declares no section; ~
                                           (describe 'nodecode-channel-~a:start-channel) ~
                                           names what it reads."
                                      id id))))))
