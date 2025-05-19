;;; chatgpt-shell-openai.el --- OpenAI-specific logic  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Package-Requires: ((emacs "28.1") (shell-maker "0.72.1"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds OpenAI specifics for `chatgpt-shell'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'shell-maker)

(declare-function chatgpt-shell-crop-context "chatgpt-shell")
(declare-function chatgpt-shell--make-chatgpt-url "chatgpt-shell")
(declare-function chatgpt-shell-validate-no-system-prompt "chatgpt-shell")

;; See https://platform.openai.com/docs/guides/reasoning
(defcustom chatgpt-shell-openai-reasoning-effort "medium"
  "The amount of reasoning effort to use for OpenAI reasoning models.

 It can be \"low\", \"medium\" or \"high\". Lower values
are faster and cheaper but higher values may work better for more
difficult problems."
  :type 'string
  :safe #'stringp
  :options '("low" "medium" "high")
  :group 'chatgpt-shell)

(cl-defun chatgpt-shell-openai-make-model (&key version short-version token-width context-window validate-command (headers #'chatgpt-shell-openai--make-headers) (key chatgpt-shell-openai-key) (url-base 'chatgpt-shell-api-url-base) (path "/v1/chat/completions") (provider "OpenAI") (label "ChatGPT") (handler #'chatgpt-shell-openai--handle-chatgpt-command) (filter #'chatgpt-shell-openai--filter-output) reasoning-effort icon other-params)
  "Create an OpenAI model.

Set VERSION, SHORT-VERSION, TOKEN-WIDTH, CONTEXT-WINDOW,
VALIDATE-COMMAND, HEADERS, KEY, URL-BASE, PATH, PROVIDER, LABEL,
HANDLER, REASONING-EFFORT, FILTER, ICON, and OTHER-PARAMS."
  (unless version
    (error "Missing mandatory :version param"))
  (unless token-width
    (error "Missing mandatory :token-width param for %s" version))
  (unless context-window
    (error "Missing mandatory :context-window param for %s" version))
  (unless (integerp token-width)
    (error ":token-width must be an integer"))
  (unless (integerp context-window)
    (error ":context-window must be an integer"))
  (append `((:version . ,version)
            (:short-version . ,short-version)
            (:label . ,label)
            (:provider . ,provider)
            (:path . ,path)
            (:token-width . ,token-width)
            (:context-window . ,context-window)
            (:handler . ,handler)
            (:filter . ,filter)
            (:payload . chatgpt-shell-openai--make-payload)
            (:headers . ,headers)
            (:url . chatgpt-shell-openai--make-url)
            (:key . ,key)
            (:reasoning-effort . ,reasoning-effort)
            (:url-base . ,url-base)
            (:validate-command . ,(or validate-command 'chatgpt-shell-openai--validate-command))
            (:other-params . ,other-params)
            (:icon . ,(or icon "openai.png")))))

(defun chatgpt-shell-openai-models ()
  "Build a list of all OpenAI LLM models available."
  ;; Context windows have been verified as of 11/26/2024.
  (list (chatgpt-shell-openai-make-model
         :version "gpt-4.1"
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-4.1
         :context-window 1047576)
        (chatgpt-shell-openai-make-model
         :version "gpt-4.1-mini"
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-4.1-mini
         :context-window 1047576)
        (chatgpt-shell-openai-make-model
         :version "gpt-4.1-nano"
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-4.1-nano
         :context-window 1047576)
        (chatgpt-shell-openai-make-model
         :version "chatgpt-4o-latest"
         :token-width 3
         ;; https://platform.openai.com/docs/models/chatgpt-4o-latest
         :context-window 128000)
        (chatgpt-shell-openai-make-model
         :version "gpt-4o"
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-4o
         :context-window 128000)
        (chatgpt-shell-openai-make-model
         :version "gpt-4o-search-preview"
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-4o-search-preview
         :context-window 128000)
        (chatgpt-shell-openai-make-model
         :version "gpt-4o-mini"
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-4o-mini
         :context-window 128000)
        (chatgpt-shell-openai-make-model
         :version "gpt-4o-mini-search-preview"
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-4o-mini-search-preview
         :context-window 128000)
        (chatgpt-shell-openai-make-model
         :version "o4-mini"
         :token-width 3
         :context-window 200000
         :reasoning-effort t
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openai-make-model
         :version "o3"
         :token-width 3
         :context-window 200000
         :reasoning-effort t
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openai-make-model
         :version "o3-mini"
         :token-width 3
         :context-window 200000
         :reasoning-effort t
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openai-make-model
         :version "o1"
         :token-width 3
         ;; https://platform.openai.com/docs/models/o1
         :context-window 200000
         :reasoning-effort t
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openai-make-model
         :version "o1-preview"
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-01
         :context-window 128000
         ;; Reasoning effort is only supported for o1-pro, o1 and o3-mini.
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openai-make-model
         :version "o1-mini"
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-01-mini
         :context-window 128000
         ;; Reasoning effort is only supported for o1 and o3-mini.
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openai-make-model
         :version "gpt-4.5-preview"
         :token-width 3
         ;; https://platform.openai.com/docs/models#gpt-4-5
         :context-window 128000)
        (chatgpt-shell-openai-make-model
         :version "gpt-3.5-turbo"
         :token-width 4
         ;; https://platform.openai.com/docs/models/gpt-3.5-turbo#gpt-3-5-turbo
         :context-window 16385)))

(defcustom chatgpt-shell-api-url-base "https://api.openai.com"
  "OpenAI API's base URL.

API url = base + path.

If you use ChatGPT through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(cl-defun chatgpt-shell-openai--make-chatgpt-messages (&key model system-prompt prompt prompt-url context)
  "Create ChatGPT messages using MODEL.

SYSTEM-PROMPT: string.

PROMPT: string.

PROMPT-URL: string.

CONTEXT: Excludes PROMPT."
  (when prompt-url
    (setq prompt-url (chatgpt-shell--make-chatgpt-url prompt-url)))
  (vconcat
   (when system-prompt
     `(((role . "system")
        (content . ,system-prompt))))
   (when context
     (chatgpt-shell-openai--user-assistant-messages
      (if model
          (chatgpt-shell-crop-context
           :model model
           :command prompt
           :context context)
        context)))
   (when (or prompt
             prompt-url)
     `(((role . "user")
        (content . ,(vconcat
                     (append
                      (when prompt
                        `(((type . "text")
                           (text . ,prompt))))
                      (when prompt-url
                        `(((type . "image_url")
                           (image_url . ((url . ,prompt-url))))))))))))))

(defun chatgpt-shell-openai-key ()
  "Get the ChatGPT key."
  (cond ((stringp chatgpt-shell-openai-key)
         chatgpt-shell-openai-key)
        ((functionp chatgpt-shell-openai-key)
         (condition-case _err
             (funcall chatgpt-shell-openai-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(cl-defun chatgpt-shell-openai-make-chatgpt-request-data (&key system-prompt prompt prompt-url context version temperature reasoning-effort streaming other-params)
  "Make request data with MESSAGES.

Optionally set PROMPT, VERSION, TEMPERATURE, STREAMING, SYSTEM-PROMPT,
and OTHER-PARAMS (list)."
  (unless version
    (error "Missing mandatory :version param"))
  (append
   `((model . ,version)
     (messages . ,(vconcat (chatgpt-shell-openai--make-chatgpt-messages
                            :system-prompt system-prompt
                            :prompt prompt
                            :prompt-url prompt-url
                            :context context))))
   (when temperature
     `((temperature . ,temperature)))
   (when reasoning-effort
     `((reasoning_effort . ,reasoning-effort)))
   (when streaming
     `((stream . t)))
   other-params))

(defun chatgpt-shell-openai--filter-output (raw-response)
  "Extract ChatGPT response from RAW-RESPONSE.

When ChatGPT responses are streamed, they arrive in the form:

  data: {...json...}
  data: {...jdon...}

Otherwise:

  {...json...}."
  (if-let* ((whole (shell-maker--json-parse-string raw-response))
            (response (or (let-alist whole
                            .error.message)
                          (let-alist whole
                            (mapconcat (lambda (choice)
                                         (let-alist choice
                                           (or .delta.content
                                               .message.content)))
                                       .choices "")))))
      response
    (when-let ((chunks (shell-maker--split-text raw-response)))
      (let ((response)
            (pending)
            (result))
        (mapc (lambda (chunk)
                ;; Response chunks come in the form:
                ;;   data: {...}
                ;;   data: {...}
                (if-let* ((is-data (equal (map-elt chunk :key) "data:"))
                          (obj (shell-maker--json-parse-string (map-elt chunk :value)))
                          (text (or
                                 ;; .choices[i].message.content
                                 ;; .choices[i].delta.content
                                 (let-alist obj
                                   (mapconcat (lambda (choice)
                                                (let-alist choice
                                                  (or (and (not (eq .delta.content :null))
                                                           .delta.content)
                                                      .message.content
                                                      "")))
                                              .choices "")))))
                    (unless (string-empty-p text)
                      (setq response (concat response text)))
                  (setq pending (concat pending
                                        (or (map-elt chunk :key) "")
                                        (map-elt chunk :value)))))
              chunks)
        (setq result
              (list (cons :filtered (unless (string-empty-p response)
                                      response))
                    (cons :pending pending)))
        result))))

(cl-defun chatgpt-shell-openai--make-url (&key _command model _settings)
  "Create the API URL using MODEL."
  (concat (symbol-value (or (map-elt model :url-base)
                            (error "Model :url-base not found")))
          (or (map-elt model :path)
              (error "Model :path not found"))))

(cl-defun chatgpt-shell-openai--make-headers (&key _model _settings (key #'chatgpt-shell-openai-key))
  "Create the API headers using KEY as the API KEY."
  (list "Content-Type: application/json; charset=utf-8"
        (format "Authorization: Bearer %s" (funcall key))))

(defun chatgpt-shell-openai--validate-command (_command _model _settings)
  "Return error string if command/setup isn't valid."
  (unless chatgpt-shell-openai-key
    "Variable `chatgpt-shell-openai-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-openai-key

or

(setq chatgpt-shell-openai-key \"my-key\")"))

(cl-defun chatgpt-shell-openai--make-payload (&key model context settings)
  "Create the API payload using MODEL CONTEXT and SETTINGS."
  (funcall
   #'chatgpt-shell-openai-make-chatgpt-request-data
   :system-prompt (map-elt settings :system-prompt)
   :context context
   :version (map-elt model :version)
   :temperature (map-elt settings :temperature)
   :reasoning-effort (and (map-elt model :reasoning-effort)
                          chatgpt-shell-openai-reasoning-effort)
   :streaming (map-elt settings :streaming)
   :other-params (map-elt model :other-params)))

(cl-defun chatgpt-shell-openai--handle-chatgpt-command (&key model command context shell settings (key #'chatgpt-shell-openai-key) (filter #'chatgpt-shell-openai--filter-output) (missing-key-msg "Your chatgpt-shell-openai-key is missing"))
  "Handle ChatGPT COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (unless (funcall key)
    (funcall (map-elt shell :write-output) missing-key-msg)
    (funcall (map-elt shell :finish-output) nil))
  (shell-maker-make-http-request
   :async t
   :url (chatgpt-shell-openai--make-url :model model)
   :proxy chatgpt-shell-proxy
   :data (chatgpt-shell-openai-make-chatgpt-request-data
          :prompt command
          :system-prompt (map-elt settings :system-prompt)
          :context context
          :version (map-elt model :version)
          :temperature (map-elt settings :temperature)
          :reasoning-effort (and (map-elt model :reasoning-effort)
                                 chatgpt-shell-openai-reasoning-effort)
          :streaming (map-elt settings :streaming)
          :other-params (map-elt model :other-params))
   :headers (list "Content-Type: application/json; charset=utf-8"
                  (format "Authorization: Bearer %s" (funcall key)))
   :filter filter
   :shell shell))

(defun chatgpt-shell-openai--user-assistant-messages (history)
  "Convert HISTORY to ChatGPT format.

Sequence must be a vector for json serialization.

For example:

 [
   ((role . \"user\") (content . \"hello\"))
   ((role . \"assistant\") (content . \"world\"))
 ]"
  (let ((result))
    (mapc
     (lambda (item)
       (when (car item)
         (push (list (cons 'role "user")
                     (cons 'content (car item))) result))
       (when (cdr item)
         (push (list (cons 'role "assistant")
                     (cons 'content (cdr item))) result)))
     history)
    (nreverse result)))

(provide 'chatgpt-shell-openai)

;;; chatgpt-shell-openai.el ends here
