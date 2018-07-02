;;; gh-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gh-api" "../../../.emacs.d/elpa/gh-20180308.1338/gh-api.el"
;;;;;;  "38881a8bfebe86b28b06d628456603d7")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-api.el

(require 'eieio)

(eieio-defclass-autoload 'gh-api 'nil "gh-api" "Github API")

(eieio-defclass-autoload 'gh-api-v3 '(gh-api) "gh-api" "Github API v3")

(eieio-defclass-autoload 'gh-api-request '(gh-url-request) "gh-api" nil)

(eieio-defclass-autoload 'gh-api-response '(gh-url-response) "gh-api" nil)

(eieio-defclass-autoload 'gh-api-paged-request '(gh-api-request) "gh-api" nil)

(eieio-defclass-autoload 'gh-api-paged-response '(gh-api-response) "gh-api" nil)

(eieio-defclass-autoload 'gh-api-callback '(gh-url-callback) "gh-api" nil)

;;;### (autoloads "actual autoloads are elsewhere" "gh-api" "../../../.emacs.d/elpa/gh-20180308.1338/gh-api.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-api.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-api" '("gh-" "initialize-instance" "logito-log")))

;;;***

;;;***

;;;### (autoloads nil "gh-auth" "../../../.emacs.d/elpa/gh-20180308.1338/gh-auth.el"
;;;;;;  "fb0f53cc635afe64a86525f13eca9dda")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-auth.el

(require 'eieio)

(eieio-defclass-autoload 'gh-authenticator 'nil "gh-auth" "Abstract authenticator")

(eieio-defclass-autoload 'gh-auth-2fa-callback '(gh-url-callback) "gh-auth" "2-factor callback")

(eieio-defclass-autoload 'gh-password-authenticator '(gh-authenticator) "gh-auth" "Password-based authenticator")

(eieio-defclass-autoload 'gh-oauth-authenticator '(gh-authenticator) "gh-auth" "Oauth-based authenticator")

;;;### (autoloads "actual autoloads are elsewhere" "gh-auth" "../../../.emacs.d/elpa/gh-20180308.1338/gh-auth.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-auth.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-auth" '("gh-" "initialize-instance")))

;;;***

;;;***

;;;### (autoloads nil "gh-cache" "../../../.emacs.d/elpa/gh-20180308.1338/gh-cache.el"
;;;;;;  "5b0231f832dc2f22ba8ce21394271406")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-cache.el

(require 'eieio)

(eieio-defclass-autoload 'gh-cache '(pcache-repository) "gh-cache" nil)

(eieio-defclass-autoload 'gh-cache-entry '(pcache-entry) "gh-cache" nil)

;;;### (autoloads "actual autoloads are elsewhere" "gh-cache" "../../../.emacs.d/elpa/gh-20180308.1338/gh-cache.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-cache.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-cache" '("gh-cache-" "pcache-")))

;;;***

;;;***

;;;### (autoloads nil "gh-comments" "../../../.emacs.d/elpa/gh-20180308.1338/gh-comments.el"
;;;;;;  "bea0ca94b0dabfb64d8020e3bb86c8d7")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-comments.el

(require 'eieio)

(eieio-defclass-autoload 'gh-comments-api-mixin 'nil "gh-comments" :abstract)

;;;### (autoloads "actual autoloads are elsewhere" "gh-comments"
;;;;;;  "../../../.emacs.d/elpa/gh-20180308.1338/gh-comments.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-comments.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-comments" '("gh-comments-")))

;;;***

;;;***

;;;### (autoloads nil "gh-common" "../../../.emacs.d/elpa/gh-20180308.1338/gh-common.el"
;;;;;;  "e6058059efc4eb18679c0d30f6a5c5f2")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-common.el

(require 'eieio)

(autoload 'gh-marshal-default-spec "gh-common" "\


\(fn SLOT)" nil nil)

(autoload 'gh-defclass "gh-common" "\


\(fn NAME SUPERCLASS SLOTS &rest OPTIONS-AND-DOC)" nil t)

(gh-defclass gh-object nil nil)

(gh-defclass gh-ref-object (gh-object) ((id :initarg :id) (url :initarg :url) (html-url :initarg :html-url)))

(gh-defclass gh-user (gh-ref-object) ((login :initarg :login) (gravatar-url :initarg :gravatar-url)) "Github user object")

(gh-defclass gh-comment (gh-ref-object) ((body :initarg :body) (user :initarg :user :initform nil :marshal-type gh-user) (created-at :initarg :created_at) (updated-at :initarg :updated_at)) "Github comment object")

;;;### (autoloads "actual autoloads are elsewhere" "gh-common" "../../../.emacs.d/elpa/gh-20180308.1338/gh-common.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-common.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-common" '("gh-" "slot-unbound")))

;;;***

;;;***

;;;### (autoloads nil "gh-gist" "../../../.emacs.d/elpa/gh-20180308.1338/gh-gist.el"
;;;;;;  "2c16b7191a5d8582ffe99424f71439d9")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-gist.el

(require 'eieio)

(eieio-defclass-autoload 'gh-gist-api '(gh-api-v3) "gh-gist" "Gist API")

(gh-defclass gh-gist-gist-stub (gh-object) ((files :initarg :files :type list :initform nil :marshal-type (list gh-gist-gist-file)) (public :initarg :public :marshal-type bool) (description :initarg :description)) "Class for user-created gist objects")

(gh-defclass gh-gist-history-change (gh-object) ((total :initarg :total) (additions :initarg :additions) (deletions :initarg :deletions)))

(gh-defclass gh-gist-history-entry (gh-object) ((user :initarg :user :initform nil :marshal-type gh-user) (version :initarg :version) (committed :initarg :committed :marshal ((alist . committed_at))) (change :initarg :change :marshal ((alist . change_status)) :marshal-type gh-gist-history-change) (url :initarg :url)))

(gh-defclass gh-gist-fork-entry (gh-ref-object) ((user :initarg :user :initform nil :marshal-type gh-user) (created :initarg :created :marshal ((alist . created_at))) (updated :initarg :updated :marshal ((alist . updated_at)))))

(gh-defclass gh-gist-gist (gh-ref-object gh-gist-gist-stub) ((date :initarg :date :marshal ((alist . created_at))) (update :initarg :update :marshal ((alist . updated_at))) (push-url :initarg :push-url :marshal ((alist . git_push_url))) (pull-url :initarg :pull-url :marshal ((alist . git_pull_url))) (comments :initarg :comments) (user :initarg :user :initform nil :marshal-type gh-user :marshal ((alist . owner))) (history :initarg :history :initform nil :type list :marshal-type (list gh-gist-history-entry)) (forks :initarg :forks :initform nil :type list :marshal-type (list gh-gist-fork-entry))) "Gist object")

(gh-defclass gh-gist-gist-file (gh-object) ((filename :initarg :filename) (size :initarg :size) (url :initarg :url :marshal ((alist . raw_url))) (content :initarg :content)))

;;;### (autoloads "actual autoloads are elsewhere" "gh-gist" "../../../.emacs.d/elpa/gh-20180308.1338/gh-gist.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-gist.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-gist" '("gh-gist-" "constructor")))

;;;***

;;;***

;;;### (autoloads nil "gh-issue-comments" "../../../.emacs.d/elpa/gh-20180308.1338/gh-issue-comments.el"
;;;;;;  "da237ed3e14591f5cea7a554657a09ff")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-issue-comments.el

(require 'eieio)

;;;***

;;;### (autoloads nil "gh-issues" "../../../.emacs.d/elpa/gh-20180308.1338/gh-issues.el"
;;;;;;  "097e95bff0f9ed556f6f727e3db7a619")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-issues.el

(require 'eieio)

(eieio-defclass-autoload 'gh-issues-api '(gh-api-v3 gh-comments-api-mixin) "gh-issues" "Github Issues api")

(gh-defclass gh-issues-issue (gh-ref-object) ((number :initarg :number) (state :initarg :state) (title :initarg :title) (body :initarg :body) (user :initarg :user :initform nil :marshal-type gh-user) (labels :initarg :labels :initform nil :marshal-type (list gh-issues-label)) (assignees :initarg :assignees :initform nil :marshal-type (list gh-user)) (assignee :initarg :assignee :initform nil :marshal-type gh-user) (milestone :initarg :milestone :initform nil :marshal-type gh-issues-milestone) (comments :initarg :comments :initform 0) (pull-request :initarg :pull-request :marshal-type gh-issues-pull-request) (closed-at :initarg :closed-at) (created-at :initarg :created-at) (updated-at :initarg :updated-at)) "issues request")

(gh-defclass gh-issues-pull-request (gh-object) ((html-url :initarg :html-url) (diff-url :initarg :diff-url) (patch-url :initarg :patch-url)))

(gh-defclass gh-issues-label (gh-ref-object) ((name :initarg :name) (color :initarg :color)))

(gh-defclass gh-issues-milestone (gh-ref-object) ((number :initarg :number) (state :initarg :state) (title :initarg :title) (description :initarg :description) (creator :initarg :creator :initform nil :marshal-type gh-user) (open-issues :initarg :open-issues) (closed-issues :initarg :closed-issues) (created-at :initarg :created-at) (due-on :initarg :due-on)) "github milestone")

(gh-defclass gh-issues-comment (gh-comment) nil)

;;;### (autoloads "actual autoloads are elsewhere" "gh-issues" "../../../.emacs.d/elpa/gh-20180308.1338/gh-issues.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-issues.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-issues" '("gh-issues-")))

;;;***

;;;***

;;;### (autoloads nil "gh-oauth" "../../../.emacs.d/elpa/gh-20180308.1338/gh-oauth.el"
;;;;;;  "733bfe52a434f43a94204985dc1cf567")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-oauth.el

(require 'eieio)

(eieio-defclass-autoload 'gh-oauth-api '(gh-api-v3) "gh-oauth" "OAuth API")

(eieio-defclass-autoload 'gh-oauth-password-authenticator '(gh-password-authenticator) "gh-oauth" nil)

(gh-defclass gh-oauth-authorization (gh-ref-object) ((scopes :initarg :scopes) (token :initarg :token) (app :initarg :app :initform nil :marshal-type gh-oauth-app) (updated-at :initarg :updated-at) (created-at :initarg :created-at)))

(gh-defclass gh-oauth-app (gh-object) ((url :initarg :url) (name :initarg :name)))

;;;### (autoloads "actual autoloads are elsewhere" "gh-oauth" "../../../.emacs.d/elpa/gh-20180308.1338/gh-oauth.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-oauth.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-oauth" '("gh-oauth-auth-" "initialize-instance")))

;;;***

;;;***

;;;### (autoloads nil "gh-orgs" "../../../.emacs.d/elpa/gh-20180308.1338/gh-orgs.el"
;;;;;;  "1283be0bc7f3385e4caf253d3a54447f")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-orgs.el

(require 'eieio)

(eieio-defclass-autoload 'gh-orgs-api '(gh-api-v3) "gh-orgs" "Orgs API")

(gh-defclass gh-orgs-org-stub (gh-ref-object) ((login :initarg :login) (avatar-url :initarg :avatar-url) (description :initarg :description)))

(gh-defclass gh-orgs-plan (gh-object) ((name :initarg :name) (space :initarg :space) (private-repos :initarg :private-repos)))

(gh-defclass gh-orgs-org (gh-orgs-org-stub) ((name :initarg :name) (company :initarg :company) (blog :initarg :blog) (location :initarg :location) (email :initarg :email) (public-repos :initarg :public-repos) (public-gists :initarg :public-gists) (followers :initarg :followers) (following :initarg :following) (created-at :initarg :created-at) (type :initarg :type) (total-private-repos :initarg :total-private-repos) (owned-private-repos :initarg :owned-private-repos) (private-gists :initarg :private-gists) (disk-usage :initarg :disk-usage) (collaborators :initarg :collaborators) (billing-email :initarg :billing-email) (plan :initarg :plan :initform nil :marshal-type gh-orgs-plan)) "Class for GitHub organizations")

;;;### (autoloads "actual autoloads are elsewhere" "gh-orgs" "../../../.emacs.d/elpa/gh-20180308.1338/gh-orgs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-orgs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-orgs" '("gh-orgs-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "gh-profile" "../../../.emacs.d/elpa/gh-20180308.1338/gh-profile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-profile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-profile" '("gh-profile-")))

;;;***

;;;### (autoloads nil "gh-pull-comments" "../../../.emacs.d/elpa/gh-20180308.1338/gh-pull-comments.el"
;;;;;;  "bb509a96ed0228edca749280f98c4180")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-pull-comments.el

(require 'eieio)

;;;***

;;;### (autoloads nil "gh-pulls" "../../../.emacs.d/elpa/gh-20180308.1338/gh-pulls.el"
;;;;;;  "5d037a6f1dece16c26d13ec944c39213")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-pulls.el

(require 'eieio)

(eieio-defclass-autoload 'gh-pulls-cache '(gh-cache) "gh-pulls" nil)

(eieio-defclass-autoload 'gh-pulls-api '(gh-api-v3 gh-comments-api-mixin) "gh-pulls" "Git pull requests API")

(gh-defclass gh-pulls-comment (gh-comment) ((path :initarg :path) (diff-hunk :initarg :diff-hunk) (position :initarg :position) (original-position :initarg :original-position) (commit-id :initarg :commit-id) (original-commit-id :initarg :original-commit-id) (in-reply-to :initarg :in-reply-to :initform nil)))

(gh-defclass gh-pulls-request-stub (gh-ref-object) ((diff-url :initarg :diff-url) (patch-url :initarg :patch-url) (issue-url :initarg :issue-url) (number :initarg :number) (state :initarg :state) (title :initarg :title) (body :initarg :body) (created-at :initarg :created-at) (updated-at :initarg :updated-at) (closed-at :initarg :closed-at) (merged-at :initarg :merged-at) (head :initarg :head :initform nil :marshal-type gh-repos-ref) (base :initarg :base :initform nil :marshal-type gh-repos-ref)))

(gh-defclass gh-pulls-request (gh-pulls-request-stub) ((merged :initarg :merged) (mergeable :initarg :mergeable) (merged-by :initarg :merged-by) (comments :initarg :comments) (user :initarg :user :initform nil :marshal-type gh-user) (commits :initarg :commits) (additions :initarg :additions) (deletions :initarg :deletions) (changed-files :initarg :changed-files)) "Git pull requests API")

;;;### (autoloads "actual autoloads are elsewhere" "gh-pulls" "../../../.emacs.d/elpa/gh-20180308.1338/gh-pulls.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-pulls.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-pulls" '("gh-pulls-")))

;;;***

;;;***

;;;### (autoloads nil "gh-repos" "../../../.emacs.d/elpa/gh-20180308.1338/gh-repos.el"
;;;;;;  "f77e384c3065dbbb0b3f98f7f4b3b535")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-repos.el

(require 'eieio)

(eieio-defclass-autoload 'gh-repos-api '(gh-api-v3) "gh-repos" "Repos API")

(gh-defclass gh-repos-repo-stub (gh-object) ((name :initarg :name) (description :initarg :description) (homepage :initarg :homepage) (private :initarg :private)) "Class for user-created repository objects")

(gh-defclass gh-repos-repo (gh-ref-object gh-repos-repo-stub) ((clone-url :initarg :clone-url) (git-url :initarg :git-url) (ssh-url :initarg :ssh-url) (svn-url :initarg :svn-url) (mirror-url :initarg :mirror-url) (owner :initarg :owner :initform nil :marshal-type gh-user) (full-name :initarg :full-name) (language :initarg :language) (fork :initarg :fork) (forks :initarg :forks) (forks-count :initarg :forks-count) (watchers :initarg :watchers) (watchers-count :initarg :watchers-count) (stargazers-count :initarg :stargazers-count) (size :initarg :size) (master-branch :initarg :master-branch) (open-issues :initarg :open-issues) (pushed-at :initarg :pushed-at) (created-at :initarg :created-at) (updated-at :initarg :updated-at) (organisation :initarg :organisation :initform nil :marshal-type gh-user) (parent :initarg :parent :marshal-type gh-repos-repo) (source :initarg :source :marshal-type gh-repos-repo) (has-issues :initarg :has-issues) (has-wiki :initarg :has-wiki) (has-downloads :initarg :has-downloads)) "Class for GitHub repositories")

(gh-defclass gh-repos-ref (gh-object) ((label :initarg :label) (ref :initarg :ref :initform nil) (sha :initarg :sha :initform nil) (user :initarg :user :initform nil :marshal-type gh-user) (repo :initarg :repo :initform nil :marshal-type gh-repos-repo)))

;;;### (autoloads "actual autoloads are elsewhere" "gh-repos" "../../../.emacs.d/elpa/gh-20180308.1338/gh-repos.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-repos.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-repos" '("gh-repos-")))

;;;***

;;;***

;;;### (autoloads nil "gh-search" "../../../.emacs.d/elpa/gh-20180308.1338/gh-search.el"
;;;;;;  "c773b865ecf881befc9f9b7094603ec5")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-search.el

(eieio-defclass-autoload 'gh-search-api '(gh-api-v3) "gh-search" nil)

;;;### (autoloads "actual autoloads are elsewhere" "gh-search" "../../../.emacs.d/elpa/gh-20180308.1338/gh-search.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-search.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-search" '("gh-search-")))

;;;***

;;;***

;;;### (autoloads nil "gh-url" "../../../.emacs.d/elpa/gh-20180308.1338/gh-url.el"
;;;;;;  "a81ed904c8e6ac5736a66152063de0f9")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-url.el

(require 'eieio)

(eieio-defclass-autoload 'gh-url-request 'nil "gh-url" nil)

(eieio-defclass-autoload 'gh-url-response 'nil "gh-url" nil)

(eieio-defclass-autoload 'gh-url-callback 'nil "gh-url" nil)

;;;### (autoloads "actual autoloads are elsewhere" "gh-url" "../../../.emacs.d/elpa/gh-20180308.1338/gh-url.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-url.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-url" '("gh-url-")))

;;;***

;;;***

;;;### (autoloads nil "gh-users" "../../../.emacs.d/elpa/gh-20180308.1338/gh-users.el"
;;;;;;  "adfe30f405ca1a42c18748882ebed4fc")
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-users.el

(require 'eieio)

(eieio-defclass-autoload 'gh-users-api '(gh-api-v3) "gh-users" "Users API")

(gh-defclass gh-users-user (gh-user) ((gravatar-id :initarg :gravatar-id) (html-url :initarg :html-url) (followers-url :initarg :followers-url) (following-url :initarg :following-url) (gists-url :initarg :gists-url) (starred-url :initarg :starred-url) (subscriptions-url :initarg :subscriptions-url) (organizations-url :initarg :organizations-url) (repos-url :initarg :repos-url) (events-url :initarg :events-url) (received-events-url :initarg :received-events-url) (type :initarg :type) (site-admin :initarg :site-admin) (name :initarg :name) (company :initarg :company) (blog :initarg :blog) (location :initarg :location) (email :initarg :email) (hireable :initarg :hireable) (bio :initarg :bio) (public-repos :initarg :public-repos) (public-gists :initarg :public-gists) (followers :initarg :followers) (following :initarg :following) (created-at :initarg :created-at) (update-at :initarg :update-at)))

;;;### (autoloads "actual autoloads are elsewhere" "gh-users" "../../../.emacs.d/elpa/gh-20180308.1338/gh-users.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gh-20180308.1338/gh-users.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gh-users" '("gh-users-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/gh-20180308.1338/gh-api.el"
;;;;;;  "../../../.emacs.d/elpa/gh-20180308.1338/gh-auth.el" "../../../.emacs.d/elpa/gh-20180308.1338/gh-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/gh-20180308.1338/gh-cache.el" "../../../.emacs.d/elpa/gh-20180308.1338/gh-comments.el"
;;;;;;  "../../../.emacs.d/elpa/gh-20180308.1338/gh-common.el" "../../../.emacs.d/elpa/gh-20180308.1338/gh-gist.el"
;;;;;;  "../../../.emacs.d/elpa/gh-20180308.1338/gh-issue-comments.el"
;;;;;;  "../../../.emacs.d/elpa/gh-20180308.1338/gh-issues.el" "../../../.emacs.d/elpa/gh-20180308.1338/gh-oauth.el"
;;;;;;  "../../../.emacs.d/elpa/gh-20180308.1338/gh-orgs.el" "../../../.emacs.d/elpa/gh-20180308.1338/gh-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/gh-20180308.1338/gh-profile.el" "../../../.emacs.d/elpa/gh-20180308.1338/gh-pull-comments.el"
;;;;;;  "../../../.emacs.d/elpa/gh-20180308.1338/gh-pulls.el" "../../../.emacs.d/elpa/gh-20180308.1338/gh-repos.el"
;;;;;;  "../../../.emacs.d/elpa/gh-20180308.1338/gh-search.el" "../../../.emacs.d/elpa/gh-20180308.1338/gh-url.el"
;;;;;;  "../../../.emacs.d/elpa/gh-20180308.1338/gh-users.el" "../../../.emacs.d/elpa/gh-20180308.1338/gh.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gh-autoloads.el ends here
