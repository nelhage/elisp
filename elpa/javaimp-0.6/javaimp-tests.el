;;; javaimp-tests.el --- javaimp module tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'javaimp)

(ert-deftest javaimp-test--maven-xml-extract-projects--project ()
  (with-temp-buffer
    (insert "<project/>")
    (let ((projects (javaimp--maven-xml-extract-projects
		     (xml-parse-region (point-min) (point-max)))))
      (should (eql (length projects) 1)))))

(ert-deftest javaimp-test--maven-xml-extract-projects--projects ()
  (with-temp-buffer
    (insert "<projects><project/><project/></projects>")
    (let ((projects (javaimp--maven-xml-extract-projects
		     (xml-parse-region (point-min) (point-max)))))
      (should (eql (length projects) 2)))))
