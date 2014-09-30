;;; private.el --- test for private functions

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest collect-added-headers ()
  "collect added headers"
  (with-c++-temp-buffer
    "
std::map<int, int> m;
std::string s;

gets();
assert  (true);
"
    (let* ((info (cpp-auto-include--parse-file))
           (added-headers (plist-get info :added)))
      (should added-headers)
      (should (= (length added-headers) 4))

      (dolist (expect '("map" "string" "cstring" "cassert"))
        (member expect added-headers)))))

(ert-deftest collect-removed-headers ()
  "collect removed headers"
  (with-c++-temp-buffer
    "
#include <map>
#include <string>
"
    (let* ((info (cpp-auto-include--parse-file))
           (removed-headers (plist-get info :removed)))
      (should removed-headers)
      (should (= (length removed-headers) 2)))))

(ert-deftest added-headers ()
  "Add necessary headers"
  (with-c++-temp-buffer
    "
std::istream = std::cin;
std::deque<double> q;
std::list<std::list<double>> lst;
std::vector<std::string> v;
std::pair<int, float> p;
"
    (let* ((info (cpp-auto-include--parse-file))
           (added-headers (plist-get info :added)))
      (cpp-auto-include--add-headers added-headers)
      (dolist (expect '("iostream" "deque" "list" "vector" "utility"))
        (let ((word (concat "#include <" expect">")))
          (goto-char (point-min))
          (should (search-forward word nil t)))))))

(ert-deftest remove-headers ()
  "remove needless headers"
  (with-c++-temp-buffer
    "
#include <map>
#include <complex>
#include <string>

std::complex<double, double> c;
"
    (let* ((info (cpp-auto-include--parse-file))
           (removed-headers (plist-get info :removed)))
      (cpp-auto-include--remove-headers removed-headers)
      (goto-char (point-min))
      (should-not (search-forward "#include <map>" nil t))
      (goto-char (point-min))
      (should-not (search-forward "#include <string>" nil t))
      (goto-char (point-min))
      (should (search-forward "#include <complex>" nil t)))))

;;; private.el ends here
