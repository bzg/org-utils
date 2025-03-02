#!/usr/bin/env bb

;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(require '[clojure.string :as str]
         '[babashka.cli :as cli])

(def cli-options
  {:input  {:desc  "Input file path"
            :alias :i}
   :output {:desc  "Output file path (defaults to stdout if not provided)"
            :alias :o}
   :help   {:desc  "Show help"
            :alias :h}})

(defn show-help []
  (println "Unwrap Paragraphs and List Items in Org-mode Files")
  (println)
  (println "Usage: org-unwrap [options] [file]")
  (println "       org-unwrap -i input.org [-o output.org]")
  (println "       org-unwrap --input input.org [--output output.org]")
  (println)
  (println "Options:")
  (println "  -i, --input FILE    Input file path")
  (println "  -o, --output FILE   Output file path (defaults to stdout if not provided)")
  (println "  -h, --help          Show this help message")
  (println)
  (println "If a file is provided without options, it will be used as the input file")
  (println "and the result will be printed to stdout."))

;; Add enumerated list item detection
(defn list-item? [line]
  (boolean (or
            ;; Bullet list items (-, +, *)
            (re-matches #"^\s*[-+*]\s+.*" line)
            ;; Enumerated list items (1., 2., etc.)
            (re-matches #"^\s*\d+\.\s+.*" line))))

(defn org-headline? [line]
  (boolean (re-matches #"^\s*\*+\s+.*" line)))

(defn metadata-line? [line]
  (boolean (re-matches #"^\s*#\+.*" line)))

;; Add comment line detection
(defn comment-line? [line]
  (boolean (re-matches #"^\s*#.*$" line)))

;; Update property line detection
(defn property-line? [line]
  (boolean (or
            (re-matches #"^\s*:[^:]+:\s.*$" line)
            (re-matches #"^\s*:.*:$" line)
            (re-matches #"^\s*:\s.*$" line))))

;; Add table line detection
(defn table-line? [line]
  (boolean (re-matches #"^\s*\|.*" line)))

;; Add block begin/end detection
(defn block-begin? [line]
  (boolean (re-matches #"(?i)^\s*#\+begin.*" line)))

(defn block-end? [line]
  (boolean (re-matches #"(?i)^\s*#\+end.*" line)))

(defn empty-line? [line]
  (str/blank? line))

;; Add continuation line detection
(defn continuation-line? [line]
  (boolean (and (re-matches #"^\s+.*" line)
                (not (list-item? line))
                (not (re-matches #"^\s*[-+*\d]" line)))))

(defn should-append? [current-line next-line in-block]
  (cond
    ;; Don't append if we're in a block
    in-block                                                       false
    ;; Don't append if current line is empty
    (empty-line? current-line)                                     false
    ;; Don't append if next line is empty
    (empty-line? next-line)                                        false
    ;; Don't append if next line is a comment
    (comment-line? next-line)                                      false
    ;; Don't append if current line is a comment
    (comment-line? current-line)                                   false
    ;; Don't append if next line is a table row
    (table-line? next-line)                                        false
    ;; Don't append if current line is a table row
    (table-line? current-line)                                     false
    ;; Don't append if next line starts with a block marker
    (block-begin? next-line)                                       false
    ;; Don't append to headlines
    (org-headline? current-line)                                   false
    ;; Don't append to metadata
    (metadata-line? current-line)                                  false
    ;; Don't append to property drawers or from property lines
    (property-line? current-line)                                  false
    ;; Don't append if next line is a property
    (property-line? next-line)                                     false
    ;; Don't append if next line is a headline
    (org-headline? next-line)                                      false
    ;; Don't append if next line is metadata
    (metadata-line? next-line)                                     false
    ;; Don't append to a list item if the next line is a new list item
    (and (list-item? current-line) (list-item? next-line))         false
    ;; Special case: DO append continuation lines to list items
    (and (list-item? current-line) (continuation-line? next-line)) true
    ;; Don't append if next line starts with whitespace (could be code blocks)
    ;; BUT we DO want to append if it's a continuation line
    (and (re-matches #"^\s+.*" next-line)
         (not (continuation-line? next-line)))                     false
    ;; The default case: append
    :else                                                          true))

(defn unwrap-text [input]
  (let [lines (str/split-lines input)]
    (loop [result    []
           remaining lines
           in-block  false]
      (if (empty? remaining)
        ;; Done processing, return the result
        (str/join "\n" result)
        (let [current    (first remaining)
              rest-lines (rest remaining)
              next-line  (first rest-lines)]
          (cond
            ;; Block begin/end handling
            (block-begin? current)
            (recur (conj result current) rest-lines true)
            (block-end? current)
            (recur (conj result current) rest-lines false)
            ;; If next line doesn't exist or we shouldn't append
            (or (nil? next-line) (not (should-append? current next-line in-block)))
            (recur (conj result current) rest-lines in-block)
            ;; Append the next line to the current one
            :else
            (let [;; Normalize whitespace when joining lines
                  trimmed-next    (str/trim next-line)
                  ;; When joining list items, normalize internal whitespace too
                  normalized-next (if (list-item? current)
                                    (str/replace trimmed-next #"\s+" " ")
                                    trimmed-next)
                  new-current     (str current " " normalized-next)]
              (recur result (cons new-current (rest rest-lines)) in-block))))))))

(defn process-file [opts]
  (if (nil? (:input opts))
    (show-help)
    (let [input-path    (:input opts)
          output-path   (:output opts)
          input-content (try
                          (slurp input-path)
                          (catch Exception e
                            (println "Error: Could not read file" input-path ":" (.getMessage e))
                            (System/exit 1)))
          result        (unwrap-text input-content)]
      (if output-path
        (do
          (spit output-path result)
          (println "Processed file saved to:" output-path))
        ;; Print to stdout if no output file is specified
        (println result)))))

(defn -main [& args]
  (if (empty? args)
    (show-help)
    (let [first-arg   (first args)
          is-help     (or (= first-arg "-h") (= first-arg "--help"))
          parsed-opts (cli/parse-opts args {:spec cli-options})
          ;; Handle the case where the first arg is a file without a flag
          final-opts  (if (and (not is-help)
                               (not (str/starts-with? first-arg "-"))
                               (not (:input parsed-opts)))
                        (assoc parsed-opts :input first-arg)
                        parsed-opts)]
      (cond
        is-help            (show-help)
        (:help final-opts) (show-help)
        :else              (process-file final-opts)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
